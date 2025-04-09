
local function load_json() 
  local json = {_version = "0.2.0"}

-------------------------------------------------------------------------------
-- Encode
-------------------------------------------------------------------------------

local encode

local escape_char_map = {
    ["\\"] = "\\",
    ["\""] = "\"",
    ["\b"] = "b",
    ["\f"] = "f",
    ["\n"] = "n",
    ["\r"] = "r",
    ["\t"] = "t"
}

local escape_char_map_inv = {["/"] = "/"}
for k, v in pairs(escape_char_map) do escape_char_map_inv[v] = k end

local function escape_char(c)
    return "\\" .. (escape_char_map[c] or string.format("u%04x", c:byte()))
end

local function encode_nil(val) return "null" end

local function encode_table(val, stack)
    local res = {}
    stack = stack or {}

    -- Circular reference?
    if stack[val] then error("circular reference") end

    stack[val] = true

    if rawget(val, 1) ~= nil or next(val) == nil then
        -- Treat as array -- check keys are valid and it is not sparse
        local n = 0
        for k in pairs(val) do
            if type(k) ~= "number" then
                error("invalid table: mixed or invalid key types")
            end
            n = n + 1
        end
        if n ~= #val then error("invalid table: sparse array") end
        -- Encode
        for i = 1, #val do res[i] = encode(val[i], stack) end
        stack[val] = nil
        return "[" .. table.concat(res, ",") .. "]"
    else
        -- Treat as an object
        local i = 1
        for k, v in pairs(val) do
            if type(k) ~= "string" then
                error("invalid table: mixed or invalid key types")
            end
            if type(v) ~= "function" then
                res[i] = encode(k, stack) .. ":" .. encode(v, stack)
                i = i + 1
            end
        end
        stack[val] = nil
        return "{" .. table.concat(res, ",") .. "}"
    end
end

local function encode_string(val)
    return '"' .. val:gsub('[%z\1-\31\\"]', escape_char) .. '"'
end

local function encode_number(val)
    -- Check for NaN, -inf and inf
    if val ~= val or val <= -math.huge or val >= math.huge then
        error("unexpected number value '" .. tostring(val) .. "'")
    end
    return string.format("%.14g", val)
end

local type_func_map = {
    ["nil"] = encode_nil,
    ["table"] = encode_table,
    ["string"] = encode_string,
    ["number"] = encode_number,
    ["boolean"] = tostring
}

encode = function(val, stack)
    local t = type(val)
    local f = type_func_map[t]
    if f then return f(val, stack) end
    error("unexpected type '" .. t .. "'")
end

function json.encode(val) return (encode(val)) end

-------------------------------------------------------------------------------
-- Decode
-------------------------------------------------------------------------------

local parse

local function create_set(...)
    local res = {}
    for i = 1, select("#", ...) do res[select(i, ...)] = true end
    return res
end

local space_chars = create_set(" ", "\t", "\r", "\n")
local delim_chars = create_set(" ", "\t", "\r", "\n", "]", "}", ",")
local escape_chars = create_set("\\", "/", '"', "b", "f", "n", "r", "t", "u")
local literals = create_set("true", "false", "null")

local literal_map = {["true"] = true, ["false"] = false, ["null"] = nil}

local function next_char(str, idx, set, negate)
    for i = idx, #str do if set[str:sub(i, i)] ~= negate then return i end end
    return #str + 1
end

local function decode_error(str, idx, msg)
    local line_count = 1
    local col_count = 1
    for i = 1, idx - 1 do
        col_count = col_count + 1
        if str:sub(i, i) == "\n" then
            line_count = line_count + 1
            col_count = 1
        end
    end
    error(string.format("%s at line %d col %d", msg, line_count, col_count))
end

local function codepoint_to_utf8(n)
    local f = math.floor
    if n <= 0x7f then
        return string.char(n)
    elseif n <= 0x7ff then
        return string.char(f(n / 64) + 192, n % 64 + 128)
    elseif n <= 0xffff then
        return string.char(f(n / 4096) + 224, f(n % 4096 / 64) + 128,
                           n % 64 + 128)
    elseif n <= 0x10ffff then
        return string.char(f(n / 262144) + 240, f(n % 262144 / 4096) + 128,
                           f(n % 4096 / 64) + 128, n % 64 + 128)
    end
    error(string.format("invalid unicode codepoint '%x'", n))
end

local function parse_unicode_escape(s)
    local n1 = tonumber(s:sub(1, 4), 16)
    local n2 = tonumber(s:sub(7, 10), 16)
    if n2 then
        return
            codepoint_to_utf8((n1 - 0xd800) * 0x400 + (n2 - 0xdc00) + 0x10000)
    else
        return codepoint_to_utf8(n1)
    end
end

local function parse_string(str, i)
    local res = {}
    local j = i + 1
    local k = j

    while j <= #str do
        local x = str:byte(j)

        if x < 32 then
            decode_error(str, j, "control character in string")
        elseif x == 92 then -- `\`: Escape
            res[#res + 1] = str:sub(k, j - 1)
            j = j + 1
            local c = str:sub(j, j)
            if c == "u" then
                local hex = str:match("^[dD][89aAbB]%x%x\\u%x%x%x%x", j + 1) or
                                str:match("^%x%x%x%x", j + 1) or
                                decode_error(str, j - 1,
                                             "invalid unicode escape in string")
                res[#res + 1] = parse_unicode_escape(hex)
                j = j + #hex
            else
                if not escape_chars[c] then
                    decode_error(str, j - 1,
                                 "invalid escape char '" .. c .. "' in string")
                end
                res[#res + 1] = escape_char_map_inv[c]
            end
            k = j + 1
        elseif x == 34 then -- `"`: End of string
            res[#res + 1] = str:sub(k, j - 1)
            return table.concat(res), j + 1
        end
        j = j + 1
    end

    decode_error(str, i, "expected closing quote for string")
end

local function parse_number(str, i)
    local x = next_char(str, i, delim_chars)
    local s = str:sub(i, x - 1)
    local n = tonumber(s)
    if not n then decode_error(str, i, "invalid number '" .. s .. "'") end
    return n, x
end

local function parse_literal(str, i)
    local x = next_char(str, i, delim_chars)
    local word = str:sub(i, x - 1)
    if not literals[word] then
        decode_error(str, i, "invalid literal '" .. word .. "'")
    end
    return literal_map[word], x
end

local function parse_array(str, i)
    local res = {}
    local n = 1
    i = i + 1
    while true do
        local x
        i = next_char(str, i, space_chars, true)
        if str:sub(i, i) == "]" then
            i = i + 1
            break
        end
        x, i = parse(str, i)
        res[n] = x
        n = n + 1
        i = next_char(str, i, space_chars, true)
        local chr = str:sub(i, i)
        i = i + 1
        if chr == "]" then break end
        if chr ~= "," then decode_error(str, i, "expected ']' or ','") end
    end
    return res, i
end

local function parse_object(str, i)
    local res = {}
    i = i + 1
    while true do
        local key, val
        i = next_char(str, i, space_chars, true)
        if str:sub(i, i) == "}" then
            i = i + 1
            break
        end
        if str:sub(i, i) ~= '"' then
            decode_error(str, i, "expected string for key")
        end
        key, i = parse(str, i)
        i = next_char(str, i, space_chars, true)
        if str:sub(i, i) ~= ":" then
            decode_error(str, i, "expected ':' after key")
        end
        i = next_char(str, i + 1, space_chars, true)
        val, i = parse(str, i)
        res[key] = val
        i = next_char(str, i, space_chars, true)
        local chr = str:sub(i, i)
        i = i + 1
        if chr == "}" then break end
        if chr ~= "," then decode_error(str, i, "expected '}' or ','") end
    end
    return res, i
end

local char_func_map = {
    ['"'] = parse_string,
    ["0"] = parse_number,
    ["1"] = parse_number,
    ["2"] = parse_number,
    ["3"] = parse_number,
    ["4"] = parse_number,
    ["5"] = parse_number,
    ["6"] = parse_number,
    ["7"] = parse_number,
    ["8"] = parse_number,
    ["9"] = parse_number,
    ["-"] = parse_number,
    ["t"] = parse_literal,
    ["f"] = parse_literal,
    ["n"] = parse_literal,
    ["["] = parse_array,
    ["{"] = parse_object
}

parse = function(str, idx)
    local chr = str:sub(idx, idx)
    local f = char_func_map[chr]
    if f then return f(str, idx) end
    decode_error(str, idx, "unexpected character '" .. chr .. "'")
end

function json.decode(str)
    if type(str) ~= "string" then
        error("expected argument of type string, got " .. type(str))
    end
    local res, idx = parse(str, next_char(str, 1, space_chars, true))
    idx = next_char(str, idx, space_chars, true)
    if idx <= #str then decode_error(str, idx, "trailing garbage") end
    return res
end

return json

end
_G.package.loaded[".json"] = load_json()
print("loaded json")
  


local function load_stringify() 
  --- The Stringify module provides utilities for formatting and displaying Lua tables in a more readable manner. Returns the stringify table.
-- @module stringify

--- The stringify table
-- @table stringify
-- @field _version The version number of the stringify module
-- @field isSimpleArray The isSimpleArray function
-- @field format The format function
local stringify = { _version = "0.0.1" }

-- ANSI color codes
local colors = {
  red = "\27[31m",
  green = "\27[32m",
  blue = "\27[34m",
  reset = "\27[0m"
}

--- Checks if a table is a simple array (i.e., an array with consecutive numeric keys starting from 1).
-- @function isSimpleArray
-- @tparam {table} tbl The table to check
-- @treturn {boolean} Whether the table is a simple array
function stringify.isSimpleArray(tbl)
  local arrayIndex = 1
  for k, v in pairs(tbl) do
    if k ~= arrayIndex or (type(v) ~= "number" and type(v) ~= "string") then
      return false
    end
    arrayIndex = arrayIndex + 1
  end
  return true
end

--- Formats a table for display, handling circular references and formatting strings and tables recursively.
-- @function format
-- @tparam {table} tbl The table to format
-- @tparam {number} indent The indentation level (default is 0)
-- @tparam {table} visited A table to track visited tables and detect circular references (optional)
-- @treturn {string} A string representation of the table
function stringify.format(tbl, indent, visited)
  indent = indent or 0
  local toIndent = string.rep(" ", indent)
  local toIndentChild = string.rep(" ", indent + 2)

  local result = {}
  local isArray = true
  local arrayIndex = 1

  if stringify.isSimpleArray(tbl) then
    for _, v in ipairs(tbl) do
      if type(v) == "string" then
        v = colors.green .. '"' .. v .. '"' .. colors.reset
      else
        v = colors.blue .. tostring(v) .. colors.reset
      end
      table.insert(result, v)
    end
    return "{ " .. table.concat(result, ", ") .. " }"
  end

  for k, v in pairs(tbl) do
    if isArray then
      if k == arrayIndex then
        arrayIndex = arrayIndex + 1
        if type(v) == "table" then
          v = stringify.format(v, indent + 2)
        elseif type(v) == "string" then
          v = colors.green .. '"' .. v .. '"' .. colors.reset
        else
          v = colors.blue .. tostring(v) .. colors.reset
        end
        table.insert(result, toIndentChild .. v)
      else
        isArray = false
        result = {}
      end
    end
    if not isArray then
      if type(v) == "table" then
        visited = visited or {}
        if visited[v] then
            return "<circular reference>"
        end
        visited[v] = true

        v = stringify.format(v, indent + 2, visited)
      elseif type(v) == "string" then
        v = colors.green .. '"' .. v .. '"' .. colors.reset
      else
        v = colors.blue .. tostring(v) .. colors.reset
      end
      k = colors.red .. k .. colors.reset
      table.insert(result, toIndentChild .. k .. " = " .. v)
    end
  end

  local prefix = isArray and "{\n" or "{\n "
  local suffix = isArray and "\n" .. toIndent .. " }" or "\n" .. toIndent .. "}"
  local separator = isArray and ",\n" or ",\n "
  return prefix .. table.concat(result, separator) .. suffix
end

return stringify

end
_G.package.loaded[".stringify"] = load_stringify()
print("loaded stringify")
  


local function load_eval() 
  --- The Eval module provides a handler for evaluating Lua expressions. Returns the eval function.
-- @module eval

local stringify = require(".stringify")
local json = require('.json')
--- The eval function.
-- Handler for executing and evaluating Lua expressions.
-- After execution, the result is stringified and placed in ao.outbox.Output.
-- @function eval
-- @tparam {table} ao The ao environment object
-- @treturn {function} The handler function, which takes a message as an argument.
-- @see stringify
return function (ao)
  return function (msg)
    -- exec expression
    local expr = msg.Data or "1 + 1"
    local func, err = load("return " .. expr, 'aos', 't', _G)
    local output = ""
    local e = nil
    if err then
      func, err = load(expr, 'aos', 't', _G)
    end
    if func then
      output, e = func()
    else
      ao.outbox.Error = err
      return
    end
    if e then 
      ao.outbox.Error = e
      return 
    end
    if HANDLER_PRINT_LOGS and output then
      table.insert(HANDLER_PRINT_LOGS, type(output) == "table" and stringify.format(output) or tostring(output))
    else
      -- set result in outbox.Output (Left for backwards compatibility)
      ao.outbox.Output = {  
        data = type(output) == "table" and stringify.format(output) or tostring(output),
        prompt = Prompt()
      }

    end
  end
end

end
_G.package.loaded[".eval"] = load_eval()
print("loaded eval")
  


local function load_process() 
  local eval = require('.eval')

local process = { _version = "2.0.5" }

function Prompt()
  return "aos> "
end

function process.handle(msg, env) 
  local ao = {outbox = {} }

  local _eval = eval(ao)
  _eval(msg) 
  return ao.outbox
end

return process

end
_G.package.loaded[".process"] = load_process()
print("loaded process")
  

local json = require('.json')
local process = require('.process')

function handle(msg, env) 
  local _msg = json.decode(msg)
  local _env = json.decode(env)

  local response = process.handle(_msg, _env)

  local result = json.encode({ ok = true, response = response}) 
  return result
end

