local bint = require('.bint')(256)

Balances = {}
Locked = {}
Claimable = "0"
LockPeriod = "5040" -- 7 days 


Operator = nil
Node = nil -- also needs to be authority

Tokens = {"0syT13r0s0tgPmIed95bJnuSqaD29HQNN8D3ElLSrsc"}

-- for every message check if any 
-- locked credits are able to be 
-- made available
local function always(msg)
  if LockPeriod ~= "Infinite" then
      local results = Utils.reduce(function (acc, record)
          local _isUnlocked = (bint(record.Height) + bint(LockPeriod)) > bint(msg['Block-Height'])
          if _isUnlocked then
              table.insert(acc.Unlocked, record)
          else
              table.insert(acc.Locked, record)
          end
          return acc
      end, { Unlocked = {}, Locked = {}}, Locked)
      -- clear table
      Locked = nil
      -- re-initialize table
      Locked = results.Locked
      Utils.map(function (record)
        -- deduct from Balance, incement claimable
        Balances[record.Account], Claimable = tostring(bint(Balances[record.Account]) - bint(record.Quantity)),
          tostring(bint(Claimable) + bint(record.Quantity))
      end, results.Unlocked)
  end
end


local function deposit(msg)
  assert(Utils.includes(msg.From, Tokens), "must be from a valid token")
  assert(bint(msg.Quantity) > bint(0), "must be greater than zero")

  Balances[msg.Sender] = Balances[msg.Sender] or "0"
  -- update account balance
  Balances[msg.Sender] = tostring(bint(Balances[msg.Sender]) + bint(msg.Quantity))
  table.insert(Locked, { Account = msg.Sender, Quantity = msg.Quantity, Height = msg['Block-Height'] })
  -- credit node
  Send({
    device = "patch@1.0",
    cache = {
      Balances = {
        [msg.Sender] = Balances[msg.Sender]
      }
    },
    Target = Node,
    path = "ledger~node-process@1.0",
    quantity = msg.Quantity,
    sender = msg.Sender
  })
end

local function receipt (msg)
  if msg.From == Node then
      local results = Utils.reduce(function (acc, record)
          local _isUnlocked = msg.Signer == record.Account
          if _isUnlocked then
              table.insert(acc.Unlocked, record)
          else
              table.insert(acc.Locked, record)
          end
          return acc
      end, { Unlocked = {}, Locked = {}}, Locked)
      -- clear table
      Locked = nil
      -- re-initialize table
      Locked = results.Locked
      -- add unlocked to claimable
      Utils.map(function (record)
        -- deduct from Balance, incement claimable
        Balances[record.Account], Claimable = tostring(bint(Balances[record.Account]) - bint(record.Quantity)),
          tostring(bint(Claimable) + bint(record.Quantity))
      end, results.Unlocked)
  end
end

local function withdraw(msg)
  assert(Utils.includes(msg.Token, Tokens), "must be a valid token")
  assert(bint(msg.Quantity) > bint(0), "must be greater than zero")
  assert(msg.From == Operator, "must be operator")
  assert(bint(Claimable) >= bint(msg.Quantity), "insufficient funds")

  Claimable = tostring(bint(Claimable) - bint(msg.Quantity))
  Send({
    Target = msg.Token,
    Quantity = msg.Quantity,
    Recipient = Operator,
    Action = "Transfer",
    device = "patch@1.0",
    cache = {
      Claimable = Claimable
    }
  })
end

Handlers.prepend("Inbound", function (msg)
  return msg.Action ~= "Eval" and "continue"
end, always)

Handlers.add("Credit-Notice", deposit)
Handlers.add("Withdraw", withdraw)
Handlers.add("Receipt", receipt)
-- Handlers.add("Balance", balance)