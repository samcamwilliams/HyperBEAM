--- ## HyperTokens: Networks of fungible, parallel ledgers.
--- # Version: 0.1.
--- 
--- An AO token standard implementation, with support for sub-ledger networks,
--- executed with the `~lua@5.3` device. This script supports both the base token
--- blueprint's 'active' keys, as well as the mainnet sub-ledger API.
--- 
--- Data access actions (e.g. `balance', `info', `total-supply') are not
--- implemented due to their redundancy. Instead, the full state of the process
--- is available via the AO-Core HTTP API, including all metadata and individual
--- account balances.
--- 
--- A full description of the hyper-token standard can be found in the
--- `token.md` file in this directory. The remainder of this module document
--- provides a breif overview of its design, and focuses on its implementation
--- details.
--- 
--- ## Design and Implementation
--- 
--- If running as a `root' token (indicated by the absence of a `token' field),
--- the `balance' field should be initialized to a table of balances for the
--- token during spawning. The `ledgers' field holds the state of a sub-ledger's
--- own balances with other ledgers. This field is always initialized to a message
--- of zero balances during the evaluation of the first assignment of the process.
--- When the token receives a `credit-notice' message, it will interpret it as a
--- deposit from the sending ledger and update its record of its own balance with
--- the sending ledger.
--- 
--- Atop the standard token transfer messages, the sub-ledger API allows for
--- `transfer' messages to specify a `route' field, which is a list of ledger
--- IDs that the transfer should be routed through in order to reach a
--- recipient on a different ledger. At each `hop' in the route, the recipient
--- ledger validates whether it trusts the sending ledger and whether it knows
--- how to route to the next hop. If the recipient ledger does not trust the
--- sending ledger, it will terminate the route with a `route-termination'
--- message. If the recipient ledger knows how to route to the next hop, it
--- will create a new `transfer' message to the next hop, with the first
--- ledger from the route removed and the remainder of the route and recipient
--- and quantity to transfer forwarded along with the message.
--- 
--- There are three security checks performed on incoming messages, above the
--- standard balance transfer checks:
--- 
--- 1. Assignments are evaluated against the `assess/assignment' message, if
---    present. If not, the assignment is evaluated against the process's own
---    scheduler address.
--- 
--- 2. If the message does not originate from an end-user (indicated by the 
---    presence of a `from-process' field), the message is evaluated against
---    the `assess/request' message, if present. If not, the message is 
---    evaluated against the `authority' field. The `authority' field may 
---    contain a list of addresses or messages that are considered to be
---    authorities.
--- 
--- 3. `credit-notice' messages that do not originate from a sub-ledger's
---    `token' are evaluated for parity of source code with the receiving
---     ledger. This is achieved by comparing the `from-base' field of the
---     credit-notice message with `process/id&commitments=none' on the receiving
---     ledger.

--- Utility functions:

-- Add a message to the outbox of the given base.
local function send(base, message)
    table.insert(base.results.outbox, message)
    return base
end

-- Add a log message to the results of the given base.
local function log_result(base, status, message)
    ao.event("token_log", {"Result: ", {
        status = status,
        message = message
    }})
    base.results = base.results or {}
    base.results.status = status
    
    if base.results.log then
        table.insert(base.results.log, message)
    else
        base.results.log = { message }
    end

    return base
end

-- Normalize a quantity value to ensure it is a proper integer.
-- Returns either the normalized integer value or nil and an error message.
local function normalize_int(value)
    local num
    -- Handle string conversion
    if type(value) == "string" then
        -- Check for decimal part (not allowed)
        if string.find(value, "%.") then
            return nil
        end
        -- Convert to number
        num = tonumber(value)
        if not num then
            return nil
        end
    elseif type(value) == "number" then
        num = value
        -- Check if it's an integer
        if num ~= math.floor(num) then
            return nil
        end
    else
        -- Any other type is invalid.
        return nil
    end
    
    return num
end

--- Security verification functions:

-- Ensure that the message is signed by one of the specified addresses. The
-- message type is either `assignment' or `request'.
local function is_signed_by(addresses, target, assignment)
    -- Normalize the addresses to a table if only one address string is provided.
    if type(addresses) == "string" then
        addresses = { addresses }
    end

    -- Get the message body based on the target type.
    local msg
    if target == "assignment" then msg = assignment
    elseif target == "request" then msg = assignment.body
    end

    -- Get the committers from the message and check if any of them match the
    -- given addresses.
    local committers = ao.get("committers", {"as", "message@1.0", msg})
    ao.event({ "Committers: ", { element = target, committers = committers } })
    for _, committer in ipairs(committers) do
        for _, address in ipairs(addresses) do
            if committer == address then
                return true
            end
        end
    end

    return false, {
        ["received-committers"] = committers,
        ["expected-committers"] = addresses,
        ["root-message"] = assignment,
        target = target
    }
end

-- Ensure that the assignment is trusted. Either by running the assessment
-- process, or by checking the signature against the process's own scheduler
-- address and those it explicitly trusts.
local function is_trusted_assignment(base, assignment)
    if base.assess and base.assess.assignment then
        ao.event({ "Running assessment message against assignment." },
            { assessment = base.assess.assignment, assignment = assignment })
        local status, result = ao.resolve(base.assess.assignment, assignment)
        if (status == "ok") and (result == true) then
            ao.event({ "Assessment of assignment passed." }, {
                assignment = assignment,
                status = status,
                result = result
            })
            return true
        else
            ao.event({ "Assessment of assignment failed.", {
                assignment = assignment,
                status = status,
                result = result
            }})
            return false, {
                ["assessment-message"] = base.assess.assignment,
                status = status,
                result = result
            }
        end
    end

    -- If the assessment message is not present, check the signature against
    -- the process's own scheduler address.
    ao.event({ "Trusted scheduler(s): ", {
        scheduler = base.scheduler
    }})
    return is_signed_by(base.scheduler, "assignment", assignment)
end

-- Ensure that message's sent on-behalf of the process are trusted by the
-- process's specification.
local function is_trusted_compute(base, assignment)
    local request = assignment.body
    if base.assess and base.assess.request then
        ao.event({ "Running assessment message against request." },
            { assessment = base.assess.request, request = request })
        local status, result = ao.resolve(base.assess.request, request)
        if (status == "ok") and (result == true) then
            ao.event({ "Assessment of request passed." }, {
                request = request,
                status = status,
                result = result
            })
            return true
        else
            ao.event({ "Assessment of request failed.", {
                request = request,
                status = status,
                result = result
            }})
            return false
        end
    end

    -- If the assessment message is not present, check the signature against
    -- the process's authority list.
    ao.event({ "Trusted authorities: ", {
        authority = base.authority
    }})
    return is_signed_by(base.authority, "request", assignment)
end

-- Determine if a request is from a known ledger. Makes no assessment of whether
-- a request is otherwise trustworthy.
local function is_known_ledger(base, request)
    if request.from == base["token"] then
        return true
    end

    return base.ledgers and (base.ledgers[request.from] ~= nil)
end

-- Determine if the ledger indicated by `base` is the root ledger.
local function is_root(base)
    return base.token == nil
end

-- Ensure that a credit-notice from another ledger is admissible. It must either
-- be from our own root ledger, or from a sub-ledger that is precisely the same
-- as our own.
local function validate_new_peer_ledger(base, request)
    ao.event({ "Validating peer ledger: ", { request = request } })

    -- Check if the request is from the root ledger.
    if is_root(base) or request.from == base["token"] then
        ao.event({ "Deposit is from parent token. Accepting." }, {
            request = request
        })
        return true
    end

    -- Calculate the expected from our own `process/id&commitments=none`.
    -- This ensures that the process we are receiving the `credit-notice` from
    -- has the same structure as our own process.
    local expected = ao.get("process/id&commitments=none", base)

    -- Check if the `from-base' field is present in the assignment.
    if not request["from-base"] then
        ao.event({ "from-base field not found in message: ", {
            request = request
        }})
        return false
    end

    -- Check if the `from-base' field matches the expected ID.
    local base_matches = request["from-base"] == expected
    ao.event({ "Peer registration messages match: ", {
        base_matches = base_matches,
        expected = expected,
        request = request
    }})
    return base_matches
end

-- Ensure that the ledger is initialized.
local function ensure_initialized(base, assignment)
    -- Ensure that the base has a `result' field before we try to register.
    base.results = base.results or {}
    base.results.outbox = {}
    base.results.status = "OK"
    -- If the ledger is not being initialized, we can skip the rest of the
    -- function.
    if assignment.slot ~= 0 then
        return "ok", base
    end
    base.balance = base.balance or {}

    -- Ensure that the `ledgers' map is initialized: present and empty.
    base.ledgers = base.ledgers or {}
    ao.event({ "Ledgers before initialization: ", base.ledgers })

    for _, ledger in ipairs(base.ledgers) do
        base.ledgers[ledger] = 0
    end
    ao.event({ "Ledgers after initialization: ", base.ledgers })

    if not base.token then
        ao.event({ "Ledger has no source token. Skipping registration." })
        return "ok", base
    end

    ao.event({ "Registering self with known token ledgers: ", {
        ledgers = base.ledgers
    }})

    for _, ledger in ipairs(base.ledgers) do
        -- Insert the register result into the base.
        base.results = send(base, {
            action = "Register",
            target = ledger
        })
    end

    return "ok", base
end

-- Verify that an assignment has not been processed and that the request is
-- valid. If it is, update the `from' field to the address that signed the
-- request.
local function validate_request(incoming_base, assignment)
    -- Ensure that the ledger is initialized.
    local status, base = ensure_initialized(incoming_base, assignment)
    if status ~= "ok" then
        return "error", log_result(incoming_base, "error", {
            message = "Ledger initialization failed.",
            assignment = assignment,
            status = status,
        })
    end

    -- First, ensure that the message has not already been processed.
    ao.event("Deduplicating message.", {
        ["history-length"] = #(base.dedup or {})
    })

    status, base =
        ao.resolve(
            incoming_base,
            {"as",
                "dedup@1.0",
                {
                    path = "compute",
                    ["subject-key"] = "body",
                    body = assignment.body
                }
            }
        )

    -- Set the device back to `process@1.0`.
    base.device = "process@1.0"
    if status ~= "ok" then
        return "error", log_result(base, "error", {
            message = "Deduplication failure.",
            assignment = assignment,
            status = status,
            incoming_base = incoming_base,
            resulting_base = base
        })
    end

    -- Next, ensure that the assignment is trusted.
    local trusted, details = is_trusted_assignment(base, assignment)
    if not trusted then
        return "error", log_result(base, "error", {
            message = "Assignment is not trusted.",
            details = details
        })
    end

    if assignment.body["from-process"] then
        -- If the request is proxied, we need to check that the source
        -- computation is trusted.
        trusted, details = is_trusted_compute(base, assignment)
        if not trusted then
            return "error", log_result(base, "error", {
                message = "Message computation is not trusted.",
                details = details
            })
        end
        assignment.body.from = assignment.body["from-process"]
        return "ok", base, assignment.body
    else
        -- If the request is not proxied, we set the `from' field to the address
        -- that signed the request.
        local committers = ao.get("committers", assignment.body)
        if #committers == 0 then
            return "error", log_result(base, "error", {
                message = "No request signers found."
            })
        end
        
        -- Only accept single-signed requests to avoid ambiguity
        if #committers > 1 then
            return "error", log_result(base, "error", {
                message = "Multiple signers detected, making sender ambiguous. " ..
                    "Only singly-signed requests are supported for end-user " ..
                    "requests (those that do not originate from another " ..
                    "computation)."
            })
        end
        
        assignment.body.from = committers[1]
        return "ok", base, assignment.body
    end
end

-- Ensure that the source has the required funds, then debit the source. Takes
-- an origin, which can be used to identify the reason for the debit in logging.
-- Returns error if the source balance is not viable, or `ok` and the updated
-- base state if the debit is successful. Does not credit any funds.
local function debit_balance(origin, base, request)
    local source = request.from

    ao.event({ "Attempting to deduct balance.",
        { origin = origin },
        { request = request },
        { balances = base.balance or {} }
    })

    -- Ensure that the `source' and `quantity' fields are present in the request.
    if not source or not request.quantity then
        return "error", log_result(base, "error", {
            message = "Fund source or quantity not found in request.",
            origin = origin
        })
    end

    -- Normalize the quantity value.
    request.quantity = normalize_int(request.quantity)
    if not request.quantity then
        ao.event({ "Invalid quantity value: ", { quantity = request.quantity } })
        base.results = {
            status = "error",
            error = "Invalid quantity value.",
            quantity = request.quantity
        }
        return "error", base
    end

    -- Ensure that the source has the required funds.
    -- Check 1: The source balance is present in the ledger.
    local source_balance
    -- if is_known_ledger(base, request) then
    --     source_balance = base.ledgers[source]
    -- else
        source_balance = base.balance[source]
    -- end

    if not source_balance then
        return log_result(base, "error", {
            message = "Source balance not found.",
            from = source,
            quantity = request.quantity,
            ["is-root"] = base.token == nil
        })
    end

    -- Check 2: The source balance is a valid number.
    if type(source_balance) ~= "number" then
        return log_result(base, "error", {
            message = "Source balance is not a number.",
            balance = source_balance
        })
    end

    -- Check 3: Ensure that the quantity to deduct is a non-negative number.
    if request.quantity < 0 then
        return log_result(base, "error", {
            message = "Quantity to deduct is negative.",
            quantity = request.quantity
        })
    end

    -- Check 4: Ensure that the source has enough funds.
    if source_balance < request.quantity then
        return log_result(base, "error", {
            message = "Insufficient funds.",
            from = source,
            quantity = request.quantity,
            balance = source_balance
        })
    end

    ao.event({ "Deducting funds:", { origin = origin , request = request } })
    if is_known_ledger(base, request) then
        base.ledgers[source] = source_balance - request.quantity
    else
        base.balance[source] = source_balance - request.quantity
    end
    ao.event({ "Balances after deduction:",
        { balances = base.balance, ledgers = base.ledgers } }
    )
    return "ok", base
end

-- Perform a standard user-to-user transfer.
local function transfer_user_to_user(raw_base, request)
    -- We are processing a standard transfer. Debit the source account, then
    -- credit the destination account.
    local status, base = debit_balance("transfer", raw_base, request)
    if status ~= "ok" then
        return log_result(raw_base, "error", {
            message = "Transfer request failed.",
            details = base
        })
    end

    -- Credit the destination account.
    base.balance[request.recipient] =
        (base.balance[request.recipient] or 0) + request.quantity

    -- Notify the parties that the funds have been transferred.
    base = send(base, {
        target = request.recipient,
        action = "Credit-Notice",
        quantity = request.quantity,
        sender = request.from
    })

    base = send(base, {
        target = request.from,
        action = "Debit-Notice",
        quantity = request.quantity,
        recipient = request.recipient
    })

    return log_result(base, "ok", {
        message = "Standard token transfer request processed successfully.",
        route = request.route,
        from_user = request.from,
        to_user = request.recipient,
        quantity = request.quantity
    })
end

-- Perform the first step of a multi-hop transfer.
function transfer_user_to_ledger(raw_base, request)
    -- Debit the source account.
    local status, base = debit_balance("transfer", raw_base, request)
    if status ~= "ok" then
        return log_result(raw_base, "error", {
            message = "User-to-ledger transfer request failed.",
            details = base
        })
    end

    if #request.route < 1 then
        return log_result(base, "error", {
            message = "No route provided."
        })
    end
    local next_hop = request.route[1]
    table.remove(request.route, 1)

    -- Check that we trust the ledger we are sending to, or that we are the root
    -- ledger.
    if is_root(base) or is_known_ledger(base, request) or (next_hop == base.token) then
        -- Increment the balance of the ledger we are sending to.
        base.balance[next_hop] =
            (base.balance[next_hop] or 0) + request.quantity
        base = send(base, {
            action = "Transfer",
            target = next_hop,
            recipient = request.recipient,
            quantity = request.quantity,
            sender = request.from,
            route = request.route
        })
        return log_result(base, "ok", {
            message = "Ledger sent tokens to peer ledger successfully.",
            route = request.route,
            from_user = request.from,
            to_ledger = next_hop,
            quantity = request.quantity
        })
    else
        return log_result(base, "error", {
            message = "Cannot route to untrusted ledger.",
            route = request.route
        })
    end
end

-- Process end-of-chain interledger transfers.
function transfer_ledger_to_user(base, request)
    ao.event({ "Transfer with empty route key received. "..
        "We are the destination ledger of an interledger transfer.",
        { request = request },
        { balances = base.balance }
    })

    -- Ensure that the credit notice is from a valid source.
    if not is_known_ledger(base, request) then
        return log_result(base, "error", {
            message = "Invalid interledger token deposit. Credit notice from " ..
                request.from .. " not admissible."
        })
    end

    -- Deduct the quantity from the sending ledger's balance, unless it is the
    -- root ledger. The root ledger is the only ledger that can send funds to
    -- its sub-ledgers without a balance on that ledger.
    local status
    if request.from ~= base.token then
        status, base = debit_balance("deposit", base, request)
        if status ~= "ok" then
            return log_result(base, "error", {
                message =
                    "Terminal transfer in route failed; cannot debit balance.",
                details = base
            })
        end
    end

    -- Credit our own balance with the sending ledger to record the deposit.
    ao.event({ "Recording interledger deposit: ", {
        ledger = request.from,
        amount = request.quantity,
        previous = base.balance[request.from] or 0
    }})

    -- Credit the user's balance on this ledger with the quantity.
    base.balance[request.recipient] =
        (base.balance[request.recipient] or 0) + request.quantity

    -- Notify the parties that the funds have been transferred.
    base = send(base, {
        target = request.recipient,
        action = "Credit-Notice",
        quantity = request.quantity,
        sender = request.from
    })
    base = send(base, {
        target = request.from,
        action = "Debit-Notice",
        quantity = request.quantity,
        recipient = request.recipient
    })
    return log_result(base, "ok", {
        message = "Ledger-to-user transfer processed successfully.",
        route = request.route,
        from_ledger = request.from,
        to_user = request.recipient,
        quantity = request.quantity
    })
end

-- Perform a 'hop' in a multi-hop transfer, for which we are not the destination
-- ledger.
local function transfer_ledger_to_ledger(base, request)
    ao.event({ "Ledger-to-ledger transfer request received: ", {
        request = request,
        ledgers = base.ledgers,
        token = base.token or "[root]"
    }})

    -- Determine if we know how to route the request.
    local next_hop = request.route[1]
    table.remove(request.route, 1)

    local known = is_root(base)
        or is_known_ledger(base, request)
        or next_hop == base.token

    -- If we don't know how to route the request, return an error.
    if not known then
        ao.event({ "Cannot route next hop: ", {
            route = request.route,
            ledgers = base.ledgers,
        }})
        base = send(
            base,
            {
                target = request.recipient,
                action = "Route-Termination",
                error = "Next hop in route not found.",
                route = request.route,
                ledgers = base.ledgers,
            }
        )
        -- We cannot route the request, but we now hold the funds of the source.
        -- This is treated as we would a ledger-to-user transfer.
        return transfer_ledger_to_user(base, request)
    end

    -- We know how to route the request, so we attempt to decrement the funds
    -- from the source token.
    local status
    status, base = debit_balance("transfer-route", base, request)
    if status ~= "ok" then
        ao.event({ "Balance deduction failed for route: ",
            { request = request, details = base } })
        return "ok", base
    end

    -- Now that we know we can route the request and that the source has been
    -- debited, route the request to the next hop.
    base = send(base, {
        action = "Transfer",
        target = next_hop,
        sender = request.sender,
        recipient = request.recipient,
        route = request.route,
        quantity = request.quantity
    })

    base.ledgers[next_hop] = (base.ledgers[next_hop] or 0) + request.quantity

    return "ok", base
end

-- Transfer the specified amount from the given account to the given account,
-- optionally routing to a different sub-ledger if necessary.
-- There are four differing types of transfer requests. They have the following
-- characteristics. `IF' indicates how the case is determined, and `=>' indicates
-- the actions taken.
-- 1. A standard transfer from one user to another.
--    IF: There is no `route` field.
--    => Debit the source account
--    => Credit the destination account
-- 2. Start an interledger transfer from a user to another ledger.
--    IF: The requester is not a known ledger.
--    => Debit the source account
--    => Credit the sending ledger's balance
--    => Route to the sub-ledger (forwarding `sender', `route', and `quantity')
-- 3. End an interledger transfer from a ledger to a user.
--    IF: There is a length-1 `route` field.
--    => Debit the sending ledger's balance
--    => Credit the destination account
-- 4. Continue an interledger transfer from a ledger to another ledger.
--    IF: The length of the `route` field is greater than 1.
--    => Debit the sending ledger's balance
--    => Route to the next hop in the route
--    => Decrement our balance with the sending ledger

-- Semantics:
-- Balance == owed to X. Credit == Owed to subject by X.
-- User on root -> User on sub-ledger:
-- Root = Dec User balance, Inc Sub-ledger balance

-- Sub-ledger = Inc User balance, Inc Root credit
-- User on sub-ledgerA -> User on sub-ledgerB:
-- Sub-ledgerA = Dec User balance, Inc Sub-ledgerB balance
-- Sub-ledgerB = Inc User balance, Inc Sub-ledgerA credit

-- User on sub-ledgerB -> User on sub-ledgerA:
-- Sub-ledgerB = Dec user balance, Dec sub-ledger A credit
-- Sub-ledgerA = Inc user balance, Dec sub-ledger B balance

-- User on sub-ledger -> User on root:
-- Sub-ledger = Dec User balance, Dec Root credit
-- Root = Dec sub-ledger balance, Inc User balance

function transfer(base, assignment)
    ao.event({ "Transfer request received", { assignment = assignment } })
    -- Verify the security of the request.
    local status, request
    status, base, request = validate_request(base, assignment)
    if status ~= "ok" or not request then
        return "ok", base
    end

    -- Ensure that the recipient is known.
    if not request.recipient then
        return log_result(base, "error", {
            message = "Transfer request has no recipient."
        })
    end

    -- Normalize the quantity value.
    local quantity = normalize_int(request.quantity)
    if not quantity then
        return log_result(base, "error", {
            message = "Invalid quantity value.",
            quantity = request.quantity
        })
    end

    -- The globally applicable requirements upon the transfer request are met.
    -- We can now process the transfer.
    if not request.route then
        -- Case 1: We are processing a standard transfer.
        return transfer_user_to_user(base, request)
    elseif not is_known_ledger(base, request) then
        -- Case 2: We are processing the first step of an interledger transfer.
        return transfer_user_to_ledger(base, request)
    elseif #request.route == 0 then
        -- Case 3: We are processing the final step of a multi-hop transfer.
        return transfer_ledger_to_user(base, request)
    elseif #request.route >= 1 then
        -- Case 4: We are processing a multi-hop transfer.
        return transfer_ledger_to_ledger(base, request)
    end
end


-- Process registration requests from other ledgers.
function register(raw_base, assignment)
    ao.event({ "Register request received", { assignment = assignment } })
    
    local status, base, request = validate_request(raw_base, assignment)
    if (status ~= "ok") or (type(request) ~= "table") then
        return "ok", base
    end

    if base.ledgers[request.from] then
        ao.event({ "Ledger already registered. Ignoring registration request." })
        base.results = {
            message = "Ledger already registered."
        }
        return "ok", base
    end
    
    -- Validate the registering ledger
    if not validate_new_peer_ledger(base, assignment.body) then
        base.results = {
            status = "error",
            error = "Ledger validation failed for registration"
        }
        return "ok", base
    end
    
    -- Add to known ledgers
    base.ledgers[request.from] = base.ledgers[request.from] or 0
    
    -- Send a reciprocal registration request to the remote ledger.
    base = send(base, {
        target = request.from,
        action = "register"
    })
    
    return "ok", base
end

-- Register ourselves with a remote ledger, at the request of a user or another
-- ledger.
_G["register-remote"] = function (raw_base, assignment)
    ao.event({ "Register-Remote request received", { assignment = assignment } })
    
    -- Validate the request.
    local status, base, request = validate_request(raw_base, assignment)
    if (status ~= "ok") or (type(request) ~= "table") then
        return "ok", base
    end

    -- Send a registration request to the remote ledger. Our request is simply
    -- a `Register' message, as the recipient will be assessing our unsigned
    -- process ID in order to validate that we are an appropriate peer. This is
    -- added by our `push-device`, so no further action is required on our part.
    base = send(base, {
        target = request.peer,
        action = "Register"
    })

    return "ok", base
end

--- Index function, called by the `~process@1.0` device for scheduled messages.
--- We route any `action' to the appropriate function based on the request path.
function compute(base, assignment)
    ao.event({ "compute called",
        { balance = base.balance, ledgers = base.ledgers } })

    assignment.body.action = string.lower(assignment.body.action or "")
    
    if assignment.body.action == "credit-notice" then
        return _G["credit-notice"](base, assignment)
    elseif assignment.body.action == "transfer" then
        return transfer(base, assignment)
    elseif assignment.body.action == "register" then
        return register(base, assignment)
    elseif assignment.body.action == "register-remote" then
        return _G["register-remote"](base, assignment)
    else
        -- Handle unknown `action' values.
        base.results = {
            status = "error",
            error = "Unknown action: " .. assignment.body.action
        }
        ao.event({ "Unknown action", { action = assignment.body.action } })
        return "ok", base
    end
end