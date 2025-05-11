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
---     ledger. This is achieved by comparing the `from-process-uncommitted'
---     field of the credit-notice message with `process/id&commitments=none' on
---     the receiving ledger.

--- Utility functions:

-- Add a message to the outbox of the given base.
local function send(base, message)
    table.insert(base.result.outbox, message)
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

    return false
end

-- Ensure that the assignment is trusted. Either by running the assessment
-- process, or by checking the signature against the process's own scheduler
-- address and those it explicitly trusts.
local function is_trusted_assignment(base, assignment)
    if base.assess and base.assess.assignment then
        ao.event({ "Running assessment message against assignment." },
            { assessment = base.assess.assignment, assignment = assignment })
        local status, result = ao.resolve(base.assess.assignment, assignment)
        if (status ~= "ok") or (result ~= true) then
            ao.event({ "Assessment of assignment failed.", {
                assignment = assignment,
                status = status,
                result = result
            }})
            return false
        end
    end

    -- If the assessment message is not present, check the signature against
    -- the process's own scheduler address.
    ao.event({ "Trusted authorities: ", {
        scheduler = base.scheduler
    }})
    return is_signed_by(base.scheduler, "assignment", assignment)
end

-- Ensure that message's sent on-behalf of the process are trusted by the
-- process's specification.
local function is_trusted_request(base, request)
    if base.assess and base.assess.request then
        ao.event({ "Running assessment message against request." },
            { assessment = base.assess.request, request = request })
        local status, result = ao.resolve(base.assess.request, request)
        if (status ~= "ok") or (result ~= true) then
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
    return is_signed_by(base.authority, "request", request)
end

-- Ensure that a credit-notice from another ledger is admissible. It must either
-- be from our own root ledger, or from a sub-ledger that is precisely the same
-- as our own.
local function validate_peer_ledger(base, assignment)
    if assignment.body.from == base["token"] then
        ao.event({ "Credit-notice from parent token. Accepting." }, {
            assignment = assignment
        })
        return true
    end

    -- Calculate the expected from our own `process/id&commitments=none`.
    -- This ensures that the process we are receiving the `credit-notice` from
    -- has the same structure as our own process.
    local expected = ao.get("process/id&commitments=none", base)

    -- Check if the `from-process-uncommitted` field is present in the assignment.
    if not assignment.body["from-process-uncommitted"] then
        ao.event({ "from-process-uncommitted field not found in message: ", {
            assignment = assignment
        }})
        return false
    end

    -- Check if the `from-process-uncommitted` field matches the expected ID.
    local process_matches = assignment.body["from-process-uncommitted"] == expected
    ao.event({ "Peer registration messages match: ", {
        process_matches = process_matches,
        expected = expected,
        assignment = assignment
    }})
    return process_matches
end

-- Ensure that the ledger is initialized.
local function ensure_initialized(base, assignment)
    -- Ensure that the base has a `result' field before we try to register.
    base.result = base.result or {}
    base.result.outbox = {}
    base.result.status = "OK"
    -- If the ledger is not being initialized, we can skip the rest of the
    -- function.
    if assignment.slot ~= 0 then
        return "ok", base
    end

    -- Ensure that the `ledgers' map is initialized: present and empty.
    base.ledgers = base.ledgers or {}
    ao.event({ "Ledgers before initialization: ", base.ledgers })

    for _, ledger in ipairs(base.ledgers) do
        base.ledgers[ledger] = 0
    end
    ao.event({ "Ledgers after initialization: ", base.ledgers })

    if not base["token"] then
        ao.event({ "Ledger has no source token. Skipping registration." })
        return "ok", base
    end

    ao.event({ "Registering self with known token ledgers: ", {
        ledgers = base.ledgers
    }})

    for _, ledger in ipairs(base.ledgers) do
        -- Insert the register result into the base.
        base.result = send(base, {
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
        ao.event({ "Ledger initialization failed.", {
            assignment = assignment,
            status = status,
            base = base
        }})
        return "error", base, "Ledger initialization failed."
    end
    -- First, ensure that the message has not already been processed.
    ao.event("Deduplicating message.")
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
    base.device = "process@1.0"
    ao.event("Deduplication complete.")
    if status ~= "ok" then
        ao.event({ "Deduplication failure.",
            assignment = assignment,
            status = status,
            incoming_base = incoming_base,
            resulting_base = base
        })
        return "error", base, "Duplicate message detected."
    end

    -- Next, ensure that the assignment is trusted.
    if not is_trusted_assignment(base, assignment) then
        base.result = {
            status = "error",
            error = "Assignment commitments required."
        }
        return "error", base, "Assignment not trusted."
    end

    if assignment.body["from-process"] then
        -- If the request is proxied, we need to check that the source
        -- computation is trusted.
        if not is_trusted_request(base, assignment.body) then
            base.result = {
                status = "error",
                error = "Source computation not trusted."
            }
            return "error", base, "Computation not trusted."
        end
        assignment.body.from = assignment.body["from-process"]
        return "ok", base, assignment.body
    else
        -- If the request is not proxied, we set the `from' field to the address
        -- that signed the request.
        local committers = ao.get("committers", assignment.body)
        if #committers == 0 then
            base.result = {
                status = "error",
                error = "No request signers found."
            }
            return "error", base, "No request signers found."
        end
        
        -- Only accept single-signed requests to avoid ambiguity
        if #committers > 1 then
            base.result = {
                status = "error",
                error =
                    "Multiple signers detected, making sender ambiguous. " ..
                    "Only singly-signed requests are supported for end-user " ..
                    "requests (those that do not originate from another " ..
                    "computation)."
            }
            return "error", base, "Multiple signers not supported."
        end
        
        assignment.body.from = committers[1]
        return "ok", base, assignment.body
    end
end

-- Ensure that the source has the required funds, then debit the source. Takes
-- an origin, which can be used to identify the reason for the debit in logging.
-- Returns error if the source balance is not viable, or `ok` and the updated
-- base state if the debit is successful. Does not credit any funds.
local function deduct_balance(origin, base, request)
    local source = request.from
    local quantity = request.quantity

    ao.event({ "Deducting balance.", { origin = origin }, { request = request } })

    -- Ensure that the `source' and `quantity' fields are present in the request.
    if not source or not quantity then
        ao.event({ "Failure: Fund source or quantity not found in request.",
            { origin = origin }, { request = request } })
        base.result = {
            status = "error",
            error = "Fund source or quantity not present."
        }
        return "error", base
    end

    -- Normalize the quantity value.
    quantity = normalize_int(quantity)
    if not quantity then
        ao.event({ "Invalid quantity value: ", { quantity = quantity } })
        base.result = {
            status = "error",
            error = "Invalid quantity value.",
            quantity = quantity
        }
        return "error", base
    end

    -- Ensure that the source has the required funds.
    -- Check 1: The source balance is present in the ledger.
    local source_balance = base.balance[source]
    if not source_balance then
        ao.event({ "Failure: Source balance not found.", { origin = origin },
            { request = request } })
        base.result = {
            status = "error",
            error = "Source balance not found."
        }
        return "error", base
    end

    -- Check 2: The source balance is a valid number.
    if type(source_balance) ~= "number" then
        ao.event({ "Failure: Source balance is not a number.", { origin = origin },
            { request = request } })
        base.result = {
            status = "error",
            error = "Source balance is not a number.",
            ["balance"] = source_balance
        }
        return "error", base
    end

    -- Check 3: Ensure that the quantity to deduct is a non-negative number.
    if quantity < 0 then
        ao.event({ "Failure: Quantity to deduct is negative.", { origin = origin },
            { request = request } })
        base.result = {
            status = "error",
            error = "Quantity to deduct is negative."
        }
        return "error", base
    end

    -- Check 4: Ensure that the source has enough funds.
    if source_balance < quantity then
        ao.event({ "Failure: Insufficient funds.", { origin = origin },
            { request = request } })
        base.result = {
            status = "error",
            error = "Insufficient funds."
        }
        return "error", base
    end

    ao.event({ "Debiting funds", { origin = origin }, { request = request } })
    base.balance[source] = source_balance - quantity
    ao.event({ "Debit processed", { balances = base.balance } })
    return "ok", base
end

-- Withdraw the specified amount to the next sub-ledger in the route.
local function transfer_route_next(base, request)
    ao.event({ "Route request received: ", {
        request = request,
        ledgers = base.ledgers
    }})

    -- Check that we know how to route the request.
    -- First, check if the parent token is the next hop.
    local known = false
    if request.route[1] == base["token"] then
        known = true
    end

    -- Next, check if a registered subledger is in the route.
    for _, ledger in ipairs(base.ledgers) do
        if request.route[1] == ledger then
            known = true
        end
    end

    -- Normalize the quantity value.
    local quantity = normalize_int(request.quantity)
    if not quantity then
        ao.event({ "Invalid quantity value: ", { quantity = request.quantity } })
        base.result = {
            status = "error",
            error = "Invalid quantity value.",
            quantity = quantity
        }
        return "ok", base
    end

    -- We know how to route the request, so we attempt to decrement the funds
    -- from the source token.
    local status
    status, base = deduct_balance("transfer-route", base, request)
    if status ~= "ok" then
        ao.event({ "Balance deduction failed for route: ", { request = request } })
        return "ok", base
    end

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
        -- Subsequently, if we are an inter-mediate hop on a multi-hop route,
        -- we credit the source with the funds and note the balance that we now
        -- hold on the source ledger. This processing follows the same logic as
        -- if we had received a normal `Credit-Notice` from the source ledger.
        if base.ledgers[request.from] then
            ao.event({ "Crediting self with funds on source ledger: ", {
                ledger = request.from,
                amount = quantity,
                previous = base.ledgers[request.from] or 0
            }})
            base.ledgers[request.from] = (base.ledgers[request.from] or 0) + quantity
            -- Credit the user's balance with the quantity.
            ao.event({ "Crediting user with funds: ", {
                ledger = request.recipient,
                amount = quantity,
                previous = base.balance[request.recipient] or 0
            }})
            base.balance[request.recipient] =
                (base.balance[request.recipient] or 0) + quantity
        end

        return "ok", base
    end

    -- Now that we know we can route the request and that the source has enough
    -- funds, route the request to the next hop.
    local next_hop = request.route[1]
    table.remove(request.route, 1)
    local route_message = {
        action = "Transfer",
        target = next_hop,
        recipient = request.recipient,
        quantity = quantity
    }

    -- If there are more hops to route the request to, create a new route
    -- message. Otherwise, create a normal transfer message.
    if #request.route >= 1 then
        route_message.route = request.route
    end

    -- Add the route message to the outbox.
    base = send(base, route_message)

    ao.event({ "Route transfer added to outbox: ", {
        route_message = route_message,
        base = base
    }})
    return "ok", base
end

-- Transfer the specified amount from the given account to the given account,
-- optionally routing to a different sub-ledger if necessary.
function transfer(raw_base, assignment)
    ao.event({ "Transfer request received", { assignment = assignment } })

    -- Verify the security of the request.
    local status, base, request = validate_request(raw_base, assignment)
    if (status ~= "ok") or (type(request) ~= "table") then
        return "ok", base
    end

    if not request.recipient then
        ao.event({ "Transfer request has no recipient. Skipping." })
        base.result = {
            status = "error",
            error = "Transfer request has no recipient."
        }
        return "ok", base
    end

    -- If the request specifies a route, handle it in the route_to function.
    if request.route and #request.route > 1 then
        return transfer_route_next(base, request)
    end

    -- Debit the source account.
    status, base = deduct_balance("transfer", base, request)
    if status ~= "ok" then
        ao.event({ "Transfer request failed", { assignment = assignment } })
        return "ok", base
    end

    -- Normalize the quantity value.
    local quantity = normalize_int(request.quantity)
    if not quantity then
        ao.event({ "Invalid quantity value: ", { quantity = request.quantity } })
        base.result = {
            status = "error",
            error = "Invalid quantity value.",
            quantity = quantity
        }
        return "ok", base
    end

    -- We are processing a standard transfer. Credit the destination account.
    base.balance[request.recipient] =
        (base.balance[request.recipient] or 0) + quantity
    ao.event({ "Transfer request processed", { balances = base.balance } })

    -- Notify the parties that the funds have been transferred.
    base = send(base, {
        target = request.recipient,
        action = "Credit-Notice",
        quantity = quantity,
        recipient = request.recipient
    })

    base = send(base, {
        target = request.from,
        action = "Debit-Notice",
        quantity = quantity,
        recipient = request.recipient
    })

    return "ok", base
end

-- Process deposit notifications from other ledgers.
_G["credit-notice"] = function (raw_base, assignment)
    ao.event({ "Credit-Notice received. Validating as deposit from peer ledger.",
        { assignment = assignment },
        { balances = raw_base.balance }
    })

    local status, base, request = validate_request(raw_base, assignment)
    if (status ~= "ok") or (type(request) ~= "table") then
        ao.event({ "Credit notice validation failed: ",
            { assignment = assignment },
            { balances = raw_base.balance }
        })
        return status, base
    end

    -- Ensure that the credit notice is from a valid source.
    if not validate_peer_ledger(base, assignment) then
        base.result = {
            status = "error",
            error = "Invalid token deposit. Credit notice from " ..
                request.from .. " not admissible."
        }
        return "ok", base
    end

    -- Normalize the quantity value.
    local quantity = normalize_int(request.quantity)
    if not quantity then
        ao.event({ "Invalid quantity value: ", { quantity = request.quantity } })
        base.result = {
            status = "error",
            error = "Invalid quantity value: " .. request.quantity
        }
        return "ok", base
    end

    -- Ensure that the recipient is known.
    if not request.recipient then
        ao.event({ "Credit-Notice has no recipient. Returning." })
        request.path = "id&commitments=all"
        local request_id = ao.get(request)
        base = send(base, {
            target = request.from,
            action = "Credit-Notice-Rejection",
            reason = "Recipient not found.",
            notice = request_id
        })
        return "ok", base
    end

    -- Credit our own balance with the sending ledger to record the deposit.
    ao.event({ "Incrementing balance for ledger: ", {
        ledger = request.from,
        amount = quantity,
        previous = base.ledgers[request.from] or 0
    }})
    base.ledgers[request.from] = (base.ledgers[request.from] or 0) + quantity

    -- Credit the user's balance with the quantity.
    base.balance[request.recipient] =
        (base.balance[request.recipient] or 0) + quantity

    ao.event({ "Deposit processed: ", { balances = base.ledgers } })
    return "ok", base
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
        base.result = {
            message = "Ledger already registered."
        }
        return "ok", base
    end
    
    -- Validate the registering ledger
    if not validate_peer_ledger(base, assignment) then
        base.result = {
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
    ao.event({ "compute", { assignment = assignment }, 
        { balance = base.balance, ledgers = base.ledgers } })

    assignment.body.action = string.lower(assignment.body.action)
    
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
        base.result = {
            status = "error",
            error = "Unknown action: " .. assignment.body.action
        }
        ao.event({ "Unknown action", { action = assignment.body.action } })
        return "ok", base
    end
end