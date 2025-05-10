--- A token-blueprint and sub-ledger compatible script with modifications to
--- allow an `admin' account perform non-user-initiated transfers. This is 
--- intended to be used to support payments to node operators for services
--- rendered to users. If used in an environment where the node's execution is
--- verified (through a security means that the user accepts), for example when
--- the node is operating in a TEE or under ZKP enforcement, the `admin' account
--- does not represent a security risk. If the intended execution environment
--- does not provide these assurances, the `admin' key can be unset (or any 
--- non-address value) to disable the admin `debit' action.
--- 
--- This script expects the following parameters to be present on the base
--- message:
--- 
--- * `admin`: The address (if any) that is allowed to invoke the `debit' action.
--- * `source-token`: The address of root token for the sub-ledger.
--- * `parent-token`: The address of the parent token.
--- * `balances`: A table of balances for the ledger. Defaults to `{}`.
--- 
--- This script also supports the base token blueprint parameters:
--- 
--- * `logo`: The logo of the token.
--- * `name`: The name of the token.
--- * `ticker`: The ticker of the token.
--- * `denomination`: The denomination of the token.

-- Ensure that the message is signed by one of the specified addresses. The
-- message type is either `assignment' or `request'.
local function is_signed_by(addresses, target, assignment)
    -- Normalize the addresses to a table if only one address string is provided.
    if type(addresses) == "string" then
        addresses = { addresses }
    end

    -- Get the message body based on the target type.
    local msg
    if target == "assignment" then msg = assignment.body
    elseif target == "request" then msg = assignment
    end

    -- Get the committers from the message and check if any of them match the
    -- given addresses.
    local committers = ao.get("committers", msg)
    for _, committer in ipairs(committers) do
        for _, address in ipairs(addresses) do
            if committer == address then
                return true
            end
        end
    end

    return false
end

-- Ensure that the assignment is trusted.
local function is_trusted_assignment(base, assignment)
    return is_signed_by({ base.admin, base.scheduler }, "assignment", assignment)
end

-- Verify that an assignment has not been processed and that the request is
-- valid. If it is, update the `from' field to the address that signed the
-- request.
local function validate_request(incoming_base, assignment)
    -- Ensure that the incoming base has a `result' field before we try to
    -- validate the request.
    incoming_base.result = incoming_base.result or {}

    -- First, ensure that the message has has not already been processed.
    local status, base = ao.resolve({"as", "dedup@1.0", incoming_base}, "compute")
    if status ~= "ok" then
        ao.event({ "Deduplication failure.",
            assignment = assignment,
            status = status,
            incoming_base = incoming_base,
            resulting_base = base
        })
        return "error", base, "Unprocessable request."
    end

    -- Next, ensure that the assignment is trusted.
    if not is_trusted_assignment(base, assignment) then
        base.result = {
            status = "error",
            error = "Assignment signature required."
        }
        return "error", base, "Unprocessable request."
    end

    if assignment.body["from-process"] then
        -- If the request is proxied, we need to check the signature is from an
        -- authority.
        local trusted_addresses = base.authorities or base.authority or {}
        if is_signed_by(trusted_addresses, "request", assignment.body) then
            assignment.body.from = assignment.body["from-process"]
            return "ok", base, assignment.body
        end
    else
        -- If the request is not proxied, we set the `from' field to the address
        -- that signed the request.
        assignment.body.from = ao.get("committers", assignment.body)
        return "ok", base, assignment.body
    end
end

-- Ensure that the source has the required funds, then debit the source. Takes
-- an origin, which can be used to identify the reason for the debit in logging.
-- Returns error if the source balance is not viable, or `ok` and the updated
-- base state if the debit is successful.
local function deduct_tokens(origin, base, request)
    local source = request.from
    local quantity = request.quantity

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

    -- Check 2: The source balance is a valiable number.
    if type(source_balance) ~= "number" then
        ao.event({ "Failure: Source balance is not a number.", { origin = origin },
            { request = request } })
        base.result = {
            status = "error",
            error = "Source balance is not a number.",
            ["source-balance"] = source_balance
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
    base.balance[request.to] = (base.balance[request.to] or 0) + quantity
    ao.event({ "Debit processed", { origin = origin }, { request = request } })
    return "ok", base
end

-- Debit the specified account by the given amount. Must be called with the
-- node's wallet. Debits regardless of the source balance, allowing the node to
-- put the source into debt, as deemed necessary by the computations it runs.
function debit(raw_base, assignment)
    ao.event({ "Debit request received", { assignment = assignment } })
    local status, base, request = validate_request(raw_base, assignment)
    if status == "error" then
        return "ok", base
    end

    -- Ensure that the request is signed by the admin.
    if not is_signed_by(base.admin, "request", request) then
        base.result = {
            status = "error",
            error = "Debit request not signed by node."
        }
        return "ok", base
    end

    -- Ensure that the quantity and account are present in the request.
    if not request.quantity or not request.account then
        ao.event({ "Failure: Quantity or account not found in request.",
            { request = request } })
        base.result = {
            status = "error",
            error = "Quantity or account not found in request."
        }
        return "ok", base
    end

    -- Debit the source. Note: We do not check the source balance here, because
    -- the node is capable of debiting the source at-will -- even it puts the
    -- source into debt. This is important because the node may estimate the
    -- cost of an execution at lower than its actual cost. Subsequently, the
    -- ledger should at least debit the source, even if the source may not
    -- deposit to restore this balance.
    ao.event({ "Debit request validated: ", { assignment = assignment } })
    base.balance = base.balance or {}
    base.balance[request.account] =
        (base.balance[request.account] or 0) - request.quantity
    
    ao.event({ "Debit request processed", { balances = base.balance } })
    return "ok", base
end

-- Withdraw the specified amount from the given account to the parent ledger.
function withdraw(raw_base, assignment)
    ao.event({ "Withdraw request received: ", { assignment = assignment } })
    local status, base, request = validate_request(raw_base, assignment)
    if status ~= "ok" then
        ao.event({ "Withdraw request message metadata validation failed",
            { assignment = assignment },
            { balances = base.balance }
        })
        return "ok", base
    end

    status, base = deduct_tokens("withdraw", base, request)
    if status ~= "ok" then
        ao.event({ "Withdraw request failed", { assignment = assignment } })
        -- Return the base unchanged.
        return "ok", base
    end

    -- Notify the parent ledger that the funds have been withdrawn.
    base.result = {
        status = "OK",
        outbox = {
            {
                target = base["parent-token"],
                action = "Credit-Notice",
                quantity = request.quantity,
                recipient = request.account
            }
        }
    }

    ao.event({ "Withdraw request processed", { balances = base.balance } })
    return "ok", base
end

-- Transfer the specified amount from the given account to the given account.
function transfer(raw_base, assignment)
    ao.event({ "Transfer request received", { assignment = assignment } })
    local status, base, request = validate_request(raw_base, assignment)
    if status ~= "ok" then
        return "ok", base
    end

    status, base = deduct_tokens("transfer", base, request)
    if status ~= "ok" then
        return "ok", base
    end

    -- Credit the destination account.
    base.balance[request.recipient] =
        (base.balance[request.recipient] or 0) + request.quantity
    ao.event({ "Transfer request processed", { balances = base.balance } })

    -- Notify the recipient that the funds have been transferred.
    base.result = {
        status = "OK",
        outbox = {
            {
                target = request.recipient,
                action = "Credit-Notice",
                quantity = request.quantity,
                recipient = request.recipient
            },
            {
                target = request.from,
                action = "Debit-Notice",
                quantity = request.quantity,
                recipient = request.recipient
            }
        }
    }
    
    return "ok", base
end

-- Process deposit notifications from the parent token.
_G["credit-notice"] = function (raw_base, assignment)
    ao.event({ "Credit-Notice received. Validating as deposit.",
        { assignment = assignment },
        { balances = raw_base.balance }
    })

    local status, base, request = validate_request(raw_base, assignment)
    if status ~= "ok" then
        ao.event({ "Credit notice validation failed: ",
            { assignment = assignment },
            { balances = raw_base.balance }
        })
        return status, base
    end

    -- Ensure that the credit notice is from the parent token. We do not transfer
    -- funds back to the sending token, for the sake of minimizing attack 
    -- surface. This makes computations running this script a `token-sink' if
    -- users send incorrect tokens to the ledger.
    if request.from ~= base["parent-token"] then
        base.result = {
            status = "error",
            error = "Invalid token deposit. Credit notice from " ..
                request.from .. " not admissible. Only " ..
                base["parent-token"] .. " is allowed on this sub-ledger."
        }
        return "ok", base
    end

    ao.event({ "Credit notice validated: ", { request = request } })
    base.balance = base.balance or {}
    base.balance[request.recipient] =
        (base.balance[request.recipient] or 0) +
        (request.quantity or 0)

    ao.event({ "Credit notice processed: ", { balances = base.balance } })
    return "ok", base
end

-- Register the sub-ledger with its parent token.
function register(base)
    -- Ensure that the base has a `result' field before we try to register.
    base.result = base.result or {}
    local SourceToken = base['source-token'] or ao.env.Process.Tags['Source-Token']
    local ParentToken = base['parent-token'] or ao.env.Process.Tags['Parent-Token']
    ao.event({ "Registering sub-ledger with parent token: ", {
        ParentToken = ParentToken,
        SourceToken = SourceToken
    }})

    -- Insert the register result into the base.
    table.insert(base.result, {
        status = "OK",
        outbox = {
            {
                action = "Register",
                target = ParentToken
            }
        }
    })
end

-- Return `info` about the sub-ledger.
function info(base, assignment)
    -- Attempt to validate the request. If it fails, return the base unchanged.
    local status, base, request = validate_request(base, assignment)
    if status ~= "ok" then
        return "ok", base
    end

    -- Insert the info result into the base.
    table.insert(base.result, {
        status = "OK",
        outbox = {
            {
                action = "Info",
                target = request.from,
                logo = base.logo or "",
                name = base.name or "",
                ticker = base.ticker or "",
                denomination = base.denomination or 0,
                parent_token = base["parent-token"] or "",
                source_token = base["source-token"] or ""
            }
        }
    })

    return "ok", base
end

_G["total-supply"] = function (raw_base, assignment)
    local status, base, request = validate_request(raw_base, assignment)
    if status ~= "ok" then
        return "ok", base
    end

    local total_supply = 0
    for _, balance in pairs(base.balance) do
        total_supply = total_supply + balance
    end

    -- Return the total supply.
    base.result = {
        status = "OK",
        outbox = {
            {
                action = "Total-Supply",
                target = request.from,
                data = total_supply,
                ticker = base.ticker or ""
            }
        }
    }

    return "ok", base
end

--- Index function, called by the `~process@1.0` device for scheduled messages.
--- We route any `action' to the appropriate function based on the request path.
function compute(base, assignment)
    ao.event({ "compute", { assignment = assignment }, { balances = base.balance } })
    if assignment.body.action == "debit" then
        return debit(base, assignment.body)
    elseif assignment.body.action == "credit-notice" then
        return _G["credit-notice"](base, assignment.body)
    elseif assignment.body.action == "withdraw" then
        return withdraw(base, assignment.body)
    elseif assignment.body.action == "transfer" then
        return transfer(base, assignment.body)
    elseif assignment.body.action == "balance" then
        return balance(base, assignment.body)
    elseif assignment.body.action == "info" then
        return info(base, assignment.body)
    elseif assignment.slot == 0 then
        -- If the slot is 0, we are registering the sub-ledger with its parent
        -- token.
        return register(base)
    end
end