--- A dynamic route generator in an AO `~process@1.0'.
--- This generator grants a routing table, found at `/now/routes', that is 
--- compatible with the `~router@1.0' interface. Subsequently, it can be
--- used for routing by HyperBEAM nodes' via setting the `route-provider'
--- node message key.
--- 
--- The configuration options are as follows:
--- /is-admissible = A message to call with the registration request's body. Should
---                return a boolean indicating whether the peer is admissible.
--- /sampling-rate = The frequency at which random sampling of registered nodes
---                should be performed, rather than scored routing. Default = 0.1.
--- /pricing-weight = The level to which pricing should be preferred relative to
---                   performance in the scoring algorithm. Default = 1.
--- /performance-weight = The level to which performance should be preferred
---                        relative to pricing in the scoring algorithm.
---                        Default = 1.
--- /score-preference = The level to which the scoring algorithm influence routing
---                      decisions amongst scored route generations. Default = 1,
---                      yielding an exponential decay in preference for better
---                      performing nodes.

DefaultIsAdmissible = {
    path = "/default",
    default = true
}

DefaultSamplingRate = 0.1
DefaultPricingWeight = 1
DefaultPerformanceWeight = 1
DefaultScorePreference = 1

local function ensure_opts(state)
    state["is-admissible"] = state["is-admissible"] or DefaultIsAdmissible
    state["sampling-rate"] = state["sampling-rate"] or DefaultSamplingRate
    state["pricing-weight"] = state["pricing-weight"] or DefaultPricingWeight
    state["performance-weight"] = state["performance-weight"] or DefaultPerformanceWeight
    state["score-preference"] = state["score-preference"] or DefaultScorePreference
    return state
end

-- Find the current route message for a template.
local function current_route(routes, template, opts)
    local status, res =
        ao.resolve({
            path = "/~router@1.0/match",
            ["route-path"] = template, -- Only supports binary templates for now.
            routes = routes
        })
    if status == "ok" then
        return res
    else
        return { template = template, nodes = {} }
    end
end

-- Compute the decay for a given score, modulated by the score preference.
local function decay(state, score)
    return math.exp(-state["score-preference"] * score)
end

-- Compute the scores for all routes.
local function recalculate_scores(state, routes, opts)
    -- Calculate the score per node.

    for _, route in ipairs(routes) do
        -- The performance score for the node on the route should be scaled by
        -- the performance weight, moderated by the sampling rate. The sampling
        -- rate is used to ensure that new nodes (and improving nodes) are given
        -- a chance to be selected.
        local perf_score =
            decay(state, route.performance * state["performance-weight"]) *
            ((1 - route.price * state["sampling-rate"]) + state["sampling-rate"])
        -- The price score for the node on the route should be scaled by the
        -- pricing weight. It is not moderated by the sampling rate, as we want
        -- to ensure that the node is selected if it has a low price. New nodes
        -- can improve their likelihood of being selected by lowering their price.
        local price_score = decay(state, route.price * state["pricing-weight"])

        route.weight = perf_score + price_score

    end

    return routes
end

-- Compute the new routes, with their weights, based on the current routes and
-- a new route.
local function recalculate_routes(state, route, opts)
    local routes = state.routes
    local path = route["route-path"] or route.path
    local price = route.price

    local r = current_route(routes, path, opts)
    local nodes = r.nodes
    table.insert(nodes, {
        node = route.node,
        price = price,
        performance = 0
    })

    r = recalculate_scores(state, r, opts)

    -- TODO: Remove the prior route from the list before adding the new one.
    table.insert(routes, r)

    return routes
end

local function add_route(state, req, opts)
    local path = req.path
    if path == "register" then
        local status, is_admissible = ao.resolve(state["is-admissible"])
        if status == "ok" and is_admissible ~= false then
            print "TRUSTED PEER"
            state.routes = recalculate_routes(state, req.route)
        else
            print "UNTRUSTED PEER"
        end

        return state
    end
end

-- Main handler for incoming scheduled messages.
function compute(state, assignment, opts)
    state = ensure_opts(state)
    state.routes = state.routes or {}
    local req = assignment.body
    local path = req.path

    if path == "register" then
        add_route(state, req, opts)
    end

end