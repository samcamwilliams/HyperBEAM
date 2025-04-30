# Exposing Process State with the Patch Device

The `~patch@1.0` device provides a mechanism for AO processes to expose parts of their internal state, making it readable via direct HTTP GET requests along the process's HyperPath.

## Why Use the Patch Device?

Standard AO process execution typically involves sending a message to a process, letting it compute, and then potentially reading results from its outbox or state after the computation is scheduled and finished. This is asynchronous.

The `patch` device allows for a more direct, synchronous-like read pattern. A process can use it to "patch" specific data elements from its internal state into a location that becomes directly accessible via a HyperPath GET request *before* the full asynchronous scheduling might complete.

This is particularly useful for:

*   **Web Interfaces:** Building frontends that need to quickly read specific data points from an AO process without waiting for a full message round-trip.
*   **Data Feeds:** Exposing specific metrics or state variables for monitoring or integration with other systems.
*   **Caching:** Allowing frequently accessed data to be retrieved efficiently via simple HTTP GETs.

## How it Works

1.  **Process Logic:** Inside your AO process code (e.g., in Lua or WASM), when you want to expose data, you construct an **Outbound Message** targeted at the `~patch@1.0` device.
2.  **Patch Message Format:** This outbound message typically includes tags that specify:
    *   `Device = "patch@1.0"` (or just `patch@1.0`)
    *   A key-value pair representing the data to expose. The **key** becomes the final segment in the HyperPath used to access the data, and the **value** is the data itself.
    *   Example Lua using `aos`: `Send({ Target = ao.id, Device = "patch@1.0", myDataKey = MyValue })`
3.  **HyperBEAM Execution:** When HyperBEAM executes the process schedule and encounters this outbound message:
    *   It invokes the `dev_patch` module.
    *   `dev_patch` inspects the message.
    *   It takes the specified key (`myDataKey` in the example) and its associated value (`MyValue`) and makes this value available at a specific path.
4.  **HTTP Access:** You (or any HTTP client) can now access this data directly using a GET request:
    ```
    GET /<process-id>~process@1.0/compute/<myDataKey>
    # Or potentially using /now/
    GET /<process-id>~process@1.0/now/<myDataKey>
    ```
    The HyperBEAM node serving the request will resolve the path up to `/compute` (or `/now`), then use the logic associated with the patched data (`myDataKey`) to return the `MyValue` directly.

## Example (Lua in `aos`)

```lua
-- In your process code (e.g., loaded via .load)
Handlers.add(
  "PublishData",
  Handlers.utils.hasMatchingTag("Action", "PublishData"),
  function (msg)
    local dataToPublish = "Some important state: " .. math.random()
    -- Expose 'currentStatus' key with the data
    Send({ Target = ao.id, Device = "patch@1.0", currentStatus = dataToPublish })
    print("Published data to /currentStatus")
  end
)

-- Spawning and interacting
[aos]> MyProcess = spawn(MyModule)

[aos]> Send({ Target = MyProcess, Action = "PublishData" })
-- Wait a moment for scheduling

-- Now you can access the data via HTTP (using curl or a browser)
-- GET <node_url>/<MyProcess_ID>~process@1.0/compute/currentStatus
-- Or using aos utilities if available:
[aos]> ReadState(MyProcess).currentStatus
```

## Key Points

*   **Key Naming:** The tag name you use in the `Send` call (e.g., `currentStatus`) becomes the final segment in the accessible HyperPath.
*   **Data Types:** The `patch` device typically handles basic data types (strings, numbers) effectively. Complex nested tables might require specific encoding or handling (like flattened tables for deep message navigation, though that's a more advanced feature).
*   **`compute` vs `now`:** Accessing patched data via `/compute/...` typically serves the last known patched value quickly. Accessing via `/now/...` might involve more computation to ensure the absolute latest state before checking for the patched key.
*   **Not a Replacement for State:** Patching is primarily for *exposing* reads. It doesn't replace the core state management within your process handler logic.

By using the `patch` device, you can make parts of your AO process state easily and efficiently readable over standard HTTP, bridging the gap between decentralized computation and web-based applications.
