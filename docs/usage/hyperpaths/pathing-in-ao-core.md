# Pathing in AO-Core

## Overview

Understanding how to construct and interpret paths in AO-Core is fundamental to working with HyperBEAM. This guide explains the structure and components of AO-Core paths, enabling you to effectively interact with processes and access their data.

## Path Structure

Let's examine a typical HyperBEAM endpoint piece-by-piece:

```
https://router-1.forward.computer/YOUR_STATE_PROCESS_ID~process@1.0/now/cache
```

### Node URL (`router-1.forward.computer`)

The HTTP response from this node includes a signature from the host's key. By accessing the `~snp@1.0` device, you can verify that the node is running in a genuine Trusted Execution Environment (TEE), ensuring computation integrity. You can replace `router-1.forward.computer` with any HyperBEAM TEE node operated by any party while maintaining trustless guarantees.

### Process Path (`/YOUR_STATE_PROCESS_ID~process@1.0`)

Every path in AO-Core represents a program. Think of the URL bar as a Unix-style command-line interface, providing access to AO's trustless and verifiable compute. Each path component (between `/` characters) represents a step in the computation. In this example, we instruct the AO-Core node to:
1. Load a specific message from its caches (local, another node, or Arweave)
2. Interpret it with the `process@1.0` device
3. The process device implements a shared computing environment with consistent state between users

### State Access (`/now` or `/compute`)

Devices in AO-Core expose keys accessible via path components. Each key executes a function on the device:
- `now`: Calculates real-time process state
- `compute`: Serves the latest known state (faster than checking for new messages)

Under the surface, these keys represent AO-Core messages. As we progress through the path, AO-Core applies each message to the existing state. You can access the full process state by visiting:
```
/procID~process@1.0/now
```

### State Navigation

You can browse through sub-messages and data fields by accessing them as keys. For example:
```
/procID~process@1.0/compute/at-slot
```
This shows the latest 'slot' (number of interactions) of your process. Each response is:
- A message with a signature attesting to its correctness
- A hashpath describing its generation
- Transferable to other AO-Core nodes for uninterrupted execution

### Cache Access (`/cache`)

The final component accesses the 'cache' element of the state. This works because:
1. The HyperBEAM node executes the schedule on the process
2. Finds the `method = "PATCH"` message in its outbox
3. Invokes the `patch@1.0` device
4. Merges your message with the process's state
5. Allows execution of the `cache` key to retrieve your data

!!! note
    The final path parameter (`cache`) is set by using `Send({ device = 'patch@1.0', cache = msg.Data })`. You can use different values than `cache` by changing this parameter.

## Deep Message Navigation

The `patch@1.0` device supports 'deep messages' in your state. By sending a full Lua table in the data field (unencoded or flattened), you can browse through the state and resolve keys individually, downloading only needed information.

For example, with a Balances table in your state process, you can access individual holders directly:
```
/procid~process@1.0/compute/cache/balances/address
```

## Best Practices

1. Always verify cryptographic signatures on responses
2. Use appropriate caching strategies for frequently accessed data
3. Implement proper error handling for network requests
4. Consider rate limits and performance implications
5. Keep sensitive data secure and use appropriate authentication methods 