# Pathing in AO-Core

## Overview

Understanding how to construct and interpret paths in AO-Core is fundamental to working with HyperBEAM. This guide explains the structure and components of AO-Core paths, enabling you to effectively interact with processes and access their data.

## HyperPATH Structure

Let's examine a typical HyperBEAM endpoint piece-by-piece:

```
https://router-1.forward.computer/<procId>[`~process@1.0`](../devices/process-at-1-0.md)/now
```

### Node URL (`router-1.forward.computer`)

The HTTP response from this node includes a signature from the host's key. By accessing the [`~snp@1.0`](../resources/source-code/dev_snp.md) device, you can verify that the node is running in a genuine Trusted Execution Environment (TEE), ensuring computation integrity. You can replace `router-1.forward.computer` with any HyperBEAM TEE node operated by any party while maintaining trustless guarantees.

### Process Path (`/<procId>~process@1.0`)

Every path in AO-Core represents a program. Think of the URL bar as a Unix-style command-line interface, providing access to AO's trustless and verifiable compute. Each path component (between `/` characters) represents a step in the computation. In this example, we instruct the AO-Core node to:

1. Load a specific message from its caches (local, another node, or Arweave)
2. Interpret it with the [`~process@1.0`](../devices/process-at-1-0.md) device
3. The process device implements a shared computing environment with consistent state between users

### State Access (`/now` or `/compute`)

Devices in AO-Core expose keys accessible via path components. Each key executes a function on the device:

- `now`: Calculates real-time process state
- `compute`: Serves the latest known state (faster than checking for new messages)

Under the surface, these keys represent AO-Core messages. As we progress through the path, AO-Core applies each message to the existing state. You can access the full process state by visiting:
```
/<procId>[`~process@1.0`](../devices/process-at-1-0.md)/now
```

### State Navigation

You can browse through sub-messages and data fields by accessing them as keys. For example, if a process stores its interaction count in a field named `at-slot`, you can access it like this:
```
/<procId>[`~process@1.0`](../devices/process-at-1-0.md)/compute/at-slot
```
This shows the latest 'slot' (number of interactions) of your process. Each response is:

- A message with a signature attesting to its correctness
- A hashpath describing its generation
- Transferable to other AO-Core nodes for uninterrupted execution

## Best Practices

1. Always verify cryptographic signatures on responses
2. Use appropriate caching strategies for frequently accessed data
3. Implement proper error handling for network requests
4. Consider rate limits and performance implications
5. Keep sensitive data secure and use appropriate authentication methods 

## Example: Using the `message` Device

Let's break down another common HyperPATH pattern using the built-in [`~message@1.0`](../devices/message-at-1-0.md) device:

```
http://localhost:10000/[`~message@1.0`](../devices/message-at-1-0.md)&hello=world&k=v/k
```

Here's how this works:

1.  **`http://localhost:10000/`**: The base address of your local HyperBEAM node.
2.  **[`~message@1.0`](../devices/message-at-1-0.md)**: This designates `~message@1.0` as the *root device*. The request is routed to the corresponding module (`dev_message`).
3.  **`&hello=world&k=v`**: These query parameters define the initial message. The `dev_message` device interprets them as key-value pairs, creating the starting message map: `#{ <<"hello">> => <<"world">>, <<"k">> => <<"v">> }`.
4.  **`/k`**: This path segment tells the `dev_message` device which key's value to retrieve from the message map constructed in the previous step.

The `dev_message` device receives the initial message and the path `/k`. It performs a lookup for the key `k` within the message `#{ <<"hello">> => <<"world">>, <<"k">> => <<"v">> }` and returns the corresponding value `v` as the response.

This pattern is useful for simple data retrieval or manipulating transient messages without needing a persistent process. 