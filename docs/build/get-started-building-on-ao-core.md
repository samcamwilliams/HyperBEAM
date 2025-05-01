# Getting Started Building on AO-Core

Welcome to building on AO, the decentralized supercomputer!

AO combines the permanent storage of Arweave with the flexible, scalable computation enabled by the AO-Core protocol and its HyperBEAM implementation. This allows you to create truly autonomous applications, agents, and services that run trustlessly and permissionlessly.

## Core Idea: Processes & Messages

At its heart, building on AO involves:

1.  **Creating Processes:** Think of these as independent programs or stateful contracts. Each process has a unique ID and maintains its own state.
2.  **Sending Messages:** You interact with processes by sending them messages. These messages trigger computations, update state, or cause the process to interact with other processes or the outside world.

Messages are processed by [Devices](../begin/ao-devices.md), which define *how* the computation happens (e.g., running WASM code, executing Lua scripts, managing state transitions).

## Starting `aos`: Your Development Environment

The primary tool for interacting with AO and developing processes is `aos`, a command-line interface and development environment.

=== "npm"
    ```bash
    npm i -g https://get_ao.g8way.io
    ```

=== "bun"
    ```bash
    bun install -g https://get_ao.g8way.io
    ```

=== "pnpm"
    ```bash
    pnpm add -g https://get_ao.g8way.io
    ```

**Starting `aos`:**

Simply run the command in your terminal:

```bash
aos
```

This launches an interactive Lua environment connected to the AO network. By default, it connects to the mainnet.

**What `aos` is doing:**

*   **Connecting:** Establishes a connection to AO network nodes (usually public routers).
*   **Loading Wallet:** Looks for a default Arweave key file (usually `~/.aos.json` or specified via arguments) to sign outgoing messages.
*   **Providing Interface:** Gives you a Lua prompt (`[aos]>`) where you can:
    *   Load code for new processes.
    *   Send messages to existing processes.
    *   Inspect process state.
    *   Manage your local environment.

## Your First Interaction: Spawning a Process

From the `aos` prompt, you can spawn a simple process. Let's spawn a basic Lua process that just holds some data:

```lua
[aos]> .load my-first-process.lua
-- Assume my-first-process.lua contains: print("Hello from my process!")

[aos]> MyProcess = spawn(MyModule)
-- This sends a message to AO to create a new process
-- using the code loaded from my-first-process.lua.
-- The unique process ID is stored in the 'MyProcess' variable.

[aos]> MyProcess
-- Displays the Process ID
```

You've just created your first decentralized program on AO!

## Next Steps

This is just the beginning. To dive deeper:

*   **AO Cookbook:** Explore practical examples and recipes for common tasks: [AO Cookbook](https://cookbook_ao.arweave.net/)
*   **Expose Process State:** Learn how to make your process data accessible via HTTP using the `patch` device: [Exposing Process State](./exposing-process-state.md)
*   **Serverless Compute:** Discover how to run WASM or Lua computations within your processes: [Serverless Decentralized Compute](./serverless-decentralized-compute.md)
*   **aos Documentation:** Refer to the official `aos` documentation for detailed commands and usage.
