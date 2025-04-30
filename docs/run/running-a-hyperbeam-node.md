# Running a HyperBEAM Node

This guide provides the basics for running your own HyperBEAM node, allowing you to participate in the AO network, contribute resources, and potentially offer services.

## Prerequisites

Before running a node, ensure you have:

*   The necessary [system dependencies installed](./configuring-your-machine.md), including **Erlang/OTP 27** and **Rebar3**.
*   Cloned the [HyperBEAM repository](https://github.com/permaweb/HyperBEAM) (`git clone ...`).
*   Compiled the source code (`rebar3 compile` in the repo directory).
*   An Arweave **wallet keyfile** (e.g., generated via [Wander](https://www.wander.app)).

## Starting a Basic Node

The simplest way to start a HyperBEAM node for development or testing is using `rebar3` from the repository's root directory:

```bash
rebar3 shell
```

This command:

1.  Starts the Erlang Virtual Machine (BEAM) with all HyperBEAM modules loaded.
2.  Initializes the node with default settings (from `hb_opts.erl`).
3.  Starts the default HTTP server (typically on **port 10000**), making the node accessible via HyperPaths.
4.  Drops you into an interactive Erlang shell where you can interact with the running node.

This basic setup is suitable for local development and exploring HyperBEAM's functionalities.

## Optional Build Profiles

HyperBEAM uses build profiles to enable optional features, often requiring extra dependencies. To run a node with specific profiles enabled, use `rebar3 as ... shell`:

**Available Profiles (Examples):**

*   `genesis_wasm`: Enables Genesis WebAssembly support.
*   `rocksdb`: Enables the RocksDB storage backend.
*   `http3`: Enables HTTP/3 support.

**Example Usage:**

```bash
# Start with RocksDB profile
rebar3 as rocksdb shell

# Start with RocksDB and Genesis WASM profiles
rebar3 as rocksdb,genesis_wasm shell
```

*Note: Choose profiles *before* starting the shell, as they affect compile-time options.*

## Configuration Options (Environment Variables & Args)

You can customize your node's behavior using environment variables or command-line arguments passed via `erl_opts`.

**Common Options:**

*   **`HB_PORT=<port_number>`:** Sets the port for the HTTP server.
    *   Example: `HB_PORT=8080 rebar3 shell`
*   **`HB_KEY=<path/to/wallet.key>`:** Specifies the Arweave key file the node will use to sign its messages. If not provided, a temporary key might be generated or a default sought.
    *   Example: `HB_KEY=~/.keys/arweave_key.json rebar3 shell`
*   **`HB_STORE=<directory_path>`:** Sets a specific local directory for the node's data store. Useful for isolating data when running multiple nodes or for persistent storage.
    *   Example: `HB_STORE=./node_data_1 rebar3 shell`
*   **`HB_PRINT=<setting>`:** Controls the level of debug logging output. `<setting>` can be `true` (or `1`), or a comma-separated list of modules/topics (e.g., `hb_path,hb_ao,ao_result`).
    *   Example: `HB_PRINT=hb_http,dev_router rebar3 shell`

**Using `erl_opts` (Alternative):**

You can pass Erlang VM arguments directly:

```bash
rebar3 shell --erl_opts "-hb_port 8080 -hb_key path/to/key.json"
```

Refer to the `hb_opts.erl` source file for a comprehensive list of configuration options.

## Verify Installation

To quickly check if your node is running and accessible, you can send a request to its `~meta@1.0` device (assuming default port 10000):

```bash
curl http://localhost:10000/~meta@1.0/info
```

A JSON response containing node information indicates success.

## Running in Mainnet Mode

For connecting to the main AO network, specific configurations might be required. A helper function `hb:start_mainnet/1` exists for common mainnet setups:

```erlang
% Inside the rebar3 shell:
hb:start_mainnet(#{ port => 9001, key_location => "path/to/wallet.key" }).
```

Consult further documentation or community resources for detailed mainnet deployment guides.

## Stopping the Node

To stop the node running in the shell, press `Ctrl+C` twice or use the Erlang command `q().`.

## Next Steps

*   **Configure Your Machine:** Ensure all [system dependencies](./configuring-your-machine.md) are correctly installed.
*   **TEE Nodes:** Learn about running nodes in [Trusted Execution Environments](./tee-nodes.md) for enhanced security.
*   **Routers:** Understand how to configure and run a [router node](./joining-running-a-router.md).
