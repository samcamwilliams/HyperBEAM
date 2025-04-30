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
3.  Starts the default HTTP server (typically on **port 10000**), making the node accessible via HyperPATHs.
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
rebar3 as rocksdb, genesis_wasm shell
```

*Note: Choose profiles **before** starting the shell, as they affect compile-time options.*

## Configuration (config.flat)

The primary way to configure your HyperBEAM node is through a `config.flat` file located in the node's working directory or specified by the `HB_CONFIG_LOCATION` environment variable.

This file uses a simple `Key = Value.` format (note the period at the end of each line).

**Example `config.flat`:**

```erlang
% Set the HTTP port
hb_port = 8080.

% Specify the Arweave key file
hb_key = "/path/to/your/wallet.json".

% Set the data store directory
hb_store = "./node_data_mainnet".

% Enable verbose logging for specific modules
hb_print = "hb_http,dev_router".
```

Refer to the `src/hb_opts.erl` source file for a comprehensive list of available configuration keys and their default values.

## Overrides (Environment Variables & Args)

You can override settings from `config.flat` or provide values if the file is missing using environment variables or command-line arguments.

**Using Environment Variables:**

Environment variables typically use an `HB_` prefix followed by the configuration key in uppercase.

*   **`HB_PORT=<port_number>`:** Overrides `hb_port`.
    *   Example: `HB_PORT=8080 rebar3 shell`
*   **`HB_KEY=<path/to/wallet.key>`:** Overrides `hb_key`.
    *   Example: `HB_KEY=~/.keys/arweave_key.json rebar3 shell`
*   **`HB_STORE=<directory_path>`:** Overrides `hb_store`.
    *   Example: `HB_STORE=./node_data_1 rebar3 shell`
*   **`HB_PRINT=<setting>`:** Overrides `hb_print`. `<setting>` can be `true` (or `1`), or a comma-separated list of modules/topics (e.g., `hb_path,hb_ao,ao_result`).
    *   Example: `HB_PRINT=hb_http,dev_router rebar3 shell`
*   **`HB_CONFIG_LOCATION=<path/to/config.flat>`:** Specifies a custom location for the configuration file.

**Using `erl_opts` (Direct Erlang VM Arguments):**

You can also pass arguments directly to the Erlang VM using the `-<key> <value>` format within `erl_opts`. This is generally less common for application configuration than `config.flat` or environment variables.

```bash
rebar3 shell --erl_opts "-hb_port 8080 -hb_key path/to/key.json"
```

**Order of Precedence:**

1.  Command-line arguments (`erl_opts`).
2.  Environment variables (`HB_*`).
3.  Settings in `config.flat`.
4.  Default values from `hb_opts.erl`.

## Verify Installation

To quickly check if your node is running and accessible, you can send a request to its `~meta@1.0` device (assuming default port 10000):

```bash
curl http://localhost:10000/~meta@1.0/info
```

A JSON response containing node information indicates success.

## Running for Production (Mainnet)

While you can connect to the main AO network using the `rebar3 shell` for testing purposes (potentially using specific configurations or helper functions like `hb:start_mainnet/1` if available and applicable), the standard and recommended method for a stable production deployment (like running on the mainnet) is to build and run a **release**.

**1. Build the Release:**

From the root of the HyperBEAM repository, build the release package. You might include specific profiles needed for your mainnet setup (e.g., `rocksdb` if you intend to use it):

```bash
# Build release with default profile
rebar3 release

# Or, build with specific profiles (example)
# rebar3 as rocksdb release
```

This command compiles the project and packages it along with the Erlang Runtime System (ERTS) and all dependencies into a directory, typically `_build/default/rel/hb`.

**2. Configure the Release:**

Navigate into the release directory (e.g., `cd _build/default/rel/hb`). You will find a `config.flat` file (or you might need to copy your customized one here). Edit this `config.flat` file to set your desired mainnet parameters (port, key file location, store path, specific peers, etc.). Environment variables can also be used to override settings in the release's `config.flat`, just like in the shell environment.

**3. Start the Node:**

Use the generated start script (`bin/hb`) to run the node:

```bash
# Start the node in the foreground (logs to console)
./bin/hb console

# Start the node as a background daemon
./bin/hb start

# Check the status
./bin/hb ping
./bin/hb status

# Stop the node
./bin/hb stop
```

Consult the generated `bin/hb` script or Erlang/OTP documentation for more advanced start-up options (e.g., attaching a remote shell).

Running as a release provides a more robust, isolated, and manageable way to operate a node compared to running directly from the `rebar3 shell`.

## Stopping the Node (rebar3 shell)

To stop the node running *within the `rebar3 shell`*, press `Ctrl+C` twice or use the Erlang command `q().`.

## Next Steps

*   **Configure Your Machine:** Ensure all [system dependencies](./configuring-your-machine.md) are correctly installed.
*   **TEE Nodes:** Learn about running nodes in [Trusted Execution Environments](./tee-nodes.md) for enhanced security.
*   **Routers:** Understand how to configure and run a [router node](./joining-running-a-router.md).
