# Running a HyperBEAM Node

This guide provides the basics for running your own HyperBEAM node, allowing you to participate in the AO network, contribute resources, and potentially offer services.

## Prerequisites

Before running a node, ensure you have the necessary [system dependencies installed](./configuring-your-machine.md) and have cloned the [HyperBEAM repository](https://github.com/permaweb/HyperBEAM).

## Starting a Basic Node

The simplest way to start a HyperBEAM node for development or testing is using `rebar3` from the repository's root directory:

```bash
rebar3 shell
```

This command:

1.  Compiles the HyperBEAM source code (if necessary).
2.  Starts the Erlang Virtual Machine (BEAM) with all HyperBEAM modules loaded.
3.  Initializes the node with default settings.
4.  Starts the default HTTP server (typically on port 80, but may vary), making the node accessible via HyperPaths.
5.  Drops you into an interactive Erlang shell where you can interact with the running node.

This basic setup is suitable for local development and exploring HyperBEAM's functionalities.

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
