# Configuring Your HyperBEAM Node

This guide details the various ways to configure your HyperBEAM node's behavior, including ports, storage, keys, and logging.

## Configuration (`config.flat`)

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

Refer to the [`src/hb_opts.erl`](../resources/source-code/hb_opts.md) source file for a comprehensive list of available configuration keys and their default values.

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
2.  Settings in `config.flat`.
3.  Environment variables (`HB_*`).
4.  Default values from `hb_opts.erl`.

## Configuration in Releases

When running a release build (see [Running a HyperBEAM Node](./running-a-hyperbeam-node.md)), configuration works similarly:

1.  A `config.flat` file will be present in the release directory (e.g., `_build/default/rel/hb/config.flat`). Edit this file to set your desired parameters for the release environment.
2.  Environment variables (`HB_*`) can still be used to override the settings in the release's `config.flat` when starting the node using the `bin/hb` script.
