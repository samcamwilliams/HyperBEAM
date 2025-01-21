# HyperBEAM: A Decentralized Supercomputer on Arweave

HyperBEAM is an Erlang implementation of the Converge Protocol, a novel computation layer built on top of Arweave. It leverages Arweave's permanent storage to create a decentralized, verifiable, and scalable compute environment.  This README provides developers with the necessary information to understand and contribute to the HyperBEAM project.

## Purpose

HyperBEAM transforms Arweave from a simple data storage network into a global, shared, and permissionless supercomputer. It enables the execution of arbitrary logic (including WASM modules) on persistent data, generating verifiable computation results.

## Key Features

* **Decentralized Computation:** Execute code on a globally distributed network, eliminating single points of failure.
* **Verifiable Computations:** Generate cryptographically verifiable results, ensuring trustworthiness.
* **Scalability:** The architecture intrinsically avoids protocol-level scalability limitations.
* **WASM Support:** Execute WebAssembly modules for enhanced computational capabilities.
* **HTTP-Native Interface:** Seamless integration with existing internet infrastructure using HTTP.
* **AO (Actor-Oriented) Framework:**  Provides a process-based environment for orchestrating computations (Implemented within HyperBEAM).
* **Flexible Attestation:** Support for dual attestation (TPM & AMD SEV-SNP) for enhanced security.
* **Modular Design:**  Uses a "device" system for modularity and extensibility.

## Technologies Used

* **Erlang:** The primary programming language for the backend.
* **WebAssembly (WASM):**  Used for extending computation capabilities.
* **WAMR (Wasm Micro Runtime):** The WebAssembly runtime engine.
* **Cowboy:** The Erlang HTTP server library.
* **Gun:**  An Erlang HTTP client library.
* **Prometheus:**  For monitoring and metrics.
* **Grafana:** For visualizing metrics.
* **RocksDB:**  A persistent key-value store (optional).
* **Quicker:** (For HTTP/3)
* **Apache Avro:** Used for efficient encoding of Tags.
* **Base64:** Used to ensure safe, stable conversions of ID structures.
* **OpenSSL:** Under-the-hood for the core cryptography primitives.
* **Google Cloud Platform (GCP):** Used for continuous integration/deployment (CI/CD).
* **Packer:**  For creating and managing GCP VMs.

## Prerequisites

Before getting started, ensure you have the following installed:

* **Erlang/OTP:**  [https://www.erlang.org/downloads](https://www.erlang.org/downloads)
* **Rebar3:**  The Erlang build tool.  You can install it with `curl -sL https://raw.githubusercontent.com/rebar/rebar3/master/bootstrap | bash`
* **Git:** For cloning the repository.
* **Node.js and npm:** For certain development tasks (if applicable).
* **Docker and docker-compose:** Used for running the Prometheus/Grafana metrics.
* **Google Cloud SDK:** For deploying to GCP (only if you plan to run the CD pipeline).
* **Packer:**  [https://developer.hashicorp.com/packer/get-started/install](https://developer.hashicorp.com/packer/get-started/install) (For GCP deployment).
* **`go-tpm-tools`:** For managing TPM operations (Required for deploying to GCP).


## Installation

1. **Clone the repository:**

   ```bash
   git clone https://github.com/permaweb/HyperBEAM.git
   cd HyperBEAM
   ```

2. **Install dependencies:**

   ```bash
   rebar3 get-deps
   ```

3. **Compile the project:**

   ```bash
   rebar3 compile
   ```

4. **Build the release:**

   ```bash
   rebar3 release
   ```

This creates the `_build/default/rel/ao` directory containing the HyperBEAM release.


## Usage Examples

HyperBEAM exposes a powerful HTTP API.  See `docs/converge-http-api.md` for detailed API specification.  Here are some basic examples using `curl`:

**Simple Computation (WASM):**

First, build a WASM module and place it in the `test/` directory (e.g., `test/test.wasm`).  Then, assuming `hyperbeam` is running on port 8734:

```bash
curl -X POST -H "Content-Type: application/octet-stream" -d @test/test.wasm http://localhost:8734/Init/Compute -H "Device: WASM-64/1.0" -H "WASM-Function: fac" -H "WASM-Params: [5]"
```

**AO Process Management:**

This requires more complex message construction. Refer to the documentation for AO process specifics.  Examples might include scheduling messages (`POST /process_id/Schedule`), retrieving the process state (`GET /process_id/Compute/result`), and pushing messages (`POST /process_id/Push`).

**GCP Deployment (CI/CD):**

Requires setting up GCP credentials as a GitHub secret (`CD_SERVICE_ACCOUNT`). Then, push to the `main` branch to trigger the CI/CD pipeline:

```bash
git push origin main
```

This pipeline builds the HyperBEAM image using Packer and deploys it to a confidential VM on GCP. See `.github/workflows/cd.yaml` for details.


## Configuration

Configuration is primarily handled through environment variables and the `rebar.config` file.  Key environment variables include:

* `HB_KEY`: Path to the private key file.
* `HB_PORT`: The port number to run the HTTP server on (default: 8734).
* `HB_STORE`: The type and configuration of the persistent store (e.g., `"hb_store_fs"`).  Refer to `packer.pkr.hcl` for the GCP configuration.  See `GCP-notes.md` and `DEPLOY-notes.md` for deployment configuration details.
* `HB_MODE`: Run mode (`debug` or `prod`).  Production mode disables verbose logging and runs the system with more stringent security settings.
* `HB_PRINT`: Controls the detail of CLI event logging.  A string containing comma separated topic names,  `true`, or `1` for verbose output.


## Project Structure

```
HyperBEAM/
├── .editorconfig           # Editor configuration.
├── .githooks/               # Git hooks for enforcing code style and commit message conventions.
│   ├── _/install.sh        # Git hook installation script.
│   └── commit-msg          # Git hook to validate commit messages.
├── .gitpod.Dockerfile      # Dockerfile for Gitpod.
├── .gitpod.yml             # Gitpod configuration.
├── .github/                # GitHub Actions workflows.
│   ├── documentation/     # Documentation for GitHub Actions workflows.
│   │   └── workflows.md    # Main workflows document.
│   └── workflows/          # Actual workflow yaml files.
│       └── cd.yaml         # CI/CD deployment workflow.
├── certificates/           # Security certificates (e.g., AMD SEV-SNP).
├── c_src/                  # C source code for the BEAMR driver (WASM interfacing).
│   └── hb_beamr.c          # Main C code file.
├── docs/                    # Project documentation.
│   ├── converge-http-api.md # HTTP API documentation.
│   ├── converge-protocol.md # Converge protocol documentation.
│   ├── gathering-metrics-locally.md # Documentation for setting up local metrics (Prometheus/Grafana).
│   ├── hacking-on-hyperbeam.md # Guide for contributing and working with the codebase.
│   ├── style-guide.md      # Project style guide.
│   └── workflows.md        # Github Actions workflows descriptions.
├── erlang_ls.config        # Configuration for Erlang Language Server (ELS).
├── LICENSE.md              # License information (Business Source License 1.1).
├── Makefile                # Build instructions.
├── metrics/                # Prometheus and Grafana configuration.
│   ├── dashboards.yaml     # Grafana dashboard configuration.
│   ├── datasources.yaml    # Grafana data source configuration.
│   ├── docker-compose.yml  # Docker Compose configuration.
│   └── prometheus.yml     # Prometheus configuration.
├── packer.pkr.hcl          # Packer configuration (GCP).
├── README.md                # This file.
├── rebar.config            # Rebar3 configuration.
├── rebar.lock              # Rebar3 lock file.
├── src/                     # Erlang source code.
└── test/                    # Test files (WASM modules).
    └── README.md
```

## Contributing Guidelines

See `docs/hacking-on-hyperbeam.md` for detailed guidelines.  In short:  Follow the project style guide, write tests, and ensure all tests pass before submitting a pull request.


## License

This project is licensed under the Business Source License 1.1 (BSL-1.1).  See `LICENSE.md` for details.  Consult [https://arweave.medium.com/arweave-is-an-evolutionary-protocol-e072f5e69eaa](https://arweave.medium.com/arweave-is-an-evolutionary-protocol-e072f5e69eaa) for information regarding the future licensing of the AO codebase.


## Error Handling

The system uses a flexible error-handling mechanism controlled via the `error_strategy` option (default: `throw`).  Error messages are generally returned as JSON-encoded maps with descriptive error details.  Common error messages include:

* `invalid_message`:  The input message was not properly formatted.
* `device_call_failed`: A call to a device function failed.
* `invalid_path`: The message path does not exist in the message.
* `module_not_admissable`: Attempted to load a device module that is not explicitly allowed by configuration.
* `remote_devices_disabled`: Attempted to load a device module from a remote node, but this feature is disabled in the config.
* `device_signer_not_trusted`: Attempted to load a remote device module, but the module signer was not trusted.

Check the logs (`HB_PRINT`) for more detailed error diagnostics.  The logs will print stack traces for debugging purposes unless explicitly configured otherwise (`debug_print_trace`).


## Known Issues

* **Security:**  Some cryptographic algorithms and aspects of the architecture are experimental and need further auditing.  Use in production requires careful consideration.
* **Scalability:** While the architecture promotes scalability, real-world performance testing is ongoing.


## Future Work

* Extensive security audits.
* Enhanced error handling and reporting.
* Improved documentation and examples.
* Support for additional storage backends (e.g., distributed databases).
* Further performance optimization.



