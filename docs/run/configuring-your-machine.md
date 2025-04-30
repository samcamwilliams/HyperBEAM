# Configuring Your Machine for HyperBEAM

To successfully build and run a HyperBEAM node, your system needs several software dependencies installed. This guide outlines the necessary components.

## Core Build Tools & Libraries (Linux - apt)

On Debian-based Linux distributions (like Ubuntu), you can install most core dependencies using `apt`:

```bash
sudo apt-get update && sudo apt-get install -y --no-install-recommends \
    build-essential \
    cmake \
    git \
    pkg-config \
    ncurses-dev \
    libssl-dev \
    sudo \
    curl \
    ca-certificates
```

*   `build-essential`: Basic C/C++ compilers and build tools (gcc, g++, make).
*   `cmake`: Build system generator.
*   `git`: Version control for fetching the source code.
*   `pkg-config`: Helps find installed libraries during compilation.
*   `ncurses-dev`: Required for some terminal interface elements used by Erlang/OTP.
*   `libssl-dev`: Necessary for cryptographic operations and secure connections (HTTPS).
*   `sudo`: Needed for system-level installations.
*   `curl`: Used for downloading dependencies or interacting with web services.
*   `ca-certificates`: Required for validating SSL certificates.

*(Note: Package names may vary slightly on other Linux distributions or macOS. Use your system's package manager accordingly, e.g., `yum`, `dnf`, `pacman`, `brew`.)*

## Erlang/OTP

HyperBEAM is built on Erlang/OTP. You need a compatible version installed (check the `rebar.config` or project documentation for specific version requirements, but typically a recent version like 25.x or 26.x is recommended).

Installation methods vary:

*   **Package Managers:** `sudo apt install erlang`, `brew install erlang`.
*   **Source Build:** Download from [erlang.org](https://www.erlang.org/downloads) and build.
*   **Version Managers:** Tools like `asdf-vm` with the `asdf-erlang` plugin are highly recommended for managing multiple Erlang versions.
    ```bash
    asdf plugin add erlang
    asdf install erlang <version>
    asdf global erlang <version>
    ```

## Rebar3

Rebar3 is the build tool for Erlang projects.

*   **Download:** Get the `rebar3` binary from the [official website](https://rebar3.org/).
*   **Installation:** Place the downloaded `rebar3` file in your system's `PATH` (e.g., `/usr/local/bin`) and make it executable (`chmod +x rebar3`).
*   **Via `asdf`:** If using `asdf`, you can install it via the `rebar` plugin:
    ```bash
    asdf plugin add rebar
    asdf install rebar <version>
    asdf global rebar <version>
    ```

## Node.js

Node.js might be required for certain JavaScript-related tools or dependencies used within the broader AO ecosystem or specific device functionalities.

*   **Package Managers:** `sudo apt install nodejs npm`, `brew install node`.
*   **Version Managers:** `asdf-vm` with the `asdf-nodejs` plugin is recommended.

## Rust

Rust is needed if you intend to work with or build components involving WebAssembly (WASM) or certain Native Implemented Functions (NIFs) used by some devices (like `~snp@1.0`).

*   **`rustup`:** The recommended way to install Rust is via `rustup`:
    ```bash
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    source "$HOME/.cargo/env"
    ```

## Platform Specific Notes

*   **macOS:** Use [Homebrew](https://brew.sh/) (`brew`) to install most dependencies (`cmake`, `git`, `pkg-config`, `openssl`, `ncurses`, `erlang`, `rebar3`, `node`). You might need to set specific environment variables for `openssl` during Erlang compilation if building from source.
*   **Windows:** Setting up the environment on Windows can be more complex. Using the Windows Subsystem for Linux (WSL) with a distribution like Ubuntu is often the easiest path.

Once these dependencies are installed, you should be able to clone the HyperBEAM repository and build it using `rebar3 compile`.
