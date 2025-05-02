# What is HyperBEAM?

HyperBEAM is the primary, production-ready implementation of the [AO-Core protocol](./what-is-ao-core.md), written in Erlang/OTP. It functions as a decentralized operating system, providing the runtime environment and necessary services to execute AO-Core computations across a network of distributed nodes.

## Key Roles

*   **AO-Core Implementation:** HyperBEAM translates the abstract concepts of AO-Core (Messages, Devices, Paths) into a concrete, runnable system.
*   **Decentralized OS:** It provides the necessary infrastructure for nodes to participate in the AO network, manage resources, execute computations, and communicate with each other.
*   **Erlang/OTP Foundation:** Built on the highly concurrent, fault-tolerant BEAM virtual machine, HyperBEAM inherits robustness, scalability, and features ideal for distributed systems, such as lightweight processes and message passing.
*   **Hardware Abstraction:** HyperBEAM allows AO computations to run independently of the underlying hardware, enabling a diverse network of nodes to contribute resources.
*   **Node Coordination:** It manages how individual nodes join the network, offer services (by running specific Devices), and interact with each other.

## Core Components & Features

*   **Preloaded Devices:** HyperBEAM comes with a suite of built-in devices (e.g., `~meta`, `~relay`, `~process`, `~scheduler`, `~wasm64`) providing essential functionalities for node operation, computation, and communication.
*   **HTTP Interface:** Nodes expose an HTTP server, allowing interaction via standard web requests and HyperPATHs.
*   **Modularity:** Its design allows for easy extension and addition of new devices and functionalities.
*   **Developer Tooling:** Includes tools for testing (`rebar3 eunit`), debugging (`?event` logging), and profiling (`eflame`).

In essence, HyperBEAM is the engine that powers the AO Computer, enabling the vision of a scalable, trust-minimized, decentralized supercomputer built on permanent storage.

*See also: [HyperBEAM GitHub Repository](https://github.com/permaweb/HyperBEAM)*
