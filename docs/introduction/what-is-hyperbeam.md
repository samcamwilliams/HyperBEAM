# What is HyperBEAM?

HyperBEAM is the primary, production-ready implementation of the [AO-Core protocol](./what-is-ao-core.md), built on the robust Erlang/OTP framework. It serves as a decentralized operating system, powering the AO Computer—a scalable, trust-minimized, distributed supercomputer built on permanent storage. HyperBEAM provides the runtime environment and essential services to execute AO-Core computations across a network of distributed nodes.

## Why HyperBEAM Matters

HyperBEAM transforms the abstract concepts of AO-Core—such as [Messages](./what-is-ao-core.md#core-concepts), [Devices](./what-is-ao-core.md#core-concepts), and [Paths](./what-is-ao-core.md#core-concepts)—into a concrete, operational system. Here's why it's pivotal to the AO ecosystem:

- **Modularity via Devices:** HyperBEAM introduces a uniquely modular architecture centered around [Devices](./ao-devices.md). These pluggable components define specific computational logic (like running WASM, managing state, or relaying data), allowing for unprecedented flexibility, specialization, and extensibility in a decentralized system.
- **Decentralized OS:** It equips nodes with the infrastructure to join the AO network, manage resources, execute computations, and communicate seamlessly.
- **Erlang/OTP Powerhouse:** Leveraging the BEAM virtual machine, HyperBEAM inherits Erlang's concurrency, fault tolerance, and scalability—perfect for distributed systems with lightweight processes and message passing.
- **Hardware Independence:** It abstracts underlying hardware, allowing diverse nodes to contribute resources without compatibility issues.
- **Node Coordination:** It governs how nodes join the network, offer services through specific Devices, and interact with one another.
- **Verifiable Computation:** Through hashpaths and the Converge Protocol, HyperBEAM ensures computation results are cryptographically verified and trustworthy.

In essence, HyperBEAM is the engine that drives the AO Computer, enabling a vision of decentralized, verifiable computing at scale.

## Core Components & Features

- **Pluggable Devices:** The heart of HyperBEAM's extensibility. It includes essential built-in devices like [`~meta`](../devices/meta-at-1-0.md), [`~relay`](../devices/relay-at-1-0.md), [`~process`](../devices/process-at-1-0.md), [`~scheduler`](../devices/scheduler-at-1-0.md), and [`~wasm64`](../devices/wasm64-at-1-0.md) for core functionality, but the system is designed for easy addition of new custom devices.
- **Message System:** Everything in HyperBEAM is a "Message"—a map of named functions or binary data that can be processed, transformed, and cryptographically verified.
- **HTTP Interface:** Nodes expose an HTTP server for interaction via standard web requests and HyperPATHs, structured URLs that represent computation paths.
- **Modularity:** Its design supports easy extension, allowing new devices and functionalities to be added effortlessly.

## Architecture

*   **Initialization Flow:** When a HyperBEAM node starts, it initializes the name service, scheduler registry, timestamp server, and HTTP server, establishing core services for process management, timing, communication, and storage.
*   **Compute Model:** Computation follows the pattern `Message1(Message2) => Message3`, where messages are resolved through their devices and paths.
*   **Scheduler System:** The scheduler component manages execution order using "slots" - sequential positions that organize computation in a deterministic way.
*   **Process Slots:** Each process has numbered slots starting from 0 that track message execution order, ensuring consistent computation even across distributed nodes.

## Slot System

Slots are a fundamental concept in the HyperBEAM scheduler component, providing a structured mechanism for organizing and sequencing computation.

*   **Sequential Ordering:** Slots act as numbered containers (starting at 0) that hold specific messages or tasks to be processed in a deterministic order.
*   **State Tracking:** The "current-slot" field tracks process execution state, indicating which messages have been processed and which are pending.
*   **Assignment Storage:** Each slot contains an "assignment" - the cryptographically verified message waiting to be executed.
*   **Schedule Organization:** The collection of all slots for a process forms its "schedule", which can be queried to view ranges of assignments.
*   **Application Scenarios:**
    * **Scheduling Messages:** When a message is posted to a process, it's assigned to the next available slot
    * **Status Monitoring:** Clients can query a process's current slot to check progress
    * **Task Retrieval:** Processes find their next task by requesting the next slot number
    * **Distributed Consistency:** Slots ensure deterministic execution order across nodes

This slotting mechanism is central to AO processes built on HyperBEAM, allowing for deterministic, verifiable computation that maintains consistency across a distributed system.

## HTTP API and Pathing

HyperBEAM exposes a powerful HTTP API that allows for interacting with processes and accessing data through structured URL patterns. We call URLs that represent computation paths "HyperPATHs". The URL bar effectively functions as a command-line interface for AO's trustless and verifiable compute.

For a comprehensive guide on constructing and interpreting paths in AO-Core, including detailed examples and best practices, see [Pathing in AO-Core](./pathing-in-ao-core.md).

In essence, HyperBEAM is the engine that powers the AO Computer, enabling the vision of a scalable, trust-minimized, decentralized supercomputer built on permanent storage.

*See also: [HyperBEAM GitHub Repository](https://github.com/permaweb/HyperBEAM)*
