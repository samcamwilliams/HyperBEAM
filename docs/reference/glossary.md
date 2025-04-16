# Glossary

This glossary provides definitions for terms and concepts used throughout the HyperBEAM documentation.

### AO-Core Protocol
The underlying protocol that HyperBEAM implements, enabling decentralized computing and communication between nodes. AO-Core provides a framework into which any number of different computational models, encapsulated as primitive devices, can be attached.

### Asynchronous Message Passing
A communication paradigm where senders don't wait for receivers to be ready, allowing for non-blocking operations and better scalability.

### Checkpoint
A saved state of a process that can be used to resume execution from a known point, used for persistence and recovery.

### Compute Unit (CU)
The NodeJS component of HyperBEAM that executes WebAssembly modules and handles computational tasks.

### Decentralized Execution
The ability to run processes across a distributed network without centralized control or coordination.

### Device
A functional unit in HyperBEAM that provides specific capabilities to the system, such as storage, networking, or computational resources.

### Erlang
The programming language used to implement the HyperBEAM core, known for its robustness and support for building distributed, fault-tolerant applications.

### ~flat@1.0
A format used for encoding settings files in HyperBEAM configuration, using HTTP header styling.

### Hashpaths
A mechanism for referencing locations in a program's state-space prior to execution. These state-space links are represented as Merklized lists of programs inputs and initial states.

### HyperBEAM
The Erlang-based node software that handles message routing, process management, and device coordination in the HyperBEAM ecosystem.

### Message
A data structure used for communication between processes in the HyperBEAM system. Messages can be interpreted as a binary term or as a collection of named functions (a Map of functions).

### Module
A unit of code that can be loaded and executed by the Compute Unit, typically in WebAssembly format.

### Node
An instance of HyperBEAM running on a physical or virtual machine that participates in the distributed network.

### ~p4@1.0
A device that runs as a pre-processor and post-processor in HyperBEAM, enabling a framework for node operators to sell usage of their machine's hardware to execute AO-Core devices.

### Process
An independent unit of computation in HyperBEAM with its own state and execution context.

### Process ID
A unique identifier assigned to a process within the HyperBEAM system.

### ~scheduler@1.0
A device used to assign a linear hashpath to an execution, such that all users may access it with a deterministic ordering.

### ~compute-lite@1.0
A lightweight device wrapping a local WASM executor, used for executing legacynet AO processes inside HyperBEAM.

### ~json-iface@1.0
A device that offers a translation layer between the JSON-encoded message format used by legacy versions and HyperBEAM's native HTTP message format.

### ~meta@1.0
A device used to configure the node's hardware, supported devices, metering and payments information, amongst other configuration options.

### ~process@1.0
A device that enables users to create persistent, shared executions that can be accessed by any number of users, each of whom may add additional inputs to its hashpath.

### ~relay@1.0
A device used to relay messages between nodes and the wider HTTP network. It offers an interface for sending and receiving messages using a variety of execution strategies.

### ~simple-pay@1.0
A simple, flexible pricing device that can be used in conjunction with p4@1.0 to offer flat-fees for the execution of AO-Core messages.

### ~snp@1.0
A device used to generate and validate proofs that a node is executing inside a Trusted Execution Environment (TEE).

### ~wasm64@1.0
A device used to execute WebAssembly code, using the Web Assembly Micro-Runtime (WAMR) under-the-hood.

### ~stack@1.0
A device used to execute an ordered set of devices over the same inputs, allowing users to create complex combinations of other devices.

### Trusted Execution Environment (TEE)
A secure area inside a processor that ensures the confidentiality and integrity of code and data loaded within it. Used in HyperBEAM for trust-minimized computation.

### WebAssembly (WASM)
A binary instruction format that serves as a portable compilation target for programming languages, enabling deployment on the web and other environments. 