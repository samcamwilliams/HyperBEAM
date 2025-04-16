<div class="header-main">
	<div class="header-logo">
		<div class="logo-container">
		<div class="logo-stripes">
			<div class="stripe green"></div>
			<div class="stripe yellow"></div>
			<div class="stripe blue"></div>
			<div class="stripe purple"></div>
			<div class="stripe red"></div>
		</div>
		<div class="logo-text">
			<h1>hyperBEAM.</h1>
			<p class="tagline">DOCUMENTATION</p>
		</div>
		</div>
	</div>
</div>

!!! warning "Platform Support"
    This documentation is currently written specifically for **Ubuntu 22.04**. Support for macOS and other platforms will be added in future updates.

## Overview

HyperBEAM is a client implementation of the AO-Core protocol, written in Erlang. It enables a decentralized computing platform where programs run as independent processes, communicate via asynchronous message passing, and operate across a distributed network of nodes.

For detailed technical information about HyperBEAM's architecture and functionality, see the [HyperBEAM Overview](hyperbeam/index.md).

### What is AO-Core?

AO-Core is a protocol built to enable decentralized computations, offering a series of universal primitives. Instead of enforcing a single, monolithic architecture, AO-Core provides a framework into which any number of different computational models, encapsulated as primitive and composable devices, can be attached.

AO-Core's protocol is built upon the following primitives:

- **Hashpaths**: A mechanism for referencing locations in a program's state-space prior to execution
- **Unified data structure**: For representing program states as HTTP documents
- **Attestation protocol**: For expressing attestations of states found at particular hashpaths
- **Meta-VM**: Allowing various state transformation programs (virtual machines and computational models, expressed in the form of devices) to be executed inside the AO-Core protocol

<!-- ## System Components

The HyperBEAM ecosystem consists of two main components:

1. **HyperBEAM**: The Erlang-based node software that handles message routing, process management, and device coordination.
2. **Compute Unit (CU)**: A NodeJS implementation that executes WebAssembly modules and handles computational tasks.

For detailed information about the system architecture and available devices, see the [HyperBEAM Overview](hyperbeam/index.md). -->

## Quick Start Guide

To get started with HyperBEAM:

1. [Check system requirements](getting-started/requirements.md)
2. [Install dependencies](getting-started/installation/index.md)
3. [Set up HyperBEAM](hyperbeam/setup.md)
4. [Configure the Compute Unit](compute-unit/setup.md)
5. [Verify your installation](guides/integration.md)

## Documentation Structure

This documentation is organized into the following sections:

- **[Getting Started](getting-started/index.md)**: System requirements and installation instructions
- **[HyperBEAM](hyperbeam/index.md)**: Core setup, configuration, and testing
- **[Compute Unit](compute-unit/index.md)**: Setup and configuration of the CU component
- **[Guides](guides/index.md)**: Step-by-step tutorials and walkthroughs
- **[Reference](reference/index.md)**: API documentation and troubleshooting

## Community and Support

- **GitHub HyperBEAM**: [permaweb/HyperBEAM](https://github.com/permaweb/HyperBEAM)
- **Github Local CU**: [permaweb/local-cu](https://github.com/permaweb/local-cu)
- **Discord**: [Join the community](https://discord.gg/V3yjzrBxPM)
- **Issues**: [File a bug report](https://github.com/permaweb/HyperBEAM/issues)

## License

HyperBEAM is open-source software licensed under the [MIT License](https://github.com/permaweb/HyperBEAM/blob/main/LICENSE.md).
