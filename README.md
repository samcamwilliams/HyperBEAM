<img src="https://arweave.net/dOpRkKrNNQ4HHebxZlPCo0BWfrjwJ-CEBQs2EPgrwbg" />

This repository contains a reference implementation of AO-Core, along with an
Erlang-based (BEAM) client implementing a number of devices for the protocol.

AO-Core is a protocol built to enable decentralized computations, offering a
series of universal primitives to achieve this end. Instead of enforcing a single,
monolithic architecture, AO-Core provides a framework into which any number of
different computational models, encapsulated as primitive `devices`, can be attached.

AO-Core's protocol offers a framework for decentralized computations, built upon the
following fundamental primitives:

1. Hashpaths: A mechanism for referencing locations in a program's state-space
prior to execution. These state-space `links` are represented as Merklized lists of
programs inputs and initial states.
2. A unified data structure for representing program states as HTTP documents,
as described in the [HTTP Semantics RFC](https://www.rfc-editor.org/rfc/rfc9110.html).
3. A unified protocol for expressing `attestations` of the `states` found at
particular `hashpaths`. These attestations allow nodes to participate in varied
economic and cryptographic mechanisms to prove and challenge each-other's
representations regarding the programs that operate inside the AO-Core protocol.
4. A meta-VM that allows any number of different virtual machines and computational
models (`devices`) to be executed inside the AO-Core protocol, while enabling their
states and inputs to be calculated and attested to in a unified format.

## What is HyperBeam?

HyperBeam is a client implementation of the AO-Core protocol, written in Erlang.
It can be seen as the 'node' software for the decentralized operating system that
AO enables; abstracting hardware provisioning and details from the execution of
individual programs.

HyperBEAM node operators can offer the services of their machine to others inside
the network by electing to execute any number of different `devices`, charging 
users for their computation as necessary.

Each HyperBEAM node is configured using the `~meta@1.0` device, which provides
an interface for specifying the node's hardware, supported devices, metering and
payments information, amongst other configuration options.

## Getting Started

To begin using HyperBeam, you will need to install:

- The Erlang runtime (OTP 27)
- Rebar3

Then you can clone the HyperBEAM source and build it:

```bash
git clone https://github.com/ao-labs/hyperbeam.git
cd hyperbeam
rebar3 compile
```

If you would prefer to execute HyperBEAM in a containerized environment, you
can use the provided Dockerfile to build a container image.

```bash
docker build -t hyperbeam .
```

If you intend to offer TEE-based computation of AO-Core devices, please see the
[`HyperBEAM OS`]() repo for details on configuration and deployment.

## Configuration

HyperBeam can be configured using a `~meta@1.0` device. This device is initialized
via the command line arguments provided when the node is started.

```bash
rebar3 shell --eval "hb:start_mainnet(#{ [OPTS] })."
```

For example, in order to start a node using a custom port and Arweave wallet,
you could execute the following command:

```bash
rebar3 shell --eval "hb:start_mainnet(#{ port => 10001, key_location => 'path/to/my/wallet.key' })."
```

Additionally, if you would like to modify a running node's configuration, you can
do so by sending a HTTP Signed Message using any RFC-9421 compatible client in 
the following form:

```
POST /~meta@1.0/info
Your-Config-Tag: Your-Config-Tag
```

The individual headers provided in the message will each be interpreted as additional
configuration options for the node.

## Devices

HyperBeam supports a number of different devices, each of which enable different
services to be offered by the node. There are presently 25 different devices
included in the `preloaded_devices` of a HyperBEAM node, although it is possible
to add and remove devices as necessary.

### Preloaded Devices

The following devices are included in the `preloaded_devices` of a HyperBEAM node: