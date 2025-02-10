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

## Messages

HyperBEAM describes every piece of data as a `message`, which can be interpreted as
a binary term or as collection of named functions aka. a `Map` of functions.

Every message _may_ specify a `device` which is interpreted by the AO-Core compatible
system in order to operate upon the message's contents, which to say read it, or
execute it. Executing a named function within a message, providing a map of arguments,
results in yet another `message`.

In this way, A `message` _begets_ another `message`, giving rise to a vast computational
space, leveraging function application and composition at its core.

> Notably, this computation does not require the computor of a message
> to know the values of all the keys contained therin. In other words, keys
> may be _lazily_ evaluated, and only by computors that are interested
> in their outputs, or even _sharded_ across arbitrary sets of nodes, as necessary

If a `message` does not explicitly specify a `device`, its implied `device` is `Map`,
which simply returns the binary or `message` at a given named function.

## Devices

HyperBeam supports a number of different devices, each of which enable different
services to be offered by the node. There are presently 25 different devices
included in the `preloaded_devices` of a HyperBEAM node, although it is possible
to add and remove devices as necessary.

### Preloaded Devices

The following devices are included in the `preloaded_devices` of a HyperBEAM node:

## IDs, Hashpaths & Converge.

Along the path of executing and orchestrating `devices`, HyperBEAM will compute a
a number of resultant `messages` as well as a series of cryptographic linkages. First,
each `message` has a cryptographic ID that uniquely identifies it in the space of computation.
Second, each `message` will have linked the series of function applications that produced it.
This value is called the `hashpath` and can be undestood as a memoization of the tree of execution
that was applied to produce the message.

Due to it Merkalized form, a `hashpath` can be used as concise, and cryptographically verifiable, way
to reconstruct the entire branch of inputs used to generate a given `message`.

Also, because multiple paths of executions may produce the same resultant `message`, HyperBEAM is able
to compute and store "links" between `hashpaths` that each point to the same underlying `message`.
In this sense, HyperBEAM "converges" computation, linking paths to the same data, similar to how HTTP
may respond with a `Redirect` to a Hyperlink.
