# The Permaweb Abstract Machine.
## Status: DRAFT-1
## Authors: Sam Williams, Tom Wilson, Tyler Hall, James Pichota, Vince Juliano < {first}@arweave.org>

This document contains a rough specification, in `engineering note` form, for the Permaweb Abstract Machine (PAM). PAM is a method through which the permaweb can be interpreted as not only a flat, permanent ledger of data, but also as a mutable and reactive computation environment. Through this frame, the permaweb can be interpreted as a single, shared ['system image'](https://en.wikipedia.org/wiki/Single_system_image) that can be accessed and added to permissionlessly. Notably, the PAM itself intends to be a truly minimal computation model: It does not enforce any forms of consensus upon its execution directly, nor the use of any particular virtual machine. Even the requirements it imparts upon the host runtime environment are minimal. Instead, the PAM focuses on offering the simplest possible representation of _data_ and _computation_ upon that data, tailored for Arweave's distributed environment and [HTTP](https://datatracker.ietf.org/doc/html/rfc9114) access methods.

[HyperBEAM](https://github.com/permaweb/HyperBEAM) is an implementation of the permaweb abstract machine, as well as [AO](https://ao.arweave.net), a framework ontop of the PAM that constructs an environment for _trustless_ -- not just _permissionless_ -- computation.

## Context

In this document we refer to 'the permaweb' as the collection of all pieces of permanent data stored on the [Arweave protocol](https://draft-17.arweave.dev/). In some other venues, 'the permaweb' is sometimes also used to refer to the _stack_ of protocols constructed ontop of Arweave in order to turn its permanent data replication into a 'web' of 'pages', as popularized by HTTP. For our purposes, we refer to the permaweb dataset at the level of abstract 'bundled items', as described in the [Arweave 2.0 release notes](https://github.com/ArweaveTeam/arweave/releases/tag/N.2.0.0.0), ignoring differences between 'base layer' Arweave transactions and bundled items.

In this specification we refer to a number of abstract properties of computation systems that are extremely useful, but often refered to with imprecision. For the avoidance of doubt we will be using them with the following defitions:

`Permissionlessness`: No actor in the ecosystem may be denied -- by any other, or group thereof -- the ability to use the network.

`Trustlessness`: Users can participate in the network without needing to trust other parties are not acting maliciously.

Both of these properties are extremely powerful and can only be offered by decentralized computation machines. These properties can also, unfortunately, only presently be offered in degrees. For example, Bitcoin may offer a high level of `permissionlessness`, but it is not absolute: A user still requires that a majority of the mining power does not censor their transactions or blocks containing them. Similarly, while your recipient of a Bitcoin transfer has a high degree of `trustlessness`, at minimum the user must still trust the implementors of the cryptographic verification algorithms of their client in order to use the system. Rather than offering a single, standardized approach to the problem of offering permissionless and trustlessness computation, which would necessitate enforcing the same trade-offs upon all parties, PAM and AO instead focus on allowing users to make their own appropriate choices amongst a variety of options while still being able to interoperate together.

## Machine Definition

Every item on the permaweb is described as a `Message`. Each `Message` is interpretable by PAM as a `map` of named functions, or as a concrete binary term. Each function in a message may be called through the creation of another message, providing a map of arguments to the execution.

Each `message` on the permaweb may optionally state a `Device` which should be used by PAM-compatible systems to interpret its contents. If no `Device` key is explicitly stated by the message, it must be infered as `Map`. The `Map` device should be implemented to simply return the binary or message at a given function name (a `key` in the map). Every `Device` must implement functions with the names `ID` and `Keys`. An `ID` is a function that can be used in order to refer to a message at a later time, while `Keys` should return a binary representation (with `Encoding` optionally specified as a parameter in the argument message) of each key in the message.

Concretely, these relations can be expressed as follows:
```
Types:
    Permaweb :: [Message]
    Message :: Map< Name :: Binary, (Message?) => Message > | Binary

Functions:
    Message(Key, ParameterMessage?) => Message :: PAM.apply(Message, Key, Parameters) => Message
    PAM.apply(Message[Device], Message, RuntimeEnv? :: Message) => Message

Invariants:
    ∀ message ∈ Permaweb, ∃ <<"ID">> ∈ Message
    ∀ message ∈ Permaweb, ∃ <<"Keys">> ∈ Message
    ∀ message ∈ Permaweb, ∃ (<<"Device">> ∈ Message ∨ PAM.apply(message, <<"Device">>) => <<"Map">>)
```

The PAM intends to be a computer native to the technologies of the internet. More specifically, we have focused its representation on compatibility with the HTTP family of protocols (`HTTP/{1.1,2,3}`). As such, every message in this system can be refered to via `Path`s. A path starts from a given message in the system (whether written to the permaweb yet or not), and applies a series of additional messages on top of it. Each resulting message itself must have an `ID` resolvable via its device -- subsequently enabling additional paths to be described atop the intermediate message.

For example:
```
    /StartingID/Input1ID/Input2ID/Input3ID =>
	/{PAM.apply(StartingMsg, Input1)}/Input2ID/Input3ID =>
	/OutputID1/Input2ID/Input3ID =>
	...
	/Output3ID
        
```

## Hyperbeam Devices

The hyperbeam environment implements PAM through the use of device modules. Each module in the base deployment is namespaced `dev_*`, for example `dev_scheduler`. Devices in hyperbeam can communicate the basic set of keys that they offer using the function exports. Each of these functions is interpreted as yielding the value for a key on a message containing that device, with the key's name being defined by its function name. Each of these functions may take one to three arguments of the following: `function(StateMessage, InputMessage, Environment)`. It must yield a result of the form `{Status, NewMessage}`, where `Status` is typically (but not necessarily) either `ok` or `error`. If a device does not offer one of the required functions (`ID` or `device`), the environment falls back to the underlying `message` device implementations. `tolowercase` is applied to all function names before execution, and `-` characters are replaced with `_` to allow for more readable device implementations.

The special case `info/{0,1,2}` function may be implemented by the device, signalling environment requirements to hyperbeam. The `info` function should can optionally take the `message` in question and the environment variables as arguments. It should return a map of environmental information of the following form:

```
	info([Message, [Env]]) -> #{
		handler => HandleFunc
		default => DefaultFunc,
		variant => <<"Variant/VersionID">>,
		uses => UseDefinition | #{ Key => UseDefinition }
	}
```

If the `default` parameter is provided, the function will be used as the entrypoint all key resolution when a matching function is not found. The key's name is provided as an additional first argument in this case (`defaultFun/{2,3,4}`).

If the `handler` key is provided, all key resolution is routed through it instead of using Erlang functions.

The `uses` info key may be optionally utilized to signal to the environment which keys the device -- or a specific key inside it -- needs in order to execute. `UseDefinition`s should be of the following form:

```
	UseDefition :: [Key] |
		#{ read => [Key], write => [Key] }
```

## The Stack Device

In order to allow messages to have more flexibility in their execution, hyperbeam offers an implementation of a PAM `stack`-style device, which combines a series of devices on a message into a single 'stack' of executable transformations. This device allows many complex forms of processors to be built inside the PAM environment -- for example, AO processes -- whiile transferring the architecture's modularity and flexibility to them.

When added as the highest `Device` tag on a message, the stack device scans the remainder of the message's tags looking for (and subsequently loading) any other messages it finds. When a user then calls an execution on top of a message containing a device, the device passes through each of the elements of the stack in turn, 'folding' over it.