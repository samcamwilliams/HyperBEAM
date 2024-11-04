# The Permaweb Abstract Machine.
## Status: DRAFT-1
## Authors: Sam Williams, Tom Wilson, Tyler Hall, James Pichota, Vince Juliano < {first}@arweave.org>

This document contains a rough specification, in `engineering note` form, for the Permaweb Abstract Machine (PAM). PAM is a method through which the permaweb can be interpreted as not only a flat, permanent ledger of data, but also as a reactive computation environment. Through this frame, the permaweb can be interpreted as a single, shared ['system image'](https://en.wikipedia.org/wiki/Single_system_image) that can be accessed and added to permissionlessly. Notably, the PAM itself intends to be a truly minimal computation model: It does not enforce any forms of consensus upon its execution directly, nor any the use of any particular virtual machine. Even the requirements it imparts upon the host runtime environment are minimal. Instead, the PAM focuses on offering the simplest possible representation of _data_ and _computation_ upon that data, tailored for Arweave's distributed environment and [HTTP](https://datatracker.ietf.org/doc/html/rfc9114) access methods.

[HyperBEAM](https://github.com/permaweb/HyperBEAM) is an implementation of the permaweb abstract machine, as well as [AO](https://ao.arweave.net), a framework ontop of the PAM that constructs an environment for _trustless_ (not just _permissionless_) computation.

## Context

In this document we refer to 'the permaweb' as the collection of all pieces of permanent data stored on the [Arweave protocol](https://draft-17.arweave.dev/). In some other venues, 'the permaweb' is sometimes also used to refer to the _stack_ of protocols constructed ontop of Arweave in order to turn its permanent data replication into a 'web' of 'pages', as popularized by HTTP. For our purposes, we interpret the permaweb dataset at the level of abstract 'bundled items', as described in the [Arweave 2.0 release notes](https://github.com/ArweaveTeam/arweave/releases/tag/N.2.0.0.0), ignoring differences between 'base layer' Arweave transactions and bundled items.

## Machine Definition

Every item on the permaweb is described as a `Message`. Each `Message` is interpretable by PAM as a `map` of named functions, or as a concrete binary term. Each function in a message may take another message as a `map` of arguments.

Each `message` on the permaweb may optionally state a `Device` which should be used by PAM-compatible systems to interpret its contents. If no `Device` key is explicitly stated by the message, it must be infered as `Identity`. The `Map` device should be implemented to simply return the binary or message at a given function name (a `key` in the map). Every `Device` must implement functions with the names `ID` and `Keys`. An `ID` is a function that can be used in order to refer to a message at a later time, while `Keys` should return a binary representation (with `Encoding` optionally specified as a parameter in the argument message) of each key in the message.

Concretely, these relations can be expressed as follows:
```
Types:
    Permaweb :: [Message]
    Message :: Map< Name :: Binary, (Message?) => Message > | Binary

Functions:
    Message(Key, Parameters?) => Message :: PAM.apply(Message, Key, Parameters) => Message
    PAM.apply(Message[Device], Message, Key, Parameters? :: Message) => Message

Invariants:
    ∀ message ∈ Permaweb, ∃ <<"ID">> ∈ Message
    ∀ message ∈ Permaweb, ∃ <<"Keys">> ∈ Message
    ∀ message ∈ Permaweb, ∃ (<<"Device">> ∈ Message ∨ PAM.apply(message, <<"Device">>) => <<"Map">>)
```

The PAM intends to be a computer native to the technologies of the internet. As such, every message in this system can be refered to via a `Path`. A path starts from a given message in the system (whether written to the permaweb yet or not), and applies a series of function's to the message via their names. Each resulting message itself must have an `ID` resolvable via its device -- subsequently enabling additional paths to be described atop the intermediate message.

For example:
```
    /StartingID/MethodA/MethodB/MethodC ==
    /(PAM.apply(StartingMessage[Device], <<"MethodA">>, #{}))/MethodB/MethodC =>
    ...
        
```