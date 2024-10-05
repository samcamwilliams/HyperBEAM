# Notes on the structure of Hyperbeam's CU Architecture.

## Taxonomy
- `beamr`: A WAMR-based Linked-In Driver offering a WASM execution environment in Erlang.
- `cu_process`: The 'motherboard' process for managing a live AO process inside the Hyperbeam Erlang environment.
- `Schedules`: Lists of sequential assignments (with data) for a process, retreived from a SU.
- `Checkpoints`: Serialized copies of process or component state at the _end_ of execution for a given slot.
- `Component`: User configurable optional mechanisms to layer upon the basic framework that AO provides. Components can be any of the following types:
1. `Machines`: The virtual machine implementations that the hyperbeam node supports.
2. `Interfaces (1)`: Defining the mechanism of moving information in and out of a process's environment during calculation of a message result.
3. `Transformers (0-n)`: Taking a process schedule (or fragment thereof) and returning a modified version. For example, `CRON`, which adds 'implicit' messages to the schedule of a process, allowing it to 'wake up' periodically.
4. `Devices (0-n)`: Components that do not interact with the operating mode of a process, but do offer a callable mechanism that processes can interact with (via their `Interface`).

## Components

Each component implemented in Hyperbeam must implement `init`, `terminate`, `serialize` and `deserialize` methods. Additionally, each component can optionally specify a `call` function that is exposed inside the AO process's execution environment (in the format imposed by the `Interface`).

Every process must define at maximum one `Interface` component (providing no interface component will result in the AO default being used), but may define any number of `Transformers` and `Devices`.

## Checkpoints

Checkpoints come in two `Scope`s: `Process` and `Component`. A `Process` scope checkpoint is a bundle of `Component` scope checkpoints, which in turn contain the result of running `ComponentMod:serialize` on the present state. The `Process` scope checkpoint is labeled with the appropriate `Epoch` and `Slot` number, as well as an optional additional `Sub-Schedule` field with information from the schedule `Transformer`.

## Boot

During boot, the most recent trusted `Process` scope checkpoint for the process is retreived, and each component is called to deserialize its data. If no checkpoint is found, each component has its `init` function called.

## Execution

To execute a message on a process, the `cu_process` goes through the following process:
1. If the message whose result has been requested is not a dry-run, or is a dry-run with a `Maximum-Cache` time that invalidates the local cache, the CU will retreive the latest assignments from the SU or Arweave and apply them.
2. The `TransformerMod:apply` is executed upon the schedule prior to execution in order to shape the flow of updates as the process has determined necessary.
3. The `Interface:format` function is called with the new message and its assignment, returning a function to call inside the machine and any arguments to pass. The interface may also return a list of custom handlers for functions that are to be made callable inside by the `Module` the `Machine` is running.
4. The `MachineMod:apply` function is called with the returned parameters.
5. During execution, the `Module` the `MachineMod` is running may request execution of callbacks to its `Extensions`. These requests are marshalled by the process's `Interface` component, via the functions it registered in the `Machine` component in step 3.
6. Upon termination of the called function, the `Machine` passes any returned values to the process's `Interface` component, which parses them and returns a message in response.
7. The resulting message is returned by the `cu_process` to the caller, optionally focusing on only one message found in a bundle returned in step 6, etc.