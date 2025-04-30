# Device: ~process@1.0

## Overview

The `~process@1.0` device provides the core abstraction for persistent, shared computations within AO, analogous to smart contracts in other systems but with greater flexibility. It orchestrates the interaction between scheduling, state management, and computation execution for a specific process instance.

**Status:** Stable

## Core Concept: Orchestration

A message tagged with `Device: process@1.0` (the "Process Definition Message") doesn't typically perform computation itself. Instead, it defines *which other devices* should be used for key aspects of its lifecycle:

*   **Scheduler Device:** Determines the order of incoming messages (assignments) to be processed. (Defaults to `~scheduler@1.0`).
*   **Execution Device:** Executes the actual computation based on the current state and the scheduled message. Often configured as `dev_stack` to allow multiple computational steps (e.g., running WASM, applying cron jobs, handling proofs).
*   **Push Device:** Handles the injection of new messages into the process's schedule. (Defaults to `~push@1.0`).

The `~process@1.0` device acts as a router, intercepting requests and delegating them to the appropriate configured device (scheduler, executor, etc.) by temporarily swapping the device tag on the message before resolving.

## Key Functions (Keys)

These keys are accessed via HyperPATHs relative to the Process Definition Message ID (`<ProcessID>`).

*   **`GET /<ProcessID>~process@1.0/schedule`**
    *   **Action:** Delegates to the configured Scheduler Device (via the process's `schedule/3` function) to retrieve the current schedule or state.
    *   **Response:** Depends on the Scheduler Device implementation (e.g., list of message IDs).
*   **`POST /<ProcessID>~process@1.0/schedule`**
    *   **Action:** Delegates to the configured Push Device (via the process's `push/3` function) to add a new message to the process's schedule.
    *   **Request Body:** The message to be added.
    *   **Response:** Confirmation or result from the Push Device.
*   **`GET /<ProcessID>~process@1.0/compute/<TargetSlotOrMsgID>`**
    *   **Action:** Computes the process state up to a specific point identified by `<TargetSlotOrMsgID>` (either a slot number or a message ID within the schedule). It retrieves assignments from the Scheduler Device and applies them sequentially using the configured Execution Device.
    *   **Response:** The process state message after executing up to the target slot/message.
    *   **Caching:** Results are cached aggressively (see `dev_process_cache`) to avoid recomputation.
*   **`GET /<ProcessID>~process@1.0/now`**
    *   **Action:** Computes and returns the `Results` key from the *latest* known state of the process. This typically involves computing all pending assignments.
    *   **Response:** The value of the `Results` key from the final state.
*   **`GET /<ProcessID>~process@1.0/slot`**
    *   **Action:** Delegates to the configured Scheduler Device to query information about a specific slot or the current slot number.
    *   **Response:** Depends on the Scheduler Device implementation.
*   **`GET /<ProcessID>~process@1.0/snapshot`**
    *   **Action:** Delegates to the configured Execution Device to generate a snapshot of the current process state. This often involves running the execution stack in a specific "map" mode to gather state from different components.
    *   **Response:** A message representing the process snapshot, often marked for caching.

## Process Definition Example

A typical process definition message might look like this (represented conceptually):

```text
Device: process@1.0
Scheduler-Device: scheduler@1.0
Execution-Device: stack@1.0
Execution-Stack: "scheduler@1.0", "cron@1.0", "wasm64@1.0", "PoDA@1.0"
Cron-Frequency: 10-Minutes
WASM-Image: <WASMImageTxID>
PoDA:
    Device: PoDA/1.0
    Authority: <AddressA>
    Authority: <AddressB>
    Quorum: 2
```

This defines a process that uses:
*   The standard scheduler.
*   A stack executor that runs scheduling logic, cron jobs, a WASM module, and a Proof-of-Data-Availability check.

## State Management & Caching

`~process@1.0` relies heavily on caching (`dev_process_cache`) to optimize performance. Full state snapshots and intermediate results are cached periodically (configurable via `Cache-Frequency` and `Cache-Keys` options) to avoid recomputing the entire history for every request.

## Initialization (`init`)

Processes often require an initialization step before they can process messages. This is typically triggered by calling the `init` key on the configured Execution Device via the process path (`/<ProcessID>~process@1.0/init`). This allows components within the execution stack (like WASM modules) to set up their initial state.

[process module](../resources/source-code/dev_process.md)
