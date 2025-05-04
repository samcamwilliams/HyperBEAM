# Device: ~scheduler@1.0

## Overview

The `~scheduler@1.0` device is responsible for managing the order of message execution for an AO process. It maintains the list of pending messages (assignments) and provides them sequentially to the process's Execution Device.

**Status:** Stable

## Core Concept: Message Ordering

When messages are sent to an AO process (typically via the `~push@1.0` device or a `POST` to the process's `/schedule` endpoint), they are added to a queue managed by the Scheduler Device associated with that process. The scheduler ensures that messages are processed one after another in a deterministic order, typically based on arrival time and potentially other factors like message nonces or timestamps (depending on the specific scheduler implementation details).

The `~process@1.0` device interacts with its configured Scheduler Device (which defaults to `~scheduler@1.0`) primarily through the `next` key to retrieve the next message to be executed.

## Key Functions (Keys)

These keys are typically accessed via the `~process@1.0` device, which delegates the calls to its configured scheduler.

*   **`schedule` (Handler for `GET /<ProcessID>~process@1.0/schedule`)**
    *   **Action:** Retrieves the list of pending assignments (messages) for the process. May support cursor-based traversal for long schedules.
    *   **Response:** A message map containing the assignments, often keyed by slot number or message ID.
*   **`register` (Handler for `POST /<ProcessID>~process@1.0/schedule`)**
    *   **Action:** Adds/registers a new message to the process's schedule. If this is the first message for a process, it might initialize the scheduler state.
    *   **Request Body:** The message to schedule.
    *   **Response:** Confirmation, potentially including the assigned slot or message ID.
*   **`slot` (Handler for `GET /<ProcessID>~process@1.0/slot`)**
    *   **Action:** Queries the current or a specific slot number within the process's schedule.
    *   **Response:** Information about the requested slot, such as the current highest slot number.
*   **`status` (Handler for `GET /<ProcessID>~process@1.0/status`)**
    *   **Action:** Retrieves status information about the scheduler for the process.
    *   **Response:** A status message.
*   **`next` (Internal Key used by `~process@1.0`)**
    *   **Action:** Retrieves the next assignment message from the schedule based on the process's current `at-slot` state.
    *   **State Management:** Requires the current process state (`Msg1`) containing the `at-slot` key.
    *   **Response:** `{ok, #{ "body" => <NextAssignmentMsg>, "state" => <UpdatedProcessState> }}` or `{error, Reason}` if no next assignment is found.
    *   **Caching & Lookahead:** The implementation uses internal caching (`dev_scheduler_cache`, `priv/assignments`) and potentially background lookahead workers to optimize fetching subsequent assignments.
*   **`init` (Internal Key)**
    *   **Action:** Initializes the scheduler state for a process, often called when the process itself is initialized.
*   **`checkpoint` (Internal Key)**
    *   **Action:** Triggers the scheduler to potentially persist its current state or perform other checkpointing operations.

## Interaction with Other Components

*   **`~process@1.0`:** The primary user of the scheduler, calling `next` to drive process execution.
*   **`~push@1.0`:** Often used to add messages to the schedule via `POST /schedule`.
*   **`dev_scheduler_cache`:** Internal module used for caching assignments locally on the node to reduce latency.
*   **Scheduling Unit (SU):** Schedulers may interact with external entities (like Arweave gateways or dedicated SU nodes) to fetch or commit schedules, although `~scheduler@1.0` aims for a simpler, often node-local or SU-client model.

`~scheduler@1.0` provides the fundamental mechanism for ordered, sequential execution within the potentially asynchronous and parallel environment of AO.

[scheduler module](../resources/source-code/dev_scheduler.md)
