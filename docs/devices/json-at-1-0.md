# Device: ~json@1.0

## Overview

The `json` device provides a structured way to access and interact with JSON (JavaScript Object Notation) data within the HyperBEAM environment. It allows processes to read, query, and potentially modify JSON objects stored or referenced by the device. It can also be used via HyperPATH chaining to serialize arbitrary data from other devices.

**Status:** Experimental <!-- TODO: Update status -->

## Core Functions (Keys)

These keys are typically accessed via HyperPATHs relative to the device's mount point (e.g., `/data/myJson`) or used in HyperPATH chains.

*   **`GET /<MountPoint>/<HyperPATH>` (Read Action)**
    *   **Action:** Retrieves the data located at the specified `<HyperPATH>` within the device's *own* stored JSON structure. The device serializes the targeted JSON fragment (object, array, value) into a standard JSON string format.
    *   **Example:** `GET /data/myJson/user/settings` on the example JSON below would return the string `"{\"theme\":\"dark\",\"notifications\":true}"`.
    *   **HyperPATH:** Required. Specifies the target within the device's JSON data.

*   **`/<PreviousPath>/serialize` (Serialize Action)**
    *   **Action:** Takes arbitrary input data (piped from the `<PreviousPath>` segment of a HyperPATH chain) and returns its serialized JSON string representation.
    *   **Example:** `GET /~meta@1.0/info /~json@1.0/serialize` - fetches node info and then pipes it to this device to serialize the result as JSON.
    *   **HyperPATH:** This segment (`/serialize`) is appended to a previous HyperPATH segment.

*   **`GET /<MountPoint>/query?<HyperPATHQuery>` (Query Action)**
    *   **Action:** Performs a more complex query against the device's *own* stored JSON data using a specific query syntax (details TBD).
    *   **Example:** <!-- TODO: Add Query example -->
    *   **HyperPATH:** Required. The base path to the device's data.
    *   **Query Parameter:** `?query=<HyperPATHQuery>` (Syntax TBD).

<!-- TODO: Add details on Update, Delete, or other potential actions -->

## Example JSON Data

Assuming `json` is mounted at `/data/myJson` and holds the following JSON:

```json
{
  "user": {
    "name": "Alice",
    "id": 123,
    "settings": {
      "theme": "dark",
      "notifications": true
    }
  },
  "items": [
    {"sku": "abc", "price": 10},
    {"sku": "def", "price": 20}
  ]
}
```

**Access Examples:**

-   Get user name: `GET /data/myJson/user/name`
-   Get theme setting: `GET /data/myJson/user/settings/theme`
-   Get first item price: `GET /data/myJson/items[0]/price`

## Events

<!-- TODO: Details about events emitted by the device, e.g., DataChanged -->

<!-- TODO: Link to source module if available --> 

[json module](../resources/source-code/dev_codec_json.md)