# Device: ~json@1.0

## Overview

The `json` device provides a structured way to access and interact with JSON (JavaScript Object Notation) data within the HyperBEAM environment. It allows processes to read, query, and potentially modify JSON objects stored or referenced by the device. It can also be used via HyperPATH chaining to serialize arbitrary data from other devices.

**Status:** Stable

## Core Functions (Keys)

### Serialization

*   **`GET /~json@1.0/serialize` (Direct Serialize Action)**
    *   **Action:** Serializes the input message or data into a JSON string.
    *   **Example:** `GET /~json@1.0/serialize` - serializes the current message as JSON.
    *   **HyperPATH:** The path segment `/serialize` directly follows the device identifier.

*   **`GET /<PreviousPath>/~json@1.0/serialize` (Chained Serialize Action)**
    *   **Action:** Takes arbitrary data output from `<PreviousPath>` (another device or operation) and returns its serialized JSON string representation.
    *   **Example:** `GET /~meta@1.0/info/~json@1.0/serialize` - fetches node info from the meta device and then pipes it to the JSON device to serialize the result as JSON.
    *   **HyperPATH:** This segment (`/~json@1.0/serialize`) is appended to a previous HyperPATH segment.

## HyperPATH Chaining Example

The JSON device is particularly useful in HyperPATH chains to convert output from other devices into JSON format:

```
GET /~meta@1.0/info/~json@1.0/serialize
```

This retrieves the node configuration from the meta device and serializes it to JSON.

## See Also

- [Message Device](./message-at-1-0.md) - Works well with JSON serialization
- [Meta Device](./meta-at-1-0.md) - Can provide configuration data to serialize

[json module](../resources/source-code/dev_codec_json.md)