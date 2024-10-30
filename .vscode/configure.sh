#!/bin/bash


# Get the directory of the script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Find the path to beam.smp
BEAM_PATH=$(find / -name "beam.smp" -print -quit 2>/dev/null)

if [ -z "$BEAM_PATH" ]; then
    echo "beam.smp not found."
    exit 1
fi

echo "Found beam.smp at: $BEAM_PATH"

# Define the path to your launch.json
LAUNCH_CONFIG_PATH="$SCRIPT_DIR/../.vscode/launch.json"

# Update launch.json with the found path
jq --arg path "$BEAM_PATH" '.configurations[] |= if .name == "Attach C Debugger to beam.smp" then .program = $path else . end' "$LAUNCH_CONFIG_PATH" > tmp.json && mv tmp.json "$LAUNCH_CONFIG_PATH"

echo "Updated launch.json with new beam.smp path."