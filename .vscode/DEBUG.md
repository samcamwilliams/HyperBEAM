# Debugging Erlang and C Code in VS Code

This guide provides steps to use the existing `launch.json` configuration for debugging both Erlang and C code in your project using VS Code. For a visual walkthrough on debugging Erlang and C code in VS Code, refer to the video tutorial available here: [Debugging Erlang and C Code in VS Code](https://www.youtube.com/watch?v=Ew-jEJW4LmU).

## Configuration Overview

Your `launch.json` file includes two main configurations:

1. **Launch Erlang with Dependencies**: Starts and debugs the Erlang application.
2. **Attach C Debugger to beam.smp**: Attaches a C debugger to the running Erlang process, allowing you to debug C code within the Erlang environment.

### Important: Update the Path to `beam.smp`

The `launch.json` file is already set up, but you need to update the path to `beam.smp` in the second configuration to match the location on your system. 

1. **Run the `configure.sh` Script First**: This script will automatically find the `beam.smp` file on your computer and update the `launch.json` accordingly. Make sure to run this script before any manual attempts.

2. **If `configure.sh` Doesn't Work**: If the script fails to locate `beam.smp`, you can manually run the following command in your terminal:

   ```sh
   find / -name "beam.smp" -print -quit 2>/dev/null
   ```

   After locating the file, you will need to manually update the `program` path in the `Attach C Debugger to beam.smp` configuration of your `launch.json`.

## Steps for Debugging

### 1. Debugging Erlang Only

To debug only the Erlang part of your project:

1. Run the **Launch Erlang with Dependencies** configuration.

2. **Note on Breakpoints**  
   Due to a bug in VS Code, breakpoints in Erlang code may not activate in subsequent sessions unless refreshed. To ensure breakpoints are correctly recognized:
   - **Add a new breakpoint** or **remove and re-add an existing breakpoint** before each debugging session.

### 2. Debugging C Code within the Erlang VM

To debug C code within the running Erlang VM:

1. **First**, launch the Erlang application using the **Launch Erlang with Dependencies** configuration as described above.
2. **Then**, use the **Attach C Debugger to beam.smp** configuration in `launch.json`, ensuring the `program` field is updated with the correct `beam.smp` path.

    ```json
    {
        "name": "Attach C Debugger to beam.smp",
        "program": "/path/to/your/beam.smp",  // Update this path as shown below
    }
    ```

3. **Locating `beam.smp`**  
   If you ran `configure.sh`, the path should already be updated in `launch.json`. If not, you can manually run the find command mentioned earlier to locate the correct path for `beam.smp` and replace the path in the `program` field.

4. **Selecting the Correct `beam.smp` Process**  
   When the C debugger prompts for `"processId": "${command:pickProcess}"`, you will see two `beam.smp` processes listed. To select the correct process:
   - **Hover over each process ID** to view additional information.
   - Choose the `beam.smp` process with more detailed information, which is typically the second entry in the list.

## Summary

- **Run `configure.sh` first** to automatically update the path to `beam.smp` in `launch.json`. If it fails, use the `find` command to locate `beam.smp` and update the path manually.
- **Launch Erlang with Dependencies** to debug Erlang code alone, refreshing breakpoints each time.
- Use both configurations if debugging C code, ensuring the correct path is set for `beam.smp` before attaching and selecting the appropriate process.
