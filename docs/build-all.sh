#!/bin/bash

# Script to build HyperBEAM documentation in one seamless command
# This includes compiling with rebar3, generating edoc, processing source code docs, and building/serving with mkdocs

# Ensure we're in the root directory of the project
ROOT_DIR="$(dirname "$(realpath "$0")")/.."
cd "$ROOT_DIR" || { echo "Failed to change to root directory"; exit 1; }

echo "Building HyperBEAM documentation from $ROOT_DIR"

# Step 1: Compile the project with rebar3
echo "Compiling project with rebar3..."
rebar3 compile || { echo "rebar3 compile failed"; exit 1; }

echo "Compilation completed successfully"

# Step 2: Generate edoc documentation
echo "Generating edoc documentation..."
rebar3 edoc || { echo "rebar3 edoc failed"; exit 1; }

echo "Edoc generation completed successfully"

# Step 3: Process source code documentation
echo "Processing source code documentation..."
# Updated path for source code docs
DOCS_DIR="$ROOT_DIR/docs/source-code-docs"
INDEX_FILE="$DOCS_DIR/index.md"

# Check if the directory exists
if [ ! -d "$DOCS_DIR" ]; then
  echo "Error: Source code docs directory not found at $DOCS_DIR"
  exit 1
fi
# Check if the index file exists
if [ ! -f "$INDEX_FILE" ]; then
  echo "Error: Source code index file not found at $INDEX_FILE"
  exit 1
fi

# Step 3.1: Recreate module list in index.md

# Overwrite the file with the header content
echo "# Source Code Documentation" > "$INDEX_FILE"
echo "" >> "$INDEX_FILE"
echo "Welcome to the source code documentation for HyperBEAM. This section provides detailed insights into the codebase, helping developers understand the structure, functionality, and implementation details of HyperBEAM and its components." >> "$INDEX_FILE"
echo "" >> "$INDEX_FILE"
echo "## Overview" >> "$INDEX_FILE"
echo "" >> "$INDEX_FILE"
echo "HyperBEAM is built with a modular architecture to ensure scalability, maintainability, and extensibility. The source code is organized into distinct components, each serving a specific purpose within the ecosystem." >> "$INDEX_FILE"
echo "" >> "$INDEX_FILE"
echo "## Sections" >> "$INDEX_FILE"
echo "" >> "$INDEX_FILE"
echo "- **HyperBEAM Core**: The main framework that orchestrates data processing, storage, and routing." >> "$INDEX_FILE"
echo "- **Compute Unit**: Handles computational tasks and integrates with the HyperBEAM core for distributed processing." >> "$INDEX_FILE"
echo "- **Trusted Execution Environment (TEE)**: Ensures secure execution of sensitive operations." >> "$INDEX_FILE"
echo "- **Client Libraries**: Tools and SDKs for interacting with HyperBEAM, including the JavaScript client." >> "$INDEX_FILE"
echo "" >> "$INDEX_FILE"
echo "## Getting Started" >> "$INDEX_FILE"
echo "" >> "$INDEX_FILE"
echo "To explore the source code, you can clone the repository from [GitHub](https://github.com/permaweb/HyperBEAM). For detailed setup instructions, refer to the [Development Setup](../contribute/setup.md) guide." >> "$INDEX_FILE"
echo "" >> "$INDEX_FILE"
echo "## Navigation" >> "$INDEX_FILE"
echo "" >> "$INDEX_FILE"
echo "Use the navigation menu to dive into specific parts of the codebase. Each module includes detailed documentation, code comments, and examples to assist in understanding and contributing to the project." >> "$INDEX_FILE"
echo "" >> "$INDEX_FILE"
echo "## Contributing" >> "$INDEX_FILE"
echo "" >> "$INDEX_FILE"
echo "We welcome contributions to HyperBEAM. If you're interested in contributing, please review the [Contribution Guidelines](../contribute/guidelines.md) for information on coding standards, pull request processes, and more. " >> "$INDEX_FILE"
echo "" >> "$INDEX_FILE"

# Append the table header
echo "<table>" >> "$INDEX_FILE"

# Get list of markdown files (excluding index.md and README.md), limit to first 10 for index.md
# Updated find command to use the correct DOCS_DIR
MODULE_FILES=$(find "$DOCS_DIR" -maxdepth 1 -type f -name "*.md" -not -name "index.md" -not -name "README.md" | sort | head -n 10)

# Reset count
count=0

# Add each module to the table for index.md
for file in $MODULE_FILES; do
  filename=$(basename "$file")
  module_name="${filename%.md}"
  # Use relative path for link
  echo "<tr><td><a href=\"$filename\" class=\"module\">$module_name</a></td></tr>" >> "$INDEX_FILE"
  count=$((count + 1))
  if [ $count -eq 10 ]; then
    break
  fi
done

# Close the table and add note
echo "</table>" >> "$INDEX_FILE"
echo "" >> "$INDEX_FILE"
echo "*Note: This is a partial list. Navigate through the menu or search for specific modules for detailed documentation.*" >> "$INDEX_FILE"

echo "Updated module list in $INDEX_FILE"

# Step 3.2: Remove only the ToC entries for Function Index and Function Details

# Process each markdown file (excluding index.md and README.md)
# Updated find command
for file in $(find "$DOCS_DIR" -maxdepth 1 -type f -name "*.md" -not -name "index.md" -not -name "README.md"); do
  # Temporary file for processing
  TEMP_MODULE_FILE=$(mktemp)

  # Remove only the ToC entries for Function Index and Function Details
  awk '
    # Skip the ToC lines for Function Index and Function Details
    /^\* \[Function Index\]\(#index\)$/ { next; }
    /^\* \[Function Details\]\(#functions\)$/ { next; }

    # Print all other lines
    { print; }
  ' "$file" > "$TEMP_MODULE_FILE"

  # Replace the original file with the cleaned-up content
  mv "$TEMP_MODULE_FILE" "$file"

  echo "Removed ToC entries in $file"
done

echo "Source code documentation processing completed"

# Step 4: Build and serve mkdocs
echo "Building and serving mkdocs documentation..."
# Run mkdocs from the root directory
# Remove --site-dir flag to use the one specified in mkdocs.yml (which is 'site')
mkdocs build || { echo "mkdocs build failed"; exit 1; }
mkdocs serve || { echo "mkdocs serve failed"; exit 1; }

echo "Documentation build and serve completed" 