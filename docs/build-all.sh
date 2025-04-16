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
cd docs/site/docs/source-code-docs || { echo "Failed to change to source-code-docs directory"; exit 1; }
./build-docs.sh || { echo "build-docs.sh failed"; exit 1; }
cd "$ROOT_DIR" || { echo "Failed to return to root directory"; exit 1; }

echo "Source code documentation processing completed"

# Step 4: Build and serve mkdocs
echo "Building and serving mkdocs documentation..."
cd docs/site || { echo "Failed to change to docs/site directory"; exit 1; }
mkdocs build --site-dir "$ROOT_DIR/docs/site-build" || { echo "mkdocs build failed"; exit 1; }
mkdocs serve || { echo "mkdocs serve failed"; exit 1; }

echo "Documentation build and serve completed" 