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
DOCS_DIR="$ROOT_DIR/docs/site/docs/source-code-docs"
INDEX_FILE="$DOCS_DIR/index.md"

# Temporary file to store the updated content
TEMP_FILE=$(mktemp)

# Start of the modules section marker for index.md
START_MARKER="## Modules"
# End of the modules section marker (adjust based on your index.md structure)
END_MARKER="## Getting Started"

# Step 3.1: Update module list in index.md

# Extract content before and after the modules section
awk -v start_marker="$START_MARKER" -v end_marker="$END_MARKER" '
  BEGIN { in_section=0; }
  $0 ~ start_marker { print; in_section=1; next; }
  $0 ~ end_marker { in_section=0; print; next; }
  in_section { next; }
  { print; }
' "$INDEX_FILE" > "$TEMP_FILE"

# Insert the updated modules list
awk -v start_marker="$START_MARKER" '
  $0 ~ start_marker {
    print;
    print "\nBelow is a list of key modules in the HyperBEAM application. For a complete list, refer to the [full module documentation](#).\n";
    print "<table width=\"100%\" border=\"0\" summary=\"list of modules\">";
  }
  { print; }
' "$TEMP_FILE" > "$TEMP_FILE.2"

# Get list of markdown files (excluding index.md and README.md), limit to first 10 for index.md
MODULE_FILES=$(find "$DOCS_DIR" -maxdepth 1 -type f -name "*.md" -not -name "index.md" -not -name "README.md" | sort | head -n 10)

# Reset count
count=0

# Add each module to the table for index.md
for file in $MODULE_FILES; do
  filename=$(basename "$file")
  module_name="${filename%.md}"
  echo "<tr><td><a href=\"$filename\" class=\"module\">$module_name</a></td></tr>" >> "$TEMP_FILE.2"
  count=$((count + 1))
  if [ $count -eq 10 ]; then
    break
  fi
done

# Close the table and add note
cat >> "$TEMP_FILE.2" << 'EOF'
</table>

*Note: This is a partial list. Navigate through the menu or search for specific modules for detailed documentation.*
EOF

# Replace the original index.md with updated content
mv "$TEMP_FILE.2" "$INDEX_FILE"
rm "$TEMP_FILE"

echo "Updated module list in $INDEX_FILE"

# Step 3.2: Remove only the ToC entries for Function Index and Function Details

# Process each markdown file (excluding index.md and README.md)
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
cd docs/site || { echo "Failed to change to docs/site directory"; exit 1; }
mkdocs build --site-dir "$ROOT_DIR/docs/site-build" || { echo "mkdocs build failed"; exit 1; }
mkdocs serve || { echo "mkdocs serve failed"; exit 1; }

echo "Documentation build and serve completed" 