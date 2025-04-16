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

# Temporary file to store the updated content
TEMP_FILE=$(mktemp)

# Start of the modules section marker for index.md
START_MARKER="## Modules"
# End of the modules section marker (adjust based on your index.md structure)
# Assuming the next major section starts with '##' as a fallback if END_MARKER isn't found
END_MARKER="## Getting Started" # Keep this specific if possible, otherwise adjust

# Step 3.1: Update module list in index.md

# Extract content before and after the modules section
awk -v start_marker="$START_MARKER" -v end_marker="$END_MARKER" '
  BEGIN { in_section=0; printing=1; }
  $0 ~ start_marker { print; in_section=1; printing=1; next; }
  # If we hit the end marker, stop being in the section
  $0 ~ end_marker { in_section=0; printing=1; }
  # Alternative end: if we were in the section and hit another header
  in_section && /^## / { in_section=0; printing=1; }
  # Skip lines within the section
  in_section { printing=0; next; }
  # Print lines outside the section
  printing { print; }
' "$INDEX_FILE" > "$TEMP_FILE"

# Insert the updated modules list
awk -v start_marker="$START_MARKER" '
  $0 ~ start_marker {
    print; 
    print "\nBelow is a list of key modules in the HyperBEAM application. For a complete list, refer to the [full module documentation](#).\n";
    print "<table width=\"100%\" border=\"0\" summary=\"list of modules\">";
    inserted=1
  }
  { print; }
' "$TEMP_FILE" > "$TEMP_FILE.2"

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
  echo "<tr><td><a href=\"$filename\" class=\"module\">$module_name</a></td></tr>" >> "$TEMP_FILE.2"
  count=$((count + 1))
  if [ $count -eq 10 ]; then
    break
  fi
done

# Close the table and add note
cat >> "$TEMP_FILE.2" << EOF
</table>

*Note: This is a partial list. Navigate through the menu or search for specific modules for detailed documentation.*
EOF

# Append the rest of the original file after the insertion point
awk -v start_marker="$START_MARKER" -v end_marker="$END_MARKER" '
  BEGIN { in_section=0; printing=0; }
  $0 ~ start_marker { in_section=1; printing=0; next; }
  $0 ~ end_marker { in_section=0; printing=1; }
  in_section && /^## / { in_section=0; printing=1; }
  in_section { next; }
  printing { print; }
' "$INDEX_FILE" >> "$TEMP_FILE.2"


# Replace the original index.md with updated content
mv "$TEMP_FILE.2" "$INDEX_FILE"
rm "$TEMP_FILE"

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