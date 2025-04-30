#!/bin/bash

# Script to build HyperBEAM documentation in one seamless command

# --- Color Definitions ---
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# --- Helper Functions ---
log_success() {
  echo -e "${GREEN}✓ $1${NC}"
}

log_info() {
  echo -e "${BLUE}→ $1${NC}"
}

log_step() {
  echo -e "\n${YELLOW}${BOLD}$1${NC}"
}

log_error() {
  echo -e "${RED}✗ $1${NC}"
}

# --- Script Start ---
log_step "HYPERBEAM DOCUMENTATION BUILD"

# Ensure we're in the root directory of the project
ROOT_DIR="$(dirname "$(realpath "$0")")/.."
cd "$ROOT_DIR" || { log_error "Failed to change to root directory"; exit 1; }

# --- Step 1: Compile the project with rebar3 ---
log_step "Compiling project"
rebar3 compile > /dev/null 2>&1 || { log_error "rebar3 compile failed"; exit 1; }
log_success "Compilation completed"

# --- Step 2: Generate edoc documentation ---
log_step "Generating edoc documentation"
rebar3 edoc > /dev/null 2>&1 || { log_error "rebar3 edoc failed"; exit 1; }
log_success "Edoc generation completed"

# --- Step 3: Process source code documentation ---
log_step "Processing source code documentation"
DOCS_DIR="$ROOT_DIR/docs/resources/source-code"
INDEX_FILE="$DOCS_DIR/index.md"

# Check if the directory and index file exist
if [ ! -d "$DOCS_DIR" ]; then
  log_error "Source code docs directory not found at $DOCS_DIR"
  exit 1
fi

if [ ! -f "$INDEX_FILE" ]; then
  log_error "Source code index file not found at $INDEX_FILE"
  exit 1
fi

# --- Step 3.1: Remove ToC entries for Function Index and Function Details ---
log_info "Cleaning module files"

find "$DOCS_DIR" -maxdepth 1 -type f -name "*.md" -not -name "index.md" -not -name "README.md" | while read -r file; do
  TEMP_MODULE_FILE=$(mktemp)

  awk '
    /^\* \[Function Index\]\(#index\)$/ { next; }
    /^\* \[Function Details\]\(#functions\)$/ { next; }
    { print; }
  ' "$file" > "$TEMP_MODULE_FILE"

  mv "$TEMP_MODULE_FILE" "$file"
done

log_success "Source code documentation processed"

# --- Step 3.2: Update mkdocs.yml navigation with current module list ---
log_info "Updating mkdocs.yml navigation"

# Get list of module files
MODULE_FILES=$(find "$DOCS_DIR" -maxdepth 1 -type f -name "*.md" -not -name "index.md" -not -name "README.md" | sort)

# Create temporary file for the new mkdocs.yml
MKDOCS_TEMP=$(mktemp)
MKDOCS_FILE="$ROOT_DIR/mkdocs.yml"

# Process mkdocs.yml file
awk '
BEGIN { in_modules = 0; skip_modules = 0; }
/^ *- Modules:/ { 
  print $0; 
  in_modules = 1; 
  skip_modules = 1; 
  next; 
}
{
  if (skip_modules == 0) {
    print $0;
  }
  if (in_modules == 1 && $0 ~ /^ *-/) {
    if ($0 !~ /^ *- Modules:/) {
      in_modules = 0;
      skip_modules = 0;
      print $0;
    }
  }
}
' "$MKDOCS_FILE" > "$MKDOCS_TEMP"

# Find the position to insert module entries
INSERT_LINE=$(grep -n "^ *- Modules:" "$MKDOCS_TEMP" | cut -d: -f1)

if [ -z "$INSERT_LINE" ]; then
  log_error "Could not find '- Modules:' section in mkdocs.yml"
  exit 1
fi

# Split the file at the insertion point
head -n "$INSERT_LINE" "$MKDOCS_TEMP" > "${MKDOCS_TEMP}.head"
tail -n +$((INSERT_LINE + 1)) "$MKDOCS_TEMP" > "${MKDOCS_TEMP}.tail"

# Add module entries
{
  cat "${MKDOCS_TEMP}.head"

  # Add each module with proper indentation
  for module_file in $MODULE_FILES; do
    module_name=$(basename "$module_file" .md)
    echo "        - $module_name: 'resources/source-code/$module_name.md'"
  done

  cat "${MKDOCS_TEMP}.tail"
} > "$MKDOCS_FILE"

# Clean up temporary files
rm -f "$MKDOCS_TEMP" "${MKDOCS_TEMP}.head" "${MKDOCS_TEMP}.tail"

log_success "mkdocs.yml navigation updated"

# --- Step 4: Build and serve mkdocs ---
log_step "Building mkdocs documentation"
mkdocs build > /dev/null 2>&1 || { log_error "mkdocs build failed"; exit 1; }

# Find the latest CSS files with their hashes
MAIN_CSS=$(find ./mkdocs-site/assets/stylesheets -name "main.*.min.css" | sort | tail -n 1)
PALETTE_CSS=$(find ./mkdocs-site/assets/stylesheets -name "palette.*.min.css" | sort | tail -n 1)

# Extract just the filenames from the paths
MAIN_CSS_FILE=$(basename "$MAIN_CSS")
PALETTE_CSS_FILE=$(basename "$PALETTE_CSS")

# Find all HTML files and replace the CSS references in each one
log_info "Updating CSS references in HTML files"
find ./mkdocs-site -type f -name "*.html" | while read -r html_file; do
    sed -i'' -e "s|MAIN\.CSS|assets/stylesheets/$MAIN_CSS_FILE|g" "$html_file"
    sed -i'' -e "s|MAIN_PALETTE\.CSS|assets/stylesheets/$PALETTE_CSS_FILE|g" "$html_file"
done

# Remove .html-e files
find ./mkdocs-site -type f -name "*.html-e" -delete

log_success "MkDocs build completed"

# --- Step 5: Generate LLM context files ---
log_step "Generating LLM context files"

LLM_SUMMARY_FILE="$ROOT_DIR/docs/llms.txt"
LLM_FULL_FILE="$ROOT_DIR/docs/llms-full.txt"
DOC_DIRS=(
    "$ROOT_DIR/docs/begin"
    "$ROOT_DIR/docs/run"
    "$ROOT_DIR/docs/guides"
    "$ROOT_DIR/docs/devices"
    "$ROOT_DIR/docs/resources"
)

# Get current timestamp
GENERATION_TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Generate llms.txt (routes and summary)
log_info "Creating summary and routes file"
cat > "$LLM_SUMMARY_FILE" <<EOL
Generated: $GENERATION_TIMESTAMP

## HyperBEAM Documentation Summary

This document provides an overview and routes for the HyperBEAM documentation, intended for LLM consumption.
Key sections include: Getting Started (begin), Running HyperBEAM (run), Developer Guides (guides), Device Integration (devices), and Resources (resources).

## Documentation Pages by Section
EOL

# Loop through each documentation directory
for DOC_DIR in "${DOC_DIRS[@]}"; do
    SECTION_NAME=$(basename "$DOC_DIR")
    SECTION_HEADING=$(echo "$SECTION_NAME" | sed 's/\b\(.\)/\u\1/g')

    echo "" >> "$LLM_SUMMARY_FILE"
    echo "### $SECTION_HEADING" >> "$LLM_SUMMARY_FILE"
    echo "" >> "$LLM_SUMMARY_FILE"

    find "$DOC_DIR" -type f -name "*.md" -print |
      sed "s|^$ROOT_DIR/||" |
      sed 's/^docs\///' |
      sed 's/\.md$//' |
      sort |
      while IFS= read -r base_path; do
        html_path="${base_path}.html"
        md_path_relative="docs/${base_path}.md"
        md_file_path="$ROOT_DIR/$md_path_relative"

        if [ -f "$md_file_path" ]; then
            title=$(grep -m 1 '^# ' "$md_file_path" 2>/dev/null | sed 's/^# //')
        else
            title=""
        fi

        if [ -z "$title" ]; then
            title=$(basename "$base_path" | sed -e 's/-/ /g' -e 's/\b\(.\)/\u\1/g')
        fi

        echo "* [$title](./$html_path)" >> "$LLM_SUMMARY_FILE"
      done
done

# Generate llms-full.txt (concatenated content)
log_info "Creating full documentation file"
echo "Generated: $GENERATION_TIMESTAMP" > "$LLM_FULL_FILE"
echo "" >> "$LLM_FULL_FILE"

find "${DOC_DIRS[@]}" -type f -name "*.md" | sort | while read -r doc_file; do
    relative_path="${doc_file#$ROOT_DIR/}"
    echo "--- START OF FILE: $relative_path ---" >> "$LLM_FULL_FILE"
    cat "$doc_file" >> "$LLM_FULL_FILE"
    echo "" >> "$LLM_FULL_FILE"
    echo "--- END OF FILE: $relative_path ---" >> "$LLM_FULL_FILE"
    echo "" >> "$LLM_FULL_FILE"
done

log_success "LLM context files generated"

# --- Final success message ---
echo -e "\n${GREEN}${BOLD}✓ Documentation build completed successfully${NC}\n"
