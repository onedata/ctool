#!/bin/bash
set -e

ERRORS_DIR="onedata_errors"
ERRORS_GEN_DIR="$ERRORS_DIR/generated/erlang"
CTOOL_INCLUDE_DIR="include"
CTOOL_SRC_DIR="src/errors"
HASH_FILE=".errors_generated_from"

# Get current commit hash of onedata-errors
current_hash=$(cd $ERRORS_DIR && git rev-parse HEAD)

# Check if regeneration is needed
if [ -f $HASH_FILE ] && [ "$(cat $HASH_FILE)" == "$current_hash" ]; then
    echo "Errors are up to date, skipping generation"
    exit 0
fi

echo "Generating errors from onedata-errors ($current_hash)..."

# Generate errors
# Try to find Python 3
if command -v python3 >/dev/null 2>&1; then
    PYTHON_CMD=python3
else
    echo "Error: Python 3 is required but not found"
    exit 1
fi

cd $ERRORS_DIR && PYTHONPATH=. PYTHON=$PYTHON_CMD make erlang
cd ..

# Print make result
make_result=$?
if [ $make_result -ne 0 ]; then
    echo "Error: make erlang failed with code $make_result"
    echo "Directory contents:"
    ls -la
    echo "ERRORS_DIR contents:"
    ls -la $ERRORS_DIR
    exit $make_result
fi

# Create errors directory if it doesn't exist
mkdir -p $CTOOL_SRC_DIR

# Copy generated files
cp $ERRORS_GEN_DIR/error_attrs.hrl $CTOOL_INCLUDE_DIR/
cp $ERRORS_GEN_DIR/errors.hrl $CTOOL_INCLUDE_DIR/
cp $ERRORS_GEN_DIR/errors.erl $CTOOL_SRC_DIR/
cp $ERRORS_GEN_DIR/od_error.erl $CTOOL_SRC_DIR/
rm -rf $CTOOL_SRC_DIR/types
cp -r $ERRORS_GEN_DIR/types $CTOOL_SRC_DIR/

# Save current hash
echo $current_hash > $HASH_FILE

echo "Errors generated successfully" 