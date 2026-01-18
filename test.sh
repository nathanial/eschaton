#!/bin/bash
set -e

# Build tests
LEAN_CC=/usr/bin/clang lake build tests

# Run tests
.lake/build/bin/tests
