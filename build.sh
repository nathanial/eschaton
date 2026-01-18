#!/bin/bash
LEAN_CC=/usr/bin/clang LIBRARY_PATH=/opt/homebrew/lib:$LIBRARY_PATH lake build eschaton "$@"
