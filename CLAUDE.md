# Eschaton

A Stellaris-inspired grand strategy game with Metal GPU rendering.

## Build

```bash
./build.sh        # Build the project
./run.sh          # Build and run
./test.sh         # Run tests
```

**Note:** Use the shell scripts instead of `lake` directly - they set `LEAN_CC=/usr/bin/clang` for proper macOS framework linking.

## Architecture

Uses afferent for GPU rendering and the Canopy reactive widget system for UI.

## Usage

```lean
import Eschaton
import Afferent
```
