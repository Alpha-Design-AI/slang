# ChipAgents Modifications

## Modified Files

### `tools/hier/hier.cpp`
- Adds ports, definitions, JSON Output, line numbers, etc.
- There are still some issues with getting ports on certain instances (like areas of opentitan), so hier.cpp still needs some fixes

### `build.sh`
- Custom build script for this fork
- Detects platform, configures with `cmake -B`, builds with `cmake --build`, and runs tests
- Output binaries saved to `build-<platform>-<arch>/bin/` (e.g., `build-darwin-arm64/bin/slang`)

## Quick Build Commands

**Full build (first time or clean build):**
```bash
./build.sh
```

**Quick rebuild of specific target (after initial `cmake -B`):**
```bash
cmake --build build-darwin-arm64 --target slang_hier -j
cmake --build build-darwin-arm64 --target slang -j
```

**Output Binaries:**
- Slang Binary: `build-darwin-arm64/bin/slang`
- Hier Binary: `build-darwin-arm64/bin/slang-hier`