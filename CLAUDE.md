# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal "playground" repository for experimental code and quick learning projects. Each experiment is isolated in its own numbered directory (e.g., `001-lean-hello-world/`, `002-open-code-elisp/`).

## Project Structure

The repository contains independent experiments in separate directories:

- `001-lean-hello-world/` - Lean 4 theorem prover project
- `002-open-code-elisp/` - Emacs Lisp implementation

Each directory is self-contained with its own build system and dependencies.

## Lean 4 Development (001-lean-hello-world/)

### Building and Running

Lean 4 uses the **Lake** build system (similar to Cargo for Rust).

```bash
# Build the project
cd 001-lean-hello-world
lake build

# Run the executable
./.lake/build/bin/lean4-tutorial
```

### Project Structure

- `lakefile.toml` - Lake build configuration, defines library and executable targets
- `Main.lean` - Entry point with the `main` function
- `Lean4Tutorial/` - Library directory containing reusable modules
  - `Basic.lean` - Basic definitions (currently defines `hello := "world"`)
- `Lean4Tutorial.lean` - Library root that imports all library modules
- `lean-toolchain` - Specifies Lean version (currently v4.26.0)

### Lake Configuration

The project defines:
- A library named `Lean4Tutorial` containing shared code
- An executable named `lean4-tutorial` with `Main.lean` as the entry point

When adding new modules to the library:
1. Create files in `Lean4Tutorial/` directory
2. Import them in `Lean4Tutorial.lean`
