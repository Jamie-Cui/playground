# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal "playground" repository for experimental code and quick learning projects. Each experiment is isolated in its own numbered directory (e.g., `001-lean-hello-world/`, `002-magent/`).

## Project Structure

The repository contains independent experiments in separate directories:

- `001-lean-hello-world/` - Lean 4 theorem prover project
- `002-magent/` - OpenCode AI coding agent (Emacs Lisp)

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

## OpenCode Emacs Lisp (002-magent/)

An Emacs Lisp implementation of [OpenCode](https://github.com/anomalyco/magent), an open-source AI coding agent with direct API integration to Anthropic Claude and OpenAI GPT models.

### Building and Loading

```bash
# Byte compile (optional, for performance)
cd 002-magent
make compile
```

```elisp
;; In init.el
(add-to-list 'load-path "~/proj/playground/002-magent/lisp")
(require 'magent)

;; Configure API key (or set ANTHROPIC_API_KEY / OPENAI_API_KEY env var)
(setq magent-api-key "sk-ant-...")

;; Enable globally
(global-magent-mode 1)
```

### Key Commands

| Command | Keybinding | Description |
|---------|-----------|-------------|
| `magent-prompt` | `C-c o p` | Send prompt to AI |
| `magent-prompt-region` | `C-c o r` | Send region to AI |
| `magent-ask-at-point` | `C-c o a` | Ask about symbol at point |
| `magent-clear-session` | `C-c o c` | Clear session history |
| `magent-show-session` | `C-c o s` | Show session summary |

### Architecture

```
lisp/
├── magent.el         # Main entry point, minor mode
├── magent-config.el  # Customize group (magent-*)
├── magent-api.el     # HTTP client for LLM providers
├── magent-session.el # Session & message history
├── magent-tools.el   # File operations (read, write, grep, glob, bash)
├── magent-agent.el   # Agent logic with tool calling
└── magent-ui.el      # Minibuffer interface & output buffer
```

### Configuration

Customize via `M-x customize-group RET magent RET` or set variables:

- `magent-provider` - `anthropic`, `openai`, or `openai-compatible`
- `magent-model` - Model identifier (default: `claude-sonnet-4-20250514`)
- `magent-enable-tools` - List of tools: `(read write grep glob bash)`
- `magent-buffer-name` - Output buffer name (default: `*magent*`)

### Available Tools

The AI agent has access to these tools for code operations:
- `read_file` - Read file contents
- `write_file` - Write content to file
- `grep` - Regex search in files
- `glob` - Find files matching pattern
- `bash` - Execute shell commands
