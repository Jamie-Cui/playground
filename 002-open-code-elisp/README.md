# OpenCode Emacs Lisp Implementation

An Emacs Lisp implementation of [OpenCode](https://github.com/anomalyco/opencode) - an open-source AI coding agent.

## Description

This is a core implementation of OpenCode in Emacs Lisp that provides:

- **Direct API integration** with Anthropic Claude and OpenAI GPT models
- **File operations**: read, write, grep, glob
- **Shell command execution** via bash tool
- **Session management** with conversation history
- **Minibuffer interface** for quick prompts

## Installation

Add the project to your Emacs load path:

```elisp
(add-to-list 'load-path "/path/to/002-open-code-elisp/lisp")
(require 'opencode)
```

Or use `use-package`:

```elisp
(use-package opencode
  :load-path "/path/to/002-open-code-elisp/lisp"
  :config
  (setq opencode-api-key "your-api-key-here")
  (global-opencode-mode 1))
```

## Configuration

Set your API key (required):

```elisp
(setq opencode-api-key "sk-ant-...")
```

Or use environment variables:
- `ANTHROPIC_API_KEY` for Anthropic
- `OPENAI_API_KEY` for OpenAI

Choose your provider:

```elisp
(setq opencode-provider 'anthropic)  ; or 'openai or 'openai-compatible
```

Set the model:

```elisp
(setq opencode-model "claude-sonnet-4-20250514")
```

### All Configuration Options

Customize with `M-x customize-group RET opencode RET`:

| Option | Default | Description |
|--------|---------|-------------|
| `opencode-provider` | `anthropic` | LLM provider |
| `opencode-api-key` | `nil` | API key (or use env var) |
| `opencode-base-url` | `nil` | Base URL for OpenAI-compatible APIs |
| `opencode-model` | `claude-sonnet-4-20250514` | Model identifier |
| `opencode-max-tokens` | `8192` | Maximum tokens for responses |
| `opencode-temperature` | `0.7` | Temperature (0.0-1.0) |
| `opencode-system-prompt` | (default prompt) | System prompt for the AI |
| `opencode-buffer-name` | `"*opencode*"` | Output buffer name |
| `opencode-auto-scroll` | `t` | Auto-scroll output buffer |
| `opencode-enable-tools` | `(read write grep glob bash)` | Enabled tools |
| `opencode-max-history` | `100` | Max messages in history |

## Usage

### Interactive Commands

| Command | Keybinding | Description |
|---------|-----------|-------------|
| `opencode-prompt` | `C-c o p` | Prompt for input and send to AI |
| `opencode-prompt-region` | `C-c o r` | Send selected region to AI |
| `opencode-ask-at-point` | `C-c o a` | Ask about symbol at point |
| `opencode-clear-session` | `C-c o c` | Clear current session |
| `opencode-show-session` | `C-c o s` | Show session summary |

### Example

```elisp
;; Enable the mode
(opencode-mode 1)

;; Or globally
(global-opencode-mode 1)

;; Send a prompt
M-x opencode-prompt

;; Or use keybinding
C-c o p
```

## Available Tools

The AI agent has access to these tools (can be customized via `opencode-enable-tools`):

| Tool | Description |
|------|-------------|
| `read_file` | Read file contents |
| `write_file` | Write content to a file |
| `grep` | Search for patterns in files (regex) |
| `glob` | Find files matching a pattern |
| `bash` | Execute shell commands |

## Architecture

```
lisp/
├── opencode.el         # Main entry point
├── opencode-config.el  # Configuration (customize group)
├── opencode-api.el     # HTTP API client for LLM providers
├── opencode-session.el # Session & message history
├── opencode-tools.el   # Tool implementations
├── opencode-agent.el   # Agent logic & tool calling
└── opencode-ui.el      # Minibuffer UI & display
```

## Development

### Byte Compilation

```bash
make compile
```

### Testing

```bash
make test
```

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.

## Original OpenCode

This is an Emacs Lisp implementation inspired by the main OpenCode project:
- https://github.com/anomalyco/opencode
- https://opencode.ai
