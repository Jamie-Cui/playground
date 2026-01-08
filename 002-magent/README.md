# OpenCode Emacs Lisp Implementation

An Emacs Lisp implementation of [OpenCode](https://github.com/anomalyco/magent) - an open-source AI coding agent.

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
(require 'magent)
```

Or use `use-package`:

```elisp
(use-package magent
  :load-path "/path/to/002-open-code-elisp/lisp"
  :config
  (setq magent-api-key "your-api-key-here")
  (global-magent-mode 1))
```

## Configuration

Set your API key (required):

```elisp
(setq magent-api-key "sk-ant-...")
```

Or use environment variables:
- `ANTHROPIC_API_KEY` for Anthropic
- `OPENAI_API_KEY` for OpenAI

Choose your provider:

```elisp
(setq magent-provider 'anthropic)  ; or 'openai or 'openai-compatible
```

Set the model:

```elisp
(setq magent-model "claude-sonnet-4-20250514")
```

### All Configuration Options

Customize with `M-x customize-group RET magent RET`:

| Option | Default | Description |
|--------|---------|-------------|
| `magent-provider` | `anthropic` | LLM provider |
| `magent-api-key` | `nil` | API key (or use env var) |
| `magent-base-url` | `nil` | Base URL for OpenAI-compatible APIs |
| `magent-model` | `claude-sonnet-4-20250514` | Model identifier |
| `magent-max-tokens` | `8192` | Maximum tokens for responses |
| `magent-temperature` | `0.7` | Temperature (0.0-1.0) |
| `magent-system-prompt` | (default prompt) | System prompt for the AI |
| `magent-buffer-name` | `"*magent*"` | Output buffer name |
| `magent-auto-scroll` | `t` | Auto-scroll output buffer |
| `magent-enable-tools` | `(read write grep glob bash)` | Enabled tools |
| `magent-max-history` | `100` | Max messages in history |

## Usage

### Interactive Commands

| Command | Keybinding | Description |
|---------|-----------|-------------|
| `magent-prompt` | `C-c o p` | Prompt for input and send to AI |
| `magent-prompt-region` | `C-c o r` | Send selected region to AI |
| `magent-ask-at-point` | `C-c o a` | Ask about symbol at point |
| `magent-clear-session` | `C-c o c` | Clear current session |
| `magent-show-session` | `C-c o s` | Show session summary |

### Example

```elisp
;; Enable the mode
(magent-mode 1)

;; Or globally
(global-magent-mode 1)

;; Send a prompt
M-x magent-prompt

;; Or use keybinding
C-c o p
```

## Available Tools

The AI agent has access to these tools (can be customized via `magent-enable-tools`):

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
├── magent.el         # Main entry point
├── magent-config.el  # Configuration (customize group)
├── magent-api.el     # HTTP API client for LLM providers
├── magent-session.el # Session & message history
├── magent-tools.el   # Tool implementations
├── magent-agent.el   # Agent logic & tool calling
└── magent-ui.el      # Minibuffer UI & display
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
- https://github.com/anomalyco/magent
- https://magent.ai
