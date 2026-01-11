# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal "playground" repository for experimental code and quick learning projects. Each experiment is isolated in its own numbered directory (e.g., `001-lean-hello-world/`, `002-magent/`).

## Project Structure

The repository contains independent experiments in separate directories:

- `001-lean-hello-world/` - Lean 4 theorem prover project
- `002-magent/` - OpenCode AI coding agent (Emacs Lisp)
- `003-erp/` - Personal ERP system with NFC support (Python/FastAPI)

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

## Personal ERP System (003-erp/)

A web-based inventory management system for tracking personal items with NFC tags. Built with FastAPI, PostgreSQL, and vanilla JavaScript. Uses [uv](https://github.com/astral-sh/uv) for fast package management.

### Quick Start

```bash
# Navigate to project
cd 003-erp

# Install uv (if not already installed)
curl -LsSf https://astral.sh/uv/install.sh | sh

# Install dependencies (uv creates .venv and installs Python)
uv sync

# Set up PostgreSQL (run these commands in psql)
# CREATE DATABASE erp;
# CREATE USER erp WITH PASSWORD 'erp';
# GRANT ALL PRIVILEGES ON DATABASE erp TO erp;

# Configure environment
cp .env.example .env

# Initialize database
uv run python scripts/init_db.py

# Run server
uv run backend/run.py
```

Access the web interface at `http://localhost:8000`.

### Tech Stack

- **Backend**: Python 3.10+, FastAPI, SQLAlchemy, PostgreSQL
- **Frontend**: Vanilla JavaScript, HTML5, CSS3
- **Database**: PostgreSQL with JSONB for extensible fields
- **Package Manager**: [uv](https://github.com/astral-sh/uv) - Fast Python package installer

### Project Structure

```
003-erp/
├── backend/
│   ├── app/
│   │   ├── api/          # REST API endpoints (FastAPI)
│   │   ├── core/         # Config, database connection
│   │   ├── models/       # SQLAlchemy ORM models
│   │   ├── schemas/      # Pydantic validation schemas
│   │   └── main.py       # FastAPI application entry
│   ├── requirements.txt  # Legacy dependencies (use pyproject.toml)
│   └── run.py           # Server startup script
├── frontend/
│   ├── css/style.css    # Modern responsive styling
│   ├── js/app.js        # Client-side application logic
│   └── index.html       # Single-page application
├── scanners/            # Extensible scanner interface
│   ├── base.py          # Abstract scanner interface
│   └── keyboard.py      # USB keyboard/NFC scanner
├── scripts/
│   └── init_db.py       # Database initialization script
├── pyproject.toml       # uv project configuration
├── .env.example         # Environment variables template
└── README.md            # Detailed project documentation
```

### Key Features

- **Full CRUD**: Create, read, update, delete inventory items
- **NFC/Barcode Support**: Scan NFC tags or barcodes to look up items
- **Extensible Metadata**: JSONB fields for custom attributes
- **Search & Filter**: Real-time search and type filtering
- **Pagination**: Efficient handling of large inventories
- **Pluggable Scanners**: Abstract interface for different scanner types
- **Modern UI**: Clean, responsive web interface
- **API Documentation**: Auto-generated Swagger/ReDoc at `/api/docs`

### API Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/items` | List items (with pagination/filtering) |
| POST | `/api/items` | Create new item |
| GET | `/api/items/{uuid}` | Get item by UUID |
| PUT | `/api/items/{uuid}` | Update item |
| DELETE | `/api/items/{uuid}` | Delete item |
| POST | `/api/items/scan` | Look up item by scanned tag |
| GET | `/api/items/types/list` | Get all item types |

### Database Schema

**Items Table**:
- `uuid` (UUID, primary key)
- `description` (TEXT, required)
- `quantity` (INTEGER, default 1)
- `type` (VARCHAR(100), optional)
- `metadata` (JSONB, extensible fields)
- `nfc_tag` (VARCHAR(255), unique, indexed)
- `created_at`, `updated_at` (TIMESTAMP)

### Scanner Interface

The system provides an extensible scanner interface:

```python
from scanners import KeyboardScanner

# Using context manager
with KeyboardScanner() as scanner:
    data = scanner.read()  # Returns scanned NFC tag or barcode
    print(f"Scanned: {data}")
```

**Implementing Custom Scanners**:

```python
from scanners.base import ScannerInterface

class MyScanner(ScannerInterface):
    def connect(self) -> bool:
        # Initialize scanner
        pass

    def read(self) -> Optional[str]:
        # Read from scanner, return data
        pass

    def disconnect(self) -> bool:
        # Close connection
        pass
```

### Configuration

Environment variables (`.env` file):

| Variable | Default | Description |
|----------|---------|-------------|
| `DATABASE_URL` | `postgresql://erp:erp@localhost:5432/erp` | PostgreSQL connection |
| `DEBUG` | `true` | Enable debug mode |
| `APP_NAME` | `Personal ERP System` | Application name |
| `CORS_ORIGINS` | `http://localhost:3000` | Allowed CORS origins |

### Development Notes

- **Package management**: Uses `uv` for fast dependency installation and management
- **Single-user design**: No authentication required for LAN use
- **Keyboard scanners**: Most USB barcode/NFC readers act as keyboard devices
- **JSONB metadata**: Store custom fields without schema changes
- **Auto-reload**: Server auto-reloads in debug mode
- **Static files**: Frontend served by FastAPI from `/frontend`

### Common uv Commands

```bash
# Install dependencies
uv sync

# Run the application
uv run backend/run.py

# Run any Python script
uv run python scripts/init_db.py

# Add a new dependency
uv add <package-name>

# Remove a dependency
uv remove <package-name>

# List installed packages
uv pip list
```

### Keyboard Shortcuts

- `Ctrl+N` (Cmd+N): Add new item
- `Ctrl+S` (Cmd+S): Open scan dialog
- `Escape`: Close modals
