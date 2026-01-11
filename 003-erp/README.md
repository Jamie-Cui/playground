# Personal ERP System

A web-based inventory management system for tracking personal items with NFC tags. Designed for single-user use on a local network.

## Quick Reference

| Action | Keyboard Shortcut | Description |
|--------|------------------|-------------|
| Add Item | `Ctrl+N` / `Cmd+N` | Open add item dialog |
| Scan Item | `Ctrl+S` / `Cmd+S` | Open scan dialog |
| Close Modal | `Escape` | Close any open modal |
| Access Web UI | `http://localhost:8000` | Main application |
| API Docs | `http://localhost:8000/api/docs` | Swagger UI |

## Features

- **Item Management**: Full CRUD operations for inventory items
- **NFC/Barcode Support**: Scan NFC tags or barcodes to quickly look up items
- **Flexible Metadata**: Extensible JSONB fields for custom attributes
- **Modern UI**: Clean, responsive web interface
- **Type Categorization**: Organize items by type
- **Search & Filter**: Quick search and filtering by type
- **Pagination**: Efficient handling of large inventories
- **Extensible Scanner Interface**: Plugin system for different scanner types

## Tech Stack

- **Backend**: Python 3.10+, FastAPI, SQLAlchemy, PostgreSQL
- **Frontend**: Vanilla JavaScript, HTML5, CSS3
- **Database**: PostgreSQL 14+
- **Package Manager**: [uv](https://github.com/astral-sh/uv) - Fast Python package installer

## Quick Start

### Prerequisites

1. **uv** - Install with: `curl -LsSf https://astral.sh/uv/install.sh | sh`
2. **Python 3.10+** - uv will manage this automatically
3. **PostgreSQL 14+**
4. **NFC/Barcode Scanner** (optional, but recommended)

### Installation

1. **Clone and navigate to project**:
   ```bash
   cd 003-erp
   ```

2. **Install dependencies with uv**:
   ```bash
   uv sync
   ```

   This will:
   - Create a virtual environment in `.venv/`
   - Install Python 3.10+ if needed
   - Install all project dependencies

4. **Set up PostgreSQL database**:

   Create a database and user:
   ```sql
   CREATE DATABASE erp;
   CREATE USER erp WITH PASSWORD 'erp';
   GRANT ALL PRIVILEGES ON DATABASE erp TO erp;
   ```

5. **Configure environment**:
   ```bash
   cp .env.example .env
   # Edit .env if needed (default settings should work)
   ```

6. **Initialize database**:
   ```bash
   python scripts/init_db.py
   ```

7. **Run the server**:
   ```bash
   uv run backend/run.py
   ```

   Or activate the virtual environment first:
   ```bash
   source .venv/bin/activate  # On Windows: .venv\Scripts\activate
   python backend/run.py
   ```

8. **Access the application**:
   - Open browser to: `http://localhost:8000`
   - API docs: `http://localhost:8000/api/docs`

## Usage

### Adding Items

1. Click the **Add Item** button (or press `Ctrl+N`)
2. Fill in the required fields:
   - **Description**: Item name/description (required)
   - **Quantity**: Number of items (default: 1)
   - **Type**: Category/type for organization
   - **NFC Tag**: Scan or manually enter NFC tag ID
   - **Metadata**: JSON object for custom fields (optional)
3. Click **Save**

### Scanning Items

1. Click the **Scan Item** button (or press `Ctrl+S`)
2. Click in the input field to focus
3. Scan an NFC tag or barcode with your scanner
4. System will look up the item and display details
5. If not found, you can add a new item with that tag

### Editing/Deleting Items

- **View/Edit**: Click on any item card
- **Edit**: Click the **Edit** button on an item
- **Delete**: Click the **Delete** button on an item

### Search and Filter

- **Search**: Type in the search box to filter by description or NFC tag
- **Type Filter**: Select a type from the dropdown to filter by category

### Metadata Examples

The `metadata` field accepts JSON for custom attributes. Examples:

```json
{
  "purchase_date": "2024-01-15",
  "price": 29.99,
  "location": "Living Room",
  "brand": "Acme Corp",
  "model": "XJ-9000",
  "serial": "SN123456",
  "warranty": "2026-01-15"
}
```

## Database Schema

### Items Table

| Column | Type | Description |
|--------|------|-------------|
| uuid | UUID | Primary key |
| description | TEXT | Item description (required) |
| quantity | INTEGER | Item quantity (default: 1) |
| type | VARCHAR(100) | Item category/type |
| metadata | JSONB | Custom fields as JSON |
| nfc_tag | VARCHAR(255) | NFC tag ID (unique, indexed) |
| created_at | TIMESTAMP | Creation timestamp |
| updated_at | TIMESTAMP | Last update timestamp |

## API Endpoints

### Items

- `GET /api/items` - List all items (with pagination and filtering)
- `POST /api/items` - Create new item
- `GET /api/items/{uuid}` - Get item by UUID
- `PUT /api/items/{uuid}` - Update item
- `DELETE /api/items/{uuid}` - Delete item
- `POST /api/items/scan` - Look up item by scanned tag
- `GET /api/items/types/list` - Get all unique item types

### Health

- `GET /health` - Health check endpoint

### API Examples

**Create a new item**:
```bash
curl -X POST http://localhost:8000/api/items \
  -H "Content-Type: application/json" \
  -d '{
    "description": "USB-C Cable",
    "quantity": 5,
    "type": "Cable",
    "nfc_tag": "04A3B2C1",
    "metadata": {"color": "white", "length": "2m"}
  }'
```

**List items with pagination**:
```bash
curl "http://localhost:8000/api/items?page=1&page_size=10&search=cable"
```

**Scan an NFC tag**:
```bash
curl -X POST http://localhost:8000/api/items/scan \
  -H "Content-Type: application/json" \
  -d '{"scan_data": "04A3B2C1", "scanner_type": "nfc"}'
```

**Update an item**:
```bash
curl -X PUT http://localhost:8000/api/items/<uuid> \
  -H "Content-Type: application/json" \
  -d '{"quantity": 3}'
```

### API Documentation

Interactive API documentation is available at:
- Swagger UI: `http://localhost:8000/api/docs`
- ReDoc: `http://localhost:8000/api/redoc`

## Scanner Support

### Built-in Scanner Types

1. **Keyboard Scanner** (`scanners/keyboard.py`)
   - Most USB barcode/NFC scanners act as keyboard devices
   - Automatically types the scanned data followed by Enter
   - Works out of the box with standard USB scanners

2. **Simulated Scanner** (`scanners/keyboard.py`)
   - For testing without hardware
   - Reads from stdin

### Adding Custom Scanner

Create a new class implementing the `ScannerInterface`:

```python
from scanners.base import ScannerInterface

class MyCustomScanner(ScannerInterface):
    def connect(self) -> bool:
        # Initialize scanner
        pass

    def read(self) -> Optional[str]:
        # Read from scanner
        pass

    def disconnect(self) -> bool:
        # Close connection
        pass
```

### Using Scanner in Code

```python
from scanners import KeyboardScanner

# Using context manager
with KeyboardScanner() as scanner:
    data = scanner.read()
    print(f"Scanned: {data}")
```

### Testing Your Scanner

Before setting up the full system, test your scanner:

1. **Simple test** - Open a text editor and scan a tag. If characters appear followed by Enter, your scanner works.

2. **Web UI test** - Use the built-in scan feature:
   - Open `http://localhost:8000`
   - Click "Scan Item"
   - Click in the input field
   - Scan a tag
   - The system will display the scanned data

3. **Python test**:
   ```python
   import sys
   print("Scanner test - Scan a tag now (press Ctrl+C to exit):")
   print("Your scanner should work if text appears below")
   try:
       while True:
           data = input()  # Scanner sends data as keyboard input
           if data:
               print(f"Scanned: {data}")
   except KeyboardInterrupt:
       print("\nTest complete")
   ```

## Configuration

Environment variables (`.env` file):

| Variable | Default | Description |
|----------|---------|-------------|
| DATABASE_URL | postgresql://erp:erp@localhost:5432/erp | PostgreSQL connection URL |
| DEBUG | true | Enable debug mode |
| APP_NAME | Personal ERP System | Application name |
| CORS_ORIGINS | http://localhost:3000 | Allowed CORS origins |

## Project Structure

```
003-erp/
├── backend/
│   ├── app/
│   │   ├── api/          # API route handlers
│   │   ├── core/         # Core functionality (config, database)
│   │   ├── models/       # SQLAlchemy database models
│   │   ├── schemas/      # Pydantic validation schemas
│   │   └── main.py       # FastAPI application
│   ├── requirements.txt  # Python dependencies
│   └── run.py           # Server startup script
├── frontend/
│   ├── css/             # Stylesheets
│   ├── js/              # Client-side JavaScript
│   └── index.html       # Main HTML page
├── scanners/            # Scanner interface implementations
├── scripts/             # Utility scripts
│   └── init_db.py      # Database initialization
├── .env.example        # Environment variables template
└── README.md           # This file
```

## Development

### Running in Development Mode

```bash
uv run backend/run.py
```

The server will auto-reload on code changes.

### Adding New Dependencies

```bash
# Add a new package
uv add fastapi

# Add development dependency
uv add --dev pytest

# Remove a package
uv remove fastapi
```

### Running Scripts with uv

```bash
# Initialize database
uv run python scripts/init_db.py

# Run any Python script
uv run python your-script.py
```

### Database Migrations

For production use, consider setting up Alembic migrations:

```bash
# Initialize Alembic (one-time)
alembic init alembic

# Create migration
alembic revision --autogenerate -m "描述"

# Apply migration
alembic upgrade head
```

## Production Deployment

### Security Considerations

1. **Network**: The system is designed for LAN use. Do not expose to the internet without adding authentication.
2. **Database**: Use strong passwords for PostgreSQL in production.
3. **CORS**: Limit `CORS_ORIGINS` to trusted origins only.
4. **HTTPS**: Use reverse proxy (nginx) with SSL/TLS for production.

### Using systemd

Create `/etc/systemd/system/erp.service`:

```ini
[Unit]
Description=Personal ERP System
After=network.target postgresql.service

[Service]
Type=simple
User=erp
WorkingDirectory=/path/to/003-erp
Environment="PATH=/path/to/003-erp/.venv/bin"
ExecStart=/path/to/003-erp/.venv/bin/python backend/run.py
Restart=always

[Install]
WantedBy=multi-user.target
```

Enable and start:
```bash
sudo systemctl enable erp
sudo systemctl start erp
```

### Using nginx (Reverse Proxy)

```nginx
server {
    listen 80;
    server_name your-lan-ip;

    location / {
        proxy_pass http://127.0.0.1:8000;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
```

## Troubleshooting

### Database Connection Issues

```
Error: could not connect to server
```

- Verify PostgreSQL is running: `sudo systemctl status postgresql`
- Check database credentials in `.env`
- Ensure database exists: `psql -l | grep erp`

### Scanner Not Working

1. Check scanner is recognized by system:
   - Linux: `ls /dev/input/`
   - macOS: System Information → USB
2. Test scanner in text editor (should type characters)
3. Try the web interface scan feature instead

### Port Already in Use

```bash
# Find process using port 8000
lsof -i :8000

# Kill the process
kill -9 <PID>
```

### Python Version Issues

If you get Python version errors:
```bash
# uv will automatically install the correct Python version
uv sync

# Or specify Python version explicitly
uv python install 3.10
```

### Import Errors

If you get import errors when running scripts:
```bash
# Ensure you're in the project root
cd /path/to/003-erp

# Use uv run which handles PYTHONPATH automatically
uv run python scripts/init_db.py
```

## License

This is a personal project for experimental use.

## Future Enhancements

- [ ] Item checkout/check-in tracking
- [ ] Location tracking
- [ ] Photo upload for items
- [ ] Reports and statistics
- [ ] Export to CSV/Excel
- [ ] Bulk import from CSV
- [ ] Item history/audit log
- [ ] Mobile app (PWA)
- [ ] NFC tag writing support
