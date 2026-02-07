.PHONY: help install run db-init db-reset clean test

help:
	@echo "Available commands:"
	@echo "  make install    - Install dependencies with uv"
	@echo "  make run        - Start the development server"
	@echo "  make db-init    - Initialize the database"
	@echo "  make db-reset   - Reset the database"
	@echo "  make clean      - Clean Python cache files"
	@echo "  make test       - Run tests (if configured)"
	@echo "  make help       - Show this help message"

install:
	uv sync

run:
	uv run backend/run.py

db-init:
	uv run python scripts/init_db.py

db-reset:
	@echo "Dropping and recreating database..."
	psql -U erp -d postgres -c "DROP DATABASE IF EXISTS erp;"
	psql -U erp -d postgres -c "CREATE DATABASE erp;"
	uv run python scripts/init_db.py

clean:
	find . -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null || true
	find . -type f -name "*.pyc" -delete
	find . -type f -name "*.pyo" -delete
	find . -type d -name "*.egg-info" -exec rm -rf {} + 2>/dev/null || true
	find . -type d -name ".pytest_cache" -exec rm -rf {} + 2>/dev/null || true
	find . -type d -name ".ruff_cache" -exec rm -rf {} + 2>/dev/null || true
	rm -rf .venv

test:
	@echo "No tests configured yet"
