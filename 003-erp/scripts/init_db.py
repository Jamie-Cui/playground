#!/usr/bin/env python3
"""Initialize the ERP database"""

import sys
import os

# Add backend directory to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'backend'))

from app.core.database import init_db, engine
from app.core.config import get_settings
from sqlalchemy import text


def create_database():
    """Create the database if it doesn't exist"""
    settings = get_settings()

    # Connect to PostgreSQL default database to create our database
    from sqlalchemy import create_engine

    # Parse database URL to get connection details
    db_url = settings.database_url
    if "postgresql://" in db_url:
        # Remove database name from URL to connect to default 'postgres' db
        default_url = db_url.rsplit('/', 1)[0] + '/postgres'

        engine = create_engine(default_url)
        conn = engine.connect()
        conn.execute(text("COMMIT"))  # Close any open transaction

        # Check if database exists
        db_name = db_url.rsplit('/', 1)[1].split('?')[0]
        result = conn.execute(
            text("SELECT 1 FROM pg_database WHERE datname=:db_name"), {"db_name": db_name}
        )

        if not result.fetchone():
            print(f"Creating database '{db_name}'...")
            conn.execute(text(f"CREATE DATABASE {db_name}"))
            print(f"Database '{db_name}' created successfully")
        else:
            print(f"Database '{db_name}' already exists")

        conn.close()
        engine.dispose()


def main():
    """Main initialization function"""
    print("Initializing ERP database...")

    try:
        # Create database
        create_database()

        # Create tables
        print("Creating tables...")
        init_db()
        print("Tables created successfully")

        print("\nDatabase initialization complete!")
        print(f"\nDatabase URL: {get_settings().database_url}")

    except Exception as e:
        print(f"\nError initializing database: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
