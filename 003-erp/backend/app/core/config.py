from pydantic_settings import BaseSettings
from functools import lru_cache


class Settings(BaseSettings):
    """Application configuration"""

    # Database
    database_url: str = "postgresql://erp:erp@localhost:5432/erp"

    # API
    api_prefix: str = "/api"

    # CORS
    cors_origins: list[str] = ["http://localhost:3000", "http://127.0.0.1:3000"]

    # App
    app_name: str = "Personal ERP System"
    debug: bool = True

    class Config:
        env_file = ".env"
        case_sensitive = False


@lru_cache
def get_settings() -> Settings:
    """Get cached settings instance"""
    return Settings()
