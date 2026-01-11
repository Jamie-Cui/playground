from fastapi import FastAPI
from fastapi.staticfiles import StaticFiles
from fastapi.middleware.cors import CORSMiddleware
from pathlib import Path
from .core.config import get_settings
from .core.database import init_db
from .api import api_router

settings = get_settings()

# Create FastAPI app
app = FastAPI(
    title=settings.app_name,
    debug=settings.debug,
    docs_url="/api/docs",
    redoc_url="/api/redoc",
)

# Configure CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=settings.cors_origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Include API routes
app.include_router(api_router, prefix=settings.api_prefix)

# Mount frontend static files
frontend_path = Path(__file__).parent.parent.parent / "frontend"
try:
    app.mount("/", StaticFiles(directory=str(frontend_path), html=True), name="frontend")
except Exception:
    pass  # Frontend directory might not exist during development


@app.on_event("startup")
async def startup_event():
    """Initialize database on startup"""
    try:
        init_db()
    except Exception as e:
        print(f"Warning: Could not initialize database: {e}")


@app.get("/health")
def health_check():
    """Health check endpoint"""
    return {"status": "healthy"}
