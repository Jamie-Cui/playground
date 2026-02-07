from fastapi import FastAPI, Request
from fastapi.staticfiles import StaticFiles
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import HTMLResponse
from pathlib import Path
import re
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
    app.mount("/static", StaticFiles(directory=str(frontend_path)), name="static")
except Exception:
    pass  # Frontend directory might not exist during development


# UUID pattern for item URLs
UUID_PATTERN = re.compile(
    r'^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$',
    re.IGNORECASE
)


@app.get("/health")
def health_check():
    """Health check endpoint"""
    return {"status": "healthy"}


@app.get("/{uuid}", response_class=HTMLResponse)
async def item_by_uuid(uuid: str, request: Request):
    """Redirect to item view using hash routing"""
    if UUID_PATTERN.match(uuid):
        # Return HTML that redirects to hash URL
        return f"""
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Item {uuid[:8]}...</title>
    <script>
        window.location.hash = '#/item/{uuid}';
        // Load the main app
        window.location.href = '/#/item/{uuid}';
    </script>
</head>
<body>
    <p>Redirecting to item <a href="/#/item/{uuid}">{uuid[:8]}...</a>...</p>
</body>
</html>
        """
    else:
        # Not a UUID, serve index.html
        index_path = frontend_path / "index.html"
        if index_path.exists():
            return index_path.read_text()
        return "<html><body>Frontend not found</body></html>"


@app.get("/", response_class=HTMLResponse)
async def index():
    """Serve the main index.html"""
    index_path = frontend_path / "index.html"
    if index_path.exists():
        return index_path.read_text()
    return "<html><body>Frontend not found</body></html>"


@app.on_event("startup")
async def startup_event():
    """Initialize database on startup"""
    try:
        init_db()
    except Exception as e:
        print(f"Warning: Could not initialize database: {e}")
