from fastapi import APIRouter
from .items import router as items_router

api_router = APIRouter()

api_router.include_router(items_router)

__all__ = ["api_router"]
