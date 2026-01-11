from uuid import UUID
from datetime import datetime
from typing import Optional, Dict, Any
from pydantic import BaseModel, Field, field_serializer


def _get_item_metadata(obj):
    """Helper to get item_metadata from SQLAlchemy model"""
    if hasattr(obj, 'item_metadata'):
        return obj.item_metadata
    return {}


class ItemBase(BaseModel):
    """Base item schema"""

    description: str = Field(..., min_length=1, max_length=500)
    quantity: int = Field(default=1, ge=0)
    type: Optional[str] = Field(None, max_length=100)
    item_metadata: Optional[Dict[str, Any]] = Field(
        default_factory=dict,
        alias="metadata",
        serialization_alias="metadata"
    )
    nfc_tag: Optional[str] = Field(None, max_length=255)

    model_config = {
        "populate_by_name": True,
    }


class ItemCreate(ItemBase):
    """Schema for creating an item"""

    pass


class ItemUpdate(BaseModel):
    """Schema for updating an item"""

    description: Optional[str] = Field(None, min_length=1, max_length=500)
    quantity: Optional[int] = Field(None, ge=0)
    type: Optional[str] = Field(None, max_length=100)
    item_metadata: Optional[Dict[str, Any]] = Field(
        None,
        alias="metadata",
        serialization_alias="metadata"
    )
    nfc_tag: Optional[str] = Field(None, max_length=255)

    model_config = {
        "populate_by_name": True,
    }


class ItemResponse(ItemBase):
    """Schema for item response"""

    uuid: UUID
    created_at: datetime
    updated_at: datetime
    item_metadata: Optional[Dict[str, Any]] = Field(default_factory=dict, alias="metadata")

    @field_serializer('item_metadata')
    def serialize_metadata(self, value: Optional[Dict[str, Any]]) -> Dict[str, Any]:
        """Serialize metadata field"""
        return value or {}

    @classmethod
    def from_item(cls, item):
        """Create response from SQLAlchemy item, properly handling metadata"""
        return cls(
            uuid=item.uuid,
            description=item.description,
            quantity=item.quantity,
            type=item.type,
            item_metadata=item.item_metadata,
            nfc_tag=item.nfc_tag,
            created_at=item.created_at,
            updated_at=item.updated_at,
        )

    model_config = {
        "populate_by_name": True,
        "by_alias": True,
    }


class ItemListResponse(BaseModel):
    """Schema for paginated item list"""

    items: list[ItemResponse]
    total: int
    page: int
    page_size: int


class ScanRequest(BaseModel):
    """Schema for NFC/barcode scan request"""

    scan_data: str = Field(..., description="Scanned NFC tag ID or barcode")
    scanner_type: Optional[str] = Field(
        "nfc", description="Type of scanner used (nfc, barcode, etc.)"
    )
