from sqlalchemy import Column, Integer, String, Text, JSON
from sqlalchemy.dialects.postgresql import UUID
from uuid import uuid4
from .base import BaseModel


class Item(BaseModel):
    """Item model for tracking physical items"""

    __tablename__ = "items"
    __pydantic_exclude__ = {"metadata"}  # Exclude SQLAlchemy's metadata attribute

    uuid = Column(UUID(as_uuid=True), primary_key=True, default=uuid4)
    description = Column(Text, nullable=False)
    quantity = Column(Integer, default=1, nullable=False)
    type = Column(String(100), nullable=True)
    item_metadata = Column("metadata", JSON, nullable=True, default=dict)
    nfc_tag = Column(String(255), unique=True, nullable=True, index=True)

    @property
    def _metadata(self):
        """Private property to expose item_metadata for Pydantic"""
        return self.item_metadata

    def to_dict(self):
        """Convert model to dictionary"""
        return {
            "uuid": str(self.uuid),
            "description": self.description,
            "quantity": self.quantity,
            "type": self.type,
            "metadata": self.item_metadata or {},
            "nfc_tag": self.nfc_tag,
            "created_at": self.created_at.isoformat(),
            "updated_at": self.updated_at.isoformat(),
        }
