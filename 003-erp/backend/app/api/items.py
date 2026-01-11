from uuid import UUID
from typing import Optional
from fastapi import APIRouter, Depends, HTTPException, Query
from sqlalchemy.orm import Session
from sqlalchemy import or_

from ..core.database import get_db
from ..models import Item
from ..schemas import ItemCreate, ItemUpdate, ItemResponse, ItemListResponse, ScanRequest

router = APIRouter(prefix="/items", tags=["items"])


@router.get("", response_model=ItemListResponse)
def list_items(
    page: int = Query(1, ge=1),
    page_size: int = Query(50, ge=1, le=100),
    search: Optional[str] = None,
    type: Optional[str] = None,
    db: Session = Depends(get_db),
):
    """List all items with pagination and filtering"""
    query = db.query(Item)

    # Apply search filter
    if search:
        query = query.filter(
            or_(
                Item.description.ilike(f"%{search}%"),
                Item.nfc_tag.ilike(f"%{search}%"),
            )
        )

    # Apply type filter
    if type:
        query = query.filter(Item.type == type)

    # Get total count
    total = query.count()

    # Apply pagination
    items = query.offset((page - 1) * page_size).limit(page_size).all()

    return ItemListResponse(
        items=[ItemResponse.from_item(item) for item in items],
        total=total,
        page=page,
        page_size=page_size,
    )


@router.post("", response_model=ItemResponse, status_code=201)
def create_item(item: ItemCreate, db: Session = Depends(get_db)):
    """Create a new item"""
    # Check if NFC tag already exists
    if item.nfc_tag:
        existing = db.query(Item).filter(Item.nfc_tag == item.nfc_tag).first()
        if existing:
            raise HTTPException(
                status_code=400, detail=f"Item with NFC tag '{item.nfc_tag}' already exists"
            )

    db_item = Item(**item.model_dump())
    db.add(db_item)
    db.commit()
    db.refresh(db_item)

    return ItemResponse.from_item(db_item)


@router.get("/{uuid}", response_model=ItemResponse)
def get_item(uuid: UUID, db: Session = Depends(get_db)):
    """Get item by UUID"""
    item = db.query(Item).filter(Item.uuid == uuid).first()
    if not item:
        raise HTTPException(status_code=404, detail="Item not found")

    return ItemResponse.from_item(item)


@router.put("/{uuid}", response_model=ItemResponse)
def update_item(uuid: UUID, item_update: ItemUpdate, db: Session = Depends(get_db)):
    """Update item by UUID"""
    db_item = db.query(Item).filter(Item.uuid == uuid).first()
    if not db_item:
        raise HTTPException(status_code=404, detail="Item not found")

    # Check if NFC tag already exists (if being updated)
    if item_update.nfc_tag and item_update.nfc_tag != db_item.nfc_tag:
        existing = (
            db.query(Item).filter(Item.nfc_tag == item_update.nfc_tag).first()
        )
        if existing:
            raise HTTPException(
                status_code=400, detail=f"Item with NFC tag '{item_update.nfc_tag}' already exists"
            )

    # Update fields
    update_data = item_update.model_dump(exclude_unset=True)
    for field, value in update_data.items():
        setattr(db_item, field, value)

    db.commit()
    db.refresh(db_item)

    return ItemResponse.from_item(db_item)


@router.delete("/{uuid}", status_code=204)
def delete_item(uuid: UUID, db: Session = Depends(get_db)):
    """Delete item by UUID"""
    db_item = db.query(Item).filter(Item.uuid == uuid).first()
    if not db_item:
        raise HTTPException(status_code=404, detail="Item not found")

    db.delete(db_item)
    db.commit()

    return None


@router.post("/scan", response_model=ItemResponse)
def scan_item(scan: ScanRequest, db: Session = Depends(get_db)):
    """Look up item by scanned NFC tag or barcode"""
    item = db.query(Item).filter(Item.nfc_tag == scan.scan_data).first()

    if not item:
        raise HTTPException(status_code=404, detail="No item found with this tag")

    return ItemResponse.from_item(item)


@router.get("/types/list", response_model=list[str])
def list_item_types(db: Session = Depends(get_db)):
    """Get list of all unique item types"""
    types = db.query(Item.type).filter(Item.type.isnot(None)).distinct().all()
    return [t[0] for t in types]
