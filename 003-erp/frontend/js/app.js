// API Configuration
const API_BASE = '/api';

// State
let currentPage = 1;
let pageSize = 50;
let currentSearch = '';
let currentTypeFilter = '';
let allTypes = [];

// Router
function parseRoute() {
    const hash = window.location.hash.slice(1); // Remove #
    if (hash.startsWith('/item/')) {
        const uuid = hash.split('/')[2];
        if (uuid) {
            viewItem(uuid);
            return true;
        }
    }
    return false;
}

function updateRoute(uuid) {
    window.location.hash = `/item/${uuid}`;
}

function clearRoute() {
    history.pushState("", document.title, window.location.pathname + window.location.search);
}

// DOM Elements
const itemsList = document.getElementById('itemsList');
const pagination = document.getElementById('pagination');
const searchInput = document.getElementById('searchInput');
const typeFilter = document.getElementById('typeFilter');
const addBtn = document.getElementById('addBtn');
const scanBtn = document.getElementById('scanBtn');
const itemModal = document.getElementById('itemModal');
const scanModal = document.getElementById('scanModal');
const itemForm = document.getElementById('itemForm');
const modalTitle = document.getElementById('modalTitle');
const closeModal = document.getElementById('closeModal');
const closeScanModal = document.getElementById('closeScanModal');
const cancelBtn = document.getElementById('cancelBtn');
const scanInput = document.getElementById('scanInput');
const scanResult = document.getElementById('scanResult');
const typeSuggestions = document.getElementById('typeSuggestions');

// Utility Functions
function showToast(message, type = 'info') {
    const toast = document.getElementById('toast');
    toast.textContent = message;
    toast.className = `toast ${type} active`;
    setTimeout(() => toast.classList.remove('active'), 3000);
}

function formatDate(dateString) {
    return new Date(dateString).toLocaleDateString('en-US', {
        year: 'numeric',
        month: 'short',
        day: 'numeric',
        hour: '2-digit',
        minute: '2-digit'
    });
}

async function apiCall(endpoint, options = {}) {
    try {
        const response = await fetch(`${API_BASE}${endpoint}`, {
            headers: {
                'Content-Type': 'application/json',
                ...options.headers
            },
            ...options
        });

        if (!response.ok) {
            const error = await response.json().catch(() => ({ detail: 'Unknown error' }));
            throw new Error(error.detail || 'Request failed');
        }

        return await response.json();
    } catch (error) {
        showToast(error.message, 'error');
        throw error;
    }
}

// Item Functions
async function loadItems() {
    const params = new URLSearchParams({
        page: currentPage,
        page_size: pageSize
    });

    if (currentSearch) params.append('search', currentSearch);
    if (currentTypeFilter) params.append('type', currentTypeFilter);

    const data = await apiCall(`/items?${params}`);
    renderItems(data.items);
    renderPagination(data.total, data.page, data.page_size);
}

async function loadItemTypes() {
    allTypes = await apiCall('/items/types/list');

    // Update filter dropdown
    typeFilter.innerHTML = '<option value="">All Types</option>' +
        allTypes.map(type => `<option value="${type}">${type}</option>`).join('');

    // Update datalist suggestions
    typeSuggestions.innerHTML = allTypes.map(type => `<option value="${type}">`).join('');
}

function renderItems(items) {
    if (items.length === 0) {
        itemsList.innerHTML = `
            <div class="empty-state">
                <svg viewBox="0 0 20 20" fill="none" stroke="currentColor" stroke-width="2">
                    <rect x="3" y="3" width="14" height="14" rx="2"/>
                    <path d="M7 7h6M7 10h6M7 13h4"/>
                </svg>
                <h3>No items found</h3>
                <p>${currentSearch || currentTypeFilter ? 'Try adjusting your filters' : 'Add your first item to get started'}</p>
            </div>
        `;
        return;
    }

    itemsList.innerHTML = items.map(item => `
        <div class="item-card" data-uuid="${item.uuid}" onclick="viewItem('${item.uuid}')">
            <div class="item-header">
                <div>
                    <div class="item-description">${escapeHtml(item.description)}</div>
                    <div class="item-meta">
                        <span class="item-meta-item">
                            <svg width="14" height="14" viewBox="0 0 20 20" fill="none" stroke="currentColor" stroke-width="2">
                                <rect x="3" y="3" width="14" height="14" rx="2"/>
                            </svg>
                            ${item.uuid.slice(0, 8)}
                        </span>
                        ${item.type ? `
                            <span class="item-meta-item">
                                <span class="badge">${escapeHtml(item.type)}</span>
                            </span>
                        ` : ''}
                        <span class="item-meta-item">
                            <span class="badge quantity-badge">Qty: ${item.quantity}</span>
                        </span>
                        ${item.nfc_tag ? `
                            <span class="item-meta-item">
                                <span class="badge nfc-badge">NFC: ${escapeHtml(item.nfc_tag)}</span>
                            </span>
                        ` : ''}
                    </div>
                </div>
                <div class="item-actions" onclick="event.stopPropagation()">
                    <button class="btn btn-sm btn-primary" onclick="editItem('${item.uuid}')">Edit</button>
                    <button class="btn btn-sm btn-danger" onclick="deleteItem('${item.uuid}')">Delete</button>
                </div>
            </div>
        </div>
    `).join('');
}

function renderPagination(total, page, pageSize) {
    const totalPages = Math.ceil(total / pageSize);

    if (totalPages <= 1) {
        pagination.innerHTML = '';
        return;
    }

    let html = `
        <button onclick="goToPage(${page - 1})" ${page === 1 ? 'disabled' : ''}>&laquo; Previous</button>
    `;

    for (let i = 1; i <= totalPages; i++) {
        if (i === 1 || i === totalPages || (i >= page - 2 && i <= page + 2)) {
            html += `<button onclick="goToPage(${i})" ${i === page ? 'class="active"' : ''}>${i}</button>`;
        } else if (i === page - 3 || i === page + 3) {
            html += '<button disabled>...</button>';
        }
    }

    html += `
        <button onclick="goToPage(${page + 1})" ${page === totalPages ? 'disabled' : ''}>Next &raquo;</button>
    `;

    pagination.innerHTML = html;
}

function goToPage(page) {
    currentPage = page;
    loadItems();
    window.scrollTo({ top: 0, behavior: 'smooth' });
}

async function viewItem(uuid) {
    try {
        const item = await apiCall(`/items/${uuid}`);
        showItemModal(item);
        updateRoute(uuid);
    } catch (error) {
        console.error('Failed to load item:', error);
    }
}

async function editItem(uuid) {
    try {
        const item = await apiCall(`/items/${uuid}`);
        showItemModal(item, true);
    } catch (error) {
        console.error('Failed to load item:', error);
    }
}

function showItemModal(item, isEdit = false) {
    modalTitle.textContent = isEdit ? 'Edit Item' : 'Item Details';

    document.getElementById('itemId').value = item.uuid;
    document.getElementById('description').value = item.description;
    document.getElementById('quantity').value = item.quantity;
    document.getElementById('type').value = item.type || '';
    document.getElementById('nfcTag').value = item.nfc_tag || '';
    document.getElementById('metadata').value = Object.keys(item.metadata || {}).length > 0
        ? JSON.stringify(item.metadata, null, 2)
        : '';

    // Make fields read-only if viewing
    const formControls = itemForm.querySelectorAll('input, textarea, button[type="submit"]');
    formControls.forEach(control => {
        if (control.type !== 'hidden') {
            control.disabled = !isEdit;
        }
    });

    itemModal.classList.add('active');
}

async function deleteItem(uuid) {
    if (!confirm('Are you sure you want to delete this item?')) return;

    try {
        await apiCall(`/items/${uuid}`, { method: 'DELETE' });
        showToast('Item deleted successfully', 'success');
        loadItems();
        loadItemTypes();
    } catch (error) {
        console.error('Failed to delete item:', error);
    }
}

async function saveItem(e) {
    e.preventDefault();

    const uuid = document.getElementById('itemId').value;
    const description = document.getElementById('description').value.trim();
    const quantity = parseInt(document.getElementById('quantity').value);
    const type = document.getElementById('type').value.trim();
    const nfcTag = document.getElementById('nfcTag').value.trim();
    const metadataText = document.getElementById('metadata').value.trim();

    let metadata = {};
    if (metadataText) {
        try {
            metadata = JSON.parse(metadataText);
        } catch (error) {
            showToast('Invalid JSON in metadata field', 'error');
            return;
        }
    }

    const data = {
        description,
        quantity,
        type: type || null,
        nfc_tag: nfcTag || null,
        metadata
    };

    try {
        if (uuid) {
            await apiCall(`/items/${uuid}`, {
                method: 'PUT',
                body: JSON.stringify(data)
            });
            showToast('Item updated successfully', 'success');
        } else {
            await apiCall('/items', {
                method: 'POST',
                body: JSON.stringify(data)
            });
            showToast('Item created successfully', 'success');
        }

        itemModal.classList.remove('active');
        loadItems();
        loadItemTypes();
    } catch (error) {
        console.error('Failed to save item:', error);
    }
}

function openAddModal() {
    modalTitle.textContent = 'Add Item';
    itemForm.reset();
    document.getElementById('itemId').value = '';
    document.getElementById('quantity').value = '1';

    const formControls = itemForm.querySelectorAll('input, textarea, button');
    formControls.forEach(control => control.disabled = false);

    itemModal.classList.add('active');
    document.getElementById('description').focus();
}

// Scan Functions
function openScanModal() {
    scanModal.classList.add('active');
    scanInput.value = '';
    scanResult.classList.remove('active');
    scanInput.focus();
}

async function handleScan(e) {
    const scanData = e.target.value.trim();

    if (scanData && e.inputType === 'insertText' && e.data === null) {
        // Scanner submitted (usually sends Enter key)
        try {
            const item = await apiCall('/items/scan', {
                method: 'POST',
                body: JSON.stringify({ scan_data: scanData })
            });

            scanResult.innerHTML = `
                <div class="item-card">
                    <div class="item-header">
                        <div>
                            <div class="item-description">${escapeHtml(item.description)}</div>
                            <div class="item-meta">
                                <span class="badge">${item.type || 'Uncategorized'}</span>
                                <span class="badge quantity-badge">Qty: ${item.quantity}</span>
                            </div>
                        </div>
                    </div>
                    <p style="margin-top: 10px; color: var(--text-muted);">
                        Last updated: ${formatDate(item.updated_at)}
                    </p>
                    <button class="btn btn-primary" style="margin-top: 15px;" onclick="
                        scanModal.classList.remove('active');
                        editItem('${item.uuid}');
                    ">View Details</button>
                </div>
            `;
            scanResult.classList.add('active');
            scanInput.value = '';
        } catch (error) {
            scanResult.innerHTML = `
                <div style="text-align: center; color: var(--danger);">
                    <svg width="48" height="48" viewBox="0 0 20 20" fill="none" stroke="currentColor" stroke-width="2" style="margin-bottom: 10px;">
                        <circle cx="10" cy="10" r="8"/>
                        <line x1="12" y1="8" x2="8" y2="12"/>
                        <line x1="8" y1="8" x2="12" y2="12"/>
                    </svg>
                    <p>No item found with tag: ${escapeHtml(scanData)}</p>
                    <button class="btn btn-primary" style="margin-top: 15px;" onclick="
                        scanModal.classList.remove('active');
                        openAddModal();
                        document.getElementById('nfcTag').value = '${escapeHtml(scanData)}';
                    ">Add New Item</button>
                </div>
            `;
            scanResult.classList.add('active');
        }
    }
}

// Utility
function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

// Event Listeners
document.addEventListener('DOMContentLoaded', () => {
    loadItemTypes();
    // Check URL hash for item route
    if (!parseRoute()) {
        loadItems();
    }
});

// Handle hash changes
window.addEventListener('hashchange', () => {
    if (!parseRoute()) {
        loadItems();
    }
});

searchInput.addEventListener('input', debounce((e) => {
    currentSearch = e.target.value;
    currentPage = 1;
    loadItems();
}, 300));

typeFilter.addEventListener('change', (e) => {
    currentTypeFilter = e.target.value;
    currentPage = 1;
    loadItems();
});

addBtn.addEventListener('click', openAddModal);
scanBtn.addEventListener('click', openScanModal);

itemForm.addEventListener('submit', saveItem);

closeModal.addEventListener('click', () => {
    itemModal.classList.remove('active');
    clearRoute();
});
closeScanModal.addEventListener('click', () => scanModal.classList.remove('active'));
cancelBtn.addEventListener('click', () => {
    itemModal.classList.remove('active');
    clearRoute();
});

itemModal.addEventListener('click', (e) => {
    if (e.target === itemModal) {
        itemModal.classList.remove('active');
        clearRoute();
    }
});

scanModal.addEventListener('click', (e) => {
    if (e.target === scanModal) scanModal.classList.remove('active');
});

scanInput.addEventListener('input', handleScan);

// Keyboard shortcuts
document.addEventListener('keydown', (e) => {
    if (e.key === 'Escape') {
        if (itemModal.classList.contains('active')) {
            itemModal.classList.remove('active');
            clearRoute();
        }
        scanModal.classList.remove('active');
    }
    if (e.key === 'n' && (e.metaKey || e.ctrlKey)) {
        e.preventDefault();
        openAddModal();
    }
    if (e.key === 's' && (e.metaKey || e.ctrlKey) && !itemModal.classList.contains('active')) {
        e.preventDefault();
        openScanModal();
    }
});

function debounce(func, wait) {
    let timeout;
    return function executedFunction(...args) {
        const later = () => {
            clearTimeout(timeout);
            func(...args);
        };
        clearTimeout(timeout);
        timeout = setTimeout(later, wait);
    };
}
