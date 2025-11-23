// This script maintains UI state (folder open/close, filter toggles) across page loads
// and HTMX content swaps using browser's localStorage API.

const STORAGE_KEY = 'imako-state';

// --- State Management ---
// localStorage is a browser API that persists key-value pairs (strings only).
// Data survives page reloads and browser restarts.
// We store all app state in a single object: {folders: {...}, filters: {...}}

let appState = {
  folders: {},  // {"/path/to/folder": true/false}
  filters: {}   // {"completed": true/false, "cancelled": true/false, ...}
};

function loadState() {
  const saved = localStorage.getItem(STORAGE_KEY);
  if (saved) {
    appState = JSON.parse(saved);
    // Ensure both properties exist even if saved state is incomplete
    appState.folders = appState.folders || {};
    appState.filters = appState.filters || {};
  }
}

function saveState() {
  localStorage.setItem(STORAGE_KEY, JSON.stringify(appState));
}

// --- Folder State Logic ---
// Folders use <details> elements. The `open` attribute controls expand/collapse.

function restoreFolderListeners() {
  // Find all folder elements in the DOM (Document Object Model - the HTML tree)
  // querySelectorAll returns a NodeList (array-like) of matching elements
  document.querySelectorAll('details[data-folder-path]').forEach(details => {
    const path = details.getAttribute('data-folder-path');
    if (path in appState.folders) {
      // Set the `open` property to restore expanded/collapsed state
      details.open = appState.folders[path];
    }

    // HACK: Replace element with a clone to remove old event listeners
    // After HTMX swaps content, we need to avoid attaching duplicate listeners.
    // cloneNode(true) creates a deep copy without event listeners attached.
    details.replaceWith(details.cloneNode(true));
  });

  // Re-query DOM after cloning (old references are now invalid)
  document.querySelectorAll('details[data-folder-path]').forEach(details => {
    // addEventListener attaches a function that runs when the event fires
    // 'toggle' fires when user clicks the folder to expand/collapse
    details.addEventListener('toggle', () => {
      const path = details.getAttribute('data-folder-path');
      appState.folders[path] = details.open;
      saveState();
    });
  });
}

// --- Filter State Logic ---
// Filters work by toggling CSS classes on the container (e.g., 'show-completed').
// CSS rules use these classes to show/hide tasks.

function toggleFilter(filterName, className, containerId, buttonId) {
  const container = document.getElementById(containerId);
  const btn = document.getElementById(buttonId);
  if (!container || !btn) return;

  // classList.toggle() adds/removes a CSS class and returns the new state (true = added)
  const isShowing = container.classList.toggle(className);
  // aria-pressed is an accessibility attribute for screen readers
  btn.setAttribute('aria-pressed', isShowing);
  appState.filters[filterName] = isShowing;
  saveState();
  hideEmptyContainers();
}

function hideEmptyContainers() {
  document.querySelectorAll('details[data-folder-path]').forEach(details => {
    const tasks = details.querySelectorAll('.group\\/task');
    // Filter to only tasks that are currently visible
    const visibleTasks = Array.from(tasks).filter(task => {
      // getComputedStyle returns the actual CSS styles applied to the element
      const style = window.getComputedStyle(task);
      return style.display !== 'none';
    });

    // Hide folders that have tasks but all are filtered out
    if (tasks.length > 0 && visibleTasks.length === 0) {
      details.style.display = 'none';
    } else {
      details.style.display = '';  // Empty string = remove inline style
    }
  });
}

function restoreFilterState() {
  const container = document.getElementById('task-content');
  if (!container) return;

  // Object.entries converts {key: value} to [[key, value], ...] for iteration
  Object.entries(appState.filters).forEach(([filterName, isActive]) => {
    const className = 'show-' + filterName;
    const btn = document.getElementById(filterName + '-toggle');

    // classList.toggle(className, boolean) adds if true, removes if false
    container.classList.toggle(className, isActive);
    if (btn) btn.setAttribute('aria-pressed', isActive);
  });
}

// --- Initialization ---

function initApp() {
  loadState();  // Load saved state from localStorage into appState
  restoreFolderListeners();
  restoreFilterState();
  hideEmptyContainers();
}

// DOMContentLoaded fires when HTML is fully loaded and parsed
document.addEventListener('DOMContentLoaded', initApp);

// htmx:afterSwap fires after HTMX updates part of the page
// We need to re-initialize state for the new DOM elements
document.addEventListener('htmx:afterSwap', () => {
  initApp();
});
