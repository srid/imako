/**
 * Theme State — Client-only, localStorage-persisted.
 *
 * Three modes: "light" | "dark" | "system" (default).
 * Applies the `dark` class on <html> and sets <meta name="color-scheme">.
 */

import { createSignal, createEffect } from "solid-js";

export type Theme = "light" | "dark" | "system";

const STORAGE_KEY = "imako-theme";
const CYCLE: Theme[] = ["light", "dark", "system"];

/** Load saved preference, defaulting to "system". */
function loadTheme(): Theme {
  const saved = localStorage.getItem(STORAGE_KEY);
  if (saved === "light" || saved === "dark" || saved === "system") return saved;
  return "system";
}

export const [theme, setTheme] = createSignal<Theme>(loadTheme());

/** Cycle through light → dark → system → light. */
export function cycleTheme(): void {
  const idx = CYCLE.indexOf(theme());
  const next = CYCLE[(idx + 1) % CYCLE.length];
  setTheme(next);
  localStorage.setItem(STORAGE_KEY, next);
}

/** What the user actually sees — resolves "system" to the OS preference. */
export function resolvedTheme(): "light" | "dark" {
  const t = theme();
  if (t === "light" || t === "dark") return t;
  return window.matchMedia("(prefers-color-scheme: dark)").matches
    ? "dark"
    : "light";
}

/** Sync the DOM with the current resolved theme. */
function applyTheme(): void {
  const isDark = resolvedTheme() === "dark";
  document.documentElement.classList.toggle("dark", isDark);

  // Update color-scheme meta for native UI elements (scrollbars, inputs)
  let meta = document.querySelector<HTMLMetaElement>(
    'meta[name="color-scheme"]'
  );
  if (!meta) {
    meta = document.createElement("meta");
    meta.name = "color-scheme";
    document.head.appendChild(meta);
  }
  meta.content = isDark ? "dark" : "light";
}

/**
 * Initialize theme: apply immediately and set up reactive effect + OS listener.
 * Call once in App's onMount.
 */
export function initTheme(): void {
  // Apply immediately (before first paint after hydration)
  applyTheme();

  // Re-apply whenever the signal changes
  createEffect(() => {
    theme(); // track the signal
    applyTheme();
  });

  // Listen for OS preference changes (relevant in "system" mode)
  window
    .matchMedia("(prefers-color-scheme: dark)")
    .addEventListener("change", () => {
      if (theme() === "system") applyTheme();
    });
}
