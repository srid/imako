import { Component, createSignal, createMemo, onMount, onCleanup, For, Show } from "solid-js";
import { useNavigate } from "@solidjs/router";
import { vaultInfo } from "@/store";

export const CommandPalette: Component = () => {
  const [isOpen, setIsOpen] = createSignal(false);
  const [query, setQuery] = createSignal("");
  const [selectedIndex, setSelectedIndex] = createSignal(0);
  const navigate = useNavigate();
  let inputRef: HTMLInputElement | undefined;

  // Sort notes by modification time (most recent first), then filter by query
  const filteredNotes = createMemo(() => {
    const notes = vaultInfo.notes;
    const entries = Object.entries(notes);
    
    // Sort by mtime descending
    const sorted = entries.sort(([, a], [, b]) => 
      new Date(b).getTime() - new Date(a).getTime()
    );
    
    const q = query().toLowerCase();
    if (!q) return sorted.slice(0, 20); // Show top 20 recent when no query
    
    return sorted.filter(([path]) => 
      path.toLowerCase().includes(q)
    ).slice(0, 20);
  });

  const openNote = (path: string) => {
    setIsOpen(false);
    setQuery("");
    navigate(`/n/${encodeURIComponent(path)}`);
  };

  const handleKeyDown = (e: KeyboardEvent) => {
    // Global Ctrl+P / Cmd+P listener
    if ((e.ctrlKey || e.metaKey) && e.key === "p") {
      e.preventDefault();
      setIsOpen(true);
      setSelectedIndex(0);
      setTimeout(() => inputRef?.focus(), 0);
      return;
    }

    if (!isOpen()) return;

    switch (e.key) {
      case "Escape":
        setIsOpen(false);
        setQuery("");
        break;
      case "ArrowDown":
        e.preventDefault();
        setSelectedIndex(i => Math.min(i + 1, filteredNotes().length - 1));
        break;
      case "ArrowUp":
        e.preventDefault();
        setSelectedIndex(i => Math.max(i - 1, 0));
        break;
      case "Enter":
        e.preventDefault();
        const notes = filteredNotes();
        if (notes.length > 0 && notes[selectedIndex()]) {
          openNote(notes[selectedIndex()][0]);
        }
        break;
    }
  };

  onMount(() => {
    document.addEventListener("keydown", handleKeyDown);
  });

  onCleanup(() => {
    document.removeEventListener("keydown", handleKeyDown);
  });

  // Reset selection when query changes
  const handleInput = (e: Event) => {
    setQuery((e.target as HTMLInputElement).value);
    setSelectedIndex(0);
  };

  return (
    <Show when={isOpen()}>
      {/* Backdrop */}
      <div 
        class="fixed inset-0 bg-black/50 dark:bg-black/70 z-50 flex items-start justify-center pt-[20vh]"
        onClick={() => setIsOpen(false)}
      >
        {/* Modal */}
        <div 
          class="w-full max-w-lg bg-white dark:bg-stone-900 rounded-xl shadow-2xl border border-stone-200 dark:border-stone-700 overflow-hidden"
          onClick={(e) => e.stopPropagation()}
        >
          {/* Search input */}
          <div class="p-4 border-b border-stone-200 dark:border-stone-700">
            <input
              ref={inputRef}
              type="text"
              placeholder="Search notes..."
              value={query()}
              onInput={handleInput}
              class="w-full px-4 py-3 text-lg bg-stone-100 dark:bg-stone-800 text-stone-900 dark:text-stone-100 rounded-lg border-none outline-none focus:ring-2 focus:ring-amber-500 placeholder:text-stone-400 dark:placeholder:text-stone-500"
            />
          </div>

          {/* Results */}
          <div class="max-h-80 overflow-y-auto">
            <For each={filteredNotes()}>
              {([path, mtime], index) => (
                <button
                  class={`w-full px-4 py-3 text-left flex items-center justify-between hover:bg-stone-100 dark:hover:bg-stone-800 transition-colors ${
                    index() === selectedIndex() 
                      ? "bg-amber-50 dark:bg-amber-900/20 text-amber-900 dark:text-amber-100" 
                      : "text-stone-700 dark:text-stone-300"
                  }`}
                  onClick={() => openNote(path)}
                >
                  <span class="font-medium truncate">{path}</span>
                  <span class="text-xs text-stone-400 dark:text-stone-500 ml-2 shrink-0">
                    {new Date(mtime).toLocaleDateString()}
                  </span>
                </button>
              )}
            </For>
            <Show when={filteredNotes().length === 0}>
              <div class="px-4 py-8 text-center text-stone-400 dark:text-stone-500">
                No notes found
              </div>
            </Show>
          </div>

          {/* Footer hint */}
          <div class="px-4 py-2 border-t border-stone-200 dark:border-stone-700 bg-stone-50 dark:bg-stone-800/50 text-xs text-stone-400 dark:text-stone-500 flex gap-4">
            <span><kbd class="px-1 py-0.5 bg-stone-200 dark:bg-stone-700 rounded">↑↓</kbd> navigate</span>
            <span><kbd class="px-1 py-0.5 bg-stone-200 dark:bg-stone-700 rounded">Enter</kbd> open</span>
            <span><kbd class="px-1 py-0.5 bg-stone-200 dark:bg-stone-700 rounded">Esc</kbd> close</span>
          </div>
        </div>
      </div>
    </Show>
  );
};
