import { createSignal, createEffect, Accessor } from "solid-js";

/**
 * A reusable hook for localStorage-backed Set<string> signals.
 * Used for filter state and collapse state persistence.
 *
 * @param storageKey - localStorage key for persistence
 * @returns [accessor, toggle, has] - accessor for the Set, toggle function, has check
 */
export function usePersistedSet(
  storageKey: string
): [Accessor<Set<string>>, (id: string) => void, (id: string) => boolean] {
  const load = (): Set<string> => {
    try {
      const saved = localStorage.getItem(storageKey);
      return saved ? new Set(JSON.parse(saved)) : new Set();
    } catch {
      return new Set();
    }
  };

  const [set, setSet] = createSignal<Set<string>>(load());

  // Persist whenever the set changes
  createEffect(() => {
    localStorage.setItem(storageKey, JSON.stringify([...set()]));
  });

  const toggle = (id: string) => {
    setSet((prev) => {
      const next = new Set(prev);
      if (next.has(id)) {
        next.delete(id);
      } else {
        next.add(id);
      }
      return next;
    });
  };

  const has = (id: string) => set().has(id);

  return [set, toggle, has];
}
