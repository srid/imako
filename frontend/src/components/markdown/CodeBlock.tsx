/**
 * Syntax-highlighted code block using Shiki.
 *
 * Lazily loads the Shiki highlighter on first render, then highlights code
 * with language-aware tokenization. Falls back to plain <pre><code> while loading.
 */
import { Component, createSignal, createEffect, Show } from "solid-js";
import type { Attr } from "./types";
import { createHighlighter, type Highlighter } from "shiki";

// Common languages to pre-load — needed for injection support (e.g. Haskell inside Markdown).
// Shiki's injection only works if the injected language grammar is already loaded.
const PRELOADED_LANGS = [
  "haskell", "rust", "python", "typescript", "javascript",
  "nix", "bash", "yaml", "json", "html", "css", "c",
  "markdown", "go", "toml",
];

// Singleton highlighter — loaded once, shared across all CodeBlock instances.
let highlighterPromise: Promise<Highlighter> | null = null;
const loadedLangs = new Set<string>(PRELOADED_LANGS);

async function getHighlighter(lang: string): Promise<Highlighter> {
  if (!highlighterPromise) {
    highlighterPromise = createHighlighter({
      themes: ["github-dark", "github-light"],
      langs: PRELOADED_LANGS,
    });
  }

  const hl = await highlighterPromise;

  // Dynamically load languages not in the pre-loaded set.
  if (!loadedLangs.has(lang)) {
    try {
      await hl.loadLanguage(lang as any);
      loadedLangs.add(lang);
    } catch {
      // Unknown language — will fall back to plain text.
    }
  }

  return hl;
}

export const CodeBlock: Component<{ attr: Attr; code: string }> = (props) => {
  const lang = () => props.attr[1][0] || "";
  const [html, setHtml] = createSignal("");

  createEffect(() => {
    const language = lang();
    const code = props.code;

    // Highlight asynchronously.
    getHighlighter(language || "text").then((hl) => {
      try {
        const result = hl.codeToHtml(code, {
          lang: language || "text",
          themes: { dark: "github-dark", light: "github-light" },
        });
        setHtml(result);
      } catch {
        // Fallback: render as plain text if highlighting fails.
        setHtml("");
      }
    });
  });

  return (
    <Show
      when={html()}
      fallback={
        <pre class="shiki-fallback bg-stone-50 dark:bg-stone-900 border border-stone-200 dark:border-stone-800 p-4 rounded-xl my-6 overflow-x-auto shadow-sm">
          <code class="text-sm font-mono text-stone-800 dark:text-stone-200 leading-normal">
            {props.code}
          </code>
        </pre>
      }
    >
      <div class="code-block my-6" innerHTML={html()} />
    </Show>
  );
};
