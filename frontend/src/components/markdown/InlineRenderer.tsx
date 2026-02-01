/**
 * Inline node renderer - handles text, emphasis, links, etc.
 */
import { Component, For, Switch, Match } from "solid-js";
import type { InlineNode } from "./types";

export const InlineRenderer: Component<{ nodes: InlineNode[] }> = (props) => {
  return (
    <For each={props.nodes}>
      {(node) => (
        <Switch fallback={null}>
          <Match when={node.type === "text" && node}>
            {(n) => <>{(n() as { text: string }).text}</>}
          </Match>
          <Match when={node.type === "emphasis" && node}>
            {(n) => (
              <em>
                <InlineRenderer nodes={(n() as { content: InlineNode[] }).content} />
              </em>
            )}
          </Match>
          <Match when={node.type === "strong" && node}>
            {(n) => (
              <strong>
                <InlineRenderer nodes={(n() as { content: InlineNode[] }).content} />
              </strong>
            )}
          </Match>
          <Match when={node.type === "code" && node}>
            {(n) => (
              <code class="bg-stone-100 dark:bg-stone-800 px-1 rounded text-sm">
                {(n() as { code: string }).code}
              </code>
            )}
          </Match>
          <Match when={node.type === "link" && node}>
            {(n) => {
              const link = n() as { url: string; content: InlineNode[] };
              return (
                <a
                  href={link.url}
                  class="text-amber-600 dark:text-amber-400 hover:underline"
                  target="_blank"
                  rel="noopener"
                >
                  <InlineRenderer nodes={link.content} />
                </a>
              );
            }}
          </Match>
          <Match when={node.type === "wikilink" && node}>
            {(n) => {
              const wl = n() as { target: string; display: string | null };
              return (
                <span class="text-purple-600 dark:text-purple-400">
                  [[{wl.display || wl.target}]]
                </span>
              );
            }}
          </Match>
          <Match when={node.type === "strikeout" && node}>
            {(n) => (
              <del>
                <InlineRenderer nodes={(n() as { content: InlineNode[] }).content} />
              </del>
            )}
          </Match>
          <Match when={node.type === "lineBreak"}>
            <br />
          </Match>
        </Switch>
      )}
    </For>
  );
};
