/**
 * Block node renderer - handles paragraphs, headings, lists, tasks, etc.
 */
import { Component, For, Switch, Match } from "solid-js";
import type { BlockNode, InlineNode } from "./types";
import { InlineRenderer } from "./InlineRenderer";

/**
 * Renders an array of block nodes.
 */
export const BlockRenderer: Component<{ nodes: BlockNode[] }> = (props) => {
  return (
    <For each={props.nodes}>
      {(node) => (
        <Switch fallback={null}>
          <Match when={node.type === "paragraph" && node}>
            {(n) => (
              <p class="text-stone-700 dark:text-stone-300">
                <InlineRenderer nodes={(n() as { content: InlineNode[] }).content} />
              </p>
            )}
          </Match>

          <Match when={node.type === "heading" && node}>
            {(n) => {
              const h = n() as { level: number; content: InlineNode[] };
              const classes = "font-bold mt-4 mb-2 text-stone-800 dark:text-stone-100";
              switch (h.level) {
                case 1:
                  return <h1 class={`text-2xl ${classes}`}><InlineRenderer nodes={h.content} /></h1>;
                case 2:
                  return <h2 class={`text-xl ${classes}`}><InlineRenderer nodes={h.content} /></h2>;
                case 3:
                  return <h3 class={`text-lg ${classes}`}><InlineRenderer nodes={h.content} /></h3>;
                default:
                  return <h4 class={`text-base ${classes}`}><InlineRenderer nodes={h.content} /></h4>;
              }
            }}
          </Match>

          <Match when={node.type === "bulletList" && node}>
            {(n) => {
              const items = (n() as { items: BlockNode[][] }).items;
              return (
                <div class="space-y-0.5">
                  <For each={items}>
                    {(item) => <ListItemRenderer blocks={item} />}
                  </For>
                </div>
              );
            }}
          </Match>

          <Match when={node.type === "orderedList" && node}>
            {(n) => (
              <ol class="list-decimal list-inside space-y-0.5 ml-4">
                <For each={(n() as { items: BlockNode[][] }).items}>
                  {(item) => (
                    <li class="text-stone-700 dark:text-stone-300">
                      <BlockRenderer nodes={item} />
                    </li>
                  )}
                </For>
              </ol>
            )}
          </Match>

          <Match when={node.type === "codeBlock" && node}>
            {(n) => {
              const cb = n() as { language: string; code: string };
              return (
                <pre class="bg-stone-100 dark:bg-stone-800 p-4 rounded-lg my-2 overflow-x-auto">
                  <code class="text-sm font-mono">{cb.code}</code>
                </pre>
              );
            }}
          </Match>

          <Match when={node.type === "blockquote" && node}>
            {(n) => (
              <blockquote class="border-l-4 border-amber-400 dark:border-amber-600 pl-4 my-2 italic text-stone-600 dark:text-stone-400">
                <BlockRenderer nodes={(n() as { content: BlockNode[] }).content} />
              </blockquote>
            )}
          </Match>

          <Match when={node.type === "horizontalRule"}>
            <hr class="border-stone-300 dark:border-stone-600 my-4" />
          </Match>

          <Match when={node.type === "task" && node}>
            {(n) => {
              const task = n() as { done: boolean; content: InlineNode[] };
              return (
                <div class="flex items-start gap-2">
                  <input
                    type="checkbox"
                    checked={task.done}
                    disabled
                    class="mt-1 h-4 w-4 rounded border-stone-400 dark:border-stone-500 accent-amber-500"
                  />
                  <span class={task.done ? "line-through text-stone-400 dark:text-stone-500" : "text-stone-700 dark:text-stone-300"}>
                    <InlineRenderer nodes={task.content} />
                  </span>
                </div>
              );
            }}
          </Match>

          <Match when={node.type === "div" && node}>
            {(n) => (
              <div>
                <BlockRenderer nodes={(n() as { content: BlockNode[] }).content} />
              </div>
            )}
          </Match>
        </Switch>
      )}
    </For>
  );
};

/**
 * Renders a single list item.
 * - Single paragraph: inline with bullet
 * - Tasks: checkbox without bullet
 * - Nested lists: indented
 */
const ListItemRenderer: Component<{ blocks: BlockNode[] }> = (props) => {
  const firstBlock = () => props.blocks[0];
  const isSingleParagraph = () => 
    props.blocks.length === 1 && firstBlock()?.type === "paragraph";
  const isTask = () => firstBlock()?.type === "task";
  const isNestedList = () => 
    firstBlock()?.type === "bulletList" || firstBlock()?.type === "orderedList";
  
  return (
    <Switch fallback={
      // Default: bullet + block content
      <div class="flex items-start gap-2 ml-4">
        <span class="text-stone-400 select-none">•</span>
        <div class="flex-1">
          <BlockRenderer nodes={props.blocks} />
        </div>
      </div>
    }>
      {/* Single paragraph - inline with bullet */}
      <Match when={isSingleParagraph()}>
        <div class="flex items-start gap-2 ml-4 text-stone-700 dark:text-stone-300">
          <span class="text-stone-400 select-none">•</span>
          <span>
            <InlineRenderer nodes={(firstBlock() as { content: InlineNode[] }).content} />
          </span>
        </div>
      </Match>
      
      {/* Task items - no bullet, just the task */}
      <Match when={isTask()}>
        <BlockRenderer nodes={props.blocks} />
      </Match>
      
      {/* Nested list - indent */}
      <Match when={isNestedList()}>
        <div class="ml-4">
          <BlockRenderer nodes={props.blocks} />
        </div>
      </Match>
    </Switch>
  );
};
