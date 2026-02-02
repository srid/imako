/**
 * Block node renderer - handles paragraphs, headings, lists, etc.
 * Now uses Pandoc's native Block/Inline types directly.
 */
import { Component, For, Switch, Match } from "solid-js";
import type { Block, Inline, Attr } from "./types";
import { InlineRenderer } from "./InlineRenderer";

/**
 * Renders an array of Pandoc Block nodes.
 */
export const BlockRenderer: Component<{ blocks: Block[] }> = (props) => {
  return (
    <For each={props.blocks}>
      {(block) => (
        <Switch fallback={null}>
          {/* Plain - inline content without paragraph wrapping */}
          <Match when={block.t === "Plain" && block}>
            {(b) => (
              <span class="text-stone-700 dark:text-stone-300">
                <InlineRenderer inlines={b().c} />
              </span>
            )}
          </Match>

          {/* Para - paragraph with inline content */}
          <Match when={block.t === "Para" && block}>
            {(b) => (
              <p class="text-stone-700 dark:text-stone-300">
                <InlineRenderer inlines={b().c} />
              </p>
            )}
          </Match>

          {/* Header - [level, Attr, inlines] */}
          <Match when={block.t === "Header" && block}>
            {(b) => {
              const [level, _attr, inlines] = b().c;
              const classes = "font-bold mt-4 mb-2 text-stone-800 dark:text-stone-100";
              switch (level) {
                case 1:
                  return <h1 class={`text-2xl ${classes}`}><InlineRenderer inlines={inlines} /></h1>;
                case 2:
                  return <h2 class={`text-xl ${classes}`}><InlineRenderer inlines={inlines} /></h2>;
                case 3:
                  return <h3 class={`text-lg ${classes}`}><InlineRenderer inlines={inlines} /></h3>;
                default:
                  return <h4 class={`text-base ${classes}`}><InlineRenderer inlines={inlines} /></h4>;
              }
            }}
          </Match>

          {/* BulletList - contents: Block[][] */}
          <Match when={block.t === "BulletList" && block}>
            {(b) => (
              <div class="space-y-0.5">
                <For each={b().c}>
                  {(item) => <ListItemRenderer blocks={item} />}
                </For>
              </div>
            )}
          </Match>

          {/* OrderedList - contents: [ListAttributes, Block[][]] */}
          <Match when={block.t === "OrderedList" && block}>
            {(b) => {
              const [_listAttrs, items] = b().c;
              return (
                <ol class="list-decimal list-inside space-y-0.5 ml-4">
                  <For each={items}>
                    {(item) => (
                      <li class="text-stone-700 dark:text-stone-300">
                        <BlockRenderer blocks={item} />
                      </li>
                    )}
                  </For>
                </ol>
              );
            }}
          </Match>

          {/* CodeBlock - [Attr, string] */}
          <Match when={block.t === "CodeBlock" && block}>
            {(b) => {
              const [_attr, code] = b().c;
              return (
                <pre class="bg-stone-100 dark:bg-stone-800 p-4 rounded-lg my-2 overflow-x-auto">
                  <code class="text-sm font-mono">{code}</code>
                </pre>
              );
            }}
          </Match>

          {/* BlockQuote - Block[] */}
          <Match when={block.t === "BlockQuote" && block}>
            {(b) => (
              <blockquote class="border-l-4 border-amber-400 dark:border-amber-600 pl-4 my-2 italic text-stone-600 dark:text-stone-400">
                <BlockRenderer blocks={b().c} />
              </blockquote>
            )}
          </Match>

          {/* HorizontalRule */}
          <Match when={block.t === "HorizontalRule"}>
            <hr class="border-stone-300 dark:border-stone-600 my-4" />
          </Match>

          {/* Div - [Attr, Block[]] */}
          <Match when={block.t === "Div" && block}>
            {(b) => {
              const [_attr, blocks] = b().c;
              return (
                <div>
                  <BlockRenderer blocks={blocks} />
                </div>
              );
            }}
          </Match>

          {/* LineBlock - Inline[][] (each line is a list of inlines) */}
          <Match when={block.t === "LineBlock" && block}>
            {(b) => (
              <div class="text-stone-700 dark:text-stone-300">
                <For each={b().c}>
                  {(line) => (
                    <div>
                      <InlineRenderer inlines={line} />
                    </div>
                  )}
                </For>
              </div>
            )}
          </Match>

          {/* RawBlock - [Format, string] - render as preformatted */}
          <Match when={block.t === "RawBlock" && block}>
            {(b) => {
              const [_format, text] = b().c;
              return (
                <pre class="bg-stone-100 dark:bg-stone-800 p-2 rounded text-sm font-mono">
                  {text}
                </pre>
              );
            }}
          </Match>

          {/* DefinitionList - [Inline[], Block[][]][] */}
          <Match when={block.t === "DefinitionList" && block}>
            {(b) => (
              <dl class="text-stone-700 dark:text-stone-300">
                <For each={b().c}>
                  {([term, defs]) => (
                    <>
                      <dt class="font-semibold">
                        <InlineRenderer inlines={term} />
                      </dt>
                      <For each={defs}>
                        {(def) => (
                          <dd class="ml-4">
                            <BlockRenderer blocks={def} />
                          </dd>
                        )}
                      </For>
                    </>
                  )}
                </For>
              </dl>
            )}
          </Match>

          {/* Figure - [Attr, Caption, Block[]] */}
          <Match when={block.t === "Figure" && block}>
            {(b) => {
              const [_attr, caption, blocks] = b().c;
              return (
                <figure class="my-4">
                  <BlockRenderer blocks={blocks} />
                  {caption[0] && (
                    <figcaption class="text-sm text-stone-500 dark:text-stone-400 mt-2">
                      <InlineRenderer inlines={caption[1].flatMap(blk => 
                        blk.t === "Plain" || blk.t === "Para" ? blk.c : []
                      )} />
                    </figcaption>
                  )}
                </figure>
              );
            }}
          </Match>

          {/* Table - complex, show placeholder for now */}
          <Match when={block.t === "Table"}>
            <div class="text-stone-400 italic">[Table not yet rendered]</div>
          </Match>
        </Switch>
      )}
    </For>
  );
};

/**
 * Renders a single list item.
 * - Single paragraph: inline with bullet
 * - Nested lists: indented
 */
const ListItemRenderer: Component<{ blocks: Block[] }> = (props) => {
  const firstBlock = () => props.blocks[0];
  const isSinglePara = () => 
    props.blocks.length === 1 && (firstBlock()?.t === "Para" || firstBlock()?.t === "Plain");
  const isNestedList = () => 
    firstBlock()?.t === "BulletList" || firstBlock()?.t === "OrderedList";
  
  return (
    <Switch fallback={
      // Default: bullet + block content
      <div class="flex items-start gap-2 ml-4">
        <span class="text-stone-400 select-none">•</span>
        <div class="flex-1">
          <BlockRenderer blocks={props.blocks} />
        </div>
      </div>
    }>
      {/* Single paragraph - inline with bullet */}
      <Match when={isSinglePara()}>
        <div class="flex items-start gap-2 ml-4 text-stone-700 dark:text-stone-300">
          <span class="text-stone-400 select-none">•</span>
          <span>
            <InlineRenderer inlines={(firstBlock() as { t: "Para" | "Plain"; c: Inline[] }).c} />
          </span>
        </div>
      </Match>
      
      {/* Nested list - indent */}
      <Match when={isNestedList()}>
        <div class="ml-4">
          <BlockRenderer blocks={props.blocks} />
        </div>
      </Match>
    </Switch>
  );
};
