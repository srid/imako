/**
 * Block node renderer - handles paragraphs, headings, lists, etc.
 * Now uses Pandoc's native Block/Inline types directly.
 */
import { Component, For, Switch, Match } from "solid-js";
import type { Block, Inline, Attr } from "./types";
import { InlineRenderer } from "./InlineRenderer";
import { CodeBlock } from "./CodeBlock";

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
              <p class="text-stone-800 dark:text-stone-200 leading-relaxed mb-5 last:mb-0">
                <InlineRenderer inlines={b().c} />
              </p>
            )}
          </Match>

          {/* Header - [level, Attr, inlines] */}
          <Match when={block.t === "Header" && block}>
            {(b) => {
              const [level, _attr, inlines] = b().c;
              const classes = "font-bold text-stone-900 dark:text-stone-50 tracking-tight";
              switch (level) {
                case 1:
                  return <h1 class={`text-3xl sm:text-4xl mt-10 mb-6 pb-2 border-b border-stone-200 dark:border-stone-800 ${classes}`}><InlineRenderer inlines={inlines} /></h1>;
                case 2:
                  return <h2 class={`text-2xl sm:text-3xl mt-8 mb-4 ${classes}`}><InlineRenderer inlines={inlines} /></h2>;
                case 3:
                  return <h3 class={`text-xl sm:text-2xl mt-6 mb-3 ${classes}`}><InlineRenderer inlines={inlines} /></h3>;
                case 4:
                  return <h4 class={`text-lg sm:text-xl mt-5 mb-2 ${classes}`}><InlineRenderer inlines={inlines} /></h4>;
                default:
                  return <h5 class={`text-base uppercase tracking-wider text-stone-500 dark:text-stone-400 mt-5 mb-2 font-semibold`}><InlineRenderer inlines={inlines} /></h5>;
              }
            }}
          </Match>

          {/* BulletList - contents: Block[][] */}
          <Match when={block.t === "BulletList" && block}>
            {(b) => (
              <div class="space-y-1.5 mb-5 last:mb-0">
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
                <ol class="list-decimal list-outside space-y-1.5 ml-6 mb-5 last:mb-0 text-stone-700 dark:text-stone-300">
                  <For each={items}>
                    {(item) => (
                      <li class="pl-2 marker:text-stone-500">
                        <BlockRenderer blocks={item} />
                      </li>
                    )}
                  </For>
                </ol>
              );
            }}
          </Match>

          {/* CodeBlock - [Attr, string] — syntax highlighted via Shiki */}
          <Match when={block.t === "CodeBlock" && block}>
            {(b) => {
              const [attr, code] = b().c;
              return <CodeBlock attr={attr} code={code} />;
            }}
          </Match>

          {/* BlockQuote - Block[] */}
          <Match when={block.t === "BlockQuote" && block}>
            {(b) => (
              <blockquote class="border-l-4 border-amber-500 bg-amber-50/50 dark:bg-amber-900/10 dark:border-amber-600 px-5 py-3 rounded-r-lg my-6 italic text-stone-700 dark:text-stone-300 shadow-sm transition-all hover:bg-amber-50 dark:hover:bg-amber-900/20">
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
                <pre class="bg-stone-50 dark:bg-stone-900 border border-stone-200 dark:border-stone-800 p-3 rounded-lg text-sm font-mono my-4 overflow-x-auto">
                  {text}
                </pre>
              );
            }}
          </Match>

          {/* DefinitionList - [Inline[], Block[][]][] */}
          <Match when={block.t === "DefinitionList" && block}>
            {(b) => (
              <dl class="text-stone-800 dark:text-stone-200 my-5">
                <For each={b().c}>
                  {([term, defs]) => (
                    <div class="mb-4">
                      <dt class="font-bold text-stone-900 dark:text-stone-50 mb-1">
                        <InlineRenderer inlines={term} />
                      </dt>
                      <For each={defs}>
                        {(def) => (
                          <dd class="ml-6 text-stone-600 dark:text-stone-400">
                            <BlockRenderer blocks={def} />
                          </dd>
                        )}
                      </For>
                    </div>
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

          {/* Table - [Attr, Caption, ColSpec[], TableHead, TableBody[], TableFoot] */}
          <Match when={block.t === "Table" && block}>
            {(b) => {
              const [_attr, caption, colSpecs, tableHead, tableBodies, tableFoot] = b().c;
              
              // Helper to get alignment class
              const alignClass = (align: string) => {
                switch (align) {
                  case "AlignLeft": return "text-left";
                  case "AlignRight": return "text-right";
                  case "AlignCenter": return "text-center";
                  default: return "text-left";
                }
              };
              
              // Render a row
              const renderRow = (row: [Attr, any[]]) => {
                const [_rowAttr, cells] = row;
                return (
                  <tr class="border-b border-stone-200 dark:border-stone-700">
                    <For each={cells}>
                      {(cell, colIdx) => {
                        const [_cellAttr, alignment, _rowSpan, _colSpan, blocks] = cell;
                        const colAlign = alignment !== "AlignDefault" ? alignment : colSpecs[colIdx()]?.[0];
                        return (
                          <td class={`px-3 py-2 ${alignClass(colAlign)}`}>
                            <BlockRenderer blocks={blocks} />
                          </td>
                        );
                      }}
                    </For>
                  </tr>
                );
              };
              
              return (
                <div class="my-8 overflow-x-auto rounded-xl ring-1 ring-stone-200 dark:ring-stone-800 bg-white dark:bg-stone-950 shadow-sm">
                  <table class="min-w-full border-collapse text-left text-sm whitespace-nowrap">
                    {/* Caption */}
                    {caption[1].length > 0 && (
                      <caption class="text-xs font-semibold uppercase tracking-wider text-stone-500 dark:text-stone-400 py-3 bg-stone-50 dark:bg-stone-900 border-b border-stone-200 dark:border-stone-800">
                        <BlockRenderer blocks={caption[1]} />
                      </caption>
                    )}
                    
                    {/* Head */}
                    {tableHead[1].length > 0 && (
                      <thead class="bg-stone-50/80 dark:bg-stone-900/50 text-stone-900 dark:text-white">
                        <For each={tableHead[1]}>
                          {(row) => {
                            const [_rowAttr, cells] = row;
                            return (
                              <tr class="border-b border-stone-200 dark:border-stone-800">
                                <For each={cells}>
                                  {(cell, colIdx) => {
                                    const [_cellAttr, alignment, _rowSpan, _colSpan, blocks] = cell;
                                    const colAlign = alignment !== "AlignDefault" ? alignment : colSpecs[colIdx()]?.[0];
                                    return (
                                      <th scope="col" class={`px-4 py-3 font-semibold ${alignClass(colAlign)}`}>
                                        <BlockRenderer blocks={blocks} />
                                      </th>
                                    );
                                  }}
                                </For>
                              </tr>
                            );
                          }}
                        </For>
                      </thead>
                    )}
                    
                    {/* Bodies */}
                    <For each={tableBodies}>
                      {(body) => {
                        const [_bodyAttr, _rowHeadCols, headRows, bodyRows] = body;
                        return (
                          <tbody class="divide-y divide-stone-100 dark:divide-stone-800/60 bg-white dark:bg-stone-950">
                            <For each={headRows}>{renderRow}</For>
                            <For each={bodyRows}>{renderRow}</For>
                          </tbody>
                        );
                      }}
                    </For>
                    
                    {/* Foot */}
                    {tableFoot[1].length > 0 && (
                      <tfoot class="bg-stone-50 dark:bg-stone-900/80 border-t-2 border-stone-200 dark:border-stone-800">
                        <For each={tableFoot[1]}>{renderRow}</For>
                      </tfoot>
                    )}
                  </table>
                </div>
              );
            }}
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
      <div class="flex items-start gap-3">
        <div class="w-1.5 h-1.5 rounded-full bg-stone-400 dark:bg-stone-500 mt-2.5 shrink-0" />
        <div class="flex-1">
          <BlockRenderer blocks={props.blocks} />
        </div>
      </div>
    }>
      {/* Single paragraph - inline with bullet */}
      <Match when={isSinglePara()}>
        <div class="flex items-start gap-3 text-stone-800 dark:text-stone-200">
          <div class="w-1.5 h-1.5 rounded-full bg-stone-400 dark:bg-stone-500 mt-2.5 shrink-0" />
          <span class="leading-relaxed">
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
