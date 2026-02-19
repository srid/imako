/**
 * Inline node renderer - handles text, emphasis, links, etc.
 * Uses Pandoc's native Inline type with tag discriminator.
 */
import { Component, For, Switch, Match, createSignal } from "solid-js";
import { useNavigate } from "@solidjs/router";
import katex from "katex";
import "katex/dist/katex.min.css";
import type { Inline } from "./types";
import { BlockRenderer } from "./BlockRenderer";

export const InlineRenderer: Component<{ inlines: Inline[] }> = (props) => {
  const navigate = useNavigate();
  return (
    <For each={props.inlines}>
      {(inline) => (
        <Switch fallback={null}>
          {/* Str - plain string */}
          <Match when={inline.t === "Str" && inline}>
            {(i) => <>{i().c}</>}
          </Match>

          {/* Space */}
          <Match when={inline.t === "Space"}>
            {" "}
          </Match>

          {/* SoftBreak - treated as space */}
          <Match when={inline.t === "SoftBreak"}>
            {" "}
          </Match>

          {/* LineBreak */}
          <Match when={inline.t === "LineBreak"}>
            <br />
          </Match>

          {/* Emph - italic */}
          <Match when={inline.t === "Emph" && inline}>
            {(i) => (
              <em>
                <InlineRenderer inlines={i().c} />
              </em>
            )}
          </Match>

          {/* Strong - bold */}
          <Match when={inline.t === "Strong" && inline}>
            {(i) => (
              <strong>
                <InlineRenderer inlines={i().c} />
              </strong>
            )}
          </Match>

          {/* Underline */}
          <Match when={inline.t === "Underline" && inline}>
            {(i) => (
              <u>
                <InlineRenderer inlines={i().c} />
              </u>
            )}
          </Match>

          {/* Strikeout */}
          <Match when={inline.t === "Strikeout" && inline}>
            {(i) => (
              <del class="text-stone-400 dark:text-stone-500 decoration-stone-300 dark:decoration-stone-600">
                <InlineRenderer inlines={i().c} />
              </del>
            )}
          </Match>

          {/* Superscript */}
          <Match when={inline.t === "Superscript" && inline}>
            {(i) => (
              <sup>
                <InlineRenderer inlines={i().c} />
              </sup>
            )}
          </Match>

          {/* Subscript */}
          <Match when={inline.t === "Subscript" && inline}>
            {(i) => (
              <sub>
                <InlineRenderer inlines={i().c} />
              </sub>
            )}
          </Match>

          {/* SmallCaps */}
          <Match when={inline.t === "SmallCaps" && inline}>
            {(i) => (
              <span style={{ "font-variant": "small-caps" }}>
                <InlineRenderer inlines={i().c} />
              </span>
            )}
          </Match>

          {/* Code - [Attr, string] */}
          <Match when={inline.t === "Code" && inline}>
            {(i) => {
              const [_attr, code] = i().c;
              return (
                <code class="bg-stone-100 dark:bg-stone-800/80 border border-stone-200 dark:border-stone-700/50 px-1.5 py-0.5 rounded-md text-[0.9em] font-medium text-amber-700 dark:text-amber-400">
                  {code}
                </code>
              );
            }}
          </Match>

          {/* Link - [Attr, Inline[], Target] where Target = [url, title] */}
          <Match when={inline.t === "Link" && inline}>
            {(i) => {
              const [[_id, _classes, kvs], inlines, [url, _title]] = i().c;
              
              // Check for broken link
              const isBroken = kvs.some(([k, _v]) => k === "data-broken");
              
              // Check for wikilink (for styling)
              const isWikilink = kvs.some(([k, _v]) => k === "data-wikilink");
              const wikilinkTarget = kvs.find(([k]) => k === "data-wikilink")?.[1];
              
              // Internal link (starts with /p/)
              const isInternal = url.startsWith("/p/");
              
              // Shared transition classes
              const transitionClasses = "transition-all duration-200 underline-offset-4";
              
              // Broken link: render as non-clickable span
              if (isBroken) {
                const displayText = inlines.length > 0 
                  ? inlines.map(il => il.t === "Str" ? il.c : il.t === "Space" ? " " : "").join("")
                  : wikilinkTarget || url;
                return (
                  <span 
                    class={`text-stone-400 dark:text-stone-500 cursor-not-allowed border-b border-dotted border-stone-300 dark:border-stone-600 px-0.5 ${transitionClasses}`}
                    title={`Broken link: ${wikilinkTarget || url}`}
                    data-wikilink={wikilinkTarget}
                    data-broken="true"
                  >
                    {displayText}
                  </span>
                );
              }
              
              // Internal link: use navigate()
              if (isInternal) {
                const handleClick = () => navigate(url);
                const displayText = inlines.length > 0 
                  ? inlines.map(il => il.t === "Str" ? il.c : il.t === "Space" ? " " : "").join("")
                  : wikilinkTarget || decodeURIComponent(url.slice(3)); // Remove /p/ prefix
                return (
                  <span 
                    class={isWikilink 
                      ? `text-violet-600 dark:text-violet-400 font-medium hover:text-violet-800 dark:hover:text-violet-300 underline decoration-violet-300/50 hover:decoration-violet-500 decoration-dashed cursor-pointer ${transitionClasses}`
                      : `text-indigo-600 dark:text-indigo-400 font-medium hover:text-indigo-800 dark:hover:text-indigo-300 underline decoration-indigo-300/50 hover:decoration-indigo-500 cursor-pointer ${transitionClasses}`}
                    onClick={handleClick}
                    {...(isWikilink ? { "data-wikilink": wikilinkTarget } : {})}
                  >
                    {displayText}
                  </span>
                );
              }
              
              // External link
              return (
                <a
                  href={url}
                  class={`text-indigo-600 dark:text-indigo-400 font-medium hover:text-indigo-800 dark:hover:text-indigo-300 underline decoration-indigo-300/50 hover:decoration-indigo-500 ${transitionClasses}`}
                  target="_blank"
                  rel="noopener"
                >
                  <InlineRenderer inlines={inlines} />
                </a>
              );
            }}
          </Match>

          {/* Image - [Attr, Inline[], Target] */}
          <Match when={inline.t === "Image" && inline}>
            {(i) => {
              const [_attr, alt, [src, title]] = i().c;
              // Extract alt text from inlines
              const altText = alt.map(a => a.t === "Str" ? a.c : "").join("");
              return (
                <img
                  src={src}
                  alt={altText}
                  title={title || undefined}
                  class="max-w-full h-auto rounded"
                />
              );
            }}
          </Match>

          {/* Quoted - [QuoteType, Inline[]] */}
          <Match when={inline.t === "Quoted" && inline}>
            {(i) => {
              const [quoteType, inlines] = i().c;
              const quote = quoteType === "SingleQuote" ? "'" : '"';
              return (
                <>
                  {quote}<InlineRenderer inlines={inlines} />{quote}
                </>
              );
            }}
          </Match>

          {/* Math - [MathType, string] - rendered with KaTeX */}
          <Match when={inline.t === "Math" && inline}>
            {(i) => {
              const [mathType, tex] = i().c;
              const isDisplay = mathType === "DisplayMath";
              try {
                const html = katex.renderToString(tex, {
                  displayMode: isDisplay,
                  throwOnError: false,
                });
                return (
                  <span 
                    class={isDisplay ? "block text-center my-2" : "inline"}
                    innerHTML={html}
                  />
                );
              } catch {
                // Fallback to code block on error
                return (
                  <code class={isDisplay ? "block text-center" : ""}>
                    {tex}
                  </code>
                );
              }
            }}
          </Match>

          {/* RawInline - [Format, string] */}
          <Match when={inline.t === "RawInline" && inline}>
            {(i) => {
              const [format, text] = i().c;
              // For HTML, render directly (dangerous but matches Pandoc behavior)
              if (format === "html") {
                return <span innerHTML={text} />;
              }
              return <code>{text}</code>;
            }}
          </Match>

          {/* Note - Block[] (footnote) - show as popup on hover */}
          <Match when={inline.t === "Note" && inline}>
            {(i) => {
              const [isOpen, setIsOpen] = createSignal(false);
              return (
                <span class="relative inline-block">
                  <sup 
                    class="text-violet-600 dark:text-violet-400 font-medium cursor-help px-0.5 hover:text-violet-800 hover:dark:text-violet-300 transition-colors"
                    onMouseEnter={() => setIsOpen(true)}
                    onMouseLeave={() => setIsOpen(false)}
                  >
                    [*]
                  </sup>
                  {isOpen() && (
                    <div class="absolute z-50 bottom-full left-1/2 -translate-x-1/2 mb-2 w-72 p-4 bg-white dark:bg-stone-900 rounded-xl shadow-xl shadow-stone-200/50 dark:shadow-black/50 ring-1 ring-stone-200 dark:ring-stone-800 text-sm animate-in fade-in slide-in-from-bottom-2 duration-200">
                      <div class="prose prose-sm dark:prose-invert">
                        <BlockRenderer blocks={i().c} />
                      </div>
                      <div class="absolute -bottom-2 left-1/2 -translate-x-1/2 w-4 h-4 bg-white dark:bg-stone-900 ring-1 ring-stone-200 dark:ring-stone-800 rotate-45 border-t-0 border-l-0" />
                    </div>
                  )}
                </span>
              );
            }}
          </Match>

          {/* Span - [Attr, Inline[]] - check for wikilink class */}
          <Match when={inline.t === "Span" && inline}>
            {(i) => {
              const [[_id, classes, _kvs], inlines] = i().c;
              
              // Check for wikilink
              if (classes.includes("wikilink")) {
                const target = inlines.map(il => il.t === "Str" ? il.c : "").join("");
                return (
                  <span 
                    class="text-purple-600 dark:text-purple-400 cursor-pointer hover:underline"
                    title={`Link to: ${target}`}
                  >
                    {target}
                  </span>
                );
              }
              
              return <InlineRenderer inlines={inlines} />;
            }}
          </Match>

          {/* Cite - [Citation[], Inline[]] */}
          <Match when={inline.t === "Cite" && inline}>
            {(i) => {
              const [_citations, inlines] = i().c;
              return <InlineRenderer inlines={inlines} />;
            }}
          </Match>
        </Switch>
      )}
    </For>
  );
};
