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
              <del>
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
                <code class="bg-stone-100 dark:bg-stone-800 px-1 rounded text-sm">
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
              
              // Broken link: render as non-clickable span
              if (isBroken) {
                const displayText = inlines.length > 0 
                  ? inlines.map(il => il.t === "Str" ? il.c : il.t === "Space" ? " " : "").join("")
                  : wikilinkTarget || url;
                return (
                  <span 
                    class="text-stone-400 dark:text-stone-500 cursor-not-allowed"
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
                      ? "text-purple-600 dark:text-purple-400 cursor-pointer hover:underline"
                      : "text-amber-600 dark:text-amber-400 cursor-pointer hover:underline"}
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
                  class="text-amber-600 dark:text-amber-400 hover:underline"
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
                    class="text-amber-600 dark:text-amber-400 cursor-help px-0.5"
                    onMouseEnter={() => setIsOpen(true)}
                    onMouseLeave={() => setIsOpen(false)}
                  >
                    [*]
                  </sup>
                  {isOpen() && (
                    <div class="absolute z-50 bottom-full left-0 mb-1 w-64 p-3 bg-white dark:bg-stone-800 rounded-lg shadow-lg border border-stone-200 dark:border-stone-600 text-sm">
                      <BlockRenderer blocks={i().c} />
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
