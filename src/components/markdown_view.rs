//! Markdown AST renderer — converts Block/Inline AST to Dioxus elements.

use dioxus::prelude::*;
use ob::markdown::{Block, Inline};

/// Render a sequence of blocks.
#[component]
pub fn BlockRenderer(blocks: Vec<Block>) -> Element {
    rsx! {
        for block in &blocks {
            { render_block(block) }
        }
    }
}

fn render_block(block: &Block) -> Element {
    match block {
        Block::Paragraph(inlines) => rsx! {
            p {
                class: "text-stone-800 leading-relaxed mb-5 last:mb-0",
                InlineRenderer { inlines: inlines.clone() }
            }
        },
        Block::Heading { level, content } => {
            let class = match level {
                1 => {
                    "text-3xl font-bold text-stone-900 tracking-tight mt-10 mb-6 pb-2 border-b border-stone-200"
                }
                2 => "text-2xl font-bold text-stone-900 tracking-tight mt-8 mb-4",
                3 => "text-xl font-bold text-stone-900 tracking-tight mt-6 mb-3",
                4 => "text-lg font-bold text-stone-900 tracking-tight mt-5 mb-2",
                _ => "text-base uppercase tracking-wider text-stone-500 mt-5 mb-2 font-semibold",
            };
            match level {
                1 => rsx! { h1 { class: "{class}", InlineRenderer { inlines: content.clone() } } },
                2 => rsx! { h2 { class: "{class}", InlineRenderer { inlines: content.clone() } } },
                3 => rsx! { h3 { class: "{class}", InlineRenderer { inlines: content.clone() } } },
                4 => rsx! { h4 { class: "{class}", InlineRenderer { inlines: content.clone() } } },
                _ => rsx! { h5 { class: "{class}", InlineRenderer { inlines: content.clone() } } },
            }
        }
        Block::BulletList(items) => rsx! {
            div {
                class: "space-y-1.5 mb-5 last:mb-0",
                for item in items {
                    div {
                        class: "flex items-start gap-3 text-stone-800",
                        div { class: "w-1.5 h-1.5 rounded-full bg-stone-400 mt-2.5 shrink-0" }
                        div {
                            class: "flex-1",
                            BlockRenderer { blocks: item.clone() }
                        }
                    }
                }
            }
        },
        Block::OrderedList { start, items } => rsx! {
            ol {
                class: "list-decimal list-outside space-y-1.5 ml-6 mb-5 last:mb-0 text-stone-700",
                start: "{start}",
                for item in items {
                    li {
                        class: "pl-2 marker:text-stone-500",
                        BlockRenderer { blocks: item.clone() }
                    }
                }
            }
        },
        Block::CodeBlock { language, code } => {
            let lang_label = language.as_deref().unwrap_or("");
            rsx! {
                div {
                    class: "my-4 rounded-xl overflow-hidden ring-1 ring-stone-200 bg-stone-50",
                    if !lang_label.is_empty() {
                        div {
                            class: "px-4 py-1.5 text-xs font-mono text-stone-500 bg-stone-100 border-b border-stone-200",
                            "{lang_label}"
                        }
                    }
                    pre {
                        class: "p-4 text-sm font-mono overflow-x-auto text-stone-800",
                        code { "{code}" }
                    }
                }
            }
        }
        Block::BlockQuote(children) => rsx! {
            blockquote {
                class: "border-l-4 border-amber-500 bg-amber-50/50 px-5 py-3 rounded-r-lg my-6 italic text-stone-700 shadow-sm",
                BlockRenderer { blocks: children.clone() }
            }
        },
        Block::ThematicBreak => rsx! {
            hr { class: "border-stone-300 my-4" }
        },
        Block::HtmlBlock(html) => rsx! {
            div { dangerous_inner_html: "{html}" }
        },
        Block::TaskListItem { checked, content } => {
            let icon = if *checked { "☑" } else { "☐" };
            let style = if *checked {
                "line-through text-stone-400"
            } else {
                "text-stone-800"
            };
            rsx! {
                div {
                    class: "flex items-start gap-2 py-1",
                    span { class: "text-stone-400 w-5 h-5 flex-shrink-0", "{icon}" }
                    div {
                        class: "{style}",
                        BlockRenderer { blocks: content.clone() }
                    }
                }
            }
        }
    }
}

/// Render a sequence of inline elements.
#[component]
pub fn InlineRenderer(inlines: Vec<Inline>) -> Element {
    rsx! {
        for inline in &inlines {
            { render_inline(inline) }
        }
    }
}

fn render_inline(inline: &Inline) -> Element {
    match inline {
        Inline::Text(text) => rsx! { "{text}" },
        Inline::SoftBreak => rsx! { " " },
        Inline::LineBreak => rsx! { br {} },
        Inline::Code(code) => rsx! {
            code {
                class: "bg-stone-100 border border-stone-200 px-1.5 py-0.5 rounded-md text-[0.9em] font-medium text-amber-700",
                "{code}"
            }
        },
        Inline::Emph(children) => rsx! {
            em { InlineRenderer { inlines: children.clone() } }
        },
        Inline::Strong(children) => rsx! {
            strong { InlineRenderer { inlines: children.clone() } }
        },
        Inline::Strikethrough(children) => rsx! {
            del {
                class: "text-stone-400 decoration-stone-300",
                InlineRenderer { inlines: children.clone() }
            }
        },
        Inline::Link {
            url,
            title,
            content,
        } => {
            let is_external = url.starts_with("http://") || url.starts_with("https://");
            if is_external {
                rsx! {
                    a {
                        href: "{url}",
                        class: "text-indigo-600 font-medium hover:text-indigo-800 underline decoration-indigo-300/50 hover:decoration-indigo-500 transition-all duration-200 underline-offset-4",
                        target: "_blank",
                        rel: "noopener",
                        title: "{title}",
                        InlineRenderer { inlines: content.clone() }
                    }
                }
            } else {
                rsx! {
                    a {
                        href: "{url}",
                        class: "text-indigo-600 font-medium hover:text-indigo-800 underline decoration-indigo-300/50 hover:decoration-indigo-500 transition-all duration-200 underline-offset-4",
                        title: "{title}",
                        InlineRenderer { inlines: content.clone() }
                    }
                }
            }
        }
        Inline::Image { url, title, alt } => rsx! {
            img {
                src: "{url}",
                alt: "{alt}",
                title: "{title}",
                class: "max-w-full h-auto rounded",
            }
        },
        Inline::HtmlInline(html) => rsx! {
            span { dangerous_inner_html: "{html}" }
        },
    }
}
