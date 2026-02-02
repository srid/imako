/**
 * Main AST renderer - entry point for rendering Pandoc AST.
 */
import { Component } from "solid-js";
import type { Pandoc } from "./types";
import { BlockRenderer } from "./BlockRenderer";

interface Props {
  ast: Pandoc;
}

export const AstRenderer: Component<Props> = (props) => {
  return (
    <div class="markdown-content text-stone-700 dark:text-stone-300">
      <BlockRenderer blocks={props.ast.blocks} />
    </div>
  );
};
