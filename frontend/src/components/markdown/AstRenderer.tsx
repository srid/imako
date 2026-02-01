/**
 * Main AST renderer - entry point for rendering markdown AST.
 */
import { Component } from "solid-js";
import type { AstNode } from "./types";
import { BlockRenderer } from "./BlockRenderer";

interface Props {
  ast: AstNode;
}

export const AstRenderer: Component<Props> = (props) => {
  return (
    <div class="markdown-content text-stone-700 dark:text-stone-300">
      <BlockRenderer nodes={props.ast.blocks} />
    </div>
  );
};
