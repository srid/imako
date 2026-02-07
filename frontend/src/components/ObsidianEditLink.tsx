import { Component } from "solid-js";
import { obsidianOpenUrl } from "@/utils/obsidian";
import { Icons } from "@/utils/icons";
import { vaultInfo } from "@/store";

/**
 * A link that opens a file in Obsidian without reloading the SPA.
 * Uses window.open with the obsidian:// protocol handler.
 */
export const ObsidianEditLink: Component<{ filePath: string; class?: string }> = (props) => {
  const handleClick = (e: MouseEvent) => {
    e.preventDefault();
    e.stopPropagation();
    window.open(obsidianOpenUrl(vaultInfo.vaultName, props.filePath));
  };

  return (
    <a
      href={obsidianOpenUrl(vaultInfo.vaultName, props.filePath)}
      class={props.class}
      title="Open in Obsidian"
      onClick={handleClick}
    >
      {Icons.edit}
    </a>
  );
};
