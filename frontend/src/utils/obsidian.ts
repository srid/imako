/**
 * Generate an obsidian:// URL to open a file in Obsidian
 * See: https://help.obsidian.md/Concepts/Obsidian+URI
 */
export const obsidianOpenUrl = (vaultName: string, relativePath: string): string => {
  const encodedVault = encodeURIComponent(vaultName);
  const encodedPath = encodeURIComponent(relativePath);
  return `obsidian://open?vault=${encodedVault}&file=${encodedPath}`;
};
