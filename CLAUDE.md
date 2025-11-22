
# CRITICAL SETUP STEPS

1. **MUST** acquire the Haskell skill before doing ANY Haskell work
1. **MUST** acquire the Nix skill before doing ANY Nix work
1. If skill load fails, STOP and report the error - don't proceed
1. Read README.md Architecture section (it is a Haskell app)

# UX Guidelines

UX should be simple & direct for uses of Obsidian, who are already familiar with taking notes in Markdown files, as well as the use of plugins like obsidian-tasks (tasks defined in Markdown).

## Dark Mode

The UI supports dark mode via Tailwind CSS. It automatically follows the system preference (using `prefers-color-scheme` media query). All UI components have both light and dark variants defined.
