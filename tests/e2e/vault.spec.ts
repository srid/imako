import { test, expect } from "@playwright/test";
import { App } from "./dsl";

test.describe("Sidebar Navigation", () => {
  test("renders folder tree with all folders", async ({ page }) => {
    const app = new App(page);
    await app.goto();

    const folders = await app.sidebarFolderNames();
    expect(folders).toContain("Daily");
    expect(folders).toContain("Notes");
    expect(folders).toContain("Projects");
  });

  test("renders files under folders", async ({ page }) => {
    const app = new App(page);
    await app.goto();

    await expect(app.sidebar().getByText("Welcome.md")).toBeVisible();
    await expect(app.sidebar().getByText("Tasks.md")).toBeVisible();
  });

  test("clicking a file navigates and shows content", async ({ page }) => {
    const app = new App(page);
    await app.goto();

    await app.clickFile("Welcome.md");

    expect(app.currentPath()).toBe("/p/Notes/Welcome.md");
    await app.expectHeading("Welcome to Imako");
  });

  test("switching between files updates content", async ({ page }) => {
    const app = new App(page);
    await app.goto();

    await app.clickFile("Welcome.md");
    await app.expectHeading("Welcome to Imako");

    await app.clickFile("Tasks.md");
    expect(app.currentPath()).toBe("/p/Notes/Tasks.md");
    await app.expectHeading("Tasks");
  });
});

test.describe("Note Rendering", () => {
  test("renders headings", async ({ page }) => {
    const app = new App(page);
    await app.gotoNote("Notes/Welcome.md");

    await app.expectHeading("Welcome to Imako");
    await app.expectHeading("About Imako");
    await app.expectHeading("Features");
  });

  test("renders lists", async ({ page }) => {
    const app = new App(page);
    await app.gotoNote("Notes/Welcome.md");

    await app.expectText("Task management with status tracking");
    await app.expectText("Wikilink navigation");
    await app.expectText("Live file sync");
  });

  test("renders code blocks", async ({ page }) => {
    const app = new App(page);
    await app.gotoNote("Notes/Welcome.md");

    await app.expectElement("pre");
    await app.expectText("putStrLn");
  });

  test("renders blockquotes", async ({ page }) => {
    const app = new App(page);
    await app.gotoNote("Notes/Welcome.md");

    await app.expectElement("blockquote");
  });

  test("renders emphasis and strong", async ({ page }) => {
    const app = new App(page);
    await app.gotoNote("Notes/Welcome.md");

    await app.expectElement("strong");
    await app.expectElement("em");
  });
});

test.describe("Root and Folder Views", () => {
  test("root view shows vault structure", async ({ page }) => {
    const app = new App(page);
    await app.goto();

    await expect(app.content().getByText("Daily")).toBeVisible();
    await expect(app.content().getByText("Notes")).toBeVisible();
    await expect(app.content().getByText("Projects")).toBeVisible();
  });

  test("CSS loads correctly on nested routes", async ({ page }) => {
    const app = new App(page);
    await app.gotoNote("Daily/2026-02-17.md");

    // Verify Tailwind CSS class is applied to sidebar
    const hasTailwind = await app.sidebar().evaluate((el) => {
      return el.classList.contains("bg-stone-50") ||
        window.getComputedStyle(el).backgroundColor !== "rgba(0, 0, 0, 0)";
    });
    expect(hasTailwind).toBe(true);
  });
});
