import { defineConfig } from "vite";
import { resolve } from "path";
import solid from "vite-plugin-solid";
import tailwindcss from "@tailwindcss/vite";

export default defineConfig({
  plugins: [solid(), tailwindcss()],
  resolve: {
    alias: {
      "@": resolve(__dirname, "src"),
    },
  },
  server: {
    proxy: {
      "/api": {
        target: "http://localhost:4009",
      },
      "/ws": {
        target: "ws://localhost:4009",
        ws: true,
      },
    },
  },
});
