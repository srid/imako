import { defineConfig } from "vite";
import { resolve } from "path";
import solid from "vite-plugin-solid";
import tailwindcss from "@tailwindcss/vite";

// Ports configurable via env vars (for e2e tests on different ports)
const vitePort = parseInt(process.env.VITE_PORT || "5173", 10);
const backendPort = parseInt(process.env.BACKEND_PORT || "4009", 10);

export default defineConfig({
  plugins: [solid(), tailwindcss()],
  resolve: {
    alias: {
      "@": resolve(__dirname, "src"),
    },
  },
  server: {
    port: vitePort,
    proxy: {
      "/api": {
        target: `http://localhost:${backendPort}`,
      },
      "/ws": {
        target: `ws://localhost:${backendPort}`,
        ws: true,
      },
    },
  },
});
