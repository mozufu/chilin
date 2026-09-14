import tailwindcss from "@tailwindcss/vite";
import react from "@vitejs/plugin-react";
import { defineConfig } from "vite";

// The reverse proxy serves the built assets and chilin from one origin, so the
// client always calls same-origin /api. Development reproduces that with a
// proxy rather than absolute URLs, keeping cookies and CSRF assumptions intact.
export default defineConfig({
  plugins: [react(), tailwindcss()],
  build: { outDir: "dist", sourcemap: true },
  server: {
    proxy: {
      "/api": { target: process.env.CHILIN_ORIGIN ?? "http://127.0.0.1:8080", changeOrigin: false },
      "/health": { target: process.env.CHILIN_ORIGIN ?? "http://127.0.0.1:8080", changeOrigin: false },
    },
  },
});
