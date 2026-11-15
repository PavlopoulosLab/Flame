/// <reference types="vitest/config" />
import react from '@vitejs/plugin-react'
import { defineConfig } from 'vite'

// Dev: the plumber API runs on :8000 and is proxied under /api.
// Prod: plumber serves web/dist and the API from one origin, so no CORS is needed.
export default defineConfig({
  plugins: [react()],
  server: {
    port: 5173,
    strictPort: true,
    proxy: {
      '/api': { target: 'http://127.0.0.1:8000', changeOrigin: false },
    },
  },
  build: {
    outDir: 'dist',
    sourcemap: false,
    chunkSizeWarningLimit: 4000, // plotly.js is large by design
  },
  test: {
    environment: 'node',
    include: ['src/**/*.test.ts'],
  },
})
