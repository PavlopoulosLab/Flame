import { defineConfig, devices } from '@playwright/test'

/**
 * Browser smoke tests of the web app against a running Flame server
 * (start it with `Rscript api/run.R` after `npm run build`).
 *   npm run test:e2e                       # Chromium, Firefox and WebKit
 *   FLAME_URL=https://flame.example.org npm run test:e2e
 *   FLAME_ONLINE=1 npm run test:e2e        # also runs a real enrichment (external services)
 */
export default defineConfig({
  testDir: './e2e',
  timeout: 90_000,
  expect: { timeout: 15_000 },
  fullyParallel: true,
  workers: 3,
  retries: 0,
  reporter: [['list']],
  use: {
    baseURL: process.env.FLAME_URL ?? 'http://127.0.0.1:8000',
    trace: 'retain-on-failure',
  },
  projects: [
    { name: 'chromium', use: { ...devices['Desktop Chrome'] } },
    { name: 'firefox', use: { ...devices['Desktop Firefox'] } },
    { name: 'webkit', use: { ...devices['Desktop Safari'] } },
  ],
})
