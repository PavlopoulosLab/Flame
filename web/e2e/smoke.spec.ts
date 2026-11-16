import { expect, test, type Page } from '@playwright/test'

/** Collect uncaught page errors so every test also fails on a crash. */
function watchErrors(page: Page) {
  const errors: string[] = []
  page.on('pageerror', (e) => errors.push(e.message))
  return errors
}

async function newWorkspace(page: Page, query = '') {
  await page.goto(`/start?new=1${query}`)
  await page.waitForURL(/\/w\/[0-9a-f]{32}/)
}

test('home page shows the hero, examples and links', async ({ page }) => {
  const errors = watchErrors(page)
  await page.goto('/')
  await expect(page.getByRole('heading', { level: 1 })).toContainText('Functional Analysis and Integration')
  await expect(page.locator('.featured-example').first()).toBeVisible()
  await page.getByText(/^All \d+ examples$/).click()
  await expect(page.locator('.example-row').first()).toBeVisible()
  expect(await page.locator('.example-row').count()).toBeGreaterThan(25)
  await expect(page.getByRole('link', { name: 'Flame 3.0 home' })).toBeVisible()
  expect(errors).toEqual([])
})

test('add a list by pasting genes', async ({ page }) => {
  const errors = watchErrors(page)
  await newWorkspace(page)
  await page.getByRole('button', { name: /Add list/ }).first().click()
  await page.getByRole('button', { name: /Paste or upload IDs/ }).click()
  await page.getByLabel('IDs', { exact: true }).fill('TP53\nMDM2\nCDKN1A\nBAX\nGADD45A\nFAS\nBBC3\nSESN1')
  await page.getByRole('button', { name: /Save list and continue/ }).click()
  // Saving opens the New run panel; close it and find the list in the rail.
  await page.getByRole('button', { name: 'Close' }).first().click()
  await expect(page.locator('.rail').getByText('8 IDs')).toBeVisible()
  expect(errors).toEqual([])
})

test('workspace sections switch and keep their colours', async ({ page }) => {
  const errors = watchErrors(page)
  await newWorkspace(page)
  await page.getByRole('tab', { name: /Regulators & drugs/ }).click()
  await expect(page.getByRole('heading', { name: 'Regulators, drugs and more' })).toBeVisible()
  await expect(page.locator('.tool-tile')).toHaveCount(8)
  await page.getByRole('tab', { name: /Variants & GWAS/ }).click()
  await expect(page.getByRole('heading', { name: 'Variants and GWAS' })).toBeVisible()
  await page.getByRole('tab', { name: /Gene tools/ }).click()
  await expect(page.getByRole('heading', { name: 'Gene and protein tools' })).toBeVisible()
  expect(errors).toEqual([])
})

test('volcano example: plot, live threshold and its own palette', async ({ page }) => {
  const errors = watchErrors(page)
  await newWorkspace(page, '&example=volcano')
  const dialog = page.getByRole('dialog')
  await dialog.getByRole('button', { name: 'Draw plot' }).click({ timeout: 30_000 })
  await expect(dialog.locator('.js-plotly-plot').first()).toBeVisible({ timeout: 30_000 })
  const palette = dialog.getByLabel('Colours of this visualization')
  await expect(palette).toBeVisible()
  await palette.selectOption('cb')
  await expect(palette).toHaveValue('cb')
  expect(errors).toEqual([])
})

test('help, API and about pages', async ({ page }) => {
  const errors = watchErrors(page)
  await page.goto('/help')
  await expect(page.getByRole('heading', { level: 1 })).toBeVisible()
  await page.goto('/help/regulators')
  await expect(page.getByText('Literature enrichment')).toBeVisible()
  await page.goto('/api-docs')
  await expect(page.getByRole('heading', { name: 'Quick start (curl)' })).toBeVisible()
  await page.goto('/about')
  await expect(page.getByRole('heading', { name: 'Related tools and resources' })).toBeVisible()
  expect(await page.locator('.related-card').count()).toBeGreaterThan(25)
  await expect(page.getByText('Karatzas')).toHaveCount(0)
  expect(errors).toEqual([])
})

test('interactive API reference loads the OpenAPI spec', async ({ page }) => {
  await page.goto('/__docs__/')
  await expect(page.getByText('Flame 3.0 REST API').first()).toBeVisible({ timeout: 30_000 })
  await expect(page.getByRole('button', { name: /Authorize/ })).toBeVisible()
  await expect(page.getByText(/Parser error/)).toHaveCount(0)
})

test('dark theme toggle', async ({ page }) => {
  await page.goto('/')
  await page.getByRole('button', { name: /Switch to (dark|light) theme/ }).click()
  const theme = await page.locator('html').getAttribute('data-theme')
  expect(['dark', 'light']).toContain(theme)
})

test.describe('phone width', () => {
  test.use({ viewport: { width: 375, height: 812 } })
  for (const path of ['/', '/help', '/about']) {
    test(`no horizontal scrolling on ${path}`, async ({ page }) => {
      await page.goto(path)
      await page.waitForLoadState('networkidle')
      const overflow = await page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth)
      expect(overflow).toBeLessThanOrEqual(1)
    })
  }
  test('no horizontal scrolling in a workspace', async ({ page }) => {
    await newWorkspace(page)
    await page.waitForLoadState('networkidle')
    const overflow = await page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth)
    expect(overflow).toBeLessThanOrEqual(1)
  })
})

test.describe('with external services', () => {
  test.skip(!process.env.FLAME_ONLINE, 'set FLAME_ONLINE=1 to run')
  test('worked example: two tools, results table and comparison', async ({ page }) => {
    test.setTimeout(300_000)
    const errors = watchErrors(page)
    await newWorkspace(page, '&example=demo')
    await expect(page.getByText(/All 2 analyses finished/)).toBeVisible({ timeout: 240_000 })
    await page.locator('.run-card').first().getByRole('button', { name: /View .* terms/ }).click()
    await expect(page.locator('.selection')).toBeVisible()
    await expect(page.getByRole('link', { name: /Back to workspace/ })).toBeVisible()
    // Excel export: one workbook, every term with its genes
    await page.getByRole('button', { name: /Export/ }).first().click()
    const [download] = await Promise.all([page.waitForEvent('download'), page.getByRole('button', { name: /All filtered terms \(Excel/ }).click()])
    expect(download.suggestedFilename()).toMatch(/\.xlsx$/)
    expect(errors).toEqual([])
  })
})
