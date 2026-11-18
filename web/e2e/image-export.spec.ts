import { readFileSync } from 'node:fs'
import { expect, test, type Locator, type Page } from '@playwright/test'

/**
 * Image export ("⤓ Image" on every figure): each figure is saved in every format and the
 * files are checked (format, pixel size, not blank). Figures come from the built-in examples.
 *   npm run test:e2e -- image-export            # figures that need no external services
 *   FLAME_ONLINE=1 npm run test:e2e -- image-export
 */

const FORMATS = ['png', 'svg', 'pdf', 'jpeg', 'webp'] as const
type Format = typeof FORMATS[number]
const OUT = process.env.FLAME_EXPORT_OUT // optional folder to keep the files for a look

function watchErrors(page: Page) {
  const errors: string[] = []
  page.on('pageerror', (e) => errors.push(e.message))
  return errors
}

function checkMagic(buf: Buffer, format: Format) {
  const head = buf.subarray(0, 16)
  if (format === 'png') expect(head.subarray(0, 8).toString('hex')).toBe('89504e470d0a1a0a')
  if (format === 'jpeg') expect(head.subarray(0, 3).toString('hex')).toBe('ffd8ff')
  if (format === 'webp') { expect(head.subarray(0, 4).toString()).toBe('RIFF'); expect(head.subarray(8, 12).toString()).toBe('WEBP') }
  if (format === 'pdf') { expect(head.subarray(0, 5).toString()).toBe('%PDF-'); expect(buf.subarray(-6).toString()).toContain('%%EOF') }
  if (format === 'svg') expect(buf.toString('utf8', 0, 400)).toMatch(/<svg[\s>]/)
}

/** Share of pixels that differ from the top-left pixel (0 = blank image). */
async function inkShare(page: Page, buf: Buffer, mime: string) {
  return page.evaluate(async ({ b64, mime }) => {
    const img = new Image()
    img.src = `data:${mime};base64,${b64}`
    await img.decode()
    const c = document.createElement('canvas')
    const w = Math.min(img.width, 800), h = Math.round((img.height * w) / img.width)
    c.width = w; c.height = h
    const ctx = c.getContext('2d')!
    ctx.drawImage(img, 0, 0, w, h)
    const d = ctx.getImageData(0, 0, w, h).data
    let diff = 0
    for (let i = 0; i < d.length; i += 4) if (Math.abs(d[i] - d[0]) + Math.abs(d[i + 1] - d[1]) + Math.abs(d[i + 2] - d[2]) + Math.abs(d[i + 3] - d[3]) > 30) diff++
    return { share: diff / (w * h), width: img.width, height: img.height, alpha0: d[3] }
  }, { b64: buf.toString('base64'), mime })
}

interface Choice { format: Format; scale?: number; background?: 'theme' | 'white' | 'transparent'; area?: 'all' | 'view'; legend?: boolean; plain?: boolean }

/** Open the dialog with `open`, pick the options, save, and check the file. */
async function exportOnce(page: Page, open: () => Promise<void>, label: string, c: Choice) {
  await open()
  const dlg = page.getByTestId('export-image')
  await expect(dlg).toBeVisible()
  await dlg.locator(`input[name="export-format"][value="${c.format}"]`).check()
  if (c.scale) await dlg.locator('#export-scale').selectOption(String(c.scale))
  if (c.background) await dlg.locator('#export-bg').selectOption(c.background)
  if (c.area) await dlg.locator('#export-area').selectOption(c.area)
  const legend = dlg.getByLabel('Include the legend and colour bar')
  if (c.legend !== undefined && await legend.count()) await legend.setChecked(c.legend)
  const plain = dlg.getByLabel('Leave out selection and search highlighting')
  if (c.plain !== undefined && await plain.count()) await plain.setChecked(c.plain)
  const sizeText = await dlg.locator('.export-size').innerText()
  const [download] = await Promise.all([
    page.waitForEvent('download', { timeout: 60_000 }).catch(async (e) => {
      const msg = await dlg.locator('[role="alert"]').innerText().catch(() => '')
      throw new Error(`${label} ${c.format}: no file was saved${msg ? ` — dialog says: ${msg}` : ''} (${e.message})`)
    }),
    page.getByRole('button', { name: 'Save image', exact: true }).click(),
  ])
  await expect(dlg).toBeHidden()
  const buf = readFileSync((await download.path())!)
  checkMagic(buf, c.format)
  expect(buf.length, `${label} ${c.format}: file too small`).toBeGreaterThan(c.format === 'svg' ? 300 : 1500)
  if (OUT) await download.saveAs(`${OUT}/${label.replace(/[^\w-]+/g, '_')}.${download.suggestedFilename().split('.').pop()}`)
  if (c.format === 'png' || c.format === 'jpeg' || c.format === 'webp') {
    const ink = await inkShare(page, buf, `image/${c.format}`)
    const m = sizeText.match(/^([\d,]+) × ([\d,]+) px/)
    if (m) {
      expect(ink.width, `${label}: width`).toBe(Number(m[1].replace(/,/g, '')))
      expect(ink.height, `${label}: height`).toBe(Number(m[2].replace(/,/g, '')))
    }
    expect(ink.share, `${label} ${c.format}: image looks blank`).toBeGreaterThan(0.004)
    return ink
  }
  return null
}

/** Export every figure (visible "⤓ Image" button) on the page in every format. */
async function exportAllFigures(page: Page, label: string, minFigures = 1, wait = 60_000) {
  const buttons = page.locator('.export-image-btn:visible')
  await expect(buttons.first()).toBeVisible({ timeout: wait })
  await page.waitForTimeout(800) // let charts and layouts settle
  const n = await buttons.count()
  expect(n, `${label}: figures with an image button`).toBeGreaterThanOrEqual(minFigures)
  for (let i = 0; i < n; i++) {
    const btn: Locator = buttons.nth(i)
    for (const format of FORMATS) await exportOnce(page, () => btn.click(), `${label}#${i}`, { format })
  }
  return n
}

async function newWorkspace(page: Page, example: string) {
  await page.goto(`/start?new=1&example=${example}`)
  await page.waitForURL(/\/w\/[0-9a-f]{32}/)
}

test.describe('with external services', () => {
  test.skip(!process.env.FLAME_ONLINE, 'set FLAME_ONLINE=1 to run')

  test('results and comparison views', async ({ page }) => {
    test.setTimeout(900_000)
    const errors = watchErrors(page)
    await newWorkspace(page, 'demo')
    await expect(page.getByText(/All 2 analyses finished/)).toBeVisible({ timeout: 240_000 })
    const ws = page.url().match(/\/w\/[0-9a-f]{32}/)![0]
    await page.locator('.run-card').first().getByRole('button', { name: /View .* terms/ }).click()
    const base = page.url().replace(/(\/runs\/[^/]+)(\/.*)?$/, '$1')
    const tabs = ['dotplot', 'barchart', 'scatter', 'heatmap-tg', 'heatmap-tt', 'heatmap-gg', 'network-tg', 'network-tt', 'network-gg', 'sankey', 'chord', 'hive', 'circos', 'themes']
    for (const tab of tabs) {
      await page.goto(`${base}/${tab}`)
      await exportAllFigures(page, `results-${tab}`)
    }

    // The toolbar's Export menu opens the same dialog.
    await page.goto(`${base}/dotplot`)
    await expect(page.locator('.export-image-btn:visible').first()).toBeVisible()
    await exportOnce(page, async () => {
      await page.getByRole('button', { name: /^Export/ }).click()
      await page.getByRole('button', { name: /^Image \(PNG/ }).click()
    }, 'results-menu', { format: 'png' })

    // Comparison of the two runs.
    await page.goto(`${ws}`)
    const ids = await page.evaluate(async (w) => {
      const r = await fetch('/api/workspace', { headers: { 'X-Flame-Workspace': w.slice(3), Accept: 'application/json' } }).then((x) => x.json())
      return (r.runs ?? []).map((x: { id: string }) => x.id)
    }, ws)
    expect(ids.length).toBe(2)
    await page.goto(`${ws}/compare?runs=${ids.join(',')}`)
    for (const v of ['Ranks', 'Themes', 'Overview', 'Statistics', 'Overlap (UpSet)', 'Chord (shared terms)', 'Circos (bundled)', 'Bubble plot', 'Heatmap']) {
      await page.locator('.view-tab button', { hasText: v }).click()
      await exportAllFigures(page, `compare-${v}`)
    }
    expect(errors).toEqual([])
  })
})

test('input previews (volcano, UMAP, expression, UpSet) and export options', async ({ page }) => {
  test.setTimeout(600_000)
  const errors = watchErrors(page)

  await newWorkspace(page, 'volcano')
  await page.getByRole('dialog').getByRole('button', { name: 'Draw plot' }).click({ timeout: 30_000 })
  await exportAllFigures(page, 'input-volcano')
  const volcano = page.locator('.modal .export-image-btn:visible').first()

  // Options: 4× size, transparent background, white background in the dark theme.
  const big = await exportOnce(page, () => volcano.click(), 'opt-4x', { format: 'png', scale: 4 })
  expect(big!.width).toBeGreaterThan(1500)
  const clear = await exportOnce(page, () => volcano.click(), 'opt-transparent', { format: 'png', scale: 1, background: 'transparent' })
  expect(clear!.alpha0, 'transparent background').toBe(0)
  const solid = await exportOnce(page, () => volcano.click(), 'opt-theme', { format: 'png', scale: 1, background: 'theme' })
  expect(solid!.alpha0, 'theme background').toBe(255)
  // JPEG and PDF have no transparency: the choice is disabled.
  await volcano.click()
  const d0 = page.getByTestId('export-image')
  await d0.locator('input[name="export-format"][value="jpeg"]').check()
  await expect(d0.locator('#export-bg option[value="transparent"]')).toBeDisabled()
  await page.getByRole('dialog', { name: 'Export image' }).locator('.modal-footer').getByRole('button', { name: 'Close' }).click()
  await expect(d0).toBeHidden()

  // Selection highlighting: select points, then export with and without it.
  await page.getByRole('dialog').getByRole('button', { name: /^Up \(\d+\)/ }).click()
  await page.waitForTimeout(600)
  const svgText = async (plain: boolean) => {
    await volcano.click()
    const dlg = page.getByTestId('export-image')
    await dlg.locator('input[name="export-format"][value="svg"]').check()
    await dlg.getByLabel('Leave out selection and search highlighting').setChecked(plain)
    const [d] = await Promise.all([page.waitForEvent('download'), page.getByRole('button', { name: 'Save image', exact: true }).click()])
    const text = readFileSync((await d.path())!).toString()
    if (OUT) await d.saveAs(`${OUT}/opt-plain-${plain}.svg`)
    return text
  }
  const outline = /stroke-width: 2px/g // selected points get a 2px outline
  expect((await svgText(false)).match(outline)?.length ?? 0, 'selection drawn').toBeGreaterThan(0)
  expect((await svgText(true)).match(outline)?.length ?? 0, 'selection left out').toBe(0)

  // Dark theme + white background: text is drawn dark.
  await page.keyboard.press('Escape')
  await newWorkspace(page, 'reduction')
  await expect(page.getByRole('dialog').getByRole('button', { name: 'Draw plot' })).toBeVisible({ timeout: 30_000 })
  await page.evaluate(() => document.documentElement.setAttribute('data-theme', 'dark')) // then the chart is drawn dark
  await page.getByRole('dialog').getByRole('button', { name: 'Draw plot' }).click()
  await exportAllFigures(page, 'input-umap')
  await page.locator('.modal .export-image-btn:visible').first().click()
  const dlg = page.getByTestId('export-image')
  await dlg.locator('input[name="export-format"][value="svg"]').check()
  await dlg.locator('#export-bg').selectOption('white')
  const [d] = await Promise.all([page.waitForEvent('download'), page.getByRole('button', { name: 'Save image', exact: true }).click()])
  const svg = readFileSync((await d.path())!).toString()
  expect(svg).toContain('fill: rgb(255, 255, 255)')
  // Text is drawn in light-theme (dark) colours, not the dark theme's light grey.
  const textFills = [...svg.matchAll(/font-size: \d+px; fill: rgb\((\d+), (\d+), (\d+)\)/g)].map((m) => Math.max(+m[1], +m[2], +m[3]))
  expect(textFills.length).toBeGreaterThan(5)
  expect(Math.max(...textFills), 'text should be dark on a white background').toBeLessThan(110)

  await newWorkspace(page, 'expression')
  await exportAllFigures(page, 'input-expression-clusters')
  await page.getByRole('radio', { name: 'Gene map (PCA)' }).click()
  await exportAllFigures(page, 'input-expression-map')

  await newWorkspace(page, 'upset')
  const boxes = page.getByRole('dialog').locator('input[type="checkbox"]')
  await expect(boxes.nth(1)).toBeVisible({ timeout: 30_000 })
  if (!(await page.locator('.upset-row').count())) { await boxes.nth(0).check(); await boxes.nth(1).check() }
  await exportAllFigures(page, 'input-upset')
  expect(errors).toEqual([])
})

test.describe('with external services: other pages', () => {
  test.skip(!process.env.FLAME_ONLINE, 'set FLAME_ONLINE=1 to run')

  test('variants, GWAS, STRING network, analyses, microRNAs', async ({ page }) => {
    test.setTimeout(1_500_000)
    const errors = watchErrors(page)

    for (const kind of ['gwas', 'variants']) {
      await newWorkspace(page, kind)
      await page.getByRole('button', { name: /^Annotate \d/ }).click()
      await exportAllFigures(page, `variants-${kind}`, 1, 400_000)
    }

    // STRING network (vis-network): whole network and the current view.
    await newWorkspace(page, 'ppi')
    await page.getByRole('dialog').getByRole('button', { name: 'Build network' }).click({ timeout: 60_000 })
    await exportAllFigures(page, 'string-network', 1, 300_000)
    const netBtn = page.locator('.modal .export-image-btn:visible').first()
    await exportOnce(page, () => netBtn.click(), 'string-view', { format: 'png', area: 'view', scale: 3 })
    await exportOnce(page, () => netBtn.click(), 'string-view-svg', { format: 'svg', area: 'view' })
    await exportOnce(page, async () => { await page.getByRole('button', { name: 'Save image…' }).click() }, 'string-button', { format: 'png' })

    for (const kind of ['tf', 'drugs', 'modules', 'robustness']) {
      await newWorkspace(page, kind)
      await expect(page.locator('.export-image-btn:visible').first()).toBeVisible({ timeout: 600_000 })
      await exportAllFigures(page, `analysis-${kind}`)
    }

    await newWorkspace(page, 'mirna')
    await page.getByRole('button', { name: 'Find target genes' }).click({ timeout: 60_000 })
    await exportAllFigures(page, 'input-mirna', 1, 300_000)
    expect(errors).toEqual([])
  })
})
