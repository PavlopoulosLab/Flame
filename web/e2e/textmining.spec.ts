import { expect, test } from '@playwright/test'

// Two names in the example abstract (NF-kB and p50) resolve to the same protein: the table must list it once,
// so that the number of rows, "Select all" and the number of IDs saved agree.
test('text mining lists each protein once and saves what it shows', async ({ page }) => {
  test.skip(!process.env.FLAME_ONLINE, 'needs the EXTRACT tagger (set FLAME_ONLINE=1)')
  await page.goto('/start?new=1')
  await page.waitForURL(/\/w\/[0-9a-f]{32}/)
  await page.goto(page.url().split('?')[0] + '?tab=input')
  await page.getByRole('button', { name: /From text/ }).first().click()
  const dialog = page.getByRole('dialog').last()
  await dialog.getByRole('button', { name: /Load example/ }).click()
  await dialog.getByRole('button', { name: 'Find genes' }).click()
  const heading = dialog.getByRole('heading', { name: /\d+ proteins?/ })
  await expect(heading).toBeVisible({ timeout: 60_000 })
  const proteins = Number((await heading.innerText()).match(/(\d+) protein/)![1])
  const rows = dialog.locator('tbody tr')
  await expect(rows).toHaveCount(proteins)
  const ids = await rows.locator('td:nth-child(3)').allInnerTexts()
  expect(new Set(ids).size).toBe(ids.length)
  // NF-kB and p50 are the same protein: one row, marked as a duplicate
  await expect(dialog.locator('tbody .tm-dup')).toHaveCount(1)
  await expect(dialog.locator('tbody tr', { has: page.locator('.tm-dup') })).toContainText('NF-kB, p50')
  // columns sort both ways
  await dialog.getByRole('button', { name: /^ID/ }).click()
  const asc = await rows.locator('td:nth-child(3)').allInnerTexts()
  expect(asc).toEqual([...asc].sort())
  await dialog.getByRole('button', { name: /^ID/ }).click()
  const desc = await rows.locator('td:nth-child(3)').allInnerTexts()
  expect(desc).toEqual([...asc].reverse())
  await dialog.getByRole('button', { name: /^Note/ }).click()
  await expect(rows.first().locator('.tm-dup')).toBeVisible()
  await dialog.getByRole('button', { name: 'Select all' }).click()
  await expect(dialog.getByText(`${proteins} IDs will be saved`)).toBeVisible()
})
