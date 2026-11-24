// Copies PDF.js's runtime data (standard fonts, character maps, image-decoder wasm, ICC
// profiles) next to the app, so PDFs render without loading anything from a CDN.
import { cpSync, existsSync, mkdirSync } from 'node:fs'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'
const here = dirname(fileURLToPath(import.meta.url))
const src = join(here, '..', 'node_modules', 'pdfjs-dist')
const dest = join(here, '..', 'public', 'pdfjs')
mkdirSync(dest, { recursive: true })
for (const d of ['standard_fonts', 'cmaps', 'wasm', 'iccs']) {
  if (existsSync(join(src, d))) cpSync(join(src, d), join(dest, d), { recursive: true })
}
