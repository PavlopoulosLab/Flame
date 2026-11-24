import { describe, expect, it } from 'vitest'
import { PDFDocument, StandardFonts } from 'pdf-lib'
import { annotatePdf, findMentions, mergeBoxes } from './pdfText'

describe('findMentions', () => {
  const text = 'TP53 and the breast cancer\ngene BRCA1; cancer risk. ATP53X is not TP53.'
  it('prefers longer names, crosses line breaks and keeps word boundaries', () => {
    const m = findMentions(text, ['cancer', 'breast cancer', 'TP53', 'BRCA1'])
    expect(m.map((x) => text.slice(x.start, x.end))).toEqual(['TP53', 'breast cancer', 'BRCA1', 'cancer', 'TP53'])
  })
  it('matches a multi-word name broken over two lines', () => {
    const m = findMentions('the glucocorticoid\nreceptor binds', ['glucocorticoid receptor'])
    expect(m).toHaveLength(1)
  })
})

describe('mergeBoxes', () => {
  it('joins boxes of one line and drops duplicates', () => {
    const b = mergeBoxes([
      { left: 10, top: 100, width: 40, height: 12 }, { left: 10, top: 100, width: 40, height: 12 },
      { left: 54, top: 100, width: 30, height: 12 }, { left: 10, top: 120, width: 20, height: 12 },
    ])
    expect(b).toEqual([{ left: 10, top: 100, width: 74, height: 12 }, { left: 10, top: 120, width: 20, height: 12 }])
  })
})

describe('annotatePdf', () => {
  it('adds highlight annotations with notes and leaves the page content alone', async () => {
    const doc = await PDFDocument.create()
    const page = doc.addPage([300, 200])
    page.drawText('TP53 is mutated', { x: 20, y: 150, size: 12, font: await doc.embedFont(StandardFonts.Helvetica) })
    const bytes = await doc.save()
    const out = await annotatePdf(bytes, new Map([[0, [{ rect: [20, 148, 50, 162], color: [0, 0, 1], text: 'TP53 (Gene)' }]]]))
    const back = await PDFDocument.load(out)
    const annots = back.getPage(0).node.Annots()
    expect(annots?.size()).toBe(1)
    const a = back.context.lookup(annots!.get(0))
    expect(String(a)).toContain('/Subtype /Highlight')
    expect(out.length).toBeGreaterThan(bytes.length)
  })
})
