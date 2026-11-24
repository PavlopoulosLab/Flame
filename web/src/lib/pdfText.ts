/** Finding tagged entity names in the text of a rendered PDF page, and writing highlights back into the PDF. */

export interface Mention { start: number; end: number; name: string }

const escapeRe = (s: string) => s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')

/**
 * Occurrences of the names in a page's text. Longer names win over the shorter names they contain
 * ("breast cancer" over "cancer"); matches do not overlap; whitespace inside a name may be any run
 * of spaces or line breaks; a name must not be glued to letters or digits on either side.
 */
const patterns = new Map<string, RegExp>()   // compiled once per name, reused on every page

export function findMentions(text: string, names: string[]): Mention[] {
  const uniq = [...new Set(names.filter((n) => n && n.trim().length > 1))].sort((a, b) => b.length - a.length)
  const taken = new Uint8Array(text.length)
  const out: Mention[] = []
  for (const name of uniq) {
    const words = name.trim().split(/\s+/)
    if (!text.includes(words[0])) continue          // cheap check first: most names are not on a given page
    let re = patterns.get(name)
    if (!re) {
      re = new RegExp(`(?<![\\p{L}\\p{N}])${words.map(escapeRe).join('\\s+')}(?![\\p{L}\\p{N}])`, 'gu')
      if (patterns.size > 20_000) patterns.clear()
      patterns.set(name, re)
    }
    re.lastIndex = 0
    for (const m of text.matchAll(re)) {
      const start = m.index ?? 0, end = start + m[0].length
      let free = true
      for (let i = start; i < end; i++) if (taken[i]) { free = false; break }
      if (!free) continue
      taken.fill(1, start, end)
      out.push({ start, end, name })
    }
  }
  return out.sort((a, b) => a.start - b.start)
}

/** Text nodes of a PDF.js text layer in reading order, with the page string they make (<br> = newline). */
export function textLayerString(root: Node): { text: string; nodes: { node: Text; start: number }[] } {
  const nodes: { node: Text; start: number }[] = []
  let text = ''
  const walk = (n: Node) => {
    for (const c of Array.from(n.childNodes)) {
      if (c.nodeType === 3) { nodes.push({ node: c as Text, start: text.length }); text += (c as Text).data }
      else if ((c as Element).tagName === 'BR') text += '\n'
      else walk(c)
    }
  }
  walk(root)
  return { text, nodes }
}

/** The text node and offset for a position in the page string. */
export function locate(nodes: { node: Text; start: number }[], pos: number, atEnd = false): { node: Text; offset: number } | null {
  for (let i = 0; i < nodes.length; i++) {
    const n = nodes[i], len = n.node.data.length
    if (pos < n.start) continue
    if (pos < n.start + len || (atEnd && pos === n.start + len)) return { node: n.node, offset: pos - n.start }
  }
  return null
}

export interface Box { left: number; top: number; width: number; height: number }

/**
 * Tidy the boxes of one mention: Range.getClientRects() can return a box per element and per text
 * node (duplicates), and one per word in justified text. Boxes on the same line that touch or nearly
 * touch become one; boxes inside another are dropped.
 */
export function mergeBoxes(boxes: Box[], gap = 8): Box[] {
  const sorted = [...boxes].sort((a, b) => a.top - b.top || a.left - b.left)
  const out: Box[] = []
  for (const b of sorted) {
    const last = out[out.length - 1]
    const sameLine = last && Math.abs((last.top + last.height / 2) - (b.top + b.height / 2)) < Math.max(last.height, b.height) / 2
    if (last && sameLine && b.left <= last.left + last.width + gap) {
      const right = Math.max(last.left + last.width, b.left + b.width), bottom = Math.max(last.top + last.height, b.top + b.height)
      last.left = Math.min(last.left, b.left); last.top = Math.min(last.top, b.top)
      last.width = right - last.left; last.height = bottom - last.top
    } else out.push({ ...b })
  }
  return out
}

export type RGB = [number, number, number]
export interface PdfMark { rect: [number, number, number, number]; color: RGB; text: string }

/**
 * Write highlights into a copy of the PDF as standard highlight annotations (with an appearance
 * stream, so every reader shows them) whose popup note names the entity and its database ID.
 * The page content itself is not changed.
 */
export async function annotatePdf(bytes: Uint8Array, marksByPage: Map<number, PdfMark[]>, author = 'Flame (EXTRACT)'): Promise<Uint8Array> {
  const { PDFDocument, PDFHexString } = await import('pdf-lib')
  const doc = await PDFDocument.load(bytes, { ignoreEncryption: true, updateMetadata: false })
  const ctx = doc.context
  const gs = ctx.register(ctx.obj({ Type: 'ExtGState', ca: 0.38, CA: 0.38, BM: 'Multiply' }))
  const pages = doc.getPages()
  for (const [pageIndex, marks] of marksByPage) {
    const page = pages[pageIndex]
    if (!page) continue
    for (const m of marks) {
      const [x1, y1, x2, y2] = m.rect
      const w = Math.max(0.5, x2 - x1), h = Math.max(0.5, y2 - y1)
      const [r, g, b] = m.color
      const ap = ctx.register(ctx.stream(`q /GS0 gs ${r.toFixed(3)} ${g.toFixed(3)} ${b.toFixed(3)} rg 0 0 ${w.toFixed(2)} ${h.toFixed(2)} re f Q`, {
        Type: 'XObject', Subtype: 'Form', BBox: [0, 0, w, h], Resources: { ExtGState: { GS0: gs } },
      }))
      const annot = ctx.obj({
        Type: 'Annot', Subtype: 'Highlight', Rect: [x1, y1, x2, y2], QuadPoints: [x1, y2, x2, y2, x1, y1, x2, y1],
        C: [r, g, b], CA: 0.38, F: 4, Contents: PDFHexString.fromText(m.text), T: PDFHexString.fromText(author), AP: { N: ap },
      })
      page.node.addAnnot(ctx.register(annot))
    }
  }
  return doc.save()
}
