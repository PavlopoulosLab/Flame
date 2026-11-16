import { asset } from '../api/client'

/**
 * Official logos of the enrichment tools (downloaded from each tool's website,
 * served from /logos). `icon` is a square mark for small places, `full` the
 * wordmark where there is room. Tools without a fetchable logo get a text badge.
 */
const LOGOS: Record<string, { icon?: string; full?: string; name: string; site: string }> = {
  gprofiler: { icon: '/logos/gprofiler-icon.png', full: '/logos/gprofiler.png', name: 'g:Profiler', site: 'https://biit.cs.ut.ee/gprofiler/' },
  string: { icon: '/logos/string-icon.png', full: '/logos/string.svg', name: 'STRING', site: 'https://string-db.org/' },
  enrichr: { icon: '/logos/enrichr.png', full: '/logos/enrichr.png', name: 'Enrichr', site: 'https://maayanlab.cloud/Enrichr/' },
  webgestalt: { icon: '/logos/webgestalt.svg', full: '/logos/webgestalt.svg', name: 'WebGestalt', site: 'https://www.webgestalt.org/' },
  genecodis: { icon: '/logos/genecodis-icon.png', full: '/logos/genecodis.png', name: 'GeneCodis', site: 'https://genecodis.genyo.es/' },
  clusterprofiler: { icon: '/logos/clusterprofiler.png', full: '/logos/clusterprofiler.png', name: 'clusterProfiler', site: 'https://bioconductor.org/packages/clusterProfiler/' },
  reactome: { icon: '/logos/reactome-icon.png', full: '/logos/reactome.png', name: 'Reactome', site: 'https://reactome.org/' },
  panther: { name: 'PANTHER', site: 'https://pantherdb.org/' },
}

const keyOf = (tool: string) => {
  const k = tool.toLowerCase().replace(/[^a-z]/g, '')
  if (k.startsWith('custom')) return 'custom'
  return k === 'gprofiler' || k === 'gprofilerg' ? 'gprofiler' : k
}

export function ToolLogo({ tool, size = 20, variant = 'icon', withName = false }: { tool: string; size?: number; variant?: 'icon' | 'full'; withName?: boolean }) {
  const k = keyOf(tool)
  const info = LOGOS[k]
  const src = info ? (variant === 'full' ? info.full ?? info.icon : info.icon ?? info.full) : undefined
  const label = info?.name ?? (k === 'custom' ? 'Custom gene sets' : tool)
  if (!src) {
    const text = k === 'custom' ? 'GMT' : (info?.name ?? tool).replace(/[^A-Za-z]/g, '').slice(0, variant === 'full' ? 12 : 2).toUpperCase()
    return (
      <span className={`tool-logo tool-badge${variant === 'full' ? ' full' : ''}`} style={{ height: size, minWidth: size, fontSize: Math.max(8, size * 0.42) }} title={label} aria-label={withName ? undefined : label}>
        {text}{withName && variant !== 'full' ? <span className="tool-logo-name">{label}</span> : null}
      </span>
    )
  }
  return (
    <span className="tool-logo" title={label}>
      <img src={asset(src)} alt={withName ? '' : label} style={variant === 'full' ? { height: size } : { width: size, height: size }} />
      {withName && <span className="tool-logo-name">{label}</span>}
    </span>
  )
}

export const toolSite = (tool: string) => LOGOS[keyOf(tool)]?.site
