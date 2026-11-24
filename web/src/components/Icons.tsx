import type { ReactNode } from 'react'

/**
 * Small pictures that show what an action does (inputs, tools, analyses).
 * Drawn with currentColor plus the logo's violet / orange / crimson accents,
 * so they follow the theme.
 */
const V = '#8b5cf6', O = '#f97316', C = '#e11d48', G = '#16a34a', B = '#2563eb'

const ICONS: Record<string, ReactNode> = {
  // ---- inputs
  upload: (<>
    <rect x="5" y="3" width="14" height="18" rx="2" fill="none" stroke="currentColor" strokeWidth="1.5" />
    <path d="M8 8h8M8 11.5h8M8 15h5" stroke={V} strokeWidth="1.6" strokeLinecap="round" />
  </>),
  volcano: (<>
    <path d="M3 21h18M12 21V3" stroke="currentColor" strokeOpacity="0.4" strokeWidth="1" />
    {[[11, 19], [13, 19], [10, 17], [14, 18], [12, 16], [9, 15], [15, 16]].map(([x, y], i) => <circle key={i} cx={x} cy={y} r="1" fill="currentColor" fillOpacity="0.5" />)}
    {[[6, 9], [5, 6], [7, 12]].map(([x, y], i) => <circle key={`d${i}`} cx={x} cy={y} r="1.4" fill={B} />)}
    {[[18, 8], [19, 5], [17, 11]].map(([x, y], i) => <circle key={`u${i}`} cx={x} cy={y} r="1.4" fill={C} />)}
  </>),
  reduction: (<>
    <ellipse cx="8" cy="9" rx="5" ry="4" fill={V} fillOpacity="0.15" stroke={V} strokeDasharray="2 1.5" />
    {[[6, 8], [8, 10], [9, 7.5], [7, 11]].map(([x, y], i) => <circle key={i} cx={x} cy={y} r="1.1" fill={V} />)}
    {[[15, 15], [17, 17], [16, 13], [18, 15], [14, 18]].map(([x, y], i) => <circle key={`o${i}`} cx={x} cy={y} r="1.1" fill={O} />)}
  </>),
  snps: (<>
    <path d="M7 3c0 6 10 6 10 12s-10 6-10 6M17 3c0 6-10 6-10 12" fill="none" stroke="currentColor" strokeWidth="1.4" />
    <path d="M8.5 7h7M8.5 17h7" stroke="currentColor" strokeOpacity="0.5" />
    <circle cx="12" cy="12" r="2.2" fill={C} />
  </>),
  textmining: (<>
    <rect x="4" y="3" width="16" height="18" rx="2" fill="none" stroke="currentColor" strokeWidth="1.5" />
    <path d="M7 8h10M7 12h10M7 16h6" stroke="currentColor" strokeOpacity="0.35" strokeWidth="1.6" strokeLinecap="round" />
    <rect x="9" y="10.5" width="5" height="3" rx="1" fill={O} fillOpacity="0.6" />
  </>),
  upset: (<>
    <rect x="5" y="4" width="3" height="9" rx="0.8" fill={V} /><rect x="10.5" y="7" width="3" height="6" rx="0.8" fill={O} /><rect x="16" y="9" width="3" height="4" rx="0.8" fill={C} />
    {[[6.5, 16], [12, 16], [17.5, 16], [6.5, 20], [12, 20], [17.5, 20]].map(([x, y], i) => <circle key={i} cx={x} cy={y} r="1.3" fill={[0, 1, 4, 5].includes(i) ? 'currentColor' : 'none'} stroke="currentColor" strokeOpacity="0.5" />)}
    <path d="M12 16v4" stroke="currentColor" />
  </>),
  expression: (<>
    {[0, 1, 2, 3].map((r) => [0, 1, 2, 3].map((c) => (
      <rect key={`${r}-${c}`} x={4 + c * 4.2} y={4 + r * 4.2} width="3.6" height="3.6" rx="0.6"
        fill={(r < 2) === (c < 2) ? C : B} fillOpacity={0.25 + 0.18 * ((r + c) % 4)} />
    )))}
    <path d="M2 4v16M2 12h1.5M2 7h1M2 17h1" stroke="currentColor" strokeOpacity="0.6" />
  </>),
  mirna: (<>
    <path d="M4 18c2-3 2-9 4-12M8 6c1 3 1 6 0 9" fill="none" stroke={C} strokeWidth="1.6" strokeLinecap="round" />
    <path d="M10 12h3" stroke="currentColor" strokeOpacity="0.5" strokeDasharray="1.5 1.5" />
    <rect x="14" y="5" width="7" height="3" rx="1" fill={V} fillOpacity="0.7" /><rect x="14" y="10.5" width="7" height="3" rx="1" fill={V} fillOpacity="0.5" />
    <rect x="14" y="16" width="7" height="3" rx="1" fill={V} fillOpacity="0.3" />
  </>),
  // ---- quick actions
  convert: (<>
    <text x="3" y="11" fontSize="8" fontWeight="700" fill={V}>AB</text>
    <text x="12" y="21" fontSize="8" fontWeight="700" fill={O}>12</text>
    <path d="M14 6h5l-2-2M10 18H5l2 2" fill="none" stroke="currentColor" strokeWidth="1.4" strokeLinecap="round" strokeLinejoin="round" />
  </>),
  orthologs: (<>
    <circle cx="6" cy="12" r="3.5" fill="none" stroke={V} strokeWidth="1.5" /><circle cx="18" cy="12" r="3.5" fill="none" stroke={O} strokeWidth="1.5" />
    <path d="M9.5 12h5" stroke="currentColor" strokeWidth="1.4" strokeDasharray="1.5 1.5" />
    <text x="4.2" y="14" fontSize="5" fill={V}>h</text><text x="16.2" y="14" fontSize="5" fill={O}>m</text>
  </>),
  ppi: (<>
    <path d="M6 7l6 3 6-4M12 10l-4 8M12 10l6 7M8 18h10" stroke="currentColor" strokeOpacity="0.55" strokeWidth="1.2" />
    {[[6, 7, V], [12, 10, O], [18, 6, C], [8, 18, V], [18, 17, O]].map(([x, y, c], i) => <circle key={i} cx={x as number} cy={y as number} r="2" fill={c as string} />)}
  </>),
  gmt: (<>
    <rect x="3" y="5" width="11" height="14" rx="1.5" fill={V} fillOpacity="0.15" stroke={V} />
    <rect x="7" y="3" width="11" height="14" rx="1.5" fill={O} fillOpacity="0.15" stroke={O} />
    <rect x="10" y="7" width="11" height="14" rx="1.5" fill="var(--surface-color, #fff)" stroke="currentColor" />
    <path d="M12.5 11h6M12.5 14h6M12.5 17h4" stroke="currentColor" strokeOpacity="0.5" strokeLinecap="round" />
  </>),
  // ---- analyses
  demo: (<>
    <path d="M12 3c2 4 3 6 3 9a3 3 0 0 1-6 0c0-3 1-5 3-9z" fill={O} />
    <path d="M6 21h12" stroke="currentColor" strokeWidth="1.4" strokeLinecap="round" />
    <path d="M8 17l4-2 4 2" fill="none" stroke={V} strokeWidth="1.4" />
  </>),
  gsea: (<>
    <path d="M3 20h18" stroke="currentColor" strokeOpacity="0.4" />
    {Array.from({ length: 9 }, (_, i) => <rect key={i} x={3 + i * 2} y={20 - (9 - i) * 1.3} width="1.4" height={(9 - i) * 1.3} fill={i < 4 ? C : i > 6 ? B : 'currentColor'} fillOpacity={i < 4 || i > 6 ? 0.85 : 0.35} />)}
    <path d="M3 10 C6 3 9 3 12 7 S18 12 21 11" fill="none" stroke={V} strokeWidth="1.6" />
  </>),
  'compare-tools': (<>
    <circle cx="9" cy="10" r="5.5" fill={V} fillOpacity="0.2" stroke={V} /><circle cx="15" cy="10" r="5.5" fill={O} fillOpacity="0.2" stroke={O} />
    <circle cx="12" cy="15" r="5.5" fill={C} fillOpacity="0.2" stroke={C} />
  </>),
  'compare-strategies': (<>
    <rect x="3" y="12" width="2.5" height="8" fill={V} /><rect x="6.5" y="9" width="2.5" height="11" fill={V} /><rect x="10" y="14" width="2.5" height="6" fill={V} />
    <path d="M13.5 18 C15 8 17 6 21 5" fill="none" stroke={O} strokeWidth="1.8" />
    <text x="13" y="21" fontSize="4" fill="currentColor" fillOpacity="0.6">vs</text>
  </>),
  'compare-lists': (<>
    <path d="M7 19V6M4 9l3-3 3 3" fill="none" stroke={C} strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round" />
    <path d="M17 5v13M14 15l3 3 3-3" fill="none" stroke={B} strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round" />
  </>),
  variants: (<>
    <rect x="9" y="2" width="6" height="20" rx="3" fill="none" stroke="currentColor" strokeWidth="1.4" />
    <path d="M9 8h6M9 11h6M9 15h6" stroke="currentColor" strokeOpacity="0.35" />
    <path d="M15 12h4" stroke={C} strokeWidth="1.4" /><circle cx="20" cy="12" r="1.8" fill={C} />
  </>),
  gwas: (<>
    <path d="M3 21h18" stroke="currentColor" strokeOpacity="0.4" />
    {[[4, 18], [5, 16], [6, 19], [8, 17], [9, 15], [10, 18], [13, 16], [14, 18], [16, 17], [18, 19], [19, 16]].map(([x, y], i) => <circle key={i} cx={x} cy={y} r="0.9" fill="currentColor" fillOpacity="0.45" />)}
    {[[11, 5], [11.5, 8], [10.5, 10], [12, 12]].map(([x, y], i) => <circle key={`h${i}`} cx={x} cy={y} r="1.3" fill={C} />)}
    <path d="M3 9h18" stroke={C} strokeOpacity="0.6" strokeDasharray="2 2" />
  </>),
  run: (<>
    <path d="M12 3c2.5 4.5 4 7 4 10a4 4 0 0 1-8 0c0-3 1.5-5.5 4-10z" fill={O} />
    <path d="M12 10c1 2 1.6 3 1.6 4a1.6 1.6 0 0 1-3.2 0c0-1 .6-2 1.6-4z" fill="#fde68a" />
  </>),
  compare: (<>
    <rect x="3" y="5" width="8" height="14" rx="1.5" fill={V} fillOpacity="0.18" stroke={V} />
    <rect x="13" y="5" width="8" height="14" rx="1.5" fill={O} fillOpacity="0.18" stroke={O} />
    <path d="M5 9h4M5 12h4M15 9h4M15 12h4" stroke="currentColor" strokeOpacity="0.5" />
    <path d="M9 15h6" stroke={C} strokeWidth="1.6" />
  </>),
  stats: (<>
    <path d="M3 21h18M3 21V3" stroke="currentColor" strokeOpacity="0.4" />
    {[[6, 16], [8, 14], [10, 13], [12, 10], [14, 10], [16, 7], [18, 6]].map(([x, y], i) => <circle key={i} cx={x} cy={y} r="1.2" fill={V} />)}
    <path d="M5 18 L19 5" stroke={O} strokeWidth="1.4" />
  </>),
  save: (<>
    <path d="M12 4v11M7.5 10.5L12 15l4.5-4.5" fill="none" stroke={V} strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round" />
    <path d="M4 16v3a1 1 0 0 0 1 1h14a1 1 0 0 0 1-1v-3" fill="none" stroke="currentColor" strokeWidth="1.5" />
  </>),
  load: (<>
    <path d="M12 15V4M7.5 8.5L12 4l4.5 4.5" fill="none" stroke={O} strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round" />
    <path d="M4 16v3a1 1 0 0 0 1 1h14a1 1 0 0 0 1-1v-3" fill="none" stroke="currentColor" strokeWidth="1.5" />
  </>),
  session: (<>
    <rect x="4" y="4" width="16" height="16" rx="3" fill="none" stroke="currentColor" strokeWidth="1.5" />
    <path d="M8 9h8M8 12h8M8 15h5" stroke="currentColor" strokeOpacity="0.4" strokeLinecap="round" />
    <circle cx="17" cy="16" r="3" fill={G} />
  </>),
  examples: (<>
    <path d="M9 3h6M10 3v6l-5 9a2 2 0 0 0 1.8 3h10.4a2 2 0 0 0 1.8-3l-5-9V3" fill="none" stroke="currentColor" strokeWidth="1.4" strokeLinejoin="round" />
    <path d="M7.5 15h9l2 3.5H5.5z" fill={V} fillOpacity="0.45" />
  </>),
  // ---- regulators, drugs and other analyses
  regulators: (<>
    <circle cx="6" cy="6" r="2.6" fill={O} />
    <path d="M8 7.5l3.5 3M8.4 5.6l8 -1M7 8.5l1 8" stroke={O} strokeWidth="1.3" strokeLinecap="round" />
    {[[13, 12], [18, 4.5], [8.5, 18.5], [17, 16]].map(([x, y], i) => <circle key={i} cx={x} cy={y} r="1.8" fill={i < 3 ? V : 'none'} stroke={V} strokeWidth="1.2" />)}
  </>),
  tf: (<>
    <path d="M2 17h20" stroke="currentColor" strokeWidth="1.4" />
    <path d="M14 17v-4h6" fill="none" stroke={V} strokeWidth="1.4" /><path d="M18 11l2 2-2 2" fill="none" stroke={V} strokeWidth="1.4" strokeLinejoin="round" />
    <path d="M4 17c0-5 7-5 7 0" fill={O} fillOpacity="0.85" />
    <rect x="13" y="16" width="8" height="2" rx="1" fill={V} fillOpacity="0.4" />
  </>),
  kinase: (<>
    <ellipse cx="9" cy="13" rx="6" ry="5" fill={V} fillOpacity="0.2" stroke={V} strokeWidth="1.3" />
    <path d="M14 10l4-4" stroke="currentColor" strokeWidth="1.3" />
    <circle cx="19" cy="5" r="3" fill={O} /><text x="19" y="6.6" fontSize="4.5" textAnchor="middle" fill="#fff" fontWeight="700">P</text>
  </>),
  drugs: (<>
    <g transform="rotate(-40 12 12)">
      <rect x="4" y="9" width="16" height="6" rx="3" fill="none" stroke="currentColor" strokeWidth="1.4" />
      <path d="M12 9h5a3 3 0 0 1 0 6h-5z" fill={C} fillOpacity="0.8" />
    </g>
    <path d="M17 20l2-2M19 20l-2-2" stroke={V} strokeWidth="1.3" strokeLinecap="round" />
  </>),
  metabolites: (<>
    <path d="M12 4l6 3.5v7L12 18l-6-3.5v-7z" fill="none" stroke="currentColor" strokeWidth="1.4" />
    <path d="M12 18v3M18 7.5l2.5-1.5" stroke="currentColor" strokeWidth="1.3" />
    <circle cx="12" cy="4" r="1.6" fill={C} /><circle cx="6" cy="14.5" r="1.6" fill={O} /><circle cx="20.5" cy="6" r="1.4" fill={V} />
  </>),
  celltypes: (<>
    <circle cx="8" cy="9" r="5" fill={V} fillOpacity="0.15" stroke={V} strokeWidth="1.3" /><circle cx="8" cy="9" r="1.8" fill={V} />
    <path d="M14 15c2-3 6-2 7 1s-2 5-4 4.5-4-2.5-3-5.5z" fill={O} fillOpacity="0.2" stroke={O} strokeWidth="1.3" /><circle cx="17.3" cy="17" r="1.4" fill={O} />
    <circle cx="5" cy="18.5" r="2.3" fill={C} fillOpacity="0.2" stroke={C} strokeWidth="1.2" />
  </>),
  robust: (<>
    <path d="M12 3l7 3v5c0 5-3.2 8.3-7 10-3.8-1.7-7-5-7-10V6z" fill={G} fillOpacity="0.12" stroke={G} strokeWidth="1.4" strokeLinejoin="round" />
    <path d="M8.5 12l2.5 2.5 4.5-5" fill="none" stroke={G} strokeWidth="1.8" strokeLinecap="round" strokeLinejoin="round" />
  </>),
  modules: (<>
    <circle cx="7" cy="8" r="4.2" fill={V} fillOpacity="0.15" stroke={V} strokeDasharray="2 1.2" />
    <circle cx="16.5" cy="15.5" r="4.6" fill={O} fillOpacity="0.15" stroke={O} strokeDasharray="2 1.2" />
    {[[6, 7], [8.5, 9], [6.5, 10]].map(([x, y], i) => <circle key={i} cx={x} cy={y} r="1.1" fill={V} />)}
    {[[15, 14.5], [18, 16.5], [16, 17.5], [17.5, 13.5]].map(([x, y], i) => <circle key={`o${i}`} cx={x} cy={y} r="1.1" fill={O} />)}
    <path d="M8.5 9l6.5 5.5" stroke="currentColor" strokeOpacity="0.5" />
  </>),
  activity: (<>
    {[0, 1, 2, 3].map((r) => [0, 1, 2, 3].map((c) => <rect key={`${r}${c}`} x={4 + c * 4.2} y={4 + r * 4.2} width="3.6" height="3.6" rx="0.6"
      fill={(r * 3 + c * 5) % 4 < 2 ? C : B} fillOpacity={0.25 + (((r + 1) * (c + 2)) % 4) * 0.22} />))}
  </>),
  wordcloud: (<>
    <text x="3" y="11" fontSize="8" fontWeight="700" fill={V}>Aa</text>
    <text x="13" y="9" fontSize="5" fontWeight="700" fill={O}>ab</text>
    <text x="5" y="19" fontSize="5.5" fontWeight="700" fill={C}>word</text>
    <text x="16" y="15" fontSize="4" fontWeight="700" fill="currentColor" fillOpacity="0.6">x</text>
  </>),
  disease: (<>
    <path d="M12 20s-7-4.4-7-10a4 4 0 0 1 7-2.6A4 4 0 0 1 19 10c0 5.6-7 10-7 10z" fill={C} fillOpacity="0.15" stroke={C} strokeWidth="1.4" strokeLinejoin="round" />
    <path d="M7.5 11h2.5l1-2 2 4 1-2h2.5" fill="none" stroke={C} strokeWidth="1.4" strokeLinecap="round" strokeLinejoin="round" />
  </>),
  pubmed: (<>
    <path d="M5 4h9l5 5v11H5z" fill="none" stroke="currentColor" strokeWidth="1.4" strokeLinejoin="round" />
    <path d="M14 4v5h5" fill="none" stroke="currentColor" strokeWidth="1.2" />
    <path d="M8 12h8M8 15h5" stroke="currentColor" strokeOpacity="0.35" strokeLinecap="round" />
    <circle cx="16.5" cy="17" r="2.6" fill={B} /><text x="16.5" y="18.5" fontSize="4" textAnchor="middle" fill="#fff" fontWeight="700">P</text>
  </>),
  geo: (<>
    {[0, 1, 2].map((r) => [0, 1, 2, 3].map((c) => <rect key={`${r}${c}`} x={4 + c * 4} y={5 + r * 4} width="3.4" height="3.4" rx="0.6" fill={(r + c) % 3 === 0 ? C : (r + c) % 3 === 1 ? B : 'currentColor'} fillOpacity={(r + c) % 3 === 2 ? 0.2 : 0.7} />))}
    <path d="M4 19.5h16" stroke={G} strokeWidth="1.6" strokeLinecap="round" />
  </>),
  bench: (<>
    <path d="M4 20h16" stroke="currentColor" strokeWidth="1.3" strokeLinecap="round" />
    <rect x="5.5" y="11" width="3.4" height="9" rx="0.8" fill={C} opacity=".35" /><rect x="10.3" y="6" width="3.4" height="14" rx="0.8" fill={O} opacity=".35" /><rect x="15.1" y="9" width="3.4" height="11" rx="0.8" fill={C} opacity=".35" />
    <circle cx="7.2" cy="12.5" r="1" fill={C} /><circle cx="12" cy="7.2" r="1" fill={O} /><circle cx="12" cy="9.4" r="1" fill={O} /><circle cx="16.8" cy="10.6" r="1" fill={C} />
    <path d="M10.6 3.4h5.6M13.4 3.4v1.6" stroke="currentColor" strokeWidth="1.1" strokeLinecap="round" />
  </>),
  nutrition: (<>
    <path d="M12 7.5c-1.6-2.6-6.5-2.4-7.2 1.6-.7 4 1.7 9.4 4.6 10.6 1.2.5 1.8-.3 2.6-.3s1.4.8 2.6.3c2.9-1.2 5.3-6.6 4.6-10.6-.7-4-5.6-4.2-7.2-1.6z" fill="none" stroke="currentColor" strokeWidth="1.3" />
    <path d="M12 7.5c0-2 .6-3.6 2.4-4.6" fill="none" stroke="currentColor" strokeWidth="1.3" strokeLinecap="round" />
    <path d="M12.6 4.6c1.6-1.4 3.6-1.3 4.6-.6-1 1.4-2.9 1.9-4.6.6z" fill={C} /><circle cx="9.6" cy="12.6" r="1.4" fill={O} /><circle cx="14.2" cy="14.6" r="1.1" fill={C} />
  </>),
  tcga: (<>
    <path d="M12 3.2c3.2 0 4.4 2.3 6.6 3.6 2.2 1.3 2.6 4 1.3 6.4-1.3 2.4-1.2 5.3-4 6.6-2.8 1.3-5.6 1-7.8-.5-2.2-1.5-4.6-3.3-4.3-6.3.3-3 1.6-5.2 3.6-7 1.3-1.2 2.9-2.8 4.6-2.8z" fill="none" stroke="currentColor" strokeWidth="1.3" />
    <circle cx="12" cy="12" r="3.4" fill={C} opacity=".85" /><circle cx="16.4" cy="7.6" r="1.4" fill={O} /><circle cx="7.6" cy="15.8" r="1.1" fill={O} />
  </>),
  clinvar: (<>
    <path d="M6 3c0 5 12 5 12 9s-12 4-12 9M18 3c0 5-12 5-12 9s12 4 12 9" fill="none" stroke="currentColor" strokeWidth="1.3" />
    <circle cx="12" cy="7.6" r="1.8" fill={C} /><circle cx="12" cy="16.4" r="1.8" fill={O} />
  </>),
}

const ALIASES: Record<string, string> = { robustness: 'robust', literature: 'pubmed', dbsnp: 'clinvar' }

export function ConceptIcon({ name, size = 22, label }: { name: string; size?: number; label?: string }) {
  const body = ICONS[name] ?? ICONS[ALIASES[name]]
  if (!body) return null
  return (
    <svg className="concept-icon" width={size} height={size} viewBox="0 0 24 24" role={label ? 'img' : undefined} aria-label={label} aria-hidden={label ? undefined : true}>
      {body}
    </svg>
  )
}

/** Button content with a concept icon on the left. */
export function IconLabel({ icon, children }: { icon: string; children: ReactNode }) {
  return <span className="icon-label"><ConceptIcon name={icon} size={18} />{children}</span>
}
