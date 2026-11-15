/** Icons that show what each external viewer does. */

/** Arena3D: networks stacked as 3D layers, linked across layers. */
export function Arena3DIcon({ size = 28 }: { size?: number }) {
  return (
    <svg width={size} height={size} viewBox="0 0 32 32" aria-hidden="true" className="ext-icon">
      <g strokeLinejoin="round">
        <path d="M4 22 L16 27 L28 22 L16 17 Z" fill="#e11d48" fillOpacity="0.18" stroke="#e11d48" strokeWidth="1.2" />
        <path d="M4 15 L16 20 L28 15 L16 10 Z" fill="#f97316" fillOpacity="0.18" stroke="#f97316" strokeWidth="1.2" />
        <path d="M4 8 L16 13 L28 8 L16 3 Z" fill="#8b5cf6" fillOpacity="0.18" stroke="#8b5cf6" strokeWidth="1.2" />
      </g>
      <g stroke="currentColor" strokeOpacity="0.55" strokeWidth="0.9">
        <line x1="12" y1="8" x2="13" y2="15.5" /><line x1="13" y1="15.5" x2="18" y2="22" /><line x1="20" y1="7.5" x2="18" y2="22" />
      </g>
      <g fill="currentColor">
        <circle cx="12" cy="8" r="1.5" /><circle cx="20" cy="7.5" r="1.5" /><circle cx="13" cy="15.5" r="1.5" /><circle cx="18" cy="22" r="1.5" />
      </g>
    </svg>
  )
}

/** NORMA: a network with its nodes grouped into coloured, annotated regions. */
export function NormaIcon({ size = 28 }: { size?: number }) {
  return (
    <svg width={size} height={size} viewBox="0 0 32 32" aria-hidden="true" className="ext-icon">
      <ellipse cx="11" cy="12" rx="8.5" ry="7" fill="#8b5cf6" fillOpacity="0.22" stroke="#8b5cf6" strokeWidth="1" />
      <ellipse cx="21" cy="20" rx="8.5" ry="7" fill="#f97316" fillOpacity="0.22" stroke="#f97316" strokeWidth="1" />
      <g stroke="currentColor" strokeOpacity="0.55" strokeWidth="0.9">
        <line x1="7" y1="10" x2="13" y2="8" /><line x1="13" y1="8" x2="12" y2="15" /><line x1="7" y1="10" x2="12" y2="15" />
        <line x1="12" y1="15" x2="17" y2="18" /><line x1="17" y1="18" x2="24" y2="17" /><line x1="24" y1="17" x2="22" y2="24" /><line x1="17" y1="18" x2="22" y2="24" />
      </g>
      <g fill="currentColor">
        <circle cx="7" cy="10" r="1.6" /><circle cx="13" cy="8" r="1.6" /><circle cx="12" cy="15" r="1.6" />
        <circle cx="17" cy="18" r="1.6" /><circle cx="24" cy="17" r="1.6" /><circle cx="22" cy="24" r="1.6" />
      </g>
    </svg>
  )
}
