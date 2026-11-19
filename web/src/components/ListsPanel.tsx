import { Link } from 'react-router-dom'
import { getWorkspaceId } from '../api/client'
import { useWorkspace } from '../api/hooks'
import { formatNum } from '../lib/format'

/**
 * The workspace's lists, shown on every page outside the Workspace itself
 * (results, comparison, variants) so they are always in view. Read-only:
 * adding and managing lists happens in the workspace.
 */
export function ListsPanel({ boxed = false }: { boxed?: boolean }) {
  const ws = getWorkspaceId()
  const { data } = useWorkspace(ws ?? undefined)
  if (!ws) return null
  const lists = data?.lists ?? []
  return (
    <details className={`lists-panel${boxed ? ' rail-section' : ''}`} open>
      <summary><h2>Your lists <span className="count">{lists.length}</span></h2></summary>
      {lists.length === 0
        ? <p className="muted small">No lists yet.</p>
        : (
          <ul className="plain-list">
            {lists.map((l) => (
              <li key={l.id} className="list-text">
                <span className="list-name" title={l.name}>{l.name}</span>
                <span className="muted small">{formatNum(l.size)} IDs · {l.organismName.replace(/ \[NCBI.*$/, '').replace(/ \(.*$/, '')}{l.ranked && ' · ranked'}</span>
              </li>
            ))}
          </ul>
        )}
      <p className="lists-panel-links small">
        <Link to={`/w/${ws}?tab=input`}>+ Add list</Link>
        <Link to={`/w/${ws}`}>Manage</Link>
      </p>
    </details>
  )
}
