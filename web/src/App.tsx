import { lazy, Suspense, useEffect } from 'react'
import { Navigate, Route, Routes, useNavigate, useParams, useSearchParams } from 'react-router-dom'
import { api, isWorkspaceId, setWorkspaceId } from './api/client'
import { AppShell } from './components/Shell'
import { ErrorNote, Spinner } from './components/ui'
import HomePage from './pages/Home'

const WorkspacePage = lazy(() => import('./pages/Workspace'))
const ResultsPage = lazy(() => import('./pages/Results'))
const ComparePage = lazy(() => import('./pages/Compare'))
const VariantsPage = lazy(() => import('./pages/Variants'))
const HelpPage = lazy(() => import('./pages/Help'))
const InfoPages = lazy(() => import('./pages/Info'))

const LAST_WS_KEY = 'flame-last-workspace'

/** Remember the current workspace for this browser session (sessionStorage only). */
function useWorkspaceParam() {
  const { ws } = useParams()
  const valid = isWorkspaceId(ws)
  if (valid) {
    setWorkspaceId(ws)
    try {
      sessionStorage.setItem(LAST_WS_KEY, ws)
    } catch {
      /* storage unavailable */
    }
  }
  return valid ? ws : null
}

function WorkspaceRoute({ children }: { children: React.ReactNode }) {
  const ws = useWorkspaceParam()
  if (!ws) return <Navigate to="/start" replace />
  return <>{children}</>
}

/** Creates a fresh workspace (or reopens this session's) and opens it. */
function StartWorkspace() {
  const navigate = useNavigate()
  const [params] = useSearchParams()
  useEffect(() => {
    let last: string | null = null
    try {
      last = sessionStorage.getItem(LAST_WS_KEY)
    } catch {
      /* ignore */
    }
    const fresh = params.get('new') === '1'
    if (!fresh && isWorkspaceId(last)) {
      navigate(`/w/${last}`, { replace: true })
      return
    }
    api<{ id: string }>('/workspace', { method: 'POST', workspace: null }).then(({ id }) =>
      navigate(`/w/${id}${params.get('example') ? `?example=${params.get('example')}` : ''}`, { replace: true }),
    )
  }, [navigate, params])
  return (
    <div className="page-center">
      <Spinner label="Preparing your workspace" />
    </div>
  )
}

/** v2 links: /?url_genes=TP53,BRCA1;EGFR&organism=9606 → new workspace with those lists. */
function LegacyImport() {
  const [params] = useSearchParams()
  const navigate = useNavigate()
  const genes = params.get('url_genes')
  useEffect(() => {
    if (!genes) return
    const organism = params.get('organism') ?? '9606'
    api<{ workspace: string }>(`/legacy?url_genes=${encodeURIComponent(genes)}&organism=${encodeURIComponent(organism)}`, {
      workspace: null,
    })
      .then(({ workspace }) => navigate(`/w/${workspace}`, { replace: true }))
      .catch((e) => navigate(`/?import_error=${encodeURIComponent(e.message)}`, { replace: true }))
  }, [genes, params, navigate])
  if (!genes) return <HomePage />
  return (
    <div className="page-center">
      <Spinner label="Importing your gene lists" />
    </div>
  )
}

export default function App() {
  return (
    <AppShell>
      <Suspense
        fallback={
          <div className="page-center">
            <Spinner />
          </div>
        }
      >
        <Routes>
          <Route path="/" element={<LegacyImport />} />
          <Route path="/start" element={<StartWorkspace />} />
          <Route
            path="/w/:ws"
            element={
              <WorkspaceRoute>
                <WorkspacePage />
              </WorkspaceRoute>
            }
          />
          <Route
            path="/w/:ws/runs/:runId/:tab?"
            element={
              <WorkspaceRoute>
                <ResultsPage />
              </WorkspaceRoute>
            }
          />
          <Route
            path="/w/:ws/compare"
            element={
              <WorkspaceRoute>
                <ComparePage />
              </WorkspaceRoute>
            }
          />
          <Route
            path="/w/:ws/variants/:analysisId?"
            element={
              <WorkspaceRoute>
                <VariantsPage />
              </WorkspaceRoute>
            }
          />
          <Route path="/help/:topic?" element={<HelpPage />} />
          <Route path="/api-docs" element={<InfoPages page="api" />} />
          <Route path="/cite" element={<InfoPages page="cite" />} />
          <Route path="/about" element={<InfoPages page="about" />} />
          <Route path="/privacy" element={<InfoPages page="privacy" />} />
          <Route path="*" element={<ErrorNote error="Page not found." />} />
        </Routes>
      </Suspense>
    </AppShell>
  )
}
