import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import { api } from './client'
import type { AnalyteList, AppConfig, OrganismOption, Run, WorkspaceSummary } from './types'

export function useConfig() {
  return useQuery({
    queryKey: ['config'],
    queryFn: () => api<AppConfig>('/config', { workspace: null }),
    staleTime: Infinity,
  })
}

/** Workspace summary; polls every 2 s while any run is queued or running. */
export function useWorkspace(ws: string | undefined) {
  return useQuery({
    queryKey: ['workspace', ws],
    queryFn: () => api<WorkspaceSummary>('/workspace', { workspace: ws }),
    enabled: !!ws,
    refetchInterval: (q) => {
      const runs = q.state.data?.runs ?? []
      return runs.some((r) => r.status === 'queued' || r.status === 'running') ? 1200 : false
    },
    retry: (count, err) => (err as { status?: number }).status !== 404 && count < 2,
  })
}

export function useOrganismSearch(q: string, tool?: string) {
  return useQuery({
    queryKey: ['organisms', q, tool ?? ''],
    queryFn: () =>
      api<OrganismOption[]>(
        `/organisms?q=${encodeURIComponent(q)}&limit=50${tool ? `&tool=${encodeURIComponent(tool)}` : ''}`,
        { workspace: null },
      ),
    enabled: q.length >= 2,
    staleTime: 5 * 60_000,
  })
}

export function useInvalidateWorkspace() {
  const qc = useQueryClient()
  return () => qc.invalidateQueries({ queryKey: ['workspace'] })
}

export interface CreateListBody {
  name: string
  ids?: string[]
  scores?: number[]
  ranked?: boolean
  text?: string
  skipFirstRow?: boolean
  organismTaxId: number
  analyteType?: string
  idFormat?: string
  meta?: Record<string, unknown>
  uniqueName?: boolean
}

export function useCreateList() {
  const invalidate = useInvalidateWorkspace()
  return useMutation({
    mutationFn: (body: CreateListBody) => api<AnalyteList>('/lists', { body }),
    onSuccess: invalidate,
  })
}

export function useRenameList() {
  const invalidate = useInvalidateWorkspace()
  return useMutation({
    mutationFn: ({ id, name }: { id: string; name: string }) =>
      api<AnalyteList>(`/lists/${encodeURIComponent(id)}`, { method: 'PATCH', body: { name } }),
    onSuccess: invalidate,
  })
}

export function useDeleteList() {
  const invalidate = useInvalidateWorkspace()
  return useMutation({
    mutationFn: (id: string) => api(`/lists/${encodeURIComponent(id)}`, { method: 'DELETE' }),
    onSuccess: invalidate,
  })
}

export interface StartRunsBody {
  listId: string
  paradigm?: 'ORA' | 'GSEA'
  tools: string[]
  sourceIds?: string[]
  datasources: string[]
  threshold?: number
  perTool?: Record<string, { threshold?: number; correction?: string; backgroundListId?: string }>
}

export interface StartRunsResult {
  created: string[]
  reused: string[]
  skipped: { tool: string; reason: string }[]
}

export function useStartRuns() {
  const invalidate = useInvalidateWorkspace()
  return useMutation({
    mutationFn: (body: StartRunsBody) => api<StartRunsResult>('/runs', { body }),
    onSuccess: invalidate,
  })
}

export function useDeleteRun() {
  const invalidate = useInvalidateWorkspace()
  return useMutation({
    mutationFn: (id: string) => api(`/runs/${encodeURIComponent(id)}`, { method: 'DELETE' }),
    onSuccess: invalidate,
  })
}

export const isFinished = (r: Run) => r.status === 'complete' || r.status === 'error'
