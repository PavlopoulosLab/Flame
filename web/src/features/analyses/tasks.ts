import { useQuery, useQueryClient } from '@tanstack/react-query'
import { api, getWorkspaceId } from '../../api/client'
import type { JobProgress } from '../../components/Progress'

export type TaskType = 'tf' | 'kinase' | 'drugs' | 'modules' | 'robustness' | 'metabolites'

export interface TaskSummary {
  id: string; type: TaskType; label: string; status: 'running' | 'complete' | 'error'; error?: string | null
  progress?: JobProgress | null; started: number; runId?: string | null; listId?: string
}
export interface TaskState<T = unknown> {
  id: string; type: TaskType; status: TaskSummary['status']; error?: string | null; progress?: JobProgress | null
  result?: T | null; runId?: string | null
}

export const startTask = (body: Record<string, unknown>) => api<{ id: string; type: TaskType; label: string }>('/analyses', { body })

/** All background analyses of this workspace (kept for a few hours). */
export function useTasks() {
  const ws = getWorkspaceId()
  return useQuery({
    queryKey: ['tasks', ws],
    queryFn: () => api<TaskSummary[]>('/analyses'),
    refetchInterval: (q) => ((q.state.data ?? []).some((t) => t.status === 'running') ? 1500 : false),
  })
}

/** One analysis, polled until it finishes. */
export function useTask<T>(id: string | null | undefined) {
  const ws = getWorkspaceId()
  const qc = useQueryClient()
  return useQuery({
    queryKey: ['task', ws, id],
    enabled: !!id,
    queryFn: async () => {
      const st = await api<TaskState<T>>(`/analyses/${id}`)
      if (st.status !== 'running') {
        qc.invalidateQueries({ queryKey: ['tasks', ws] })
        if (st.runId) qc.invalidateQueries({ queryKey: ['workspace', ws] })
      }
      return st
    },
    refetchInterval: (q) => (q.state.data?.status === 'running' || !q.state.data ? 1200 : false),
  })
}

// ---- result shapes -------------------------------------------------------
export interface Regulator { rank: number; name: string; score: number; libraries: { library: string; rank: number }[]; overlap: string[] }
export interface RegulatorResult { kind: 'tf' | 'kinase'; tool: string; citation: string; queried: number; libraries: string[]; regulators: Regulator[]; note: string }

export interface DrugRow {
  agent: string; mimic: number; reverse: number; upInUp?: number; downInDown?: number; upInDown?: number; downInUp?: number
  genesMimic: string[]; genesReverse: string[]
}
export interface DrugResult { kind: 'chemical' | 'crispr'; library: string; upCount: number; downCount: number; mimic: DrugRow[]; reverse: DrugRow[]; citation: string; note: string }

export interface ModuleTerm { id: string; name: string; source: string; p: number; size: number; genes?: string }
export interface NetModule { id: number; size: number; genes: string[]; label: string; terms: ModuleTerm[]; density: number }
export interface ModulesResult {
  modules: NetModule[]; membership: Record<string, number>; score: number; modularity: number; nodes: number; edges: number
  links?: { from: string; to: string; score: number }[]; method: string
}

export interface RobustTerm { id: string; name: string; source: string; padj: number; stability: number; randomRate: number; genes?: string }
export interface RobustnessResult {
  query: number; universe: number; sets: number; significant: number; randomSignificant: number[]; medianRandom: number; stable: number
  terms: RobustTerm[]; thresholds: { fdr: number; terms: number }[]; nBoot: number; nRandom: number; method: string
}

export const TASK_INFO: Record<TaskType, { title: string; icon: string; text: string; tool: string }> = {
  tf: { title: 'Transcription factors', icon: 'tf', tool: 'ChEA3', text: 'Which transcription factors may control your genes (ChEA3: ChIP-seq, co-expression and literature evidence).' },
  kinase: { title: 'Kinases', icon: 'kinase', tool: 'KEA3', text: 'Kinases whose known substrates and interactors are over-represented in your list (KEA3).' },
  drugs: { title: 'Drugs & perturbations', icon: 'drugs', tool: 'LINCS L1000', text: 'Compounds or CRISPR knock-outs that reverse or mimic an up/down signature (LINCS L1000).' },
  modules: { title: 'Network modules', icon: 'modules', tool: 'STRING', text: 'Split the STRING network of a list into modules and find what each module does.' },
  robustness: { title: 'Robustness check', icon: 'robust', tool: 'MSigDB', text: 'Are the enriched terms stable? Bootstrap subsamples, random lists and FDR thresholds.' },
  metabolites: { title: 'Metabolite pathways', icon: 'metabolites', tool: 'KEGG', text: 'Pathway enrichment of metabolites or lipids (names, KEGG or ChEBI IDs) against KEGG pathways.' },
}
