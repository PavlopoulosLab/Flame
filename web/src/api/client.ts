// Minimal typed fetch wrapper for the Flame API.
// The workspace id travels in the X-Flame-Workspace header (no cookies).

export class ApiError extends Error {
  status: number
  constructor(status: number, message: string) {
    super(message)
    this.status = status
  }
}

let currentWorkspace: string | null = null

export function setWorkspaceId(id: string | null) {
  currentWorkspace = id
}

export function getWorkspaceId(): string | null {
  return currentWorkspace
}

export const isWorkspaceId = (value: string | undefined | null): value is string =>
  !!value && /^[0-9a-f]{32}$/.test(value)

export async function api<T>(
  path: string,
  options: { method?: string; body?: unknown; workspace?: string | null; signal?: AbortSignal } = {},
): Promise<T> {
  const headers: Record<string, string> = { Accept: 'application/json' }
  const ws = options.workspace === undefined ? currentWorkspace : options.workspace
  if (ws) headers['X-Flame-Workspace'] = ws
  let body: BodyInit | undefined
  if (options.body !== undefined) {
    headers['Content-Type'] = 'application/json'
    body = JSON.stringify(options.body)
  }
  const res = await fetch(`/api${path}`, {
    method: options.method ?? (body ? 'POST' : 'GET'),
    headers,
    body,
    signal: options.signal,
    credentials: 'omit',
  })
  const text = await res.text()
  let data: unknown = null
  if (text) {
    try {
      data = JSON.parse(text)
    } catch {
      data = text
    }
  }
  if (!res.ok) {
    const message =
      data && typeof data === 'object' && 'error' in data
        ? String((data as { error: unknown }).error)
        : `Request failed (${res.status})`
    throw new ApiError(res.status, message)
  }
  return data as T
}

/** Download a server response as a file (used for list CSV export). */
export async function downloadFromApi(path: string, filename: string) {
  const headers: Record<string, string> = {}
  if (currentWorkspace) headers['X-Flame-Workspace'] = currentWorkspace
  const res = await fetch(`/api${path}`, { headers, credentials: 'omit' })
  if (!res.ok) throw new ApiError(res.status, 'Download failed')
  saveBlob(await res.blob(), filename)
}

export function saveBlob(blob: Blob, filename: string) {
  const url = URL.createObjectURL(blob)
  const a = document.createElement('a')
  a.href = url
  a.download = filename
  document.body.appendChild(a)
  a.click()
  a.remove()
  setTimeout(() => URL.revokeObjectURL(url), 1000)
}

export function saveText(text: string, filename: string, type = 'text/plain;charset=utf-8') {
  saveBlob(new Blob([text], { type }), filename)
}
