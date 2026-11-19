import { Component, type ReactNode } from 'react'

/**
 * Catches an error thrown while a page is being drawn, so the app shows a
 * message with a way out instead of going blank. Give it a `resetKey` (e.g.
 * the current path) so moving to another page clears the error.
 */
export class ErrorBoundary extends Component<{ children: ReactNode; resetKey?: string }, { error: Error | null }> {
  state: { error: Error | null } = { error: null }
  static getDerivedStateFromError(error: Error) { return { error } }
  componentDidUpdate(prev: { resetKey?: string }) {
    if (this.state.error && prev.resetKey !== this.props.resetKey) this.setState({ error: null })
  }
  render() {
    if (!this.state.error) return this.props.children
    return (
      <div className="page-center stack" role="alert">
        <h1>Something went wrong on this page</h1>
        <p className="muted">Flame could not draw this view. Your lists and results are safe in the workspace.</p>
        <p className="note note-error small">{this.state.error.message || String(this.state.error)}</p>
        <div className="row">
          <button className="btn btn-primary" onClick={() => this.setState({ error: null })}>Try again</button>
          <button className="btn btn-secondary" onClick={() => window.location.reload()}>Reload the page</button>
        </div>
      </div>
    )
  }
}
