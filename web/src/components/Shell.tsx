import { useEffect, useState, type ReactNode } from 'react'
import { Link, NavLink, useLocation } from 'react-router-dom'
import { getWorkspaceId } from '../api/client'
import { UndoProvider } from './Undo'

export function Logo({ size = 32 }: { size?: number }) {
  return <img src="/flame-logo.svg?v=11" alt="" width={size} height={size} className="logo" />
}

/** The "Flame 3.0" wordmark, in the logo's flame gradient. */
export function Wordmark({ className = '' }: { className?: string }) {
  return <span className={`wordmark ${className}`}><span>Flame</span><span className="wm-version">3.0</span></span>
}

/** Logo mark + wordmark, side by side (top bar) or stacked (home page). */
export function BrandLockup({ size = 30, stacked = false }: { size?: number; stacked?: boolean }) {
  return <span className={`lockup${stacked ? ' lockup-stacked' : ''}`}><Logo size={size} /><Wordmark /></span>
}

function ThemeToggle() {
  const [theme, setTheme] = useState(() => document.documentElement.getAttribute('data-theme') ?? 'light')
  useEffect(() => {
    if (theme === 'dark') document.documentElement.setAttribute('data-theme', 'dark')
    else document.documentElement.removeAttribute('data-theme')
    try {
      sessionStorage.setItem('flame-theme', theme)
    } catch {
      /* storage unavailable */
    }
  }, [theme])
  return (
    <button
      type="button"
      className="icon-btn"
      onClick={() => setTheme((t) => (t === 'dark' ? 'light' : 'dark'))}
      aria-label={theme === 'dark' ? 'Switch to light theme' : 'Switch to dark theme'}
      title={theme === 'dark' ? 'Light theme' : 'Dark theme'}
    >
      {theme === 'dark' ? '☀' : '☾'}
    </button>
  )
}

export function AppShell({ children }: { children: ReactNode }) {
  const loc = useLocation()
  const ws = getWorkspaceId()
  const workspaceHref = ws ? `/w/${ws}` : '/start'
  const inWorkspace = loc.pathname.startsWith('/w/') || loc.pathname === '/start'
  return (
    <div className="app">
      <a className="skip-link" href="#main">
        Skip to content
      </a>
      <header className="topbar">
        <Link to="/" className="brand" aria-label="Flame 3.0 home">
          <Logo size={30} />
          <Wordmark className="brand-name" />
        </Link>
        <nav className="tabs-nav" aria-label="Main">
          <NavLink to="/" end>
            Home
          </NavLink>
          <NavLink to={workspaceHref} className={() => (inWorkspace ? 'active' : '')}>
            Workspace
          </NavLink>
          <NavLink to="/help">Help</NavLink>
          <NavLink to="/api-docs">API</NavLink>
          <NavLink to="/cite">How to cite</NavLink>
          <NavLink to="/about">About</NavLink>
        </nav>
        <div className="topbar-right">
          <ThemeToggle />
        </div>
      </header>
      <main id="main" className="main">
        <UndoProvider>{children}</UndoProvider>
      </main>
      <footer className="footer">
        <span>
          Flame 3.0 · free and open to everyone · code under{' '}
          <a href="https://www.gnu.org/licenses/gpl-3.0.html" target="_blank" rel="noopener noreferrer">
            GPL-3.0
          </a>
        </span>
        <span>
          <Link to="/privacy">Privacy</Link> · <Link to="/cite">How to cite</Link> ·{' '}
          <a href="https://github.com/PavlopoulosLab/Flame" target="_blank" rel="noopener noreferrer">
            GitHub
          </a>{' '}
          ·{' '}
          <a href="https://github.com/PavlopoulosLab/Flame/issues" target="_blank" rel="noopener noreferrer">
            Report an issue
          </a>
        </span>
      </footer>
    </div>
  )
}
