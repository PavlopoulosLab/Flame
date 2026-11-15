-- Flame workspace store (SQLite). One row set per anonymous workspace.
PRAGMA journal_mode = WAL;
PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS workspaces (
  id          TEXT PRIMARY KEY,           -- 32 hex chars (128-bit random), unguessable
  created     REAL NOT NULL,              -- unix seconds
  last_seen   REAL NOT NULL,
  email       TEXT                        -- optional, only for result notification
);

CREATE TABLE IF NOT EXISTS lists (
  id          TEXT PRIMARY KEY,
  ws          TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  name        TEXT NOT NULL,
  analyte     TEXT NOT NULL DEFAULT 'GENE',
  taxid       INTEGER NOT NULL DEFAULT 9606,
  id_format   TEXT NOT NULL DEFAULT 'USERINPUT',
  ids_json    TEXT NOT NULL,
  scores_json TEXT,                       -- ranked lists only: one score per ID (same order)
  meta_json   TEXT NOT NULL DEFAULT '{}',
  created     REAL NOT NULL,
  UNIQUE (ws, name)
);

CREATE TABLE IF NOT EXISTS runs (
  id            TEXT NOT NULL,            -- e.g. gprofiler_3 (unique within a workspace)
  ws            TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  seq           INTEGER NOT NULL,         -- per-workspace counter
  display       INTEGER NOT NULL,         -- per-tool display number
  tool          TEXT NOT NULL,
  paradigm      TEXT NOT NULL DEFAULT 'ORA',
  list_id       TEXT,
  list_name     TEXT NOT NULL,
  bg_list_name  TEXT,
  taxid         INTEGER NOT NULL,
  params_json   TEXT NOT NULL,
  status        TEXT NOT NULL,            -- queued | running | complete | error
  error         TEXT,
  result_count  INTEGER,
  result_blob   BLOB,                     -- serialized execute_ora_run() output
  messages_json TEXT NOT NULL DEFAULT '[]',
  created       REAL NOT NULL,
  finished      REAL,
  PRIMARY KEY (ws, id)
);
CREATE INDEX IF NOT EXISTS runs_ws ON runs(ws);
CREATE INDEX IF NOT EXISTS lists_ws ON lists(ws);

CREATE TABLE IF NOT EXISTS sources (
  id          TEXT PRIMARY KEY,
  ws          TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  name        TEXT NOT NULL,
  term_count  INTEGER NOT NULL,
  gmt_json    TEXT NOT NULL,              -- [{id, name, genes:[...]}]
  created     REAL NOT NULL,
  UNIQUE (ws, name)
);

CREATE TABLE IF NOT EXISTS variant_analyses (
  id          TEXT NOT NULL,
  ws          TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  name        TEXT NOT NULL,
  taxid       INTEGER NOT NULL,
  status      TEXT NOT NULL,              -- queued | running | complete | error
  error       TEXT,
  input_count INTEGER,
  result_blob BLOB,                       -- serialized analyse_variants() output
  created     REAL NOT NULL,
  finished    REAL,
  PRIMARY KEY (ws, id)
);

-- Background analyses (transcription factors, kinases, drug signatures, network
-- modules, robustness, metabolites). Results are stored as JSON.
CREATE TABLE IF NOT EXISTS analyses (
  id          TEXT NOT NULL,
  ws          TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  type        TEXT NOT NULL,
  label       TEXT NOT NULL,
  list_id     TEXT,
  status      TEXT NOT NULL,              -- running | complete | error
  error       TEXT,
  result_json TEXT,
  run_id      TEXT,                       -- metabolites: the run the result became
  meta_json   TEXT NOT NULL DEFAULT '{}',
  created     REAL NOT NULL,
  finished    REAL,
  PRIMARY KEY (ws, id)
);
CREATE INDEX IF NOT EXISTS analyses_ws ON analyses(ws);
