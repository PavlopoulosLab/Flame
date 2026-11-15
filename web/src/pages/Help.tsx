import type { ReactNode } from 'react'
import { Link, NavLink, useParams } from 'react-router-dom'

interface Topic { id: string; title: string; body: ReactNode }

const Example = ({ kind, children }: { kind: string; children: ReactNode }) => (
  <Link className="btn btn-secondary btn-sm" to={`/start?new=1&example=${kind}`}>{children}</Link>
)

const TOPICS: Topic[] = [
  {
    id: 'start',
    title: 'Getting started (tutorial)',
    body: (
      <>
        <p>Flame takes one or more lists of genes or proteins and tells you which biological functions, pathways, diseases and other annotations are over-represented in them. A full analysis takes three steps.</p>
        <ol className="tutorial">
          <li>
            <h3>Open a workspace</h3>
            <p>Press <strong>Start an analysis</strong> on the Home tab. Flame creates a private workspace with its own link — bookmark it to come back later. No account is needed.</p>
          </li>
          <li>
            <h3>Add a list</h3>
            <p>Press <strong>+ Add list</strong> and choose how to provide genes: paste IDs, upload a file, select genes on a volcano or 2D plot, map SNPs, find genes in text, or combine existing lists. Every method has a <em>Load example</em> button.</p>
          </li>
          <li>
            <h3>Run enrichment</h3>
            <p>Press <strong>+ New run</strong>, pick one or more tools and the annotation sources to search (GO and KEGG are selected by default), then <strong>Run enrichment</strong>. Each tool becomes a card that shows progress and the number of significant terms.</p>
          </li>
          <li>
            <h3>Explore</h3>
            <p>Open a finished card to see the results as a dot plot, bar chart, table, heatmaps and networks. Filter and search on the left; click terms to collect them on the right and export them.</p>
          </li>
          <li>
            <h3>Compare</h3>
            <p>Tick two or more finished cards and press <strong>Compare</strong> to see which terms the tools (or lists) agree on, with a combined p-value.</p>
          </li>
        </ol>
        <p><Example kind="demo">Open the worked example</Example> — it loads a 200-gene list and runs g:Profiler and STRING so you can explore real output immediately.</p>
      </>
    ),
  },
  {
    id: 'inputs',
    title: 'Adding lists',
    body: (
      <>
        <h3>Paste or upload IDs</h3>
        <p>One identifier per line, or separated by commas, tabs or semicolons. Duplicates are removed (case-insensitive) and entries with unusual characters are skipped and reported. Choose the organism and the ID type; "Gene symbol" works for most tools. A list holds up to 3,000 IDs, and a workspace up to 10 lists. <Example kind="upload">Example</Example></p>
        <h3>From a volcano plot</h3>
        <p>Upload differential-expression results (CSV/TSV) with a gene column, a log2 fold-change column and a p-value column. Flame guesses the columns; check them, set the cut-offs, then select genes by dragging a box or lasso, or with the <em>Up</em>, <em>Down</em> and <em>All significant</em> buttons. Red dotted lines show the cut-offs. <Example kind="volcano">Example</Example></p>
        <h3>From a 2D plot (UMAP, t-SNE, PCA)</h3>
        <p>Upload a table with a gene column and two coordinate columns; optionally a column to colour by (e.g. cluster) and one to size by. Lasso the points you want. <Example kind="reduction">Example</Example></p>
        <h3>From SNPs</h3>
        <p>Paste dbSNP rsIDs (human only). Flame maps them to genes with g:SNPense and shows the variant effects; save the genes as symbols or Ensembl IDs. <Example kind="snps">Example</Example></p>
        <h3>From text</h3>
        <p>Paste an abstract or any text. The EXTRACT tagger finds gene and protein names for the chosen organism. Ambiguous names are not selected by default — tick them if they are right. <Example kind="textmining">Example</Example></p>
        <h3>Combining lists (UpSet)</h3>
        <p>Pick 2–6 lists with the same organism and ID type. <em>Only in</em> shows genes unique to each combination, <em>Shared by</em> shows genes common to all lists of a combination, and <em>Union</em> merges them. Click a bar to preview and save it. <Example kind="upset">Example</Example></p>
      </>
    ),
  },
  {
    id: 'tools',
    title: 'Tools and annotation sources',
    body: (
      <>
        <p>Flame runs established enrichment services and harmonises their output so you can compare them:</p>
        <table className="table">
          <thead><tr><th>Tool</th><th>Strengths</th></tr></thead>
          <tbody>
            <tr><td>g:Profiler</td><td>Broad source coverage (GO, KEGG, Reactome, WikiPathways, TRANSFAC, miRTarBase, HPA, CORUM, HP) and the g:SCS correction.</td></tr>
            <tr><td>STRING</td><td>Over 12,000 organisms; also literature (PubMed), protein domains, tissues and diseases.</td></tr>
            <tr><td>Enrichr</td><td>Large curated libraries for human, mouse, fly, yeast, worm and fish.</td></tr>
            <tr><td>WebGestalt</td><td>Disease and drug databases (DisGeNET, OMIM, DrugBank, GLAD4U).</td></tr>
            <tr><td>PANTHER</td><td>PANTHER pathways and protein classes with Fisher's exact test.</td></tr>
            <tr><td>GeneCodis</td><td>Regulatory annotations (transcription factors, miRNAs) and co-annotation.</td></tr>
          </tbody>
        </table>
        <p>In the New run panel, sources that none of the selected tools offer are greyed out, and coloured dots show which tools provide each source. Tools that do not support the list's organism cannot be selected.</p>
        <h3>Custom gene sets</h3>
        <p>Upload your own gene sets as a GMT file (one set per line: ID, description, genes — tab separated) under <em>Custom sources</em>. They are tested with a hypergeometric test and Benjamini–Hochberg correction; the universe is all genes in the file unless you choose a background list.</p>
      </>
    ),
  },
  {
    id: 'parameters',
    title: 'Parameters explained',
    body: (
      <>
        <h3>Significance threshold and correction</h3>
        <p>Testing hundreds of terms at once produces false positives by chance, so p-values are corrected for multiple testing. Flame keeps terms whose corrected p-value is below the threshold (0.05 by default). FDR (Benjamini–Hochberg) is the usual choice; Bonferroni is stricter; g:SCS is g:Profiler's method that accounts for the structure of GO.</p>
        <h3>Background</h3>
        <p>The background (statistical universe) is the set of genes that <em>could</em> have appeared in your list. By default tools use all annotated genes of the organism. If your experiment measured only some genes (e.g. those expressed in a tissue, or present on an array), save them as a list and choose it as the background — this gives more realistic p-values. Not every tool supports a custom background.</p>
        <h3>ID conversion</h3>
        <p>Each tool needs IDs in its own format. Flame converts automatically (to Ensembl, Entrez or STRING IDs as needed) and maps hits back to your original IDs. <em>Run details</em> shows how many IDs were mapped and which were not.</p>
      </>
    ),
  },
  {
    id: 'results',
    title: 'Reading the results',
    body: (
      <>
        <p>Every view shows the top terms after your filters (20 by default, change it with <em>Show</em> and <em>by</em>). Term statistics:</p>
        <dl className="details">
          <dt>P-value</dt><dd>Corrected significance — smaller is stronger evidence.</dd>
          <dt>Genes found</dt><dd>How many of your genes are annotated to the term.</dd>
          <dt>Term size</dt><dd>How many genes the term has in total.</dd>
          <dt>Gene ratio</dt><dd>Genes found ÷ size of your list.</dd>
          <dt>Coverage %</dt><dd>Genes found ÷ term size × 100 — how much of the term your list covers.</dd>
          <dt>Fold enrichment</dt><dd>How many times more often the term appears in your list than expected by chance. Needs the background size, which some tools do not report.</dd>
        </dl>
        <h3>Views</h3>
        <ul>
          <li><strong>Dot plot</strong> — one row per term; x = gene ratio (or fold enrichment / coverage), dot size = genes found, colour = significance. Look for large, dark dots on the right.</li>
          <li><strong>Bar chart</strong> — the most significant terms, coloured by source.</li>
          <li><strong>Scatter</strong> — significance against coverage; terms at the top right are both significant and well covered.</li>
          <li><strong>Heatmap T×G</strong> — which genes drive which terms; genes shared by many terms appear as filled columns.</li>
          <li><strong>Heatmap T×T</strong> — overlap between terms (shared genes ÷ all their genes). Blocks of high similarity are redundant terms describing one process.</li>
          <li><strong>Heatmap G×G</strong> — how many terms each pair of genes shares.</li>
          <li><strong>Networks</strong> — the same relations as graphs. Choose a layout; for term–term and gene–gene networks raise the cut-off to keep only strong links. <em>3D view</em> opens the network in Arena3Dweb.</li>
          <li><strong>Table</strong> — every statistic, sortable, with links to the source databases.</li>
        </ul>
        <h3>Search syntax</h3>
        <p>Words must all appear (<code>immune response</code>); use quotes for exact phrases (<code>"cell cycle"</code>), <code>OR</code> for alternatives (<code>apoptosis OR autophagy</code>) and a minus sign to exclude (<code>-mitotic</code>).</p>
        <h3>Selecting and exporting</h3>
        <p>Click (shift-click to add) or box/lasso-select terms in any view; they collect in the <em>Selected</em> panel with their genes and database links, and stay selected when you switch views. Export the selection as CSV, HTML or print/PDF, figures as PNG or SVG, and tables as CSV.</p>
        <p><Example kind="demo">See live sample output</Example></p>
      </>
    ),
  },
  {
    id: 'compare',
    title: 'Comparing runs',
    body: (
      <>
        <p>Comparing runs shows how far different tools — or different lists — agree. Terms are matched by their identifier across runs.</p>
        <ul>
          <li><strong>Combined p-value</strong> — for terms found by more than one active run, Fisher's method combines their p-values (χ² with 2k degrees of freedom).</li>
          <li><strong>Agreement</strong> — how many of the active runs found the term.</li>
          <li><strong>Overview</strong> — per run: terms found, terms found only by that run, and "consensus blockers" (terms every other run found but this one did not). Summary cards count all distinct terms, the consensus (found by all runs) and single-run terms. Click any bar or card to select those terms.</li>
          <li><strong>Bubble plot</strong> — mean fold enrichment against combined significance; bubble size = agreement.</li>
          <li><strong>Heatmap</strong> — −log10 p per term and run; blank cells mean the run did not report the term.</li>
        </ul>
        <p>Use the <em>Runs</em> filter to include or exclude runs, and <em>Found by all</em> to keep only consensus terms.</p>
      </>
    ),
  },
  {
    id: 'utilities',
    title: 'Quick actions',
    body: (
      <>
        <h3>Convert IDs</h3>
        <p>Translates a list to another identifier type with g:Convert. <em>Best only</em> keeps one target per input; <em>All</em> keeps every mapping. Select rows and save them as a new list.</p>
        <h3>Find orthologs</h3>
        <p>Maps a list to the corresponding genes of another organism with g:Orth — useful for running mouse data through human-only resources.</p>
        <h3>Protein interaction network</h3>
        <p>Shows STRING interactions among the proteins of a list (up to 500). Edges are coloured by the strongest evidence type, or drawn thicker for higher confidence. Download the network as TSV or PNG, open it in STRING, or save its proteins as a list.</p>
      </>
    ),
  },
  {
    id: 'session',
    title: 'Workspaces, saving and privacy',
    body: (
      <>
        <p>Your workspace link is the only key to your data — keep it to yourself unless you want to share results. Workspaces are deleted after 30 days without use.</p>
        <p><em>Save, load or clear session</em> lets you download everything (lists, custom sources and finished results) as one JSON file and load it again later, here or on another Flame server. You can also clear all data or delete the workspace permanently.</p>
        <p>See the <Link to="/privacy">privacy page</Link> for details.</p>
      </>
    ),
  },
  {
    id: 'faq',
    title: 'FAQ',
    body: (
      <dl className="faq">
        <dt>Do I need an account?</dt><dd>No. Flame is free and open to everyone without login or registration.</dd>
        <dt>A run finished with no significant terms. Why?</dt><dd>The list may be small or functionally diverse, the IDs may not match the organism, or the chosen sources may not cover your genes. Check <em>Run details</em> for unmapped IDs, add sources, or try another tool.</dd>
        <dt>Why do tools give different results?</dt><dd>They use different database versions, statistics and backgrounds. Comparing runs shows where they agree; consensus terms are the most robust.</dd>
        <dt>Fold enrichment is missing.</dt><dd>Some tools do not report the background size needed to compute it. Provide a custom background list, or use gene ratio instead.</dd>
        <dt>Can I analyse metabolites or lipids?</dt><dd>Not yet — Flame currently analyses genes and proteins.</dd>
        <dt>How do I cite Flame?</dt><dd>See <Link to="/cite">How to cite</Link>.</dd>
        <dt>Can I use Flame from scripts?</dt><dd>Yes — see the <Link to="/api-docs">API page</Link> and the Python example.</dd>
      </dl>
    ),
  },
]

export default function HelpPage() {
  const { topic } = useParams()
  const current = TOPICS.find((t) => t.id === topic) ?? TOPICS[0]
  return (
    <div className="doc-layout">
      <nav className="doc-nav" aria-label="Help topics">
        <h2>Help</h2>
        {TOPICS.map((t) => <NavLink key={t.id} to={`/help/${t.id}`} className={() => (t.id === current.id ? 'active' : '')}>{t.title}</NavLink>)}
      </nav>
      <article className="doc">
        <h1>{current.title}</h1>
        {current.body}
      </article>
    </div>
  )
}
