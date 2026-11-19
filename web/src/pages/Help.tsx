import type { ReactNode } from 'react'
import { Link, NavLink, useParams } from 'react-router-dom'
import { API_BASE } from '../api/client'
import { CopyButton } from '../components/ui'

interface Topic { id: string; title: string; body: ReactNode }

const Example = ({ kind, children }: { kind: string; children: ReactNode }) => (
  <Link className="btn btn-secondary btn-sm" to={`/start?new=1&example=${kind}`}>{children}</Link>
)


/** A file sample with real tab characters: tabs are highlighted on screen and copied as tabs. */
function Sample({ text }: { text: string }) {
  const lines = text.split('\n')
  return (
    <div className="format-sample-wrap">
      <pre className="format-sample" aria-label="Example file content (columns separated by tabs)">
        {lines.map((l, i) => (
          <span key={i}>{l.split('\t').map((cell, j) => (
            <span key={j}>{j > 0 && <span className="tab-char">{'\t'}</span>}{cell}</span>
          ))}{i < lines.length - 1 ? '\n' : ''}</span>
        ))}
      </pre>
      <CopyButton text={text} label="Copy sample" />
    </div>
  )
}

interface Format { id: string; title: string; ext: string; use: string; columns: ReactNode; sample: string; file: string; example?: string; notes?: ReactNode }
const FORMATS: Format[] = [
  { id: 'list', title: 'Gene or protein list', ext: '.txt', use: 'Paste or upload IDs', file: '/inputs/examples/upload/file', example: 'upload',
    columns: <>One identifier per line (commas, semicolons and tabs also work). Gene symbols, Ensembl, Entrez, UniProt and 15 more ID types; choose the type when uploading.</>,
    sample: 'CACNB2\nALOX15B\nDUSP1\nSTEAP2\nMAOA',
    notes: <>Example: 200 genes drawn at random, on every load, from the 500 most changed by dexamethasone in airway smooth-muscle cells (Himes et al. 2014, <em>PLoS One</em>; GEO GSE52778).</> },
  { id: 'ranked', title: 'Ranked list (for GSEA)', ext: '.tsv', use: 'Paste or upload IDs → “IDs with scores”', file: '/inputs/examples/ranked/file', example: 'gsea',
    columns: <>Two tab-separated columns: <code>gene</code> and <code>score</code> (any number: log2 fold change, signed −log10 p, t-statistic). Include all measured genes. A header row is allowed (tick “skip first row”).</>,
    sample: 'gene\tscore\nCACNB2\t9.5884\nALOX15B\t9.2522\nDUSP1\t9.004\nDNM1\t-8.6878',
    notes: <>Example: all ~12,400 expressed genes of the airway dexamethasone experiment (Himes et al. 2014), scored by sign(log2FC) × −log10 p from limma-voom paired by donor.</> },
  { id: 'volcano', title: 'Differential-expression table (volcano plot)', ext: '.tsv / .csv', use: 'From a volcano plot', file: '/inputs/examples/volcano/file', example: 'volcano',
    columns: <>A header row and at least three columns: gene ID, log2 fold change and p-value (raw or adjusted). Other columns are ignored. Flame guesses which column is which; you can change it.</>,
    sample: 'gene\tlog2FC\tpvalue\tpadj\nCACNB2\t3.2059\t2.58e-10\t3.297e-06\nALOX15B\t7.9912\t5.595e-10\t3.297e-06\nDUSP1\t2.8649\t9.908e-10\t3.297e-06',
    notes: <>Example: dexamethasone vs untreated airway smooth-muscle cells, 4 donors (Himes et al. 2014, <em>PLoS One</em> 9:e99625; Bioconductor <code>airway</code>), analysed with limma-voom paired by donor.</> },
  { id: 'reduction', title: '2D coordinates (UMAP, t-SNE, PCA)', ext: '.tsv / .csv', use: 'From a 2D plot', file: '/inputs/examples/reduction/file', example: 'reduction',
    columns: <>A header row; a gene column and two coordinate columns. Optional: a column to colour by (e.g. cluster) and a numeric column to size by.</>,
    sample: 'gene\tUMAP1\tUMAP2\tcell_type\tmean_expression\nIL7R\t-2.167\t-4.609\tMemory CD4 T cells\t0.869\nLYZ\t2.143\t2.092\tCD14 monocytes\t1.846\nMS4A1\t-3.335\t-0.197\tB cells\t0.353',
    notes: <>Example: a real UMAP of 2,500 genes from 2,638 blood cells (10x Genomics PBMC 3k; Zheng et al. 2017, <em>Nat Commun</em>; CC BY 4.0). Genes expressed in the same cells sit together; colour = the cell type where a gene is specific.</> },
  { id: 'expression', title: 'Expression matrix', ext: '.tsv / .csv', use: 'From expression data', file: '/inputs/examples/expression/file', example: 'expression',
    columns: <>A header row; first column = gene ID, then one numeric column per sample (counts, TPM, intensities or log values). Counts and intensities are log2(x + 1) transformed automatically. Up to 30,000 genes and 2 MB.</>,
    sample: 'gene\tuntreated_N052611\tuntreated_N061011\tdexamethasone_N052611\tdexamethasone_N061011\nZBTB16\t5\t12\t429\t1113\nKLF15\t52\t34\t702\t1355\nDUSP1\t1213\t695\t4541\t6840',
    notes: <>Example (the sample shows 4 of the 8 columns): raw counts of the 4,000 most variable genes in 8 airway samples (4 donors, untreated vs dexamethasone; Himes et al. 2014). One donor is female, so Y-chromosome genes form their own cluster — a real donor effect.</> },
  { id: 'mirna', title: 'microRNA list', ext: '.txt', use: 'From microRNAs', file: '/inputs/examples/mirna/file', example: 'mirna',
    columns: <>One microRNA per line in miRBase form (<code>hsa-miR-21-5p</code>, <code>hsa-let-7a-5p</code>). A name without <code>-5p</code>/<code>-3p</code> uses both arms. Flame finds their predicted targets (miRDB v6, score &gt; 80) and lets you keep genes hit by several of your microRNAs.</>,
    sample: 'hsa-miR-21-5p\nhsa-miR-17-5p\nhsa-miR-191-5p\nhsa-miR-155-5p',
    notes: <>Example: the 21 microRNAs up-regulated in at least 3 of 6 solid cancers (Volinia et al. 2006, <em>PNAS</em> 103:2257, Table 2), with 2006 names mapped to current miRBase names.</> },
  { id: 'snps', title: 'SNP list', ext: '.txt', use: 'From SNPs', file: '/inputs/examples/snps/file', example: 'snps',
    columns: <>One dbSNP rsID per line (human).</>, sample: 'rs429358\nrs7412\nrs1801133\nrs6025' },
  { id: 'variants', title: 'Variants', ext: '.txt / .vcf', use: 'Input → From variants or GWAS', file: '/variants/examples/variants/file', example: 'variants',
    columns: <>One variant per line, in any of these forms: rsID; VCF columns <code>CHROM POS ID REF ALT</code> (tab- or space-separated; header lines starting with <code>##</code> are skipped); <code>chr:pos:ref:alt</code>; or HGVS (<code>ENST…:c.803C&gt;T</code>). Positions are GRCh38.</>,
    sample: 'rs429358\nrs7412\n7\t140753336\t.\tA\tT\n17:7674220:C:T\nENST00000366667:c.803C>T' },
  { id: 'gwas', title: 'GWAS summary statistics', ext: '.tsv / .csv', use: 'Input → From variants or GWAS', file: '/variants/examples/gwas/file', example: 'gwas',
    columns: <>A header row with a variant column (<code>SNP</code>, <code>rsid</code>…) and a p-value column (<code>P</code>, <code>pval</code>…); optionally <code>CHR</code> and <code>BP</code> (GRCh38). Up to 2,000 of the most significant rows are analysed.</>,
    sample: 'SNP\tCHR\tBP\tP\nrs6733839\t2\t127135234\t6e-118\nrs3851179\t11\t86157598\t3e-48\nrs679515\t1\t207577223\t7e-46',
    notes: <>Example: 89 lead associations for Alzheimer’s disease from Bellenguez et al. 2022, <em>Nat Genet</em> 54:412 (GWAS Catalog GCST90027158).</> },
  { id: 'gmt', title: 'Custom gene sets (GMT)', ext: '.gmt', use: 'New run → Your own pathway sets → + Upload GMT', file: '/inputs/examples/gmt/file', example: 'gmt',
    columns: <>One gene set per line, tab-separated: set ID, description, then the genes (as many columns as needed). No header row.</>,
    sample: 'HALLMARK_ANGIOGENESIS\tangiogenesis\tAPOH\tAPP\tCCND2\tCOL3A1\tCOL5A2\nHALLMARK_APOPTOSIS\tapoptosis\tADD1\tAIFM3\tANKH\tANXA1\tAPP',
    notes: <>The example file is the MSigDB Hallmark collection (human gene symbols).</> },
  { id: 'text', title: 'Free text', ext: '.txt', use: 'From text', file: '/inputs/examples/textmining/file', example: 'textmining',
    columns: <>Any plain text, such as an abstract. Gene and protein names are found with the EXTRACT tagger.</>,
    sample: 'The combined treatment of aspirin and cisplatin suppressed the expression of the anti-apoptotic protein Bcl-2…' },
]

function FormatsTopic() {
  return (
    <>
      <p>Every file format Flame reads, with a sample and a downloadable example file you can open, edit and upload. In the samples, <span className="tab-char tab-legend">{'\t'}</span> marks a <strong>tab character</strong>: copying a sample keeps the tabs, so it pastes correctly into Flame or a spreadsheet. Comma-separated (.csv) files work wherever tab-separated files do.</p>
      <nav className="format-index" aria-label="Formats">{FORMATS.map((f) => <a key={f.id} href={`#fmt-${f.id}`}>{f.title}</a>)}</nav>
      {FORMATS.map((f) => (
        <section key={f.id} id={`fmt-${f.id}`} className="format-card">
          <div className="row between wrap">
            <h3>{f.title} <span className="muted small">{f.ext}</span></h3>
            <span className="row wrap">
              <a className="btn btn-secondary btn-sm" href={`${API_BASE}${f.file}`} download>Download example file</a>
              {f.example && <Link className="btn btn-ghost btn-sm" to={`/start?new=1&example=${f.example}`}>Open this example</Link>}
            </span>
          </div>
          <p className="small"><strong>Use with:</strong> {f.use}</p>
          <p className="small">{f.columns}</p>
          <Sample text={f.sample} />
          {f.notes && <p className="muted small">{f.notes}</p>}
        </section>
      ))}
      <section className="format-card">
        <h3>Session file <span className="muted small">.json / .json.gz</span></h3>
        <p className="small">Written by <em>Session → Save</em> and read by <em>Session → Load</em>: a JSON document (optionally gzip-compressed) with your lists, gene sets, finished results, variant analyses and display settings. It holds plain data only. See <Link to="/help/session">Sessions</Link>.</p>
      </section>
    </>
  )
}

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
            <p>Press <strong>Start an analysis</strong> on the Home tab. Flame creates a private workspace with its own link — bookmark it to come back later.</p>
          </li>
          <li>
            <h3>Add a list</h3>
            <p>Open the <strong>Input</strong> tab (or press <strong>+ Add list</strong>) and choose how to provide genes: paste IDs, upload a file, select genes on a volcano or 2D plot, map SNPs, find genes in text, or combine existing lists — or fetch genes from a disease, PubMed articles, a GEO dataset or ClinVar. Every method has a <em>Load example</em> button. <strong>Save list</strong> stores the list and brings you back to the Input tab, so you can add another; the saved list appears under <em>Lists</em> on the left.</p>
          </li>
          <li>
            <h3>Run enrichment</h3>
            <p>Press <strong>+ New run</strong> on the Enrichment tab, tick the list (or several lists) to analyse, pick one or more tools and the annotation sources to search (GO and KEGG are selected by default), then <strong>Run enrichment</strong>. Each tool and list becomes a card that shows progress and the number of significant terms. To analyse several lists at once, tick them under <em>Lists</em> and press <strong>Run enrichment on selected</strong>.</p>
          </li>
          <li>
            <h3>Explore</h3>
            <p>Open a finished card to see the results as a dot plot, bar chart, table, heatmaps and networks. The tabs at the top show all annotation sources together or one at a time (GO:BP, GO:CC, KEGG…). Filter and search on the left; click terms to collect them on the right and export them.</p>
          </li>
          <li>
            <h3>Compare</h3>
            <p>Tick two or more finished cards and press <strong>Compare</strong> to see which terms the tools (or lists) agree on, with a combined p-value.</p>
          </li>
        </ol>
        <p><Example kind="demo">Open the worked example</Example> — it loads a 200-gene list and runs g:Profiler and STRING so you can explore real output immediately.</p>
        <p>Every option has a ready-made example: use the <strong>Examples</strong> menu in the workspace (or the cards on the Home tab) to load data for GSEA, comparing tools or strategies, custom gene sets, protein networks, expression matrices, microRNAs, variants and GWAS. Example and format files are listed under <Link to="/help/formats">Input files and formats</Link>.</p>
        <p>Working with SNPs or GWAS results rather than gene lists? Start from <Link to="/help/variants">Variants and GWAS</Link>. Have a ranked list (all genes with a score)? See <Link to="/help/gsea">Ranked lists (GSEA)</Link>.</p>
      </>
    ),
  },
  {
    id: 'inputs',
    title: 'Adding lists',
    body: (
      <>
        <h3>The Input tab and your lists</h3>
        <p>The workspace has four tabs: <strong>Input</strong>, <strong>Enrichment</strong>, <strong>Regulators &amp; drugs</strong> and <strong>Gene tools</strong>. Variant and GWAS analysis is one of the input options (<em>From variants or GWAS</em>), since its result is a gene list; your variant analyses are listed under the input options. A new workspace opens on <strong>Input</strong>, which shows every way to add a list as a card; <strong>+ Add list</strong> takes you there from anywhere. After <strong>Save list</strong> you are back on the Input tab, with a line confirming what was saved and a <em>Run enrichment</em> shortcut.</p>
        <p>Your lists are always in view: in the workspace they sit in the panel on the left, and the results, comparison and variants pages show them in a <em>Your lists</em> box. In the workspace panel you can:</p>
        <ul>
          <li><strong>See a list's contents</strong> — click its name (or <em>⋯ → View contents</em>). The IDs are shown with their annotations where available: gene symbol, Ensembl gene ID and description, fetched from g:Profiler (g:Convert). The search box matches IDs, names and descriptions.</li>
          <li><strong>Edit a list</strong> — <em>⋯ → Edit contents</em>: one ID per line (or <code>ID score</code> pairs for a ranked list). Duplicates and invalid IDs are dropped on save, and <em>Undo</em> restores the previous contents. Runs already made from the list keep their results; start a new run to analyse the edited list.</li>
          <li><strong>Tick lists</strong> and then <strong>Run enrichment on selected</strong> (the same analysis on every ticked list) or <strong>Compare selected</strong> (an UpSet plot of two to six lists, see <em>Combining lists</em> below).</li>
          <li>Rename, download (Excel, CSV), convert IDs, find orthologs, build a protein network or delete a list from its <em>⋯</em> menu.</li>
        </ul>
        <p>Steps that take a while — fetching from public databases, mapping, saving — show a progress bar with the elapsed time. These are single requests, so the bar shows that work is in progress, not a percentage; enrichment runs and variant analyses report real progress.</p>
        <h3>Paste or upload IDs</h3>
        <p>One identifier per line, or separated by commas, tabs or semicolons. Duplicates are removed (case-insensitive) and entries with unusual characters are skipped and reported. Choose the organism and the ID type; "Gene symbol" works for most tools. A list holds up to 3,000 IDs, and a workspace up to 10 lists. <Example kind="upload">Example</Example></p>
        <h3>IDs with scores (ranked list)</h3>
        <p>For gene set enrichment analysis, choose <em>IDs with scores (ranked list, for GSEA)</em> in the upload dialog and give two columns: the ID and a score (e.g. log2 fold change, signed −log10 p, or a t-statistic). Include all measured genes, not only the significant ones. A header row can be skipped. Ranked lists are marked <em>ranked</em> in the list rail; they can also be used for ordinary (ORA) runs, which ignore the scores.</p>
        <h3>From a volcano plot</h3>
        <p>Upload differential-expression results (CSV/TSV) with a gene column, a log2 fold-change column and a p-value column. Flame guesses the columns; check them, set the cut-offs, then select genes by dragging a box or lasso, or with the <em>Up</em>, <em>Down</em> and <em>All significant</em> buttons. Red dotted lines show the cut-offs. The <strong>Selected genes</strong> table under the plot lists what you picked, most significant first, with log2 fold change, p-value and direction; remove single genes with ×, copy them, then <em>Save list</em>. <em>Save whole table as ranked list</em> keeps every gene with the score sign(log2FC) × −log10 p, ready for GSEA. <Example kind="volcano">Example</Example></p>
        <h3>From a 2D plot (UMAP, t-SNE, PCA)</h3>
        <p>Upload a table with a gene column and two coordinate columns; optionally a column to colour by (e.g. cluster) and one to size by. Lasso the points you want; they appear in the <strong>Selected genes</strong> table under the plot (with their colour value, e.g. the cluster), where you can remove single genes before saving. <Example kind="reduction">Example</Example></p>
        <h3>From SNPs</h3>
        <p>Paste dbSNP rsIDs (human only). Flame maps them to genes with g:SNPense and shows the variant effects; save the genes as symbols or Ensembl IDs. <Example kind="snps">Example</Example> For a full variant report (consequences, impact, ClinVar, traits, VCF and GWAS input) use <Link to="/help/variants">Variants and GWAS</Link>.</p>
        <h3>From text</h3>
        <p>Paste an abstract or any text. The EXTRACT tagger finds gene and protein names for the chosen organism. Ambiguous names are not selected by default — tick them if they are right. <Example kind="textmining">Example</Example></p>
        <h3>From expression data</h3>
        <p>Upload a genes × samples matrix (counts, TPM, intensities or log values). Three ways to pick genes: <strong>co-expression clusters</strong> — the most variable genes clustered by correlation and shown as a heatmap of z-scores; tick one or more clusters (or click them on the heatmap) to select their genes, and tick again to remove them; <strong>gene map</strong> — a PCA of the genes' profiles where you lasso or box-select genes; <strong>compare two groups</strong> — assign samples to groups A and B and Flame runs a Welch t-test per gene (Benjamini–Hochberg) and opens the result in the volcano selector, from which you can also save all genes as a ranked list for GSEA. In the cluster and gene-map views the <strong>Selected genes</strong> table lists every picked gene with its cluster before you save. <Example kind="expression">Example</Example></p>
        <h3>From microRNAs</h3>
        <p>Paste microRNAs and get their predicted target genes (miRDB v6, MirTarget score &gt; 80, via MSigDB). Keep genes targeted by several of your microRNAs — they are the most likely to be regulated — or click one microRNA to take only its targets, then run enrichment on them. <Example kind="mirna">Example</Example></p>
        <h3>Combining lists (UpSet)</h3>
        <p>Open it with <strong>Combine lists</strong> on the Input tab, or tick lists on the left and press <strong>Compare selected</strong>. Pick 2–6 lists with the same organism and ID type. The UpSet plot shows each list's size on the left and one bar per combination, with dots marking the lists involved.</p>
        <ul>
          <li><strong>Each bar shows</strong> — <em>Only in</em>: IDs in exactly those lists and no other (the parts never overlap); <em>Shared by</em>: IDs common to those lists, whatever the others contain; <em>Union of</em>: IDs in at least one of them.</li>
          <li><strong>Select bars</strong> — click one or more bars; click again to remove one.</li>
          <li><strong>Set operations on all lists</strong> — <em>Intersection</em> (in every list), <em>Union</em> (in any list), <em>XOR</em> (in exactly one list), <em>In 2 or more</em> and <em>Not in all</em> select the matching bars for you.</li>
          <li><strong>See the IDs</strong> — every selected bar is listed and opens to show its own IDs; the table below lists each selected ID once with the lists it is found in. Copy or download them.</li>
          <li><strong>Save</strong> — <em>Save list</em> stores the combined selection as a new list. <em>Copy table</em>, <em>Excel</em> and <em>TSV</em> export every combination, and <em>⤓ Image</em> the plot.</li>
        </ul>
        <p><Example kind="upset">Example</Example></p>
        <h3>From public resources</h3>
        <ul>
          <li><strong>From a disease</strong> — search a disease or trait. Tick the <em>databases to search</em> (Open Targets, DisGeNET, OMIM, Orphanet, ClinVar, Jensen DISEASES, GWAS Catalog; <em>Select all</em> / <em>None</em>) — fewer databases answer faster. Results come as two tables, diseases and gene sets. Click a name to open it alone, or tick several rows and press <em>Get genes of selected</em>: their genes are merged and ranked by how many of the ticked results list each gene. <em>Back to results</em> returns to the tables with your ticks kept. Disease genes come from the Open Targets Platform, ranked by an overall score that combines genetics (GWAS, rare variants), somatic mutations, known drugs, expression, literature and animal models; keep the scores to get a ranked list for GSEA. <em>GWAS variants</em> lists the GWAS Catalog associations (p ≤ 5e-8) with their mapped genes, and can send the variants to a full variant analysis. Gene sets from DisGeNET, OMIM, Orphanet, ClinVar, Jensen DISEASES and the GWAS Catalog are matched by name (via Enrichr's libraries). <Example kind="disease">Asthma example</Example></li>
          <li><strong>From PubMed articles</strong> — give PMIDs, or a PubMed search (the most relevant articles are used). Genes come from NCBI Gene's links to the articles (gene2pubmed: curated links and GeneRIFs) and are ranked by the number of articles. To find gene names in the text itself, use <em>From text</em>. <Example kind="pubmed">PubMed example</Example></li>
          <li><strong>From a GEO dataset</strong> — search a GEO series (GSE52778) or keywords. RummaGEO (Marino et al. 2024) re-analysed thousands of human and mouse series from ARCHS4 with limma-voom; each pair of sample groups gives an up and a down signature, shown with the groups compared. <Example kind="geo">GSE52778 example</Example></li>
          <li><strong>From ClinVar</strong> — a gene (<code>APOE</code>), a condition (<code>cystic fibrosis</code>), rsIDs, ClinVar accessions (<code>VCV000017864</code>) or any ClinVar search, optionally keeping only pathogenic or likely pathogenic variants. Shows each variant's genes, location, consequence, clinical classification, review status and conditions; save the genes as a list (ranked by number of variants), or annotate the variants that have an rsID in full (Ensembl VEP, gnomAD, GWAS traits) with <em>Annotate variants</em>. <Example kind="clinvar">ClinVar example</Example></li>
        </ul>
        <p>Each source ranks its genes (score, number of articles or variants) where it can, and lists them in a table. <em>Keep the top N</em> (number box or slider) chooses how many are saved; highlighted rows and coloured bars are the genes that will be saved. To hand-pick genes instead, tick them in the table or click their bars in the bar chart (click again to remove); <em>Back to top N</em> returns to the automatic choice. Only your query is sent to the service; if a service is down, the message names it.</p>
      </>
    ),
  },
  {
    id: 'formats',
    title: 'Input files and formats',
    body: <FormatsTopic />,
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
            <tr><td>Reactome</td><td>Reactome's own Analysis Service (Fabregat et al. 2017; Milacic et al. 2024): the most current Reactome release, curated human pathways and inferred pathways for 15 other species, including disease pathways. Each pathway opens in the Reactome browser with your genes highlighted. Uses the whole Reactome universe (no custom background). <Example kind="reactome">Example</Example></td></tr>
            <tr><td>clusterProfiler</td><td>Runs locally on the MSigDB collections (GO, KEGG, Reactome, WikiPathways, HPO, Hallmark, BioCarta, PID, cell types, ImmuneSigDB, oncogenic signatures) for 20 organisms. The only tool that also does <Link to="/help/gsea">GSEA</Link> on ranked lists.</td></tr>
          </tbody>
        </table>
        <p>MSigDB gene sets are downloaded once per organism on first use and then cached, so the first clusterProfiler run for an organism takes longer.</p>
        <p>The New run panel lists your lists with checkboxes: tick several (or press <em>Run enrichment on selected</em> in the lists panel) to run the same tools and settings on each of them; the footer shows the number of runs that will start. If the lists are from different organisms, a tool that does not support one of them is skipped for that list and reported.</p>
        <p>In the New run panel, sources that none of the selected tools offer are greyed out, and coloured dots show which tools provide each source. Tools that do not support the list's organism cannot be selected.</p>
        <h3>Your own pathway sets (custom gene sets)</h3>
        <p>A GMT file is a collection of named gene sets — your own pathways, published signatures, or a collection Flame does not offer. Upload it in the <strong>New run</strong> panel, under <em>Which tools → Your own pathway sets → + Upload GMT</em> (one set per line: ID, description, genes — tab separated), and tick it there; Flame tests your list against it as a separate run, beside the tool runs. It works with over-representation runs, not ranked-list GSEA. It is not one of your gene lists and not a background: a background is an ordinary list that you choose under <em>Background</em> in a run's per-tool settings (see below). They are tested with a hypergeometric test and Benjamini–Hochberg correction; the universe is all genes in the file unless you choose a background list.</p>
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
        <p>Every view shows the top terms after your filters (20 by default, change it with <em>Show</em> and <em>by</em>). The <strong>source tabs</strong> above the views show <em>All sources</em> together or one annotation source at a time (GO:BP, GO:CC, GO:MF, KEGG…), each with its number of terms; a source tab applies to every view, and <em>top 20</em> then means the top 20 of that source. The <em>Sources</em> chips in the filters do the same for any mix of sources. Term statistics:</p>
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
          <li><strong>Networks</strong> — the same relations as graphs. For term–term and gene–gene networks raise the cut-off to keep only strong links. See <em>Network layouts and style</em> below.</li>
          <li><strong>Sankey</strong> (under <em>More views</em>) — flows from genes through terms to annotation sources. The 40 genes in most terms are shown; the rest are grouped as <em>Other genes</em>. Wide flows mark genes that drive many terms.</li>
          <li><strong>Chord diagram</strong> — each arc is a term, ribbons join terms that share genes. Thick ribbons between arcs point to redundant terms.</li>
          <li><strong>Circos (bundled)</strong> — terms grouped by annotation source around a circle with your genes in their own sector; term–gene links are bundled (hierarchical edge bundling) so genes shared by many terms show as thick bundles. Outer bars: −log10 p for terms, share of terms for genes. Hover to trace links, click to select, adjust the bundling strength with the slider.</li>
          <li><strong>Hive plot</strong> — one axis for genes and one per annotation source, nodes ordered by rank. Useful to see whether the same genes drive terms in different sources.</li>
          <li><strong>Table</strong> — every statistic, sortable, with links to the source databases.</li>
        </ul>
        <h3>Network layouts and style</h3>
        <p>The <em>Layout</em> menu offers two kinds of layout. <strong>Static</strong> layouts (force-directed, Kamada–Kawai, GraphOpt, MDS, tree, star, circle, grid) are computed once on the server; drag nodes to adjust them. <strong>Interactive</strong> layouts run in your browser with vis-network: three physics solvers (Barnes–Hut, Force Atlas 2, repulsion), hierarchical repulsion, and a <em>hierarchical (layered)</em> layout that puts the most connected nodes first.</p>
        <p><em>Network style</em> sets, without re-running anything:</p>
        <ul>
          <li><strong>Layout</strong> — spacing; for physics, whether to freeze once settled, and buttons to simulate or stop; for hierarchical, the direction and how levels are ordered.</li>
          <li><strong>Nodes</strong> — shape (by type, or dot, box, ellipse, diamond, star, hexagon and more), size fixed or by number of connections, labels, label size and outline, shadows.</li>
          <li><strong>Edges</strong> — straight or curved (dynamic, continuous, clockwise, Bézier and others), width by weight, opacity, dashes.</li>
          <li><strong>Interaction</strong> — zoom buttons and arrow keys, node dragging, hiding edges while panning (faster for large networks).</li>
          <li><strong>Clusters</strong> — <em>Group hubs</em> collapses each highly connected node with its neighbours; <em>Group leaves</em> folds nodes with a single link into their neighbour. Double-click a cluster to open it, or <em>Open all</em>.</li>
        </ul>
        <p>The same options are available for the STRING protein network (Regulators &amp; drugs). Style choices are kept for the browser session; ⤓ Image saves the whole network or the current view.</p>
        <h3>Open in Arena3D or NORMA</h3>
        <p><em>Arena3D ↗</em> (network, heatmap, Sankey, chord and hive views) opens the view in <a href="https://www.arena3d.org/" target="_blank" rel="noopener noreferrer">Arena3D</a> as a 3D multilayer network: the terms of each annotation source on their own layer and the genes on another. <em>NORMA ↗</em> (network views) opens it in <a href="https://normaviz.org/" target="_blank" rel="noopener noreferrer">NORMA</a>, with terms drawn as annotated groups. Only the shown network (term names, IDs and genes) is sent; the link opens in a new tab and expires after a while (one hour for Arena3D).</p>
        <h3>Pathway maps and gene pages</h3>
        <p>Open a selected term in the <em>Selected</em> panel to see its pathway links: for KEGG terms, the organism's KEGG map with <strong>your genes highlighted</strong> (gene IDs are mapped through NCBI Gene to KEGG with the KEGG API; for ranked lists, red = positive score, blue = negative); for Reactome terms, the Pathway Browser with your genes flagged; WikiPathways diagrams; QuickGO, AmiGO and HPO pages; and searches for the same biology in KEGG, Reactome, WikiPathways and Pathway Commons. Click any gene to open it in the Ensembl or UCSC genome browser, NCBI Gene, UniProt and, for human genes, GeneCards, gnomAD, ClinVar, GTEx and the Human Protein Atlas.</p>
        <h3>Search syntax</h3>
        <p>Words must all appear (<code>immune response</code>); use quotes for exact phrases (<code>"cell cycle"</code>), <code>OR</code> for alternatives (<code>apoptosis OR autophagy</code>) and a minus sign to exclude (<code>-mitotic</code>).</p>
        <h3>Selecting and exporting</h3>
        <p>Click (shift-click to add) or box/lasso-select terms in any view; they collect in the <em>Selected</em> panel with their genes and database links, and stay selected when you switch views. Below them, the <strong>Genes</strong> list ranks every gene of the selected terms — by how many selected terms contain it, by how many significant terms contain it (the default when one term is selected), by its score in your list (ranked lists) or by its position in your list. Click a gene to see its terms, open it in gene databases, or add every significant term that contains it.</p>
        <p><strong>Themes</strong> group overlapping terms that share most of their genes (Jaccard index ≥ the cutoff, Louvain communities), name each theme after its most significant term and summarise it with keywords. The <strong>word cloud</strong> sizes each word of the term names by the summed −log10 p of the terms that use it; click a word to select those terms.</p>
        <p>Every table lists the genes behind each term (all of them, as in Flame 2.0). Export tables, selections and analysis results as <strong>Excel</strong> workbooks — one sheet with all terms plus one sheet per annotation source (GO:BP, KEGG, Reactome…), with the genes of every term — or as CSV, HTML or print/PDF. Every figure has an <strong>⤓ Image</strong> button: save it as PNG, SVG (vector), PDF, JPEG or WebP, at 1×–4× resolution (96–384 dpi), on the theme colour, white or a transparent background, with or without its legend and the selection highlighting. Changes to filters, view settings and the selection can be undone and redone with the ↶ ↷ buttons or <kbd>Ctrl</kbd>/<kbd>⌘</kbd>+<kbd>Z</kbd> and <kbd>Shift</kbd>+<kbd>Ctrl</kbd>/<kbd>⌘</kbd>+<kbd>Z</kbd>.</p>
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
          <li><strong>Overlap (UpSet)</strong> — one column per combination of runs, with the number of terms found by exactly that combination. Click a column to select its terms.</li>
          <li><strong>Chord</strong> — arcs are runs; ribbon width = number of terms two runs share.</li>
          <li><strong>Circos (bundled)</strong> — one sector per run holding the terms it found, in the same order in every run; bundled links join the same term across runs, so consensus terms form thick bundles and run-specific terms have none.</li>
          <li><strong>Table</strong> — every term with the runs that found it, each run's p-value and genes (or its rank in that run), the agreement and a <strong>consensus rank</strong>: each run ranks its terms by p-value, and Robust Rank Aggregation (Kolde et al. 2012) combines those ranks, so terms ranked high by several runs come first. A run that did not report a term counts it as last.</li>
          <li><strong>Ranks</strong> — a bump chart of the best consensus terms: one line per term, its rank in each run (log scale). Flat lines at the top = terms all runs agree on; lines dropping to “not found” = terms some runs miss. Click a line to select the term. <Example kind="compare-tools">Compare six tools</Example></li>
          <li><strong>Themes</strong> — terms grouped into themes across runs, with the runs that support each theme (see <em>Results</em>).</li>
          <li><strong>Bubble plot</strong> — mean fold enrichment against combined significance; bubble size = agreement.</li>
          <li><strong>Heatmap</strong> — −log10 p per term and run; blank cells mean the run did not report the term.</li>
        </ul>
        <h3>Statistics</h3>
        <p>The <em>Statistics</em> tab measures how far the runs agree — whether they are different tools on the same list, the same tool on different lists, or different strategies (ORA vs GSEA):</p>
        <ul>
          <li><strong>Overlap</strong> — Jaccard index (shared ÷ all terms) and overlap coefficient (shared ÷ smaller set), as a heatmap for every pair of runs; an <strong>MDS map</strong> places similar runs close together.</li>
          <li><strong>Rank agreement</strong> — Spearman ρ and Kendall τ on the terms both runs found: do they rank them in the same order? The <strong>concordance scatter</strong> plots −log10 p of one run against the other (box or lasso-select terms).</li>
          <li><strong>Concordance at the top</strong> — of each run's k best terms, the share the other run also has in its top k, for growing k.</li>
          <li><strong>Gene agreement</strong> — for shared terms, how similar the reported genes are.</li>
          <li><strong>Tests</strong> — McNemar (2 runs) or Cochran's Q (3+) for differences in which terms are detected; Kruskal–Wallis for term size (broad vs specific) and for significance; a paired Wilcoxon test per pair for systematically stronger p-values. Pairwise p-values are Benjamini–Hochberg adjusted.</li>
          <li><strong>Terms per source</strong>, <strong>term-size</strong> box plots and an <strong>agreement</strong> histogram — click or select to collect the terms.</li>
        </ul>
        <p>Terms are matched by identifier, so runs that use different term IDs for the same pathway (e.g. Enrichr names) overlap less. Tests use only terms found by at least one compared run.</p>
        <p>Use the <em>Runs</em> filter to include or exclude runs, and <em>Found by all</em> to keep only consensus terms. <em>Arena3D ↗</em> opens the comparison in 3D, with one layer per run, a layer of genes, and links between the same term in different runs.</p>
      </>
    ),
  },
  {
    id: 'utilities',
    title: 'Quick actions',
    body: (
      <>
        <p>The <strong>Gene tools</strong> tab (and each list's <em>⋯</em> menu) holds tools that inspect or transform a list without running an enrichment. The first three share one lookup at MyGene.info, which recognises gene symbols, older and alternative names, Ensembl, Entrez and UniProt IDs.</p>
        <h3>Annotate a list</h3>
        <p>A table of every gene with its current symbol, full name, gene type (protein-coding, ncRNA, pseudogene…), chromosome position and band, and Ensembl and Entrez IDs, with links to GeneCards, Ensembl and NCBI. Search it, copy the symbols, or export it as Excel or TSV.</p>
        <h3>Check and clean a list</h3>
        <p>Counts the IDs that are recognised as given, those that are older or alternative names of a gene (e.g. <code>MLL</code>, now <code>KMT2A</code>), those not found, and those that point to the same gene as another ID. Choose what to fix — remove the IDs not found, replace old names with the current symbol (lists of gene symbols only), keep one ID per gene — and save the cleaned list. The original list is kept.</p>
        <h3>Filter a list</h3>
        <p>Keep genes by type (<em>Protein-coding only</em> is one click) and by chromosome, and optionally remove placeholder and antisense names such as <code>LOC…</code>, <code>LINC…</code>, <code>C1orf…</code> and <code>…-AS1</code>. Save what is left as a new list. Useful after fetching genes from ClinVar or a disease, which can include such entries. For a ranked list, the cleaned or filtered list is saved without the scores.</p>
        <h3>Convert IDs</h3>
        <p>Translates a list to another identifier type with g:Convert. <em>Best only</em> keeps one target per input; <em>All</em> keeps every mapping. Select rows and save them as a new list.</p>
        <h3>Find orthologs</h3>
        <p>Maps a list to the corresponding genes of another organism with g:Orth — useful for running mouse data through human-only resources.</p>
      </>
    ),
  },
  {
    id: 'regulators',
    title: 'Regulators, drugs and more',
    body: (
      <>
        <p>The <em>Regulators &amp; drugs</em> section of the workspace goes beyond term enrichment. Each analysis runs in the background with a progress bar; results are stored with your workspace and included when you save the session.</p>
        <h3>Transcription factors and kinases</h3>
        <p><strong>ChEA3</strong> (Keenan et al. 2019, <em>Nucleic Acids Res</em> 47:W212) ranks transcription factors whose targets — from ChIP-seq (ENCODE, ReMap, literature), co-expression (GTEx, ARCHS4) and co-occurrence — overlap your list. <strong>KEA3</strong> (Kuleshov et al. 2021, <em>Nucleic Acids Res</em> 49:W304) does the same for kinases from substrate, interaction and co-expression data. The <em>mean rank</em> integrates the evidence libraries: lower is stronger. Click or box-select bars to see the factors' target genes in your list, ranked, and save them as a list. Both use human symbols; map other organisms with <em>Find orthologs</em> first.</p>
        <p className="row wrap"><Example kind="tf">Transcription factors example</Example> <Example kind="kinase">Kinases example</Example></p>
        <h3>Drugs and perturbations</h3>
        <p>Give up- and down-regulated genes (for example from a volcano plot). Flame compares them with LINCS L1000 consensus signatures (Subramanian et al. 2017, <em>Cell</em> 171:1437) of compounds or CRISPR knock-outs via Enrichr. <strong>Reverse</strong> = perturbations that push the genes the other way (up genes go down, down genes go up) — candidate treatments; <strong>mimic</strong> = perturbations that cause a similar response. Score = −log10 adjusted p of the up genes plus that of the down genes. These are hypotheses from cell lines, not treatment advice.</p>
        <p><Example kind="drugs">Drug signatures example</Example></p>
        <h3>Cell types</h3>
        <p>An ordinary enrichment run: in the New run panel press <em>Cell types</em> under <em>Where to look</em>. It tests your list against marker-gene collections: PanglaoDB, CellMarker 2024, Azimuth, Tabula Sapiens (Enrichr) and MSigDB C8 (clusterProfiler). Useful for lists from single-cell clusters or deconvolution.</p>
        <p><Example kind="celltypes">Cell types example (NK cells, PBMC 3k)</Example></p>
        <h3>Literature enrichment</h3>
        <p>An ordinary enrichment run: in the New run panel press <em>Literature</em> under <em>Where to look</em>. The sources are <strong>PubMed</strong> (STRING: articles whose text mentions several of your genes), <strong>GeneSigDB</strong> (gene signatures published in papers) and <strong>Rummagene</strong> (Clarke et al. 2024: about 640,000 gene sets mined from supplementary tables of PubMed Central articles), the last two through Enrichr. Each term links to the article on PubMed or PubMed Central, so the result reads as “papers that reported a gene set like yours”.</p>
        <p><Example kind="literature">Literature example</Example></p>
        <h3>Protein network and modules</h3>
        <p>Shows STRING interactions among the proteins of a list (up to 500). Edges are coloured by the strongest evidence type, or drawn thicker for higher confidence. Select proteins in the network and save them as a list; download the network as TSV, Excel or an image, or open it in STRING or in Arena3D (each evidence type becomes its own edge channel).</p>
        <p><strong>Find modules</strong> splits the network into modules with Louvain community detection, runs g:Profiler on each module and colours the network by module, so a long list becomes a few functional units. Each module card shows its top terms; click a module to select its proteins, see all of its enriched terms in a table and save it as a list. The Excel export includes the modules and every module's terms.</p>
        <p><Example kind="ppi">Protein network example</Example></p>
        <h3>Robustness check</h3>
        <p>Tests whether an enrichment is solid, using MSigDB gene sets locally: <strong>stability</strong> = share of 50 subsamples (80% of the genes) in which a term stays significant; <strong>random rate</strong> = share of 50 random lists of the same size in which it is significant; and the number of terms at several FDR thresholds. Few stable terms, or random lists that give almost as many terms as yours, mean the result should be treated with caution.</p>
        <p><Example kind="robustness">Robustness example</Example></p>
        <h3>Pathway activity per sample</h3>
        <p>In <em>From expression data → Pathway activity</em>, every sample gets a score per pathway: <strong>ssGSEA</strong> (Barbie et al. 2009, rank-based), a <strong>combined z-score</strong> (Lee et al. 2008) or <strong>PROGENy</strong> (Schubert et al. 2018; 14 signalling pathways from footprint genes). The heatmap shows the most variable pathways, scaled per pathway.</p>
        <p><Example kind="activity">Pathway activity example (airway RNA-seq)</Example></p>
      </>
    ),
  },
  {
    id: 'gsea',
    title: 'Ranked lists (GSEA)',
    body: (
      <>
        <p>Over-representation analysis (ORA) asks whether a function is over-represented among a <em>cut-off</em> list of genes. Gene set enrichment analysis (GSEA) needs no cut-off: it takes <em>all</em> measured genes ranked by a score and asks whether the genes of a function sit unusually high or low in the ranking. It picks up coordinated, modest changes that a cut-off would miss.</p>
        <h3>Running GSEA</h3>
        <ol>
          <li>Add a ranked list: upload IDs with scores, or <em>Save whole table as ranked list</em> from a volcano plot. GWAS gene scores from a <Link to="/help/variants">variant analysis</Link> also work.</li>
          <li>In <strong>+ New run</strong>, switch to <em>Ranked list (GSEA)</em>. Only ranked lists and GSEA-capable tools (clusterProfiler, using fgsea) are offered.</li>
          <li>Choose MSigDB collections and run. The background option is hidden: in GSEA the universe is the ranked list itself.</li>
        </ol>
        <h3>Reading GSEA results</h3>
        <dl className="details">
          <dt>NES</dt><dd>Normalised enrichment score. Positive = the set's genes are concentrated at the top of your ranking (e.g. up-regulated); negative = at the bottom. Its size is comparable across sets.</dd>
          <dt>P-value</dt><dd>Corrected for multiple testing with the method you chose (Benjamini–Hochberg by default).</dd>
          <dt>Genes found</dt><dd>The <em>leading edge</em>: the genes that account for the enrichment signal.</dd>
        </dl>
        <p>GSEA runs open with NES on the x-axis of the dot plot and bar chart, and the table gains an NES column. Sets with fewer than 10 or more than 500 genes in your list are skipped, and the run reports how many.</p>
      </>
    ),
  },
  {
    id: 'variants',
    title: 'Variants and GWAS',
    body: (
      <>
        <p>For geneticists who start from variants rather than genes. Choose <strong>From variants or GWAS</strong> on the workspace's Input tab; analyses you have already run are listed under the input options.</p>
        <h3>Input</h3>
        <ul>
          <li><strong>Variant list</strong> — dbSNP rsIDs, VCF lines (CHROM POS ID REF ALT), <code>chr:pos:ref:alt</code> or HGVS notation, one per line. <Example kind="variants">Example</Example></li>
          <li><strong>GWAS summary statistics</strong> — a table with columns <code>SNP</code>, <code>CHR</code>, <code>BP</code> and <code>P</code> (tab, comma or space separated). <Example kind="gwas">Example</Example></li>
        </ul>
        <p>Up to 2,000 variants per analysis. Human and several model organisms are supported (the organisms Ensembl annotates). Annotation takes about 1–2 seconds per variant.</p>
        <h3>What Flame does</h3>
        <p>Each variant is annotated with the <a href="https://rest.ensembl.org/" target="_blank" rel="noopener noreferrer">Ensembl Variant Effect Predictor</a>: the most severe consequence, its impact (HIGH, MODERATE, LOW, MODIFIER), the affected genes, population frequency and ClinVar significance. Known associations with traits and diseases (GWAS Catalog, ClinVar, OMIM and others) are retrieved for rsIDs.</p>
        <h3>The report</h3>
        <ul>
          <li><strong>Summary cards</strong> and bar charts of impact, consequence, ClinVar significance and chromosome.</li>
          <li><strong>Manhattan plot</strong> (GWAS input) — −log10 p along the genome; the dotted red line marks genome-wide significance (5 × 10⁻⁸).</li>
          <li><strong>Variants → genes → traits</strong> Sankey diagram, the most frequent traits first.</li>
          <li><strong>Genome Circos</strong> — chromosomes around a circle with your variants in genomic order, outer bars for −log10 p (GWAS) or impact, and bundled links between variants associated with the same trait. Click a trait to focus on it.</li>
          <li><strong>Traits table</strong> — click a trait to filter the variants. <strong>Variants table</strong> — filter by impact, download as CSV. Click a variant or gene to open it in the Ensembl, UCSC or NCBI genome browsers, dbSNP, gnomAD, ClinVar, the GWAS Catalog or LitVar.</li>
          <li><strong>Arena3D ↗</strong> — the same three layers (variants coloured by impact, genes, traits) as a 3D network.</li>
        </ul>
        <h3>From variants to enrichment</h3>
        <p>In the <em>Genes</em> panel, save the affected genes as a list and run any enrichment tool on it. For GWAS input you can also save a <em>ranked list</em> (gene score = −log10 of its best variant p-value) and run <Link to="/help/gsea">GSEA</Link>, which uses all loci rather than only genome-wide significant ones.</p>
      </>
    ),
  },
  {
    id: 'display',
    title: 'Colours, undo and accessibility',
    body: (
      <>
        <h3>Chart palettes</h3>
        <p>Every visualization has its own palette menu (◐) in its toolbar — on each results tab, each comparison view, the variant report and the analysis dialogs with plots (the dialogs for adding a list do not show it). The choice applies only to that visualization and is remembered for it in this browser, so one figure can be colour-blind safe or grayscale while the others keep their colours:</p>
        <ul>
          <li><strong>Flame</strong> — the default colours, matching the logo; annotation sources keep their usual colours.</li>
          <li><strong>Colour-blind safe</strong> — Okabe–Ito categories and the Cividis scale, distinguishable with all common colour-vision deficiencies.</li>
          <li><strong>Vivid</strong> — Tableau 10 and Plasma, high contrast for slides.</li>
          <li><strong>Pastel</strong> — soft colours for posters and backgrounds.</li>
          <li><strong>Grayscale (print)</strong> — shades of grey for black-and-white printing.</li>
        </ul>
        <p>Continuous scales always run from light (weak) to dark (strong). Exported figures use the palette of that visualization. The ☾ button switches between light and dark themes.</p>
        <h3>Undo and redo</h3>
        <p>On result and comparison pages, ↶ and ↷ (or <kbd>Ctrl</kbd>/<kbd>⌘</kbd>+<kbd>Z</kbd>, <kbd>Shift</kbd>+<kbd>Ctrl</kbd>/<kbd>⌘</kbd>+<kbd>Z</kbd>, <kbd>Ctrl</kbd>+<kbd>Y</kbd>) step through changes to the view, filters, settings and selection. In the workspace, deleting or renaming a list, removing a run, or clearing all data shows an <em>Undo</em> button for a few seconds before it takes effect.</p>
        <h3>Selecting in charts</h3>
        <p>Charts that hold items you can select show a small switch: <strong>Box select</strong>, <strong>Lasso</strong>, <strong>Zoom</strong> and <strong>Pan</strong> (Plotly charts; the gene bar charts of the fetch dialogs are simply clicked), or <strong>Click / drag</strong>, <strong>Box select</strong> and <strong>Lasso</strong> (networks, Circos and hive plots). Hold Shift to add to the selection. Selections feed the <em>Selected</em> panel (results and comparisons), the variants table (variant reports) or a <em>Save as list</em> box (inputs and the protein network).</p>
        <h3>Keyboard and screen readers</h3>
        <p>All controls can be reached with the keyboard; <em>Skip to content</em> appears on the first Tab. Charts have text alternatives, and every chart's data can be downloaded as a table.</p>
      </>
    ),
  },
  {
    id: 'session',
    title: 'Sessions: save, load and status',
    body: (
      <>
        <p>Your workspace link is the only key to your data — keep it to yourself unless you want to share results. A workspace is deleted after 30 days without use; every visit restarts the clock.</p>
        <h3>Status</h3>
        <p>The bar under the workspace link shows whether everything is in your last session file (<em>Saved</em>), whether you changed anything since (<em>Changes since last save</em>) or never saved (<em>Not saved to a file</em>), how many jobs are still running, and until when the workspace is kept. The <em>Status</em> tab of the Session dialog adds counts of lists, runs, analyses and gene sets, and the amount of stored data.</p>
        <h3>Save</h3>
        <p><em>Save</em> (or <kbd>Ctrl</kbd>/<kbd>⌘</kbd>+<kbd>S</kbd>) downloads a session file. Choose what to include — gene lists (with scores for ranked lists), custom gene sets, finished enrichment results (all or selected runs), variant and GWAS analyses, and your display settings (theme, chart palette, network style, visible tabs) — the file name, and whether to compress it (.json.gz, usually 5–10× smaller). Running jobs are not saved.</p>
        <h3>Load</h3>
        <p><em>Load</em> accepts .json and .json.gz session files (choose or drop the file). Flame shows what the file contains and when it was saved; pick the parts to load and where: <strong>add</strong> to this workspace, <strong>replace</strong> its contents (undoable for a few seconds), or open it in a <strong>new workspace</strong>. Session files hold plain data only and are checked on loading; anything that cannot be read is skipped and reported. Files up to 25 MB can be loaded.</p>
        <h3>Clear or delete</h3>
        <p>Clear all data (undoable for a few seconds) or delete the workspace and its link permanently. See the <Link to="/privacy">privacy page</Link> for details.</p>
      </>
    ),
  },
  {
    id: 'faq',
    title: 'FAQ',
    body: (
      <dl className="faq">
        <dt>A run finished with no significant terms. Why?</dt><dd>The list may be small or functionally diverse, the IDs may not match the organism, or the chosen sources may not cover your genes. Check <em>Run details</em> for unmapped IDs, add sources, or try another tool.</dd>
        <dt>Why do tools give different results?</dt><dd>They use different database versions, statistics and backgrounds. Comparing runs shows where they agree; consensus terms are the most robust.</dd>
        <dt>Fold enrichment is missing.</dt><dd>Some tools do not report the background size needed to compute it. Provide a custom background list, or use gene ratio instead.</dd>
        <dt>I have SNPs or GWAS results, not genes.</dt><dd>Use <Link to="/help/variants">Variants and GWAS</Link>: it annotates the variants, finds the genes and traits they affect, and turns them into a gene list or ranked list for enrichment.</dd>
        <dt>I work with microRNAs. What can Flame do?</dt><dd>Two things. <strong>Starting from microRNAs</strong> (e.g. those changed in your experiment): choose <em>From microRNAs</em> on the Input tab, paste them, and Flame finds their predicted target genes (miRDB via MSigDB); keep the genes hit by several of your microRNAs and run enrichment on them. <strong>Starting from genes</strong>: run a gene list (or a ranked list with GSEA) against the microRNA sources — miRTarBase in g:Profiler, miRNA annotations in GeneCodis, and MSigDB miRDB target sets in clusterProfiler — to see which microRNAs may regulate it. <Example kind="mirna">Open the microRNA example</Example></dd>
        <dt>Should I use ORA or GSEA?</dt><dd>Use ORA when you have a clear list of genes (e.g. significant hits). Use GSEA when you have scores for all measured genes; it needs no cut-off and detects subtle, coordinated shifts. See <Link to="/help/gsea">Ranked lists (GSEA)</Link>.</dd>
        <dt>Are the figures colour-blind friendly?</dt><dd>Choose <em>Colour-blind safe</em> in the ◐ palette menu of the figure; see <Link to="/help/display">Colours, undo and accessibility</Link>.</dd>
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
