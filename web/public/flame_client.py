#!/usr/bin/env python3
"""
Flame 3.0 API: command-line client and template.

Every command creates a private workspace and
prints its link, so you can open the same results in the web app.

    pip install requests

    # Over-representation analysis of gene lists (one ID per line), several tools
    python flame_client.py enrich genes_a.txt genes_b.txt --tools gprofiler,string --stats --pathways 5

    # GSEA of a ranked list (two columns: gene<TAB>score)
    python flame_client.py gsea ranked.tsv --sources HALLMARK,KEGG,REAC

    # Variants (rsIDs, VCF lines, chr:pos:ref:alt, HGVS) or GWAS summary statistics
    python flame_client.py variants gwas.tsv --enrich

    # microRNAs: predicted targets hit by at least 2 of them, then enrichment
    python flame_client.py mirna mirnas.txt --min-mirnas 2

    # Expression matrix (genes x samples): two-group comparison, then enrichment
    python flame_client.py expression matrix.tsv --group-a t1,t2,t3 --group-b c1,c2,c3

    # Upstream regulators, drug signatures, network modules, robustness of a list
    python flame_client.py analysis tf genes.txt
    python flame_client.py analysis drugs up.txt --down down.txt
    python flame_client.py analysis robustness genes.txt --sources GO:BP,KEGG,REAC
    python flame_client.py analysis metabolites metabolites.txt

    # Fetch a gene list from public resources (then optionally enrich it)
    python flame_client.py fetch disease "asthma" --top 200 --enrich
    python flame_client.py fetch pubmed "glucocorticoid receptor airway smooth muscle"
    python flame_client.py fetch geo GSE52778 --signature 1
    python flame_client.py fetch dbsnp "APOE[gene] AND pathogenic[clinical_significance]"

Add --server https://YOUR-FLAME-SERVER to use another Flame server, and
--save-session FILE.json to download the whole workspace as a session file.
"""
import argparse
import csv
import json
import sys
import time

import requests

TERM_FIELDS = ["source", "id", "name", "pvalue", "nes", "intersection", "termSize", "coverage", "foldEnrichment", "genes", "link"]


class Flame:
    def __init__(self, server):
        self.base = server.rstrip("/")
        self.api = self.base + "/api/v1"
        self.http = requests.Session()
        self.ws = self.post("/workspace")["id"]
        self.http.headers["X-Flame-Workspace"] = self.ws
        print(f"Workspace: {self.base}/w/{self.ws}")

    def _check(self, r):
        if r.status_code >= 400:
            try:
                msg = r.json().get("error", r.text)
            except ValueError:
                msg = r.text
            raise SystemExit(f"Flame API error {r.status_code}: {msg}")
        return r.json()

    def get(self, path, **params):
        return self._check(self.http.get(self.api + path, params=params, timeout=300))

    def post(self, path, body=None, **params):
        return self._check(self.http.post(self.api + path, json=body, params=params, timeout=300))

    # ---- helpers -------------------------------------------------------
    def add_list(self, name, text=None, ids=None, organism=9606, ranked=False):
        body = {"name": name, "organismTaxId": organism, "uniqueName": True}
        if ids is not None:
            body["ids"] = ids
        else:
            body["text"] = text
            if ranked:
                body.update({"ranked": True, "skipFirstRow": not _first_score_is_number(text)})
        lst = self.post("/lists", body)
        print(f"List '{lst['name']}': {lst['size']} IDs{' (ranked)' if lst.get('ranked') else ''}")
        return lst

    def start_runs(self, list_id, tools, sources, threshold=0.05, paradigm="ORA"):
        res = self.post("/runs", {"listId": list_id, "tools": tools, "datasources": sources, "threshold": threshold, "paradigm": paradigm})
        for s in res.get("skipped", []):
            print(f"  skipped {s['tool']}: {s['reason']}")
        return res["created"] + res["reused"]

    def wait_runs(self, run_ids):
        print("Waiting for runs", end="", flush=True)
        while True:
            runs = [r for r in self.get("/runs") if r["id"] in run_ids]
            if all(r["status"] in ("complete", "error") for r in runs):
                print()
                return runs
            print(".", end="", flush=True)
            time.sleep(3)

    def save_terms(self, runs):
        done = []
        for run in runs:
            if run["status"] == "error":
                print(f"{run['title']}: failed ({run.get('error')})")
                continue
            terms = self.post(f"/runs/{run['id']}/view", {"view": "table", "top": "all"})["terms"]
            out = f"flame_{run['id']}.csv"
            with open(out, "w", newline="", encoding="utf-8") as fh:
                w = csv.DictWriter(fh, fieldnames=TERM_FIELDS, extrasaction="ignore")
                w.writeheader()
                w.writerows(terms)
            print(f"{run['title']} on {run['listName']}: {len(terms)} terms -> {out}")
            if terms:
                done.append((run, terms))
        return done

    def pathways(self, finished, n):
        print(f"\nPathway maps (top {n} KEGG / Reactome terms per run):")
        for run, terms in finished:
            picked = [t for t in terms if "KEGG" in t["source"].upper() or "REAC" in t["source"].upper()][:n]
            for t in picked:
                links = self.post(f"/runs/{run['id']}/pathway-links", {"termId": t["id"]})["links"]
                maps = [l for l in links if l["group"] == "Pathway maps"]
                if maps:
                    print(f"  {run['title']}: {t['name']} ({t['id']})\n    {maps[0]['label']}: {maps[0]['url']}")

    def compare(self, run_ids, stats=False):
        cmp = self.post("/comparisons", {"runIds": run_ids, "view": "bubble", "top": 10, "sortBy": "Agreement"})
        print("\nTop terms across runs (agreement, combined p):")
        for t in cmp["terms"]:
            print(f"  {t['agreement']} runs  p={t['activeCombPvalue']:.2e}  {t['name']} ({t['id']})")
        if stats:
            s = self.post("/comparisons/statistics", {"runIds": run_ids})
            label = {r["id"]: r["label"] for r in s["runs"]}
            print("\nAgreement between runs:")
            for p in s["pairs"]:
                rho = "-" if p.get("spearman") is None else f"{p['spearman']:.2f}"
                print(f"  {label[p['a']]} vs {label[p['b']]}: {p['shared']}/{p['union']} shared (Jaccard {p['jaccard']:.2f}), Spearman rho {rho}")
            for name, t in s["tests"].items():
                if t:
                    print(f"  {name}: {t['method']}, p = {t['p']:.2e}")
            with open("flame_comparison_statistics.json", "w", encoding="utf-8") as fh:
                json.dump(s, fh, indent=1)
            print("  full statistics -> flame_comparison_statistics.json")

    def save_session(self, path):
        data = self.get("/workspace/export")
        with open(path, "w", encoding="utf-8") as fh:
            json.dump(data, fh)
        c = data.get("contents", {})
        print(f"Session saved to {path} ({c}). Load it in the web app with Session -> Load.")


def _first_score_is_number(text):
    first = text.strip().splitlines()[0].replace(",", "\t").split()
    try:
        float(first[1])
        return True
    except (IndexError, ValueError):
        return False


def _read(path):
    with open(path, encoding="utf-8") as fh:
        return fh.read()


def _write_rows(path, rows, fields):
    with open(path, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=fields, extrasaction="ignore")
        w.writeheader()
        for r in rows:
            w.writerow({k: (";".join(map(str, v)) if isinstance(v, list) else v) for k, v in r.items()})
    print(f"  -> {path} ({len(rows)} rows)")


# ---- commands --------------------------------------------------------------
def cmd_enrich(fl, a):
    run_ids = []
    for path in a.files:
        lst = fl.add_list(path, text=_read(path), organism=a.organism)
        run_ids += fl.start_runs(lst["id"], a.tools.split(","), a.sources.split(","), a.threshold)
    finished = fl.save_terms(fl.wait_runs(run_ids))
    if a.pathways:
        fl.pathways(finished, a.pathways)
    if len(finished) >= 2:
        fl.compare([r["id"] for r, _ in finished], stats=a.stats)


def cmd_gsea(fl, a):
    lst = fl.add_list(a.file, text=_read(a.file), organism=a.organism, ranked=True)
    runs = fl.wait_runs(fl.start_runs(lst["id"], ["clusterprofiler"], a.sources.split(","), a.threshold, paradigm="GSEA"))
    finished = fl.save_terms(runs)
    for run, terms in finished:
        print("\nStrongest gene sets (NES > 0: top of your ranking; NES < 0: bottom):")
        for t in sorted(terms, key=lambda t: -abs(t.get("nes") or 0))[:10]:
            print(f"  NES {t['nes']:+.2f}  p={t['pvalue']:.2e}  {t['name']}")
    if a.pathways:
        fl.pathways(finished, a.pathways)


def cmd_variants(fl, a):
    start = fl.post("/variants", {"text": _read(a.file), "organismTaxId": a.organism, "name": a.file})
    print(f"Variant analysis ({start['mode']}): {start['inputCount']} variants; annotating with Ensembl VEP", end="", flush=True)
    while True:
        st = fl.get(f"/variants/{start['id']}")
        if st["status"] in ("complete", "error"):
            break
        print(".", end="", flush=True)
        time.sleep(4)
    print()
    if st["status"] == "error":
        raise SystemExit(f"Analysis failed: {st.get('error')}")
    r = st["result"]
    with open("flame_variants.csv", "w", newline="", encoding="utf-8") as fh:
        cols = ["id", "location", "alleles", "consequence", "impact", "gene", "gnomadAF", "clinicalSignificance", "pvalue"]
        w = csv.DictWriter(fh, fieldnames=cols, extrasaction="ignore")
        w.writeheader()
        w.writerows(r["variants"])
    print(f"{r['annotatedCount']} of {r['inputCount']} variants annotated -> flame_variants.csv")
    print(f"{len(r['genes'])} genes affected; top traits:")
    for t in r["summary"]["traits"][:10]:
        print(f"  {t['variants']} variants  {t['trait']}")
    if a.enrich:
        genes = [g["gene"] for g in r["genes"] if g["gene"]]
        lst = fl.add_list("Genes of the variants", ids=genes, organism=a.organism)
        fl.save_terms(fl.wait_runs(fl.start_runs(lst["id"], a.tools.split(","), a.sources.split(","), a.threshold)))


def cmd_mirna(fl, a):
    r = fl.post("/inputs/mirna/targets", {"text": _read(a.file), "organismTaxId": a.organism, "minMirnas": a.min_mirnas})
    for m in r["mirnas"]:
        print(f"  {m['input']}: {m['targets']} predicted targets")
    if r["unmatched"]:
        print("  not found in miRDB:", ", ".join(r["unmatched"]))
    genes = [g["gene"] for g in r["genes"] if g["count"] >= a.min_mirnas]
    print(f"{len(genes)} of {r['totalTargets']} targets are hit by at least {a.min_mirnas} microRNAs ({r['source']})")
    if len(genes) < 5:
        raise SystemExit("Too few target genes; lower --min-mirnas.")
    lst = fl.add_list(f"miRNA targets (>={a.min_mirnas})", ids=genes[:3000], organism=a.organism)
    fl.save_terms(fl.wait_runs(fl.start_runs(lst["id"], a.tools.split(","), a.sources.split(","), a.threshold)))


def cmd_expression(fl, a):
    text = _read(a.file)
    de = fl.post("/inputs/expression/de", {"text": text, "fileName": a.file, "groupA": a.group_a.split(","), "groupB": a.group_b.split(",")})
    with open("flame_expression_de.tsv", "w", encoding="utf-8") as fh:
        fh.write(de["text"])
    print(f"{de['significant']} of {de['n']} genes differ (BH p <= 0.05) -> flame_expression_de.tsv. {de['note']}")
    rows = [line.split("\t") for line in de["text"].splitlines()[1:]]
    sig = [g for g, lfc, p, padj in rows if float(padj) <= 0.05 and abs(float(lfc)) >= a.min_lfc]
    if len(sig) < 5:
        raise SystemExit("Too few significant genes for enrichment.")
    lst = fl.add_list("Differentially expressed genes", ids=sig, organism=a.organism)
    fl.save_terms(fl.wait_runs(fl.start_runs(lst["id"], a.tools.split(","), a.sources.split(","), a.threshold)))


def cmd_analysis(fl, a):
    body = {"type": a.type}
    if a.type == "metabolites":
        body.update({"text": _read(a.file), "name": a.file, "organismTaxId": a.organism, "threshold": a.threshold})
    elif a.type == "drugs":
        up = fl.add_list(a.file, text=_read(a.file), organism=a.organism)
        body.update({"upListId": up["id"], "kind": a.kind})
        if a.down:
            body["downListId"] = fl.add_list(a.down, text=_read(a.down), organism=a.organism)["id"]
    else:
        body["listId"] = fl.add_list(a.file, text=_read(a.file), organism=a.organism)["id"]
        if a.type == "modules":
            body["score"] = a.score
        if a.type == "robustness":
            body["sources"] = a.sources.split(",")
    task = fl.post("/analyses", body)
    print(f"{task['label']}", end="", flush=True)
    while True:
        st = fl.get(f"/analyses/{task['id']}")
        if st["status"] != "running":
            break
        print(".", end="", flush=True)
        time.sleep(3)
    print()
    if st["status"] == "error":
        raise SystemExit(f"Failed: {st.get('error')}")
    r = st.get("result") or {}
    if a.type in ("tf", "kinase"):
        for x in r["regulators"][:15]:
            print(f"  {x['rank']:>3}  {x['name']:<10} mean rank {x['score']:<8} {len(x['overlap'])} genes")
        _write_rows(f"flame_{a.type}.csv", [{**x, "overlap": x["overlap"]} for x in r["regulators"]], ["rank", "name", "score", "overlap"])
    elif a.type == "drugs":
        for d in ("reverse", "mimic"):
            print(f"  {d}: " + ", ".join(x["agent"] for x in r[d][:10]))
            _write_rows(f"flame_drugs_{d}.csv", r[d], ["agent", d, "upInUp", "downInDown", "upInDown", "downInUp"])
    elif a.type == "modules":
        for m in r["modules"]:
            print(f"  module {m['id']}: {m['size']} proteins - {m['label']}")
        _write_rows("flame_modules.csv", [{"module": m["id"], "label": m["label"], "size": m["size"], "genes": m["genes"]} for m in r["modules"]],
                    ["module", "label", "size", "genes"])
    elif a.type == "robustness":
        print(f"  {r['significant']} significant terms, {r['stable']} stable (>= 80% of subsamples), median {r['medianRandom']} in random lists")
        _write_rows("flame_robustness.csv", r["terms"], ["id", "name", "source", "padj", "stability", "randomRate"])
    elif a.type == "metabolites":
        print(f"  Became enrichment run {st.get('runId')}")
        fl.save_terms([r for r in fl.get("/runs") if r["id"] == st.get("runId")])


def cmd_fetch(fl, a):
    q = a.query
    if a.source == "disease":
        hits = fl.post("/inputs/disease/search", {"query": q})
        if not hits["diseases"]:
            raise SystemExit("No disease matches.")
        d = hits["diseases"][0]
        print(f"Disease: {d['name']} ({d['id']})")
        res = fl.post("/inputs/disease/genes", {"source": "opentargets", "id": d["id"], "size": a.top})
        genes = [g["gene"] for g in res["genes"]]
        scores = [g["score"] for g in res["genes"]]
        name = f"{d['name']} (Open Targets)"
    elif a.source == "pubmed":
        res = fl.post("/inputs/pubmed/genes", {"query": q, "organismTaxId": a.organism, "max": a.articles})
        print(f"{res['articles']} articles")
        res["genes"] = res["genes"][: a.top]
        genes = [g["gene"] for g in res["genes"]]
        scores = [g["articles"] for g in res["genes"]]
        name = f"PubMed: {q[:50]}"
    elif a.source == "geo":
        found = fl.post("/inputs/geo/search", {"query": q})
        for i, s_ in enumerate(found["sets"][:20], 1):
            c = s_.get("contrast") or {}
            print(f"  {i:>2}. {s_['gse']} {s_['direction']:<4} {s_['size']:>5} genes  {c.get('b', s_['term'])[:50]} vs {c.get('a', '')[:40]}")
        if not found["sets"]:
            raise SystemExit("No GEO signatures match.")
        pick = found["sets"][max(0, a.signature - 1)]
        res = fl.post("/inputs/geo/genes", {"id": pick["id"]})
        genes, scores = [g["gene"] for g in res["genes"]][: a.top], None
        name = pick["term"]
    else:
        res = fl.post("/inputs/dbsnp/lookup", {"query": q})
        _write_rows("flame_dbsnp_variants.csv", res["variants"], ["rsid", "chrpos", "genes", "consequence", "clinical", "frequency"])
        genes, scores = [g["gene"] for g in res["genes"]][: a.top], None
        name = f"dbSNP: {q[:50]}"
    print(f"{len(genes)} genes: {', '.join(genes[:15])}{' ...' if len(genes) > 15 else ''}")
    lst = fl.add_list(name[:80], ids=genes, organism=res.get("organismTaxId", a.organism))
    with open(f"flame_{a.source}_genes.txt", "w", encoding="utf-8") as fh:
        fh.write("\n".join(genes) + "\n")
    print(f"  -> flame_{a.source}_genes.txt")
    if scores and a.ranked:
        fl.post("/lists", {"name": name[:70] + " (ranked)", "ids": genes, "scores": scores, "organismTaxId": res.get("organismTaxId", a.organism), "uniqueName": True})
    if a.enrich:
        fl.save_terms(fl.wait_runs(fl.start_runs(lst["id"], a.tools.split(","), a.sources.split(","), a.threshold)))


def main():
    common = argparse.ArgumentParser(add_help=False)
    common.add_argument("--server", default="http://127.0.0.1:8000", help="Flame server URL")
    common.add_argument("--organism", type=int, default=9606, help="NCBI taxonomy ID (default: 9606, human)")
    common.add_argument("--threshold", type=float, default=0.05, help="significance threshold")
    common.add_argument("--save-session", metavar="FILE.json", help="download the workspace as a session file at the end")

    ap = argparse.ArgumentParser(description="Flame 3.0 from the command line", formatter_class=argparse.RawDescriptionHelpFormatter, epilog=__doc__)
    sub = ap.add_subparsers(dest="command", required=True)

    p = sub.add_parser("enrich", parents=[common], help="over-representation analysis of gene lists")
    p.add_argument("files", nargs="+", help="text files with one gene ID per line")
    p.add_argument("--tools", default="gprofiler,string", help="gprofiler,string,enrichr,webgestalt,panther,genecodis,clusterprofiler,reactome")
    p.add_argument("--sources", default="GO:BP,GO:MF,GO:CC,KEGG,REAC", help="annotation sources")
    p.add_argument("--stats", action="store_true", help="also print agreement statistics between runs")
    p.add_argument("--pathways", type=int, default=0, metavar="N", help="print KEGG/Reactome map links (genes highlighted) for the top N pathway terms")
    p.set_defaults(func=cmd_enrich)

    p = sub.add_parser("gsea", parents=[common], help="gene set enrichment analysis of a ranked list")
    p.add_argument("file", help="two columns: gene and score (header optional)")
    p.add_argument("--sources", default="HALLMARK,KEGG,REAC,GO:BP", help="MSigDB collections")
    p.add_argument("--pathways", type=int, default=0, metavar="N")
    p.set_defaults(func=cmd_gsea)

    p = sub.add_parser("variants", parents=[common], help="annotate variants or GWAS summary statistics")
    p.add_argument("file", help="rsIDs, VCF lines, chr:pos:ref:alt, HGVS, or a GWAS table (SNP, CHR, BP, P)")
    p.add_argument("--enrich", action="store_true", help="run enrichment on the affected genes")
    p.add_argument("--tools", default="gprofiler")
    p.add_argument("--sources", default="GO:BP,KEGG,REAC")
    p.set_defaults(func=cmd_variants)

    p = sub.add_parser("expression", parents=[common], help="two-group comparison of an expression matrix, then enrichment")
    p.add_argument("file", help="genes x samples table (first column: gene ID)")
    p.add_argument("--group-a", required=True, help="comma-separated sample names of group A")
    p.add_argument("--group-b", required=True, help="comma-separated sample names of group B")
    p.add_argument("--min-lfc", type=float, default=1.0, help="minimum |log2 fold change| for the gene list")
    p.add_argument("--tools", default="gprofiler")
    p.add_argument("--sources", default="GO:BP,KEGG,REAC")
    p.set_defaults(func=cmd_expression)

    p = sub.add_parser("mirna", parents=[common], help="microRNAs to target genes (miRDB), then enrichment")
    p.add_argument("file", help="one microRNA per line, e.g. hsa-miR-21-5p")
    p.add_argument("--min-mirnas", type=int, default=2, help="keep genes targeted by at least this many of the microRNAs")
    p.add_argument("--tools", default="gprofiler")
    p.add_argument("--sources", default="GO:BP,KEGG,REAC")
    p.set_defaults(func=cmd_mirna)

    p = sub.add_parser("analysis", parents=[common], help="regulators, drug signatures, network modules, robustness or metabolite pathways")
    p.add_argument("type", choices=["tf", "kinase", "drugs", "modules", "robustness", "metabolites"])
    p.add_argument("file", help="gene list (drugs: up-regulated genes; metabolites: names, KEGG or ChEBI IDs)")
    p.add_argument("--down", help="drugs: down-regulated genes")
    p.add_argument("--kind", default="chemical", choices=["chemical", "crispr"], help="drugs: compound or CRISPR signatures")
    p.add_argument("--score", type=int, default=700, help="modules: minimum STRING score")
    p.add_argument("--sources", default="GO:BP,KEGG,REAC", help="robustness: MSigDB collections")
    p.set_defaults(func=cmd_analysis)

    p = sub.add_parser("fetch", parents=[common], help="gene lists from a disease, PubMed, GEO or dbSNP")
    p.add_argument("source", choices=["disease", "pubmed", "geo", "dbsnp"])
    p.add_argument("query", help="disease name, PMIDs or PubMed search, GSE accession or keywords, rsIDs or dbSNP search")
    p.add_argument("--top", type=int, default=200, help="keep the top N genes")
    p.add_argument("--articles", type=int, default=100, help="pubmed: articles to use for a search")
    p.add_argument("--signature", type=int, default=1, help="geo: which signature of the search results (1 = first)")
    p.add_argument("--ranked", action="store_true", help="disease/pubmed: also save the scores as a ranked list (for GSEA)")
    p.add_argument("--enrich", action="store_true", help="run enrichment on the fetched genes")
    p.add_argument("--tools", default="gprofiler")
    p.add_argument("--sources", default="GO:BP,KEGG,REAC")
    p.set_defaults(func=cmd_fetch)

    a = ap.parse_args()
    fl = Flame(a.server)
    a.func(fl, a)
    if a.save_session:
        fl.save_session(a.save_session)
    print(f"\nOpen the results: {fl.base}/w/{fl.ws}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
