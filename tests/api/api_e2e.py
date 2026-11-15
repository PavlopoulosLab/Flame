"""End-to-end check of the Flame REST API against a running server.

Calls the real external services (g:Profiler, STRING, Enrichr, Reactome, ChEA3,
Ensembl, NCBI, Open Targets, ...), so it needs internet access and takes a few minutes.

    Rscript api/run.R &                      # start a server
    python3 tests/api/api_e2e.py             # default: http://127.0.0.1:8000/api/v1
    python3 tests/api/api_e2e.py https://flame.example.org/api/v1
"""
import json, sys, time, urllib.request, urllib.error

BASE = (sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8000/api/v1").rstrip("/")
WS = None
ok = fail = 0


def call(method, path, body=None, ws=True, raw=False):
    data = json.dumps(body).encode() if body is not None else None
    req = urllib.request.Request(BASE + path, data=data, method=method)
    if data is not None:
        req.add_header("Content-Type", "application/json")
    if ws and WS:
        req.add_header("X-Flame-Workspace", WS)
    try:
        with urllib.request.urlopen(req, timeout=180) as r:
            txt = r.read().decode()
            return r.status, (txt if raw else json.loads(txt)), dict(r.headers)
    except urllib.error.HTTPError as e:
        txt = e.read().decode()
        try:
            return e.code, json.loads(txt), dict(e.headers)
        except Exception:
            return e.code, txt, dict(e.headers)


def check(name, cond, detail=""):
    global ok, fail
    if cond:
        ok += 1
        print(f"  ok   {name} {detail}")
    else:
        fail += 1
        print(f"  FAIL {name} {detail}")


print("== static")
s, cfg, h = call("GET", "/config", ws=False)
check("config", s == 200 and len(cfg["tools"]) == 8, f"tools={[t['key'] for t in cfg['tools']]}")
check("security headers", h.get("X-Content-Type-Options") == "nosniff" and h.get("X-Frame-Options") == "DENY")
s, orgs, _ = call("GET", "/organisms?q=mouse&limit=3", ws=False)
check("organism search", s == 200 and len(orgs) > 0, orgs[0]["label"][:40] if orgs else "")
s, t, _ = call("GET", "/organisms/9606/tools", ws=False)
check("organism tools", s == 200 and all(x["supported"] for x in t["tools"] if x["key"] in ("gprofiler", "string")))

print("== workspace and lists")
s, w, _ = call("POST", "/workspace", ws=False)
WS = w["id"]
check("create workspace", s == 201 and len(WS) == 32)
s, e, _ = call("GET", "/lists", ws=False)
check("no workspace header -> 401", s == 401, str(e))
ex = call("GET", "/inputs/examples/upload")[1]["text"]
s, p, _ = call("POST", "/inputs/upload/parse", {"text": ex + "\nTP53\nbad!id"})
check("parse ids", s == 200 and p["duplicates"] >= 0 and p["invalid"] == 1, f"unique={p['unique']} dup={p['duplicates']} invalid={p['invalid']}")
s, l1, _ = call("POST", "/lists", {"name": "Example A", "text": ex, "organismTaxId": 9606})
check("create list A", s == 201, f"size={l1.get('size')}")
ex2 = call("GET", "/inputs/examples/upload")[1]["text"]
s, l2, _ = call("POST", "/lists", {"name": "Example B", "text": ex2, "organismTaxId": 9606})
check("create list B", s == 201)
s, e, _ = call("POST", "/lists", {"name": "Example A", "ids": ["TP53"]})
check("duplicate name -> 409", s == 409, e.get("error", ""))
s, x, _ = call("POST", "/lists", {"name": "<img src=x onerror=alert(1)>", "ids": ["TP53", "BRCA1"]})
check("html name stored as text", s == 201 and x["name"] == "<img src=x onerror=alert(1)>")
s, x, _ = call("PATCH", f"/lists/{x['id']}", {"name": "renamed"})
check("rename list", s == 200 and x["name"] == "renamed")
s, csv, h = call("GET", f"/lists/{l1['id']}/csv", raw=True)
check("list csv", s == 200 and csv.startswith("id\n") and "attachment" in h.get("Content-Disposition", ""))
s, u, _ = call("POST", "/inputs/upset", {"listIds": [l1["id"], l2["id"]], "mode": "union"})
check("upset", s == 200 and len(u["combinations"]) == 3, str([(c["label"], c["count"]) for c in u["combinations"]]))

print("== table inputs")
vex = call("GET", "/inputs/examples/volcano")[1]
s, prep, _ = call("POST", "/inputs/table/prepare", {"text": vex["text"], "fileName": vex["fileName"], "kind": "volcano"})
check("volcano prepare", s == 200, f"rows={prep.get('rowCount')} defaults={prep.get('defaults')}")
s, vp, _ = call("POST", "/inputs/volcano/points", {"text": vex["text"], "fileName": vex["fileName"], "mapping": prep["defaults"], "pThreshold": 0.05, "lfcThreshold": 1})
check("volcano points", s == 200 and vp["plotted"] > 0, f"plotted={vp.get('plotted')} counts={vp.get('counts')}")
rex = call("GET", "/inputs/examples/reduction")[1]
s, rp, _ = call("POST", "/inputs/table/prepare", {"text": rex["text"], "fileName": rex["fileName"], "kind": "reduction"})
s, rpts, _ = call("POST", "/inputs/reduction/points", {"text": rex["text"], "fileName": rex["fileName"], "mapping": rp["defaults"]})
check("reduction points", s == 200 and rpts["plotted"] > 0, f"plotted={rpts.get('plotted')} colorType={rpts.get('colorType')}")
s, e, _ = call("POST", "/inputs/volcano/points", {"text": "a,b\n1,2", "mapping": {"id": "a", "logfc": "b", "pvalue": "b"}})
check("bad volcano -> 400", s == 400, e.get("error", ""))

print("== external services")
s, snp, _ = call("POST", "/inputs/snps/map", {"text": call("GET", "/inputs/examples/snps")[1]["text"]})
check("snps map", s == 200 and len(snp["rows"]) > 0, f"rows={len(snp.get('rows', []))}")
s, tm, _ = call("POST", "/inputs/textmining/extract", {"text": call("GET", "/inputs/examples/textmining")[1]["text"], "organismTaxId": 9606})
check("text mining", s == 200 and len(tm["rows"]) > 0, f"rows={len(tm.get('rows', []))}")
s, cv, _ = call("POST", "/utilities/convert", {"listId": l1["id"], "targetFormat": "ENTREZGENE_ACC", "mappingMode": "best"})
check("convert ids", s == 200 and len(cv["rows"]) > 0, f"stats={cv.get('stats', cv)}")
s, orth, _ = call("POST", "/utilities/orthologs", {"listId": l1["id"], "targetTaxId": 10090})
check("orthologs", s == 200 and len(orth["rows"]) > 0, f"stats={orth.get('stats', orth)}")
s, ppi, _ = call("POST", "/utilities/ppi", {"listId": l1["id"], "score": 700})
check("ppi network", s == 200 and ppi["stats"]["nodes"] > 0, f"stats={ppi.get('stats', ppi)} link={str(ppi.get('link'))[:40]}")

print("== custom source")
gmt = "SET1\tCell cycle set\t" + "\t".join(p["ids"][:20]) + "\nSET2\tRandom\tFOO1\tBAR2\t" + "\t".join(p["ids"][40:45]) + "\n"
s, src, _ = call("POST", "/sources", {"name": "My sets", "text": gmt})
check("upload gmt", s == 201 and src["termCount"] == 2)

print("== runs")
s, r, _ = call("POST", "/runs", {"listId": l1["id"], "tools": ["gprofiler", "string", "enrichr"], "sourceIds": [src["id"]],
                                  "datasources": ["GO:BP", "KEGG", "REAC"], "threshold": 0.05})
check("start runs", s == 202 and len(r["created"]) == 4, str(r))
s, r2, _ = call("POST", "/runs", {"listId": l1["id"], "tools": ["gprofiler"], "datasources": ["GO:BP", "KEGG", "REAC"], "threshold": 0.05})
check("identical run reused", s == 200 and len(r2["reused"]) == 1, str(r2))
t0 = time.time()
while time.time() - t0 < 240:
    runs = call("GET", "/runs")[1]
    if all(x["status"] in ("complete", "error") for x in runs):
        break
    time.sleep(2)
for x in runs:
    check(f"run {x['id']}", x["status"] == "complete", f"status={x['status']} results={x.get('resultCount')} err={x.get('error')} msgs={x.get('messages')}")
done = [x["id"] for x in runs if x["status"] == "complete" and (x.get("resultCount") or 0) > 0]

if done:
    rid = done[0]
    views = ["table", "dotplot", "barchart", "scatter", "heatmap-tg", "heatmap-tt", "heatmap-gg", "network-tg", "network-tt", "network-gg"]
    for v in views:
        s, d, _ = call("POST", f"/runs/{rid}/view", {"view": v, "top": 15, "filters": {"search": "cell"}, "settings": {"layout": "circle"}})
        extra = len(d.get("cells", [])) if "heatmap" in v else len((d.get("network") or {}).get("nodes", []))
        check(f"view {v}", s == 200 and d["shownCount"] > 0, f"shown={d.get('shownCount')}/{d.get('filteredCount')} extra={extra}")
    s, det, _ = call("GET", f"/runs/{rid}/details")
    check("run details", s == 200 and det["significantTerms"] > 0, f"mapped={det.get('mappedCount')}/{det.get('inputSize')} sources={det.get('sourcesWithHits')}")
    s, d, _ = call("POST", f"/runs/{rid}/view", {"view": "table", "top": "all"})
    check("view top=all", s == 200 and d["shownCount"] == d["filteredCount"], f"{d.get('shownCount')}")
    links = [t["link"] for t in d["terms"] if t.get("link")]
    check("links are https", all(l.startswith("http") for l in links), f"{len(links)} links")

if len(done) >= 2:
    for v in ["overview", "bubble", "heatmap"]:
        s, c, _ = call("POST", "/comparisons", {"runIds": done[:3], "view": v, "top": 10})
        check(f"compare {v}", s == 200 and len(c["terms"]) > 0, f"terms={len(c.get('terms', []))} cells={len(c.get('cells', []))}")
    s, ids, _ = call("POST", "/comparisons/overview-terms", {"runIds": done[:3], "kind": "pin", "metric": "consensus"})
    check("overview pin terms", s == 200 and isinstance(ids, list), f"n={len(ids) if isinstance(ids, list) else ids}")

print("== external viewers")
if done:
    s, a3, _ = call("POST", f"/runs/{done[0]}/arena3d", {"view": "network-tg", "top": 10})
    check("run -> Arena3D", s == 200 and a3["url"].startswith("https://www.arena3d.org/"), f"layers={a3.get('layers')} nodes={a3.get('nodes')} edges={a3.get('edges')}")
    s, a3h, _ = call("POST", f"/runs/{done[0]}/arena3d", {"view": "heatmap-tg", "top": 10})
    check("heatmap -> Arena3D", s == 200 and a3h.get("layers", 0) >= 2, str(a3h)[:80])
    s, e, _ = call("POST", f"/runs/{done[0]}/arena3d", {"view": "dotplot"})
    check("dotplot -> Arena3D refused", s == 400, e.get("error", ""))
    s, nm, _ = call("POST", f"/runs/{done[0]}/norma", {"view": "network-tg", "top": 10})
    check("run -> NORMA", s == 200 and nm["url"].startswith("https://"), nm.get("url", "")[:50])
if len(done) >= 2:
    s, a3c, _ = call("POST", "/comparisons/arena3d", {"runIds": done[:3], "top": 20})
    check("comparison -> Arena3D", s == 200 and a3c.get("layers", 0) >= 3, f"layers={a3c.get('layers')} edges={a3c.get('edges')}")
s, a3p, _ = call("POST", "/utilities/ppi/arena3d", {"listId": l1["id"], "score": 700})
check("STRING -> Arena3D", s == 200 and a3p.get("nodes", 0) > 0, f"nodes={a3p.get('nodes')} edges={a3p.get('edges')}")

print("== comparison statistics and pathway links")
if len(done) >= 2:
    s, st, _ = call("POST", "/comparisons/statistics", {"runIds": done[:3]})
    ok_pairs = s == 200 and len(st["pairs"]) == len(done[:3]) * (len(done[:3]) - 1) // 2
    check("comparison statistics", ok_pairs and st["tests"].get("detection") is not None, f"pairs={len(st.get('pairs', []))} tests={list((st.get('tests') or {}).keys())}")
    check("statistics: curves are arrays", all(isinstance(c["k"], list) and isinstance(c["overlap"], list) for c in st.get("cat", [])))
if done:
    rows = call("POST", f"/runs/{done[0]}/view", {"view": "table", "top": "all", "filters": {"sources": ["KEGG"]}})[1]["terms"]
    if rows:
        s, pl, _ = call("POST", f"/runs/{done[0]}/pathway-links", {"termId": rows[0]["id"]})
        kegg = [l for l in pl.get("links", []) if "highlighted" in l["label"]]
        check("KEGG map with genes", s == 200 and kegg and kegg[0]["url"].startswith("https://www.kegg.jp/kegg-bin/show_pathway?map="), (kegg[0]["label"] if kegg else str(pl))[:60])
    s, e, _ = call("POST", f"/runs/{done[0]}/pathway-links", {"termId": "no-such-term"})
    check("unknown term -> 404", s == 404)

print("== examples, expression and microRNAs")
for kind in ["ranked", "de_lists", "expression", "mirna", "gmt"]:
    s, ex, _ = call("GET", f"/inputs/examples/{kind}")
    check(f"example {kind}", s == 200 and (ex.get("text") or ex.get("up")), f"keys={list(ex.keys())[:4]}")
for path in ["/inputs/examples/expression/file", "/inputs/examples/mirna/file", "/variants/examples/gwas/file", "/variants/examples/variants/file"]:
    s, txt, h = call("GET", path, raw=True)
    check(f"download {path.split('/')[-2]}", s == 200 and "attachment" in h.get("Content-Disposition", "") and len(txt) > 50)
exx = call("GET", "/inputs/examples/expression")[1]
s, ov, _ = call("POST", "/inputs/expression/overview", {"text": exx["text"], "fileName": exx["fileName"], "topN": 300, "k": 4})
check("expression overview", s == 200 and ov["shownGenes"] == 300 and len(ov["clusterSizes"]) == 4, f"samples={ov.get('samples')}")
s, de, _ = call("POST", "/inputs/expression/de", {"text": exx["text"], "fileName": exx["fileName"], "groupA": [x for x in ov["samples"] if "dex" in x.lower() and "untr" not in x.lower()], "groupB": [x for x in ov["samples"] if "untr" in x.lower()]})
check("expression two groups", s == 200 and de["significant"] > 20, f"significant={de.get('significant')}")
s, e, _ = call("POST", "/inputs/expression/de", {"text": exx["text"], "groupA": ["treated_1"], "groupB": ["control_1", "control_2"]})
check("expression bad groups -> 400", s == 400, e.get("error", "")[:50])
s, mi, _ = call("POST", "/inputs/mirna/targets", {"text": "hsa-miR-21-5p\nmiR-155\nnot-a-mirna", "organismTaxId": 9606, "minMirnas": 2})
check("microRNA targets", s == 200 and mi["unmatched"] == ["not-a-mirna"] and mi["totalTargets"] > 100, f"targets={mi.get('totalTargets')} both-arms={len(mi['mirnas'][1]['matched']) if s == 200 else '-'}")

print("== session status, partial export, replace import")
s, st, _ = call("GET", "/workspace/status")
check("session status", s == 200 and st["state"] in ("unsaved", "changed", "saved"), st.get("state"))
s, part, _ = call("GET", "/workspace/export?parts=lists")
check("partial export", s == 200 and part["contents"]["runs"] == 0 and part["complete"] is False, str(part.get("contents")))
s, full, _ = call("GET", "/workspace/export")
s2, st2, _ = call("GET", "/workspace/status")
check("full export marks saved", s == 200 and st2["state"] == "saved")

print("== ranked lists and GSEA")
genes = p["ids"]
ranked_text = "gene\tscore\n" + "\n".join(f"{g}\t{3 - 6 * i / len(genes):.3f}" for i, g in enumerate(genes))
s, rp, _ = call("POST", "/inputs/upload/parse", {"text": ranked_text, "ranked": True, "skipFirstRow": True})
check("parse ranked", s == 200 and len(rp["scores"]) == rp["unique"] > 100, f"unique={rp.get('unique')}")
s, rl, _ = call("POST", "/lists", {"name": "Ranked", "text": ranked_text, "ranked": True, "skipFirstRow": True, "organismTaxId": 9606})
check("create ranked list", s == 201 and rl.get("ranked") is True, f"size={rl.get('size')}")
s, e, _ = call("POST", "/runs", {"listId": l1["id"], "paradigm": "GSEA", "tools": ["clusterprofiler"], "datasources": ["HALLMARK"]})
check("GSEA on unranked list -> 400", s == 400, e.get("error", "")[:60])
s, g, _ = call("POST", "/runs", {"listId": rl["id"], "paradigm": "GSEA", "tools": ["clusterprofiler", "webgestalt"], "datasources": ["HALLMARK", "KEGG", "GO:BP"], "threshold": 1})
check("start GSEA", s == 202 and len(g["created"]) == 1 and len(g["skipped"]) == 1, f"created={g.get('created')} skipped={[x['reason'] for x in g.get('skipped', [])]}")
if g.get("created"):
    gid = g["created"][0]
    t0 = time.time()
    while time.time() - t0 < 300:
        gr = next(x for x in call("GET", "/runs")[1] if x["id"] == gid)
        if gr["status"] in ("complete", "error"):
            break
        time.sleep(3)
    check("GSEA run", gr["status"] == "complete" and gr["paradigm"] == "GSEA", f"status={gr['status']} results={gr.get('resultCount')} err={gr.get('error')}")
    if gr["status"] == "complete" and gr.get("resultCount"):
        s, gv, _ = call("POST", f"/runs/{gid}/view", {"view": "table", "top": 10, "sortBy": "NES"})
        check("GSEA view has NES", s == 200 and all(t.get("nes") is not None for t in gv["terms"]), f"top NES={[round(t['nes'], 2) for t in gv['terms'][:3]]}")

print("== variants and GWAS")
s, sp, _ = call("GET", "/variants/species")
check("variant species", s == 200 and len(sp) > 0, f"n={len(sp)}")
for kind in ("variants", "gwas"):
    exv = call("GET", f"/variants/examples/{kind}")[1]["text"]
    s, va, _ = call("POST", "/variants", {"text": exv, "organismTaxId": 9606})
    check(f"start {kind} analysis", s in (200, 201, 202), f"id={va.get('id')} inputs={va.get('inputCount')}")
    t0 = time.time()
    while time.time() - t0 < 300:
        st = call("GET", f"/variants/{va['id']}")[1]
        if st["status"] in ("complete", "error"):
            break
        time.sleep(3)
    res = st.get("result") or {}
    check(f"{kind} analysis", st["status"] == "complete" and res.get("annotatedCount", 0) > 0,
          f"annotated={res.get('annotatedCount')}/{res.get('inputCount')} genes={len(res.get('genes', []))} traits={len((res.get('summary') or {}).get('traits', []))}")
    if st["status"] == "complete":
        s, a3v, _ = call("POST", f"/variants/{va['id']}/arena3d", {})
        check(f"{kind} -> Arena3D", s == 200 and a3v.get("layers", 0) >= 2, f"layers={a3v.get('layers')} edges={a3v.get('edges')}")
s, e, _ = call("POST", "/variants", {"text": "hello world", "organismTaxId": 9606})
check("bad variant input -> 400", s == 400, e.get("error", "")[:60])

print("== API documentation")
root = BASE.rsplit("/api", 1)[0]
try:
    with urllib.request.urlopen(root + "/openapi.json", timeout=60) as r:
        spec = json.loads(r.read().decode())
    check("OpenAPI spec", spec.get("openapi", "").startswith("3") and any(p.startswith("/api/v1/") for p in spec.get("paths", {})),
          f"{len(spec.get('paths', {}))} paths")
    check("spec: workspace auth and bodies", "workspace" in spec["components"]["securitySchemes"]
          and "requestBody" in spec["paths"]["/api/v1/runs"]["post"])
except Exception as e:
    check("OpenAPI spec", False, str(e)[:80])

print("== Reactome, background analyses and fetched gene lists")
lst = call("POST", "/lists", {"name": "e2e dexamethasone", "text": call("GET", "/inputs/examples/upload")[1]["text"], "organismTaxId": 9606})[1]
s, rr, _ = call("POST", "/runs", {"listId": lst["id"], "tools": ["reactome"], "datasources": ["REAC"]})
check("Reactome run queued", s == 202 and len(rr.get("created", [])) == 1, str(rr)[:80])
starts = {}
for t, body in [("tf", {"listId": lst["id"]}), ("robustness", {"listId": lst["id"], "sources": ["GO:BP", "KEGG"]}),
                ("metabolites", {"text": "sarcosine\nuracil\nkynurenine\nglycerol 3-phosphate\nleucine\nproline", "name": "e2e metabolites"})]:
    s, a, _ = call("POST", "/analyses", dict(type=t, **body))
    check(f"analysis {t} queued", s == 202 and a.get("id"), str(a)[:80])
    starts[t] = a.get("id")
s, bad, _ = call("POST", "/analyses", {"type": "nonsense"})
check("unknown analysis -> 400", s == 400)
t0 = time.time()
while time.time() - t0 < 400:
    items = call("GET", "/analyses")[1]
    runs_now = call("GET", "/runs")[1]
    if all(x["status"] != "running" for x in items) and all(r["status"] in ("complete", "error") for r in runs_now):
        break
    time.sleep(4)
byid = {x["id"]: x for x in items}
st = call("GET", f"/analyses/{starts['tf']}")[1]
check("ChEA3 transcription factors", st.get("status") == "complete" and len((st.get("result") or {}).get("regulators", [])) > 5, st.get("error") or "")
st = call("GET", f"/analyses/{starts['robustness']}")[1]
check("robustness check", st.get("status") == "complete" and isinstance((st.get("result") or {}).get("terms"), list), st.get("error") or "")
st = call("GET", f"/analyses/{starts['metabolites']}")[1]
check("metabolites become a run", st.get("status") == "complete" and st.get("runId"), st.get("error") or "")
rid = rr.get("created", ["?"])[0]
rv = call("POST", f"/runs/{rid}/view", {"view": "table", "top": 5})[1]
check("Reactome terms", all(t["id"].startswith("R-HSA-") for t in rv.get("terms", [])) and len(rv.get("terms", [])) > 0)
s, ds, _ = call("POST", "/inputs/disease/search", {"query": "asthma"})
check("disease search", s == 200 and len(ds.get("diseases", [])) > 0 and len(ds.get("sets", [])) > 0)
if s == 200 and ds.get("diseases"):
    s, dg, _ = call("POST", "/inputs/disease/genes", {"source": "opentargets", "id": ds["diseases"][0]["id"], "size": 50})
    check("Open Targets disease genes", s == 200 and len(dg.get("genes", [])) == 50)
s, pg, _ = call("POST", "/inputs/pubmed/genes", {"query": "24926665", "organismTaxId": 9606})
check("PubMed genes", s == 200 and pg.get("articles") == 1, str(pg.get("error", ""))[:80])
s, gs, _ = call("POST", "/inputs/geo/search", {"query": "GSE52778"})
check("GEO signatures", s == 200 and gs.get("total", 0) > 0)
s, sn, _ = call("POST", "/inputs/dbsnp/lookup", {"query": "rs429358 rs7412"})
check("dbSNP lookup", s == 200 and len(sn.get("variants", [])) == 2)

print("== session export / import / legacy")
s, exp, _ = call("GET", "/workspace/export")
check("export", s == 200 and exp["format"] == "flame-session", f"lists={len(exp['lists'])} runs={len(exp['runs'])}")
old = WS
WS = call("POST", "/workspace", ws=False)[1]["id"]
s, imp, _ = call("POST", "/workspace/import", exp)
check("import", s == 200 and imp["lists"] == len(exp["lists"]) and imp["runs"] == len(exp["runs"])
      and imp.get("analyses", 0) == len(exp.get("analyses", [])), str(imp))
if imp.get("runs"):
    nr = call("GET", "/runs")[1][0]["id"]
    s, d, _ = call("POST", f"/runs/{nr}/view", {"view": "barchart", "top": 5})
    check("imported run view", s == 200 and d["shownCount"] > 0)
s, lg, _ = call("GET", "/legacy?url_genes=TP53,BRCA1,EGFR;KRAS,MYC", ws=False)
check("legacy GET", s == 201 and len(lg["lists"]) == 2, lg.get("url", ""))
s, lg2, _ = call("POST", "/legacy", {"gene_list1": ["TP53", "BRCA1"], "gene_list2": ["MYC"]}, ws=False)
check("legacy POST", s == 201 and len(lg2["lists"]) == 2)
s, _, _ = call("DELETE", "/workspace/data")
check("clear data", s == 200 and call("GET", "/lists")[1] == [])
WS = old
s, _, _ = call("DELETE", "/workspace")
check("delete workspace", s == 200 and call("GET", "/lists")[0] == 404)

print(f"\n{ok} passed, {fail} failed")
sys.exit(1 if fail else 0)
