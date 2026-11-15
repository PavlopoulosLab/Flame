#!/usr/bin/env python3
"""
Flame 3.0 API — template client.

Creates a workspace, uploads gene lists, runs enrichment with several tools,
waits for the results, saves them as CSV, and compares the runs.

    pip install requests
    python flame_client.py --server https://YOUR-FLAME-SERVER genes1.txt genes2.txt

Each input file holds one gene ID per line. No account or key is needed.
"""
import argparse
import csv
import sys
import time

import requests


def main():
    ap = argparse.ArgumentParser(description="Run Flame enrichment from the command line")
    ap.add_argument("files", nargs="+", help="text files with one gene ID per line")
    ap.add_argument("--server", default="http://127.0.0.1:8000", help="Flame server URL")
    ap.add_argument("--organism", type=int, default=9606, help="NCBI taxonomy ID (default: human)")
    ap.add_argument("--tools", default="gprofiler,string", help="comma-separated: gprofiler,string,enrichr,webgestalt,panther,genecodis")
    ap.add_argument("--sources", default="GO:BP,GO:MF,GO:CC,KEGG,REAC", help="comma-separated annotation sources")
    ap.add_argument("--threshold", type=float, default=0.05)
    args = ap.parse_args()

    api = args.server.rstrip("/") + "/api"
    session = requests.Session()

    ws = session.post(f"{api}/workspace").json()["id"]
    session.headers["X-Flame-Workspace"] = ws
    print(f"Workspace: {args.server.rstrip('/')}/w/{ws}")

    run_ids = []
    for path in args.files:
        with open(path, encoding="utf-8") as fh:
            text = fh.read()
        lst = session.post(f"{api}/lists", json={"name": path, "text": text, "organismTaxId": args.organism, "uniqueName": True})
        lst.raise_for_status()
        lst = lst.json()
        print(f"List {lst['name']}: {lst['size']} IDs")
        res = session.post(f"{api}/runs", json={
            "listId": lst["id"], "tools": args.tools.split(","),
            "datasources": args.sources.split(","), "threshold": args.threshold,
        })
        res.raise_for_status()
        body = res.json()
        run_ids += body["created"] + body["reused"]
        for skipped in body["skipped"]:
            print(f"  skipped {skipped['tool']}: {skipped['reason']}")

    print("Waiting for runs to finish", end="", flush=True)
    while True:
        runs = session.get(f"{api}/runs").json()
        if all(r["status"] in ("complete", "error") for r in runs):
            break
        print(".", end="", flush=True)
        time.sleep(3)
    print()

    finished = []
    for run in runs:
        if run["status"] == "error":
            print(f"{run['title']}: failed ({run['error']})")
            continue
        terms = session.post(f"{api}/runs/{run['id']}/view", json={"view": "table", "top": "all"}).json()["terms"]
        out = f"flame_{run['id']}.csv"
        with open(out, "w", newline="", encoding="utf-8") as fh:
            writer = csv.DictWriter(fh, fieldnames=["source", "id", "name", "pvalue", "intersection", "termSize", "coverage", "genes", "link"], extrasaction="ignore")
            writer.writeheader()
            writer.writerows(terms)
        print(f"{run['title']} on {run['listName']}: {len(terms)} terms -> {out}")
        if terms:
            finished.append(run["id"])

    if len(finished) >= 2:
        cmp = session.post(f"{api}/comparisons", json={"runIds": finished, "view": "bubble", "top": 10, "sortBy": "Agreement"}).json()
        print("\nTop terms across runs (agreement, combined p):")
        for t in cmp["terms"]:
            print(f"  {t['agreement']} runs  p={t['activeCombPvalue']:.2e}  {t['name']} ({t['id']})")
    return 0


if __name__ == "__main__":
    sys.exit(main())
