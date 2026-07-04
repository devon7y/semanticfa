#!/usr/bin/env python
"""Export the selection-parity fixture from the research pipeline's
official v7 outputs (run inside LLM_Factor_Analysis)."""
import json
from factor_naming import _wl_single, build_whitelist, in_whitelist
from nltk.corpus import wordnet as wn

wl = build_whitelist()
cache = json.load(open("results/factor_naming/e2plus_method_cache.json"))
v7 = json.load(open("results/factor_naming/e2plus_v7_results.json"))
ties = json.load(open("results/factor_naming/tie_sets.json"))

def fam(w):
    return " ".join(wn.morphy(t, "n") or t for t in w.lower().split())

fx = {}
for k, v in v7.items():
    words, scores = cache["E2p"][k]
    cands = []
    for w, s in zip(words, scores):
        if not in_whitelist(w, wl):
            continue
        f = fam(w)
        cands.append({"word": w, "family": f,
                      "tier1": (_wl_single(f, wl) or _wl_single(w, wl)) is not None,
                      "score": round(float(s), 6)})
        if len(cands) >= 60:
            break
    fx[k] = {"scale": v["scale"], "candidates": cands,
             "expected_label": v["label"], "expected_rule": v["rule"],
             "loo_set": ties.get(k, {}).get("tie_set", [])}
json.dump(fx, open("golden_v7.json", "w"))
print(f"fixture: {len(fx)} factors")
