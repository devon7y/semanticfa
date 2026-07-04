#!/usr/bin/env python
"""Build the canonical shipped word list (run inside the research repo,
LLM_Factor_Analysis). See data-raw/README.md."""
import csv
import numpy as np
from factor_naming import _wl_single, build_whitelist, in_whitelist
from nltk.corpus import wordnet as wn

wl = build_whitelist()
base = [str(w) for w in np.load("embeddings/pool_words.npy", allow_pickle=True)]
add = [str(w) for w in np.load("embeddings/pool_additions_8B.npz",
                               allow_pickle=True)["words"]]

def family(w):
    return " ".join(wn.morphy(t, "n") or t for t in w.lower().split())

rows, base_idx, add_idx = [], [], []
for words, idxlist in ((base, base_idx), (add, add_idx)):
    for i, w in enumerate(words):
        if not in_whitelist(w, wl):
            continue
        f = family(w)
        rows.append((w, f, int((_wl_single(f, wl) or _wl_single(w, wl)) is not None)))
        idxlist.append(i)

with open("semanticfa_wordlist.csv", "w", newline="") as fh:
    wcsv = csv.writer(fh)
    wcsv.writerow(["word", "family", "tier1"])
    wcsv.writerows(rows)
np.save("semanticfa_eligible_base_idx.npy", np.array(base_idx, dtype=np.int64))
np.save("semanticfa_eligible_add_idx.npy", np.array(add_idx, dtype=np.int64))
print(f"eligible: {len(rows)} (base {len(base_idx)} + additions {len(add_idx)})")
