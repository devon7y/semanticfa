#!/usr/bin/env python3
"""Embed the items of an inventory with a Qwen3-Embedding model.

Makes the item archives of the further inventories (Section 11d of
reproduce.R): data/ECR_items_8B.npz, KIMS_items_8B.npz, HSQ_items_8B.npz,
NIS_items_8B.npz, RSE_items_8B.npz, and BAI_items_8B.npz. The route is the one
used for every other item vector in the paper (see embed_items.py):
sentence-transformers loading the model with its own module stack (last-token
pooling, then the model's Normalize module, so each vector has unit length),
float32 weights, no instruction prompt, normalize_embeddings=False.

    python3 embed_inventory.py SIZE ITEMS.csv OUT.npz [ITEMS.csv OUT.npz ...] [--no-items]

SIZE is 0.6B, 4B, or 8B. Each ITEMS.csv has the columns code, item, factor,
and scoring (+1 forward-keyed, -1 reverse-keyed), one row per item in the
order of the public response file (see items/), and is embedded into the
OUT.npz that follows it. If the environment variable MODEL_DIR
names a local snapshot of the model, the model is loaded from it, and the
snapshot's revision (its folder name) is printed. OUT.npz holds the arrays
`embeddings`, `items`, `codes`, `factors`, `scoring`, and `model`. With
--no-items the item wording is left out of OUT.npz, for an inventory whose
items are copyrighted.
"""
import csv
import os
import sys

import numpy as np
import sentence_transformers
import torch
import transformers
from sentence_transformers import SentenceTransformer


def main(size, pairs, keep_items=True):
    model_name = f"Qwen/Qwen3-Embedding-{size}"
    print(f"python {sys.version.split()[0]} | numpy {np.__version__} | "
          f"torch {torch.__version__} | transformers {transformers.__version__} | "
          f"sentence-transformers {sentence_transformers.__version__}")
    device = "cuda" if torch.cuda.is_available() else "cpu"
    source = os.environ.get("MODEL_DIR") or model_name
    if source != model_name:
        print(f"{model_name} loaded from local snapshot, revision "
              f"{os.path.basename(os.path.normpath(source))}")
    model = SentenceTransformer(source, device=device,
                                model_kwargs={"torch_dtype": torch.float32})
    print(f"model {model_name} on {device}; modules: "
          f"{[type(m).__name__ for m in model]}")

    for items_path, out_path in pairs:
        with open(items_path, newline="", encoding="utf-8") as fh:
            rows = list(csv.DictReader(fh))
        items = [r["item"] for r in rows]
        emb = model.encode(items, batch_size=25, convert_to_numpy=True,
                           normalize_embeddings=False,
                           show_progress_bar=False).astype(np.float64)
        norms = np.linalg.norm(emb, axis=1)
        print(f"{os.path.basename(items_path)}, {size}: {len(items)} items x "
              f"{emb.shape[1]} dims")
        print(f"  norms: min {norms.min():.6f}, max {norms.max():.6f}")

        arrays = dict(embeddings=emb.astype(np.float32),
                      codes=np.array([r["code"] for r in rows]),
                      factors=np.array([r["factor"] for r in rows]),
                      scoring=np.array([int(r["scoring"]) for r in rows]),
                      model=np.array(model_name))
        if keep_items:
            arrays["items"] = np.array(items)
        np.savez(out_path, **arrays)
        print(f"  wrote {out_path}" + ("" if keep_items else " (without item wording)"))


if __name__ == "__main__":
    args = [a for a in sys.argv[1:] if a != "--no-items"]
    if len(args) < 3 or len(args) % 2 == 0:
        sys.exit(__doc__)
    main(args[0], list(zip(args[1::2], args[2::2])),
         keep_items="--no-items" not in sys.argv)
