#!/usr/bin/env python3
"""Embed the three candidate items for sfa_item_fit() with Qwen3-Embedding-8B.

The demonstration vets three brand-new candidate items. So that the vetting
runs in the SAME vector space as every other analysis, the candidates are
embedded offline with the same model and the same route as the bundled item
vectors: sentence-transformers loading Qwen/Qwen3-Embedding-8B with its own
module stack (last-token pooling, then the model's Normalize module), float32
weights, no instruction prompt, normalize_embeddings=False. The vectors ship
in the reproduction archive as Big5FM_cand_8B.npz, and reproduce.R serves
them to sfa_item_fit() through a lookup embedder, so the whole demonstration
stays pure R and offline.

Run on a machine that can hold the 8B model in float32 (~32 GB; e.g., an HPC
GPU node):

    python3 embed_candidates_8B.py ../data/Big5FM_cand_8B.npz

Optional check that this route reproduces the bundled item vectors: pass the
original item archive (codes, items, embeddings) as a second argument, and the
script re-embeds the 50 items and reports their cosine with the stored ones.

    python3 embed_candidates_8B.py ../data/Big5FM_cand_8B.npz Big5FM_items_8B.npz
"""
import sys

import numpy as np
import torch
from sentence_transformers import SentenceTransformer

MODEL_NAME = "Qwen/Qwen3-Embedding-8B"

# Must match `cand` in reproduce.R exactly (asserted there).
CANDIDATES = [
    "I make friends easily.",
    "I am the life of every party.",
    "I finish what I start.",
]
CODES = ["cand1", "cand2", "cand3"]


def encode(model, texts):
    return model.encode(texts, batch_size=21, convert_to_numpy=True,
                        normalize_embeddings=False, show_progress_bar=False)


def main(out_path, check_path=None):
    device = "cuda" if torch.cuda.is_available() else "cpu"
    model = SentenceTransformer(MODEL_NAME, device=device,
                                model_kwargs={"torch_dtype": torch.float32})
    emb = encode(model, CANDIDATES).astype(np.float32)
    assert np.isfinite(emb).all()
    np.savez(out_path,
             embeddings=emb,
             items=np.array(CANDIDATES),
             codes=np.array(CODES),
             model=np.array(MODEL_NAME))
    print(f"wrote {out_path}: {emb.shape[0]} x {emb.shape[1]} ({MODEL_NAME})")
    print("candidate norms:", np.round(np.linalg.norm(emb, axis=1), 6))

    if check_path:
        z = np.load(check_path, allow_pickle=True)
        ref = z["embeddings"].astype(np.float64)
        items = [str(t) for t in z["items"]]
        new = encode(model, items).astype(np.float64)
        cos = (ref * new).sum(1) / (np.linalg.norm(ref, axis=1) *
                                    np.linalg.norm(new, axis=1))
        print(f"item check: {len(items)} items, cosine with stored vectors "
              f"min = {cos.min():.6f}, median = {np.median(cos):.6f}")
        refn = ref / np.linalg.norm(ref, axis=1, keepdims=True)
        candn = emb / np.linalg.norm(emb, axis=1, keepdims=True)
        top = (candn @ refn.T)
        for c, row in zip(CANDIDATES, top):
            j = int(row.argmax())
            print(f"  {c!r}: nearest stored item {z['codes'][j]} "
                  f"{items[j]!r} cos = {row[j]:.4f}")


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "../data/Big5FM_cand_8B.npz",
         sys.argv[2] if len(sys.argv) > 2 else None)
