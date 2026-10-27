#!/usr/bin/env python3
"""Re-embed the items of a stored archive with a Qwen3-Embedding model.

Provenance check for the item vectors used in the paper: the bundled 8B matrix
of the 50 IPIP Big-Five Factor Marker items (data(big5) in the package), the
two smaller-model archives in data/ (Big5_items_0.6B.npz, Big5_items_4B.npz),
and the three archives of the 42 DASS items (DASS_items_0.6B.npz,
DASS_items_4B.npz, DASS_items_8B.npz). The route is the one used for the
candidate items (embed_candidates_8B.py): sentence-transformers loading the
model with its own module stack (last-token pooling, then the model's
Normalize module, so each vector has unit length), float32 weights, no
instruction prompt, normalize_embeddings=False.

    python3 embed_items.py SIZE REFERENCE.npz [OUT.npz]

SIZE is 0.6B, 4B, or 8B. If the environment variable MODEL_DIR names a
local snapshot of the model (for example a Hugging Face cache snapshot on a
machine without internet), the model is loaded from it, and the snapshot's
revision (its folder name) is printed. REFERENCE.npz holds the stored vectors (arrays
`embeddings`, `items`, `codes`); the script re-embeds the same item texts in
the same order, reports the cosine between each new and stored vector and the
largest absolute coordinate difference, and optionally writes the new vectors
to OUT.npz. Run on a machine that can hold the model in float32 (the 8B model
needs about 32 GB).
"""
import os
import sys

import numpy as np
import sentence_transformers
import torch
import transformers
from sentence_transformers import SentenceTransformer


def main(size, ref_path, out_path=None):
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

    ref = np.load(ref_path, allow_pickle=True)
    items = [str(t) for t in ref["items"]]
    stored = ref["embeddings"].astype(np.float64)
    new = model.encode(items, batch_size=25, convert_to_numpy=True,
                       normalize_embeddings=False,
                       show_progress_bar=False).astype(np.float64)
    assert new.shape == stored.shape, (new.shape, stored.shape)

    cos = (stored * new).sum(1) / (np.linalg.norm(stored, axis=1) *
                                    np.linalg.norm(new, axis=1))
    print(f"{size}: {len(items)} items x {new.shape[1]} dims")
    print(f"  norms of new vectors: min {np.linalg.norm(new, axis=1).min():.6f}, "
          f"max {np.linalg.norm(new, axis=1).max():.6f}")
    print(f"  cosine(new, stored): min {cos.min():.8f}, median {np.median(cos):.8f}")
    print(f"  max |new - stored| over all coordinates: {np.abs(new - stored).max():.3e}")

    if out_path:
        np.savez(out_path, embeddings=new.astype(np.float32),
                 items=np.array(items), codes=ref["codes"],
                 model=np.array(model_name))
        print(f"  wrote {out_path}")


if __name__ == "__main__":
    if len(sys.argv) < 3:
        sys.exit(__doc__)
    main(sys.argv[1], sys.argv[2], sys.argv[3] if len(sys.argv) > 3 else None)
