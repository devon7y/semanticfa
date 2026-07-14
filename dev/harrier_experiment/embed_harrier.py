#!/usr/bin/env python3
"""Embed the 50 IPIP Big-Five items + 15 auxiliary strings with
microsoft/harrier-oss-v1-27b, mirroring the Qwen3 generation conventions
(plain document encoding, no instruction prompt).

Primary path: sentence-transformers. Fallback (some transformers versions
misroute gemma3_text through a multimodal processor): manual pipeline that
reproduces modules.json exactly -- Transformer -> last-token Pooling ->
L2 Normalize.
"""
import csv
import datetime
import os
import sys

import numpy as np
import torch

MODEL = "microsoft/harrier-oss-v1-27b"
BASE = os.path.dirname(os.path.abspath(__file__))

AUX_WORDS = [
    "extraversion", "agreeableness", "conscientiousness",
    "emotional stability", "openness to experience",
    "neuroticism", "openness",
    "anxious", "calm", "solitary", "sociable",
    "organized", "careless", "curious", "conventional",
]


def encode_st(texts):
    from sentence_transformers import SentenceTransformer
    model = SentenceTransformer(MODEL, device="cuda",
                                model_kwargs={"dtype": "auto"})
    print("ST path; dim =", model.get_sentence_embedding_dimension(),
          flush=True)
    return np.asarray(model.encode(texts, batch_size=8,
                                   show_progress_bar=True)), "sentence-transformers"


def encode_manual(texts):
    from transformers import AutoModel, AutoTokenizer
    tok = AutoTokenizer.from_pretrained(MODEL)
    model = AutoModel.from_pretrained(MODEL, dtype="auto",
                                      device_map="cuda").eval()
    outs = []
    with torch.no_grad():
        for i in range(0, len(texts), 8):
            batch = tok(texts[i:i + 8], padding=True, truncation=True,
                        max_length=512, return_tensors="pt").to("cuda")
            h = model(**batch).last_hidden_state
            # last non-pad token per sequence (mask-based; padding-side safe)
            idx = batch["attention_mask"].sum(dim=1) - 1
            pooled = h[torch.arange(h.size(0)), idx]
            pooled = torch.nn.functional.normalize(pooled, p=2, dim=1)
            outs.append(pooled.float().cpu().numpy())
            print(f"manual batch {i // 8 + 1}", flush=True)
    return np.vstack(outs), "manual last-token + L2 (mirrors modules.json)"


def main():
    import transformers
    print("transformers", transformers.__version__, "| torch",
          torch.__version__, flush=True)
    rows = list(csv.DictReader(open(os.path.join(BASE, "big5_items.csv"))))
    items = [r["item"] for r in rows]
    codes = [r["code"] for r in rows]
    factors = [r["factor"] for r in rows]
    scoring = [int(r["scoring"]) for r in rows]
    assert len(items) == 50

    texts = items + AUX_WORDS
    try:
        emb, path = encode_st(texts)
    except Exception as e:
        print("ST path failed:", repr(e)[:300], "\nfalling back to manual",
              flush=True)
        emb, path = encode_manual(texts)

    emb_items, emb_aux = emb[:50], emb[50:]
    print("items:", emb_items.shape, "aux:", emb_aux.shape, flush=True)
    norms = np.linalg.norm(emb_items, axis=1)
    print("item norms: min %.4f max %.4f" % (norms.min(), norms.max()),
          flush=True)

    meta = dict(
        model_name=MODEL, model_size="27B",
        embedding_dim=int(emb_items.shape[1]), num_items=50,
        embedding_mode="atomic", encode_path=path,
        pooling="last-token", prompt="none (document path)",
        normalized=bool(abs(norms.mean() - 1.0) < 1e-3),
        device=torch.cuda.get_device_name(0),
        timestamp=datetime.datetime.now().strftime("%Y%m%d_%H%M%S"),
    )
    np.savez(os.path.join(BASE, "Big5_items_Harrier27B.npz"),
             embeddings=emb_items.astype(np.float32),
             codes=np.array(codes), items=np.array(items),
             factors=np.array(factors), scoring=np.array(scoring),
             metadata=np.array(meta, dtype=object))
    np.savez(os.path.join(BASE, "Big5FM_aux_Harrier27B.npz"),
             embeddings=emb_aux.astype(np.float32),
             codes=np.array(AUX_WORDS),
             metadata=np.array(meta, dtype=object))
    print("DONE_OK", flush=True)


if __name__ == "__main__":
    sys.exit(main())
