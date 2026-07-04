#!/usr/bin/env python3
"""
Embed a plain candidate word list (one term per line) with Qwen3-Embedding-8B,
using the SAME last-token pooling + L2-normalization as the scale-item embeddings
(generate_scale_embeddings_single.py) so the candidate vectors live in the
identical space as the item embeddings. This produces the candidate pool for the
CSLS-based factor-naming method (Method 7) in qwen3_efa_v2.py.

Input : a headerless text file, one candidate string per line (e.g.
        word_lists/constructs_full.csv). Read as raw lines to avoid pandas
        coercing tokens like "nan"/"null" to NaN.
Output: an .npz with keys
            word_embeddings : float16 [N, D]   (L2-normalized)
            words           : object  [N]
            embedding_dim, model, normalized, pooling  (metadata)
"""

from __future__ import annotations

import argparse
import time
from pathlib import Path

import numpy as np
import torch
import torch.nn.functional as F
from torch import Tensor
from transformers import AutoModel, AutoTokenizer


def detect_device() -> tuple[str, int]:
    if torch.cuda.is_available():
        return "cuda", torch.cuda.device_count()
    if torch.backends.mps.is_available():
        return "mps", 1
    return "cpu", 1


def last_token_pool(last_hidden_states: Tensor, attention_mask: Tensor) -> Tensor:
    """Identical to generate_scale_embeddings_single.py (left-padded last token)."""
    left_padding = attention_mask[:, -1].sum() == attention_mask.shape[0]
    if left_padding:
        return last_hidden_states[:, -1]
    sequence_lengths = attention_mask.sum(dim=1) - 1
    batch_size = last_hidden_states.shape[0]
    return last_hidden_states[
        torch.arange(batch_size, device=last_hidden_states.device),
        sequence_lengths,
    ]


def read_wordlist(path: Path) -> list[str]:
    words = []
    with path.open("r", encoding="utf-8") as f:
        for line in f:
            w = line.strip()
            if not w or w.lower() == "word":  # skip blank lines / a stray header
                continue
            words.append(w)
    return words


def main() -> None:
    p = argparse.ArgumentParser()
    p.add_argument("--wordlist", required=True, help="headerless one-term-per-line file")
    p.add_argument(
        "--model-name",
        default="/home/devon7y/scratch/devon7y/huggingface/hub/"
                "models--Qwen--Qwen3-Embedding-8B/snapshots/"
                "1d8ad4ca9b3dd8059ad90a75d4983776a23d44af",
    )
    p.add_argument("--output", required=True, help="output .npz path")
    p.add_argument("--batch-size", type=int, default=1024)
    p.add_argument("--max-length", type=int, default=64,
                   help="candidates are short; 64 tokens is ample and fast")
    args = p.parse_args()

    wl_path = Path(args.wordlist)
    if not wl_path.exists():
        raise FileNotFoundError(f"Word list not found: {wl_path}")
    words = read_wordlist(wl_path)
    print(f"Loaded {len(words):,} candidate strings from {wl_path}", flush=True)

    out_path = Path(args.output)
    out_path.parent.mkdir(parents=True, exist_ok=True)

    device, num_devices = detect_device()
    print(f"Loading model: {args.model_name}\nDevice: {device}", flush=True)
    if device == "cuda":
        for idx in range(num_devices):
            print(f"  GPU {idx}: {torch.cuda.get_device_name(idx)}", flush=True)

    tokenizer = AutoTokenizer.from_pretrained(args.model_name, padding_side="left")
    model_kwargs: dict[str, object] = {"low_cpu_mem_usage": True}
    if device == "cuda":
        model_kwargs["torch_dtype"] = torch.bfloat16
    model = AutoModel.from_pretrained(args.model_name, **model_kwargs)
    model.to(device)
    model.eval()

    n = len(words)
    n_batches = (n + args.batch_size - 1) // args.batch_size
    print(f"Encoding {n:,} candidates in {n_batches:,} batches "
          f"(batch_size={args.batch_size}) ...", flush=True)

    chunks = []
    t0 = time.time()
    for bi, start in enumerate(range(0, n, args.batch_size), 1):
        end = min(start + args.batch_size, n)
        batch_text = words[start:end]
        batch_dict = tokenizer(
            batch_text, padding=True, truncation=True,
            max_length=args.max_length, return_tensors="pt",
        )
        batch_dict = {k: v.to(device) for k, v in batch_dict.items()}
        with torch.inference_mode():
            outputs = model(**batch_dict)
            emb = last_token_pool(outputs.last_hidden_state, batch_dict["attention_mask"])
            emb = F.normalize(emb, p=2, dim=1)
        chunks.append(emb.to(torch.float16).cpu().numpy())
        if bi % 50 == 0 or bi == n_batches:
            rate = end / max(time.time() - t0, 1e-6)
            eta = (n - end) / max(rate, 1e-6)
            print(f"  batch {bi:,}/{n_batches:,}  ({end:,}/{n:,})  "
                  f"{rate:,.0f} str/s  ETA {eta/60:,.1f} min", flush=True)

    embeddings = np.vstack(chunks).astype(np.float16)
    print(f"Embeddings: {embeddings.shape} dtype={embeddings.dtype}", flush=True)

    np.savez(
        out_path,
        word_embeddings=embeddings,
        words=np.array(words, dtype=object),
        embedding_dim=int(embeddings.shape[1]),
        model=str(args.model_name),
        normalized=True,
        pooling="last_token",
    )
    size_gb = out_path.stat().st_size / 1e9
    print(f"Saved {out_path}  ({size_gb:.2f} GB)", flush=True)


if __name__ == "__main__":
    main()
