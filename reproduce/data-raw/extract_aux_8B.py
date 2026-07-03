#!/usr/bin/env python3
"""Extract auxiliary Qwen3-Embedding-8B vectors for the semanticfa paper.

The paper's main analyses use precomputed Qwen3-Embedding-8B item embeddings
(Big5FM_items_8B.npz). Functions that compare items against *new* text --
sfa_anchor(anchor = "label"), sfa_project(), sfa_jinglejangle() -- need
embeddings of construct names and pole words from the SAME model so all
vectors live in one space. Those were precomputed in a large word lexicon
(constructs_full_8B.npz: 1,001,041 words x 4096, Qwen/Qwen3-Embedding-8B,
last-token pooling, L2-normalized, float16). That file is ~8 GB, so this
script extracts only the handful of rows the paper needs and writes a small
archive, Big5FM_aux_8B.npz, that ships with the reproduction materials.

The lexicon .npz stores its arrays uncompressed, so rows are read by direct
byte offset instead of loading the full array into memory.

Usage: python3 extract_aux_8B.py /path/to/constructs_full_8B.npz ../data/Big5FM_aux_8B.npz
"""
import ast
import sys
import zipfile

import numpy as np

WORDS = [
    # construct labels (the lexicon is lower-case)
    "extraversion", "agreeableness", "conscientiousness",
    "emotional stability", "openness to experience",
    "neuroticism", "openness",
    # bipolar-axis pole words for sfa_project()
    "anxious", "calm", "solitary", "sociable",
    "organized", "careless", "curious", "conventional",
]


def npy_header(f):
    """Parse a .npy header at the current file position; return (dtype, shape, data_start)."""
    magic = f.read(6)
    assert magic == b"\x93NUMPY", "not a .npy member"
    major, _minor = f.read(1), f.read(1)
    if major == b"\x01":
        hlen = int.from_bytes(f.read(2), "little")
    else:
        hlen = int.from_bytes(f.read(4), "little")
    header = ast.literal_eval(f.read(hlen).decode("latin1"))
    assert not header["fortran_order"]
    return np.dtype(header["descr"]), header["shape"], f.tell()


def main(lexicon_path, out_path):
    zf = zipfile.ZipFile(lexicon_path)
    with zf.open("words.npy") as f:
        words = np.lib.format.read_array(f, allow_pickle=True)
    index = {str(w): i for i, w in enumerate(words.tolist())}
    missing = [w for w in WORDS if w not in index]
    assert not missing, f"words not in lexicon: {missing}"

    info = zf.getinfo("word_embeddings.npy")
    assert info.compress_type == zipfile.ZIP_STORED
    raw = open(lexicon_path, "rb")
    # zip local header: 30 bytes + name + extra field precede the member data
    raw.seek(info.header_offset)
    lh = raw.read(30)
    name_len = int.from_bytes(lh[26:28], "little")
    extra_len = int.from_bytes(lh[28:30], "little")
    member_start = info.header_offset + 30 + name_len + extra_len
    raw.seek(member_start)
    dtype, shape, data_off = npy_header(raw)
    n_dim = shape[1]
    row_bytes = n_dim * dtype.itemsize

    rows = np.empty((len(WORDS), n_dim), dtype=np.float32)
    for k, w in enumerate(WORDS):
        raw.seek(data_off + index[w] * row_bytes)
        rows[k] = np.frombuffer(raw.read(row_bytes), dtype=dtype).astype(np.float32)
    raw.close()

    np.savez_compressed(
        out_path,
        embeddings=rows,
        codes=np.array(WORDS),
        metadata=np.array(
            {
                "source": "constructs_full_8B.npz (1,001,041-word lexicon)",
                "model_name": "Qwen/Qwen3-Embedding-8B",
                "pooling": "last_token",
                "normalized": True,
                "note": "rows extracted verbatim; float16 -> float32",
            },
            dtype=object,
        ),
    )
    print(f"wrote {out_path}: {rows.shape} ({', '.join(WORDS)})")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
