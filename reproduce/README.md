# Reproduction materials for the semanticfa paper

This folder regenerates every number, table, and figure in the paper's
demonstration from raw inputs. It is designed to run cleanly on an outside
machine and will be hosted on the Open Science Framework (OSF).

## Contents

| Path | What it is |
|---|---|
| `reproduce.R` | The master analysis script. Runs the full semanticfa workflow on the 50 IPIP Big-Five Factor Markers and the human-response benchmark. The high-fidelity Qwen/Qwen3-Embedding-8B item embeddings ship with the package itself (`data(big5)`). |
| `reproduce.Rout` | The captured console transcript of a complete run (`R CMD BATCH`): every command together with its real output. |
| `data/Big5FM_aux_8B.npz` | Qwen3-Embedding-8B vectors for the five construct names and the projection pole words, extracted from a precomputed 1M-word lexicon by `data-raw/extract_aux_8B.py`. |
| `data/Big5_items_0.6B.npz` | Reference embeddings of the same 50 items generated offline with the package's default on-device model (Qwen/Qwen3-Embedding-0.6B; 50 x 1024). Used to verify the live `sfa_embed()` backend and as the matching space for candidate-item vetting. |
| `data/Big5FM_data.csv` | Human responses to the same 50 items (columns `E1..O50`, 5-point Likert, `0` = skipped), from the Open-Source Psychometrics Project "Big Five Personality Test" raw-data release: <https://openpsychometrics.org/_rawdata/>. Not redistributed here if licensing forbids it — download `BIG5` from that page and keep only the 50 item columns, in the order of the header row. |
| `data-raw/extract_aux_8B.py` | Provenance script for `Big5FM_aux_8B.npz`. |
| `output/` | Machine-readable results written by the script (`results.rds` plus CSV tables). |
| `figures/` | The paper's figures, regenerated as PDFs. |

## How to run

```sh
cd reproduce
R CMD BATCH --no-save --no-timing reproduce.R
```

Requirements:

- R (>= 4.1) with **semanticfa** (>= 0.1.0), **psych**, **EGAnet**, **Rtsne**, **uwot**
- A Python visible to **reticulate** with `numpy` (used to read the `.npz`
  archives). Sections 3-4 (the NLI similarity matrix and the live
  `sfa_embed()` demonstration) additionally use `sentence-transformers`;
  reticulate provisions it automatically on first use. Both sections download
  their models from the Hugging Face Hub on first run
  (`cross-encoder/nli-deberta-v3-base`, `Qwen/Qwen3-Embedding-0.6B`).

Everything else - the factor analyses, retention methods, diagnostics,
congruence metrics, redundancy/short-form/projection analyses, and all
figures - is pure R and runs offline.

No human response data enters the semantic analyses at any point; the
responses are read only in Sections 10-11, as the validation target.
