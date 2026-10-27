# Reproduction materials for the semanticfa paper

This folder regenerates every number, table, and figure in the paper's
worked example from raw inputs. It is designed to run cleanly on an outside
machine. It lives in the package's GitHub repository
(<https://github.com/devon7y/semanticfa>), and the version used for the
paper is archived as a fixed release on Zenodo (DOI given in the paper's
Open Practices Statement).

## Contents

| Path | What it is |
|---|---|
| `reproduce.R` | The master analysis script. Runs the full semanticfa workflow on the 50 IPIP Big-Five Factor Markers and the human-response benchmark. The high-fidelity Qwen/Qwen3-Embedding-8B item embeddings ship with the package itself (`data(big5)`). |
| `reproduce.Rout` | The captured console transcript of a complete run (`R CMD BATCH`): every command together with its real output. |
| `data/Big5FM_aux_8B.npz` | Qwen3-Embedding-8B vectors for the five construct names and the projection pole words, extracted from a precomputed 1M-word lexicon by `data-raw/extract_aux_8B.py`. |
| `data/Big5FM_cand_8B.npz` | Qwen3-Embedding-8B vectors for the three new candidate items vetted by `sfa_item_fit()`, generated offline with the same settings as the item vectors by `data-raw/embed_candidates_8B.py`. |
| `data/Big5_items_0.6B.npz`, `data/Big5_items_4B.npz` | The same 50 items embedded offline with the smaller models of the same family (Qwen/Qwen3-Embedding-0.6B, 50 x 1024, and -4B, 50 x 2560), for the model-size comparison. |
| `data/DASS_items_0.6B.npz`, `data/DASS_items_4B.npz`, `data/DASS_items_8B.npz` | The 42 items of the Depression Anxiety Stress Scales (Lovibond & Lovibond, 1995) embedded offline with the three model sizes and the same settings (42 x 1024, 42 x 2560, and 42 x 4096), with each item's code and scale, for the model-size comparison with a second scale (Section 11c of the script, the Supplementary Materials of the paper). |
| `data/Big5FM_data.csv` | Human responses to the same 50 items (columns `E1..O50`, 5-point Likert, `0` = skipped, empty = missing; 1,015,341 records), from the Open-Source Psychometrics Project's IPIP-FFM release `IPIP-FFM-data-8Nov2018.zip` (November 8, 2018): <https://openpsychometrics.org/_rawdata/>. Not redistributed here. To rebuild it, download that zip and run the R snippet under "Rebuilding the human-response file" below, which keeps the first 50 columns of `data-final.csv` (`EXT1..EXT10`, `EST1..EST10`, `AGR1..AGR10`, `CSN1..CSN10`, `OPN1..OPN10`, in that order) and renames them to the package's item codes. |
| `data-raw/extract_aux_8B.py` | Provenance script for `Big5FM_aux_8B.npz`. |
| `data-raw/embed_candidates_8B.py` | Provenance script for `Big5FM_cand_8B.npz`. |
| `data-raw/embed_items.py` | Re-embeds the items of a stored archive with Qwen3-Embedding-0.6B, -4B, or -8B by the same route and reports the cosine and largest coordinate difference against the stored item vectors (the bundled 8B matrix, the two smaller-model Big Five archives, and the three DASS archives). |
| `output/` | Machine-readable results written by the script (`results.rds` plus CSV tables, including `calibration_null.csv`, the RMSR, CAF, and TEFI of the 100 fits to random embeddings, whether each rotation converged, and how many convergence warnings it issued, and `sim_nli.csv`, the signed
NLI similarity matrix before its eigenvalue repair), and `embed_items_check.txt`, the output of `data-raw/embed_items.py` for the three model sizes (Hugging Face revisions, library versions, and the cosine between each re-embedded and stored item vector), and `embed_items_check_dass.txt`, the same check for the three DASS archives. |
| `figures/` | The paper's figures, regenerated as PDFs. |

## How to run

```sh
cd reproduce
R CMD BATCH --no-save --no-timing reproduce.R
```

On systems whose R uses FlexiBLAS, set `FLEXIBLAS=NETLIB` before running
to match the reference linear algebra of the reported run.

Requirements:

- R (>= 4.1) with **semanticfa** (>= 0.5.1), **psych**, **EGAnet**, **Rtsne**, **uwot**.
  Section 8 writes the archived candidate-item vectors into the package's
  embedding cache with an internal function of semanticfa 0.5.1, the version
  of the reported run; a later version that changes that function would
  need this step adjusted
- A Python visible to **reticulate** with `numpy` (used to read the `.npz`
  archives). Section 3 (the NLI similarity matrix) additionally uses
  `sentence-transformers`; reticulate provisions it automatically on first
  use, and the cross-encoder (`cross-encoder/nli-deberta-v3-base`) is
  downloaded from the Hugging Face Hub on first run.

Everything else - the factor analyses, retention methods, diagnostics,
congruence metrics, redundancy/short-form/projection analyses, and all
figures - is pure R and runs offline.

No human response data enters the semantic analyses at any point; the
responses are read only in Sections 10-11, as the validation target.

## Rebuilding the human-response file

Download `IPIP-FFM-data-8Nov2018.zip` from
<https://openpsychometrics.org/_rawdata/> into `reproduce/`, then run from
`reproduce/`:

```r
raw <- read.delim(unz("IPIP-FFM-data-8Nov2018.zip",
                      "IPIP-FFM-data-8Nov2018/data-final.csv"),
                  na.strings = "NULL")
items <- raw[, 1:50]   # EXT1..EXT10, EST1..EST10, AGR1..AGR10, CSN1..CSN10, OPN1..OPN10
names(items) <- semanticfa::big5$codes   # E1..E10, N11..N20, A21..A30, C31..C40, O41..O50
write.csv(items, "data/Big5FM_data.csv", row.names = FALSE)
```

The result has 1,015,341 rows. Its item values are identical to those of
the file used for the reported run (checked by comparing the 50 item
columns of `data-final.csv`, with `NULL` read as missing, against that
file). `reproduce.R` then removes every record with a missing response or
a response outside 1 to 5.

The reported run was made on a compute node of the Nibi cluster of the
Digital Research Alliance of Canada (Linux x86-64, AlmaLinux 9.8, R with
FlexiBLAS set to the reference NETLIB BLAS). The batch job requested 16
CPU cores and no GPU, so the NLI matrix was computed on the CPU. The
script does not set the `mc.cores` option, so `psych::fa.parallel()`
ran its simulations through `parallel::mclapply()` with that function's
default of two worker processes.
