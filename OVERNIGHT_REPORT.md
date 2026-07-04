# Overnight report — semanticfa 0.2.0 build (2026-07-04/05)

Status legend: [x] done and verified · [~] running when you read this
(check the noted location) · [ ] not started.

## Code (all in the working tree, uncommitted — review then commit)

- [x] `R/naming.R` — `sfa_name()`, the `sfa_labels` class + print method,
  naming targets, blocked top-k retrieval, family-dedup gate, tier-1 label
  rule, geometric keeper, leave-one-out candidate sets.
  `sfa_naming_instruction()` exported.
- [x] `R/pool.R` — `sfa_pool()` fetch/build/cache; GitHub-release download
  with manifest + sha256 + multi-part reassembly (2 GB asset cap); numpy
  memory-mapped reading through reticulate (fp16 and int8+scales); local
  pool building for arbitrary models.
- [x] `sfa(label_factors = TRUE)` stores labels as `fit$labels`.
- [x] DESCRIPTION → 0.2.0 (+ jsonlite in Suggests); NEWS.md entry;
  roxygen docs regenerated (NAMESPACE exports sfa_name, sfa_pool,
  sfa_naming_instruction, print methods).
- [x] Vignette `vignettes/naming-factors.Rmd` (eval=FALSE chunks; covers
  basic use, candidate sets, two-encoder naming, reproducibility, the
  pole convention).
- [x] `data-raw/` — provenance scripts (wordlist builder, fixture
  exporter, manifest/split builder) + the canonical wordlist CSV.

## Tests

- [x] `tests/testthat/test-naming.R` — 26 assertions, all passing:
  gate dedup, tier-1-else-top rule, pole restriction + contrast in the
  targets, blocked-retrieval == direct-retrieval, keeper (incl. LOO-set
  preference), LOO sets, and an end-to-end synthetic run producing
  "anxiety"/"depression" correctly with a mocked embedder.
- [x] `tests/testthat/test-naming-golden.R` — PARITY CONFIRMED: the R
  selection layer reproduces all 42 official research-pipeline labels
  from the exported fixture, including the collision-resolved 431PTQ
  pair. The R port is faithful to the Python reference.

## Artifacts (for the GitHub release `pools-v1`)

- [x] Canonical word list: 369,703 eligible terms (367,926 census +
  1,777 ontology additions), with `family` and `tier1` columns —
  `artifacts_semanticfa/wordlist.rds` in the research repo.
- [x] 8B pool, eligible rows, fp16 (3.03 GB) + int8 (1.51 GB) + scales —
  DONE, local `artifacts_semanticfa/` (research repo).
- [x] 0.6B pool — DONE on Fir (767 MB npz, frame_anchors/embeddings/).
- [x] 4B pool — DONE (resubmitted to scratch after a home-quota kill);
  staged on Fir with 0.6B and Harrier in
  bakeoff_run/artifacts/ as fp16 + int8 + scales.
- [x] Harrier pool eligible-filtered — DONE on Fir
  (bakeoff_run/artifacts/): fp16 3.975 GB (2-part split needed),
  int8 1.9875 GB + scales (single file, fits the 2 GB cap).
- [ ] Manifests + 2 GB splitting (`data-raw/make_manifests.R`) — run after
  all pools exist; then upload assets to a `pools-v1` GitHub release and
  the download path is live end-to-end.

Expected asset sizes (single-file unless noted): 0.6B fp16 0.76 GB;
4B fp16 1.89 GB; 8B fp16 3.03 GB (2 parts) / int8 1.51 GB; Harrier fp16
3.98 GB (2 parts) / int8 1.99 GB (just under the cap — verify).

## The open decision (yours) — now with complete numbers

fp16 vs int8 as the default download. FINAL PRECISION RESULTS:

| | int8 vs fp16 | int4 vs fp16 |
|---|---|---|
| Qwen-8B (42 factors) | 1 differs ("stress resilience"->"resilience", weak factor) | 11 differ (near-synonyms) |
| Harrier (33 factors) | 2 differ (both weak factors) | 32 differ (garbage) |

int4 is dead. int8 changes 3/75 labels total, all on weak factors, all to
near-synonyms or candidate-set siblings. Your call: fp16-only (strict
label identity; 8B and Harrier need 2-part downloads), or int8 default
with the 3-label diff table published in the docs (every pool a single
<2 GB GitHub asset). Both precisions are fully built for all four models;
the choice only sets a default argument and the manifest contents.

## Not done / next

- FULL TEST SUITE GREEN: 268 passed / 0 failed / 0 skipped (whole
  package, including golden parity).
- R CMD check full pass (document() clean, tests green; the formal
  R CMD check run is the remaining pre-CRAN step).
- GitHub release creation + asset upload (needs your account; assets are
  staged once manifests run).
- README section on naming (small; can fold in with the release).
- Nothing committed to git — the working tree is yours to review.
