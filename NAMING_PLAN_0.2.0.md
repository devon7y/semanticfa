# semanticfa 0.2.0 — factor-naming integration plan

Goal: add the encoder-only factor-naming method (FACTOR_NAMING_METHOD_CURRENT.md
in the research repo) as a first-class package feature, keeping the package's
existing conventions (reticulate/sentence-transformers backend, `sfa_*` naming,
psych-compatible objects, digest-keyed caching, CRAN-shippable).

## 1. How it integrates

New exported surface (follows the `sfa_*` convention):

- **`sfa_name(fit, model = NULL, pool = NULL, n_candidates = 5L,
  instruction = NULL, collision = TRUE, loo_sets = TRUE, ...)`**
  Primary entry point. Takes an `sfa` fit object (which already carries the
  item text, loadings, rotation, and the embedding model used), returns an
  `sfa_labels` object: one row per factor with `label`, `candidates`
  (leave-one-out set), `gated_top5`, `rule` (dictionary-noun vs top word),
  `collision_moved`, plus the per-factor items used. `print()` and
  `summary()` methods; a `label_factors = TRUE` convenience flag on `sfa()`
  that calls it inline and stores the result in `fit$labels` (DECIDED: the package vocabulary is "labels" throughout).
- **`sfa_pool(model, precision = c("fp16", "int8"), dir = NULL)`**
  Fetch-or-build the candidate pool for a given embedding model: downloads a
  pre-generated pool when one exists for that model, otherwise (with explicit
  user confirmation) embeds the shipped word list locally and caches it.
- `.sfa_name_targets()` (internal only — DECIDED): builds the
  instruction-embedded, pole-aware, loading-weighted, grand-mean-contrasted
  naming target per factor. Not exported; `sfa_name()` is the only naming
  API.

Pipeline placement: naming consumes only (a) the item text, (b) the fit's
loadings (works identically for exploratory `nfactors = NULL` and
confirmatory `nfactors = k` fits — CFA-mode needs no special support), and
(c) one new encode pass of the items under the naming instruction. It never
touches the similarity/extraction code, so 0.2.0 is purely additive — no
breaking changes.

## 2. Model handling (default = extraction model, override allowed)

- `model = NULL` → reuse `fit$embed_model` (the Qwen model the user factored
  with). One model, one download, simplest story.
- `model = "microsoft/harrier-oss-v1-27b"` (or any sentence-transformers
  model id) → two-encoder mode: extraction stays as fitted; items are
  re-embedded with the naming model under the instruction; retrieval runs
  against that model's pool. The research result to cite in the docs: the
  extraction and naming tasks reward different geometry, and a larger namer
  materially improves label altitude.
- Download handling: sentence-transformers already downloads and caches any
  HF model on first `SentenceTransformer(model)` call — the existing
  `.embed_sbert()` path handles it. Additions needed: (i) a size warning
  with interactive confirmation before first download of a large naming
  model (Harrier-27B is ~54 GB — CRAN policy and basic courtesy both require
  explicit consent); (ii) clear error if the model lacks a
  sentence-transformers config; (iii) instruction formatting done by us
  ("Instruct: {task}\nQuery: {item}") rather than relying on per-model
  prompt configs, so behavior is identical across models. Pooling comes from
  each model's own ST config (Qwen3 and Harrier are both last-token).
- The naming instruction ships as an exported constant
  (`sfa_naming_instruction()`), overridable with a documented warning
  (instruction sensitivity was measured: rewordings never flip the
  construct, only adjacent phrasings).

## 3. Shipping the word list (pre-filtered)

- Ship the ELIGIBLE subset only: 367,926 terms (36.8% of the census) — the
  words that can actually become labels. Shipping ineligible words wastes
  space to no effect (selection walks the ranking and takes the first
  eligible word; absent ineligible words change nothing).
- Ship it WITH its precomputed string-derived columns, so R needs no
  WordNet/NLTK at runtime:
  `word | family | tier1`
  (family = per-token WordNet noun lemmatization used for dedup; tier1 =
  single-noun dictionary membership). All rules run offline in Python once
  (data-raw/build_pool_wordlist.py, checked into the repo with a SHA-256
  manifest); R just does data-frame walks.
- Size: ~368k rows, three columns, xz-compressed .rds ≈ 2–3 MB. That is
  inside CRAN's ~5 MB comfort zone but leaves no headroom; DECISION POINT:
  (a) ship in `inst/extdata/` (works offline, bigger tarball) or (b) treat
  the word list like the embeddings — fetched once into the user cache from
  the artifact host. DECIDED (b): fetched, tarball stays tiny, word list and embeddings
  version together.

## 4. Pre-generated pool embeddings (the big artifacts)

- Cannot ship in the package (CRAN limit). DECIDED: host on the package's
  existing GitHub via Releases if quantization validates — release assets
  cap at 2 GB/file, which all int8 pools respect (Harrier int8 ~2.0 GB is
  borderline; verify the exact byte count) — otherwise fp16 pools need
  multi-part splitting on GitHub or a HF dataset repo as fallback. The
  precision test decides. Download on first use into
  `tools::R_user_dir("semanticfa", "cache")` with explicit user
  confirmation and a printed size; extend `sfa_clear_cache()` to pools.
- Generate for the three package-default Qwen models (one H100 job each,
  scripts already exist in the research repo):

  | Model | dim | fp16 size | int8 size |
  |---|---|---|---|
  | Qwen3-Embedding-0.6B | 1024 | ~0.75 GB | ~0.38 GB |
  | Qwen3-Embedding-4B | 2560 | ~1.9 GB | ~0.94 GB |
  | Qwen3-Embedding-8B | 4096 | ~3.0 GB | ~1.5 GB |

  Plus Harrier-OSS-27B (5376-d, ~4.0 GB fp16) — DECIDED: all four pools
  (0.6B, 4B, 8B, Harrier) are pre-generated and hosted.
- Precision test RESULTS (2026-07-04, precision_test.py, both encoders):
  int8 changes 3/75 benchmark labels total (Qwen 1/42: "stress resilience"
  -> "resilience" on a weak factor; Harrier 2/33, both weak factors, both
  near-synonyms/set members). int4 FAILS both arms (Qwen 11/42 —
  gracefully, to near-synonyms; Harrier 32/33 — catastrophically).
  DECISION PENDING (PI): fp16-only vs int8-default-with-published-diff.
  Both precisions are built and staged for all four models either way.
- Arbitrary user models: `sfa_pool(model)` builds locally — embeds the 368k
  words through the ST backend with a progress bar and a time estimate
  (~minutes on GPU, hours on CPU), cached thereafter. This keeps the feature
  model-agnostic instead of Qwen-locked.
- File format: fp16 .npy exactly as produced (single flat array +
  wordlist rds). R reads it via reticulate/numpy with `mmap_mode="r"` —
  reticulate is already an Import, and memory-mapping keeps the 3 GB pool
  out of RAM. Retrieval is a blocked matrix multiply (50k-row blocks),
  ported 1:1 from the research code.

## 5. Algorithm port (R/naming.R) — everything is linear algebra + table walks

1. Naming targets: from the fit's loadings — primary assignment, dominant
   pole, |loading| weights, instruction-embedded item matrix, centroid,
   subtract questionnaire grand mean (alpha = 1), renormalize. (Port of
   the frozen research recipe; ~40 lines.)
2. Retrieval + gating: blocked cosine against the pool; walk down, skip
   family-duplicates (shipped `family` column), stop at `n_candidates`;
   label = first `tier1` candidate else top. (No WordNet at runtime.)
3. Collision keeper: same-scale same-family collisions resolved by target
   similarity; loser re-picks (LOO-set members first, then widened gate).
4. LOO candidate sets: per factor, drop each item, rebuild target, argmax
   over the factor's top-20 family shortlist; union of fold winners.
5. Composition report: if the user supplies a `constructs` vector (item →
   intended construct), report dominant construct + purity per factor
   alongside labels (the honesty column from the research method).

## 6. Version 0.2.0 package chores

- DESCRIPTION 0.2.0; NEWS.md entry; new man pages; README section with the
  two-encoder example; vignette "Naming the factors" (Big5 walkthrough:
  default model → labels; then Harrier override; then confirmatory-k).
- Tests: unit tests against a tiny bundled toy pool (~200 words, built in
  data-raw) so CRAN tests run offline in seconds; integration test that
  reproduces the DASS benchmark labels, `skip_on_cran()` +
  `skip_if_no_python()`.
- CRAN compliance checklist: no unconditional downloads, all fetches behind
  `interactive()` confirmation or explicit `download = TRUE`, cache in
  `R_user_dir`, graceful skips without Python.

## 7. Build order (proposed)

1. data-raw scripts: eligible word list + flags export; pool filtering to
   eligible subset for the existing 8B + Harrier embeddings. (Research repo
   already has everything; this is repackaging.)
2. GPU jobs: 0.6B and 4B pool embeddings (not yet generated). [cluster]
3. R/naming.R + R/pool.R with the toy-pool unit tests.
4. Integration test against the 8B pool; verify benchmark label identity
   with the research pipeline (golden test).
5. int8 validation run; publish artifacts to the HF dataset repo.
6. Vignette, docs, NEWS, version bump, R CMD check, submit.

Open questions for Devon before implementation:
- Word list in-package vs fetched (recommendation: fetched, §3)?
- Is Harrier's 54 GB model download acceptable as a documented option, or
  should two-encoder mode also support a hosted "Harrier pool + API/remote
  embedding" path for users without the hardware? (Plain answer: without a
  GPU box, Harrier-27B locally is painful; the pool download alone is fine
  but the item re-embedding needs the model.)
- int8 as default-if-validated, or fp16 default with int8 opt-in?
