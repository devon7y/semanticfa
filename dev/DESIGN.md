# semanticfa — Design Document

**Package display name:** Semantic Factor Analysis
**CRAN package name:** `semanticfa` (verified available on CRAN)
**Headline function:** `sfa()`
**License:** GPL-3
**Minimum R:** >= 4.1.0

---

## 1. Package scope

Given item text from a psychological scale, `semanticfa` embeds each item with a
language model, transforms embeddings into an item-by-item similarity matrix, and runs
exploratory factor analysis — recovering loadings, factor correlations, communalities,
and fit indices entirely from text, with no human response data required.

**In scope:** embedding → similarity → EFA pipeline, embedding-adapted diagnostics.
**Out of scope:** factor naming/labeling, item generation, CFA/SEM, anything LLM-generative.

---

## 2. Dependency strategy

### Imports (hard dependencies — always installed)
- `psych` — factor extraction, rotation, and the `fa` return structure we mirror
- `GPArotation` — required by the default `rotate = "oblimin"` (psych delegates to it);
  must be in Imports so the out-of-the-box `sfa(items)` call never fails on a clean install
- `withr` — `with_seed()` for CRAN-safe RNG hygiene (set/restore local seed without
  touching the user's global `.Random.seed`)
- `stats`, `utils`, `methods` — base R

### Suggests (optional — guarded by `requireNamespace()`)
- `reticulate` — Python bridge for sentence-transformers (`embed = "sbert"`)
- `httr2` — API embedding backends (`embed = "openai"`)
- `digest` — SHA-based cache keys for `sfa_embed()` caching; when absent, falls back
  to a base-R `paste0(nchar(), sum(utf8ToInt()))` fingerprint (less collision-resistant
  but functional)
- `EFAtools` — optional alternative extraction + `N_FACTORS()` comparison
- `EGAnet` — EGA-based factor retention
- `testthat` (>= 3.0.0), `knitr`, `rmarkdown` — testing & vignettes

**Design rationale:** `psych` + `GPArotation` + `withr` are the only real hard
dependencies. Every embedding backend is in Suggests so the package installs and runs
on CRAN with zero Python/network, using precomputed `embeddings=`. `digest` is in
Suggests rather than Imports because caching is opt-in and a base-R fallback exists.

---

## 3. Exported API

### 3.1 `sfa()` — main entry point

```r
sfa(
  items,
  nfactors        = NULL,
  rotate          = "oblimin",
  fm              = "minres",
  encoding        = "atomic_reversed",
  embed           = "sbert",
  model           = "all-MiniLM-L6-v2",
  embeddings      = NULL,
  scoring         = NULL,
  n_factors_method = "parallel",
  n.obs           = NA,
  parallel_iter   = 100,
  seed            = 42,
  calibrate       = FALSE,
  calibrate_iter  = 100,
  ...
)
```

**Argument-by-argument justification against `psych::fa()` / `EFAtools::EFA()`:**

| Argument | psych equiv. | EFAtools equiv. | Notes |
|---|---|---|---|
| `items` | `r` (data/corr) | `x` | Character vector of item text; or a data.frame with `item` column (+ optional `factor`, `scoring` columns); or a named character vector where names are item codes. |
| `nfactors` | `nfactors` | `n_factors` | `NULL` = automatic retention (embedding parallel analysis). psych defaults to 1; we default to auto since the whole point is structure discovery. |
| `rotate` | `rotate` | `rotation` | Same rotation names as psych (oblimin, promax, varimax, quartimax, none, ...). Passed through directly. Default `"oblimin"` requires `GPArotation` (in Imports). |
| `fm` | `fm` | `method` | Same extraction names as psych (minres, ml, pa, wls, gls, uls). "minres" matches the reference implementation default. |
| `encoding` | — | — | New: the similarity transform. See §4. |
| `embed` | — | — | New: embedding backend. See §5. |
| `model` | — | — | New: model name/identifier for the embedding backend. |
| `embeddings` | — | — | New: precomputed n_items × dim numeric matrix. Skips embedding entirely. |
| `scoring` | — | — | New: numeric vector of +1/−1 per item. **If NULL, defaults to all +1** (equivalent to `"atomic"` encoding) **with an informative message** when `encoding = "atomic_reversed"`. This matches the reference implementation's fallback. |
| `n_factors_method` | — | — | New: "parallel" (embedding-adapted), "kaiser", "EGA", "TEFI". |
| `n.obs` | `n.obs` | — | Passed through to psych::fa. NA by default (no sample-size-dependent indices). |
| `parallel_iter` | — | — | Iterations for embedding parallel analysis (maps to `qwen3_efa_v2.py`'s `parallel_iter=100`). |
| `seed` | — | — | Seed for parallel analysis and calibration. Used via `withr::with_seed()` — never touches the user's global `.Random.seed`. Named `seed` (not `random_state`) to match R conventions. |
| `calibrate` | — | — | If TRUE, run Monte Carlo null calibration (Pokropek 2026). |
| `calibrate_iter` | — | — | Iterations for calibration (default 100). |
| `...` | `...` | — | Passed through to `psych::fa()` for advanced options (e.g., `normalize`, `max.iter`). |

**Input handling for `items`:**
- `character vector`: each element is one item's text. Names (if present) become item codes; otherwise auto-generated (`item_01`, `item_02`, ...).
- `data.frame`: must have an `item` (or `text`) column. Optional columns: `code`, `factor`, `scoring`. If `scoring` column exists and the `scoring` argument is NULL, use it.
- In all cases, `embeddings=` overrides the embed step; `scoring=` overrides any data.frame column.

**`scoring = NULL` behaviour (graceful fallback):**
When `scoring` is NULL and no `scoring` column is found in the data.frame input:
- For `encoding = "atomic_reversed"` or `"squid"`: default to `rep(1, n_items)`,
  emit `message("No scoring provided; defaulting to all +1 (equivalent to 'atomic' encoding)")`.
- For `encoding = "atomic"` or `"mean_centered_pearson"`: silently use `rep(1, n_items)`.

### 3.2 Return value — class `"sfa"`

An S3 object of class `"sfa"` containing everything a `psych::fa` user expects, plus
embedding-specific diagnostics. The object stores the underlying `psych::fa` result
and mirrors its components at the top level.

```r
# --- psych-compatible components (direct from psych::fa) ---
$loadings        # class "loadings" matrix — works with psych::factor.congruence(), fa.sort(), etc.
$Phi             # factor correlation matrix (NULL for orthogonal rotations)
$communality     # named numeric vector
$communalities   # alias (psych uses both)
$uniquenesses    # named numeric vector
$values          # eigenvalues of the input matrix
$e.values        # alias
$Vaccounted      # variance accounted for (SS loadings, proportion, cumulative)
$rotation        # string: rotation used
$fm              # string: extraction method
$factors         # integer: number of factors extracted
$residual        # residual correlation matrix
$fit             # psych's fit index
$fit.off         # off-diagonal fit
$complexity      # Hofmann's item complexity
$Structure       # structure matrix (for oblique rotations)
$rot.mat         # rotation matrix
$weights         # factor score weights
$scores          # NULL (no response data for scoring)
$n.obs           # NA or user-supplied
$Call            # the sfa() call

# --- Embedding-specific components (new) ---
$encoding        # string: encoding method used
$embed_method    # string: embedding backend used
$embed_model     # string: model name
$embedding_dim   # integer: embedding dimensionality
$sim_matrix      # the computed similarity matrix (before EFA)
$embeddings      # the (optionally transformed) embedding matrix
$kmo             # list: $total (overall KMO), $per_item (per-item KMO)
$tefi            # numeric: Total Entropy Fit Index
$rmsr            # numeric: root mean square residual (off-diagonal)
$caf             # numeric: Common part Accounted For
$omega           # data.frame: McDonald's omega per factor
$parallel        # list: $n_factors, $observed, $percentiles (from embedding PA)
$calibration     # list or NULL: Monte Carlo null distributions (if calibrate=TRUE)
$heywood         # logical vector: TRUE for items with communality > 1 (see §10)
$item_data       # data.frame: item codes, text, scoring, theoretical factors (if provided)

# --- Internal (not typically accessed) ---
$.fa             # the raw psych::fa object (for fallback access to any psych component)
```

**Key design choice:** We do NOT inherit from `"psych"` class. Instead, `sfa` is its
own class with `$loadings` of class `"loadings"` (which is what psych functions check).
This avoids method-dispatch conflicts with `print.psych()` while still being compatible:

```r
# These all work:
psych::factor.congruence(sfa_fit$loadings, human_fit$loadings)
psych::fa.sort(sfa_fit$loadings)
print(sfa_fit$loadings, cutoff = 0.3)  # uses loadings print method
```

We provide an explicit `as_psych()` generic + method that returns the internal `.fa`
object (class `c("psych", "fa")`) for users who need full psych dispatch.

### 3.3 Methods

```r
print.sfa(x, cutoff = 0.3, sort = TRUE, ...)
  # Prints: encoding, model, n_factors, rotation, KMO, TEFI, RMSR, CAF,
  # then the sorted loadings (delegating to print.loadings), then Phi,
  # then variance accounted for. If Heywood cases exist, prints a warning.

summary.sfa(object, ...)
  # More detailed: adds omega per factor, eigenvalue table, per-item KMO,
  # communalities (flagging Heywood cases), calibration results if present.

plot.sfa(x, type = c("scree", "loadings", "residuals"), ...)
  # type="scree": scree plot with parallel analysis threshold line
  # type="loadings": heatmap of factor loadings (sorted)
  # type="residuals": histogram of off-diagonal residuals
```

### 3.4 `sfa_embed()` — standalone embedding

```r
sfa_embed(
  items,
  embed = "sbert",
  model = "all-MiniLM-L6-v2",
  cache = TRUE,
  ...
)
# Returns: numeric matrix (n_items x embedding_dim), with item text as rownames.
# Cached in tools::R_user_dir("semanticfa", "cache") keyed by digest hash
# (or base-R fingerprint when digest is unavailable).
```

Users call this to precompute/inspect embeddings, then pass to `sfa(embeddings=)`.

### 3.5 `sfa_similarity()` — standalone similarity computation

```r
sfa_similarity(
  embeddings,
  encoding = "atomic_reversed",
  scoring = NULL
)
# Returns: n_items x n_items numeric matrix (the similarity/correlation matrix).
# scoring = NULL → defaults to all +1 with message (same as sfa()).
```

Useful for users who want to inspect or use the similarity matrix directly.

### 3.6 `sfa_parallel()` — embedding-adapted parallel analysis

```r
sfa_parallel(
  sim_matrix,
  embeddings,
  n_iter = 100,
  percentile = 95,
  seed = 42
)
# Returns: list with $n_factors, $observed (eigenvalues), $percentiles.
# Uses random-unit-vector null (no n_obs needed).
# Seed handled via withr::with_seed().
```

### 3.7 `sfa_nfactors()` — unified retention diagnostic table

```r
sfa_nfactors(
  sim_matrix,
  embeddings = NULL,
  methods = c("parallel", "kaiser", "TEFI"),
  seed = 42,
  parallel_iter = 100,
  max_factors = NULL,
  ...
)
# Returns: an object of class "sfa_nfactors" with:
#   $methods     — data.frame: method, n_factors, details (one row per method)
#   $consensus   — integer: modal recommendation across methods
#   $eigenvalues — numeric vector: observed eigenvalues
#   $parallel    — list: parallel analysis details (if "parallel" in methods)
#
# Mirrors EFAtools::N_FACTORS() — a single call that runs all requested
# retention rules and tabulates the results for comparison.
# "EGA" is available when EGAnet is installed.
```

`print.sfa_nfactors()` produces a compact table like:

```
Factor retention analysis (embedding-adapted)

  Method       n_factors
  Parallel           5
  Kaiser             6
  TEFI               5
  ──────────────────────
  Consensus          5

Eigenvalues: 12.34  5.67  3.21  2.10  1.45  0.98 ...
```

### 3.8 `sfa_congruence()` — optional comparison helper

```r
sfa_congruence(
  sfa_fit,
  target,
  metrics = c("tucker", "nmi", "ari", "frobenius", "disattenuated")
)
# sfa_fit: an "sfa" object
# target: a psych::fa object, loadings matrix, or factor label vector
# Returns: list with computed agreement metrics.
```

This thin wrapper computes:
- **Tucker phi** (via `psych::factor.congruence`) — factor-by-factor congruence
- **NMI** — normalized mutual information of item-to-factor partitions
- **ARI** — adjusted Rand index of item-to-factor partitions
- **Frobenius** — normalized Frobenius similarity of inter-factor correlation matrices
- **Disattenuated correlation** (Hommel & Arslan 2025) — latent correlation between
  the flattened lower-triangles of the two similarity/correlation matrices, corrected
  for measurement unreliability

All five are in the default `metrics` vector. This is the only human-comparison
surface; everything else uses existing psych/EFAtools tools.

### 3.9 `as_psych()` — coercion

```r
as_psych(x, ...)
as_psych.sfa(x, ...)
# Returns the internal psych::fa object with class c("psych", "fa").
```

---

## 4. Encoding / similarity transforms (`encoding=`)

All transforms ported from `qwen3_efa_v2.py`'s `build_similarity_matrix()`.

| Value | Internal function | Algorithm | Source |
|---|---|---|---|
| `"atomic_reversed"` (default) | `.apply_atomic_reversed()` | Multiply each embedding by its +1/−1 scoring, L2-normalize, cosine similarity. | Guenole et al. |
| `"atomic"` | `.apply_atomic_reversed()` with all-+1 scoring | No sign-flip; L2-normalize, cosine similarity. Equivalent to `atomic_reversed` with `scoring = rep(1, n)`. | Guenole et al. |
| `"squid"` | `.apply_squid()` | Subtract questionnaire-mean embedding, apply scoring sign-flip, L2-normalize, cosine similarity. Recovers negative correlations between reverse-keyed dimensions. | Pellert et al. 2026 |
| `"mean_centered_pearson"` | `.apply_mean_centered_pearson()` | Apply scoring, mean-center each embedding across its dimensions, L2-normalize. Cosine then equals Pearson correlation → true correlation matrix. | Pokropek 2026; Kmetty et al. 2021 |

**Note on `"macro"` (Guenole et al.):** Macro encoding embeds entire subscales as single
documents rather than individual items. This produces a different-shaped matrix
(subscale × subscale, not item × item) and doesn't fit the item-level EFA pipeline.
We omit it from v1 and document it as a potential extension.

**Implementation detail:** All transforms produce a symmetric matrix with 1s on the
diagonal. For `mean_centered_pearson`, the matrix is a proper correlation matrix
(all diagonal = 1, all off-diagonal in [−1, 1]), making CFA-style fit indices
admissible if a user passes it to lavaan. For the other modes, the matrix is a cosine
similarity matrix — still PSD but not necessarily a correlation matrix in the strict
sense, so sample-size-dependent fit indices (chi-square, RMSEA, CFI, TLI) are reported
with a warning or suppressed.

---

## 5. Embedding backends (`embed=`)

All backends are optional (in `Suggests`). The precomputed `embeddings=` path has zero
external dependencies beyond `psych`/`GPArotation`/`withr`.

| `embed` value | Backend | Dependency | Default model |
|---|---|---|---|
| `"sbert"` | sentence-transformers via reticulate | `reticulate` + Python `sentence-transformers` | `"all-MiniLM-L6-v2"` (384-dim, fast, no API key) |
| `"openai"` | OpenAI API | `httr2` | `"text-embedding-3-small"` |
| `function(text) ...` | User-supplied function | none | — |
| (not specified when `embeddings=` is provided) | Precomputed matrix | none | — |

**Error messaging:** When a backend's dependency is missing, the error message tells the
user exactly how to install it, and suggests the `embeddings=` alternative:

```
Error: The "sbert" embedding backend requires the 'reticulate' package and
Python 'sentence-transformers'. Install with:
  install.packages("reticulate")
  reticulate::py_install("sentence-transformers")
Or pass precomputed embeddings: sfa(items, embeddings = your_matrix)
```

**Caching:** `sfa_embed()` caches embeddings in `tools::R_user_dir("semanticfa", "cache")`
keyed by `digest::digest(list(items, model))` when `digest` is installed, falling back
to a base-R `paste0(nchar(paste(items, collapse="")), "_", sum(utf8ToInt(paste(items, collapse=""))))` fingerprint when it is not.
Cache is opt-in (`cache = TRUE`) and users can clear it with `sfa_clear_cache()`.

---

## 6. Factor retention (`n_factors_method=`)

| Method | Algorithm | Source |
|---|---|---|
| `"parallel"` (default) | Embedding-adapted parallel analysis: null = random unit vectors in the actual embedding dimension. No n_obs needed. | Adapted from Horn 1965; embedding variant from `qwen3_efa_v2.py` |
| `"kaiser"` | Eigenvalues > 1 rule. | Kaiser 1960 |
| `"EGA"` | Exploratory Graph Analysis via `EGAnet::EGA()`. | Golino & Epskamp; Golino preprint |
| `"TEFI"` | Minimize TEFI across candidate nfactors values (1..max). | Golino preprint |

When `nfactors` is explicitly set (non-NULL), the retention method is skipped and the
user's value is used directly.

**Unified diagnostic:** `sfa_nfactors()` runs all requested methods and returns a
tabulated comparison (see §3.7), mirroring `EFAtools::N_FACTORS()`.

---

## 7. Fit diagnostics

Computed automatically and stored in the return object:

| Diagnostic | What | Source | Notes |
|---|---|---|---|
| KMO | Sampling adequacy from the similarity matrix | Kaiser 1974 | Computed directly from the matrix (no n_obs). Warns if < 0.6. |
| TEFI | Von Neumann entropy of the normalized correlation matrix | Golino preprint | Lower = tighter block structure. No n_obs needed. |
| RMSR | Root mean square of off-diagonal residuals | Suárez-Álvarez et al. 2025 | From reproduced correlation: L Φ L' + diag(u). |
| CAF | Common part Accounted For = 1 − KMO(residual) | Lorenzo-Seva; Suárez-Álvarez et al. 2025 | ≥ 0.90 excellent, ≥ 0.80 good. |
| McDonald's ω | Reliability per extracted factor | Milano et al. 2025a | ω = (Σl)² / ((Σl)² + Σu) for items assigned to each factor. |
| DAAL | Dominant Average Absolute Loading | Reference impl. | Cross-tabulation: extracted factors × theoretical factors. |
| Monte Carlo calibration | Null distribution of RMSR/CAF/TEFI for random items | Pokropek 2026 | Only when `calibrate = TRUE`. |

Bartlett's test is **not computed** for embedding matrices (no sample size). A note is
printed explaining why.

---

## 8. Human-comparison design

**Decision: lean design.** The package does NOT build a human-comparison subsystem.

The workflow is:
1. User runs `sfa(items)` → gets an `"sfa"` object with `$loadings` of class `"loadings"`
2. User runs `psych::fa(response_data)` → gets a `"psych"` `"fa"` object
3. User compares with standard tools:
   ```r
   psych::factor.congruence(sfa_fit$loadings, human_fit$loadings)
   ```
4. For NMI/ARI/Frobenius/disattenuated, the thin `sfa_congruence()` helper is available.

This keeps the package focused, composable, and small. The comparison workflow is
documented in the vignette.

---

## 9. Bundled example data

**Scale:** IPIP Big Five 50-item inventory — 50 items, 5 factors (Extraversion,
Agreeableness, Conscientiousness, Neuroticism, Openness), 32 positively keyed and
18 negatively keyed. This exercises:
- `atomic_reversed` sign-flipping on the 18 reverse-keyed items
- SQuID's recovery of negative between-dimension correlations
- A realistic 5-factor structure for parallel analysis

The IPIP item pool is **public domain** (International Personality Item Pool;
ipip.ori.org).

**Dataset name:** `big5`

```r
data(big5)
# A list with:
#   $items      — character(50): item text
#   $codes      — character(50): item codes (E1, E2, ..., O10)
#   $factors    — character(50): theoretical factor labels
#   $scoring    — numeric(50): +1 or -1 per item
#   $embeddings — matrix(50, 384): precomputed all-MiniLM-L6-v2 embeddings
```

**Size:** 50 × 384 × 8 bytes (float64) = ~153 KB for embeddings + ~5 KB for text.
Well under CRAN's 5 MB limit.

**Generation:** A `data-raw/big5.R` script generates this by calling `sfa_embed()`
with `all-MiniLM-L6-v2`. The precomputed embeddings are stored as an `.rda` file in
`data/`.

**Golden test:** The package's Big5 results under `encoding="atomic_reversed"`,
`fm="minres"`, `rotate="oblimin"` are validated against `qwen3_efa_v2.py`'s output
on the same embeddings (using a tolerance for numerical differences between R's
psych::fa and Python's factor_analyzer).

---

## 10. Heywood cases and non-Gramian matrices

Cosine similarity matrices from embeddings frequently produce communalities > 1
(Heywood cases) during factor extraction, especially with small item sets or high
embedding dimensionality. This is more common than with empirical correlation matrices.

**Handling strategy:**

1. **Matrix regularization** (R/utils.R): `.regularize_corr(C, alpha = 1e-6)` adds
   `alpha * I` and re-normalizes to unit diagonal. Applied to the similarity matrix
   before KMO/inversion when it is near-singular. Matches the reference implementation.

2. **Heywood detection after extraction:** After `psych::fa()` returns, check
   communalities. If any > 1:
   - Store `$heywood` logical vector (TRUE for offending items) on the return object.
   - `print.sfa()` emits a warning: `"Note: N item(s) have communality > 1 (Heywood
     cases). Consider reducing nfactors or using encoding = 'mean_centered_pearson'."`.
   - Do NOT silently clamp communalities — the user should see the raw values and
     decide.

3. **Non-positive-definite similarity matrices:** The `atomic_reversed` and `squid`
   transforms can occasionally yield matrices with small negative eigenvalues (not
   Gramian). Before passing to `psych::fa()`:
   - Check smallest eigenvalue. If negative, apply the same `alpha * I` regularization
     and warn: `"Similarity matrix was not positive semi-definite; regularized with
     alpha = {alpha}."`.
   - This is preferred over nearest-PD projection (which can distort structure).

---

## 11. RNG hygiene (CRAN requirement)

Every stochastic operation uses `withr::with_seed(seed, { ... })`:
- `sfa_parallel()` — random unit vector generation
- `.random_item_calibration()` — random embedding generation
- Any future stochastic diagnostics

This guarantees:
- Reproducibility (same `seed` → same results).
- No side effects on the user's global `.Random.seed`.
- CRAN compliance (no `set.seed()` calls without restoration).

---

## 12. Package file structure

```
semanticfa/
├── DESCRIPTION
├── NAMESPACE                    # roxygen2-generated
├── LICENSE
├── R/
│   ├── sfa.R                    # sfa() main function
│   ├── similarity.R             # encoding transforms + sfa_similarity()
│   ├── embed.R                  # sfa_embed(), embedding backends, sfa_clear_cache()
│   ├── retention.R              # sfa_parallel(), sfa_nfactors(), retention methods
│   ├── diagnostics.R            # TEFI, KMO, RMSR, CAF, omega, DAAL, calibration
│   ├── congruence.R             # sfa_congruence(), NMI, ARI, Frobenius, Tucker, disattenuated
│   ├── class.R                  # S3 class definition, print, summary, plot, as_psych
│   ├── utils.R                  # internal helpers (regularize_corr, heywood check, etc.)
│   └── data.R                   # roxygen2 docs for bundled datasets
├── man/                         # roxygen2-generated
├── data/
│   └── big5.rda
├── data-raw/
│   └── big5.R                   # script that generated big5.rda
├── tests/
│   └── testthat/
│       ├── test-similarity.R    # atomic_reversed with +1/-1 scoring, squid negatives, etc.
│       ├── test-retention.R
│       ├── test-diagnostics.R   # including Heywood case detection
│       ├── test-sfa.R           # end-to-end on big5 bundled data
│       ├── test-congruence.R    # NMI, ARI, Frobenius, disattenuated
│       ├── test-nfactors.R      # sfa_nfactors() tabulation
│       └── test-embed.R         # skip_on_cran() for network/Python tests
├── vignettes/
│   └── introduction.Rmd         # end-to-end workflow with bundled data
├── inst/
│   └── CITATION
├── README.md
├── NEWS.md
├── cran-comments.md
└── .Rbuildignore
```

---

## 13. Internal implementation plan

### Phase 1a: Similarity transforms (R/similarity.R)
Port from `qwen3_efa_v2.py`:
- `.apply_atomic_reversed(embeddings, scoring)` → normalized matrix
- `.apply_squid(embeddings, scoring)` → normalized matrix
- `.apply_mean_centered_pearson(embeddings, scoring)` → normalized matrix
- `sfa_similarity(embeddings, encoding, scoring)` → sim matrix

### Phase 1b: Diagnostics (R/diagnostics.R)
Port from `qwen3_efa_v2.py`:
- `.compute_kmo(corr_matrix)` → list(total, per_item)
- `.compute_tefi(corr_matrix)` → numeric
- `.compute_rmsr_caf(observed, fa_obj)` → list(rmsr, caf, residual)
- `.compute_omega(loadings, assignments)` → data.frame
- `.compute_daal(loadings, factors)` → matrix
- `.random_item_calibration(n_items, embed_dim, n_factors, ...)` → list of null distributions

### Phase 1c: Retention (R/retention.R)
- `sfa_parallel(sim_matrix, embeddings, ...)` → list with n_factors, eigenvalues, percentiles
- `.retention_kaiser(eigenvalues)` → integer
- `.retention_ega(sim_matrix)` → integer (requires EGAnet)
- `.retention_tefi(sim_matrix, max_factors)` → integer
- `sfa_nfactors(sim_matrix, embeddings, methods, ...)` → class "sfa_nfactors"
- `print.sfa_nfactors()` → compact comparison table

### Phase 1d: Embedding (R/embed.R)
- `sfa_embed(items, embed, model, cache, ...)` → matrix
- `.embed_sbert(items, model)` → matrix (via reticulate)
- `.embed_openai(items, model, api_key)` → matrix (via httr2)
- `.embed_custom(items, fn)` → matrix
- `sfa_clear_cache()` → invisible(NULL)
- `.cache_key(items, model)` → string (digest or base-R fallback)

### Phase 1e: Main function + class (R/sfa.R, R/class.R)
- `sfa(items, ...)`:
  1. Resolve items → character vector + codes + scoring (with NULL → all-+1 fallback)
  2. Obtain embeddings (embed or precomputed)
  3. Build similarity matrix (with non-PD check + regularization)
  4. Determine n_factors (if NULL)
  5. Call `psych::fa(sim_matrix, nfactors, rotate, fm, n.obs, ...)`
  6. Check for Heywood cases
  7. Compute embedding diagnostics
  8. Assemble and return `"sfa"` object
- `print.sfa`, `summary.sfa`, `plot.sfa`
- `as_psych.sfa`

### Phase 1f: Comparison (R/congruence.R)
- `sfa_congruence(sfa_fit, target, metrics)` → list
- `.compute_nmi(labels_a, labels_b)` → numeric
- `.compute_ari(labels_a, labels_b)` → numeric
- `.compute_frobenius(mat_a, mat_b)` → numeric
- `.compute_disattenuated(x, y, rel_x, rel_y)` → numeric (Hommel & Arslan 2025)

---

## 14. Resolved design decisions

| Decision | Resolution | Rationale |
|---|---|---|
| Package name | `semanticfa` | Available on CRAN; clear and descriptive |
| License | GPL-3 | Standard for R packages depending on GPL-licensed psych |
| GPArotation | Imports | Default `rotate="oblimin"` requires it; must work on clean install |
| digest | Suggests with base-R fallback | Caching is opt-in; avoids hard dep for a convenience feature |
| withr | Imports | CRAN-mandated RNG hygiene; no viable base-R equivalent |
| scoring = NULL | Graceful fallback to all +1 with message | Matches reference impl; avoids surprising errors |
| Bundled data | Big5 50-item (not DASS) | Exercises sign-flipping (18 reverse-keyed), SQuID negatives, 5-factor structure |
| Human comparison | Lean: `sfa_congruence()` only | Composes with existing psych/EFAtools tools |
| Disattenuated corr | In default metrics | Hommel & Arslan 2025; important for embedding vs. empirical comparison |
| N_FACTORS table | `sfa_nfactors()` exported | Mirrors EFAtools::N_FACTORS() for multi-method retention comparison |
| Heywood cases | Detect, warn, store; don't clamp | User should see raw values; common with cosine matrices |
| Non-PD matrices | Regularize with alpha*I + warn | Preferred over nearest-PD projection (less structural distortion) |
| macro encoding | Omitted from v1 | Different paradigm (subscale-level, not item-level EFA) |
