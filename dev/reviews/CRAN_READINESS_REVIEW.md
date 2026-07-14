# CRAN Readiness Review

Date: 2026-06-06

Scope: critical review for publishing `semanticfa` on CRAN. This review suggests
fixes, hardening, documentation cleanup, and submission hygiene only; it does
not propose new package features.

## Evidence Collected

- Built the package from a temporary directory with `R CMD build /Users/devon7y/VS_Code/semanticfa`.
- Ran `R CMD check --as-cran semanticfa_0.1.0.tar.gz` on macOS with R 4.5.0.
- Result: `1 ERROR | 1 WARNING | 3 NOTEs`.
- The test suite passed under `--as-cran`: `FAIL 0 | WARN 0 | SKIP 1 | PASS 194`.
- The built source tarball was small (`220K`) and correctly excluded the large
  `method_papers/`, `data-raw/`, `semfa/`, `DASS_walkthrough.*`, and prior
  tarball artifacts.
- The built source tarball still included `tests/testthat/Rplots.pdf`.
- Current CRAN and Bioconductor package indexes did not contain `semanticfa`
  when checked with `available.packages()`.

Primary policy references used:

- CRAN Repository Policy: https://cran.r-project.org/web/packages/policies.html
- Writing R Extensions: https://cran.r-project.org/doc/manuals/r-release/R-exts.html

## Must Fix Before Submission

### 1. Do not submit with stale `cran-comments.md` check results

`cran-comments.md` currently reports:

```text
0 errors | 0 warnings | 0 notes
```

The current local `--as-cran` run did not match that claim:

```text
Status: 1 ERROR, 1 WARNING, 3 NOTEs
```

The error and warning were from manual PDF generation:

```text
LaTeX Error: File `inconsolata.sty' not found.
```

This is probably a local TeX installation problem rather than a package defect,
but it still means the stated local result is currently false. Re-run with a
complete TeX setup, or on a clean CI image that can build the manual, before
claiming clean results.

Suggested action:

- Install the missing TeX package or use a full TinyTeX/TeX Live setup.
- Re-run `R CMD build` and `R CMD check --as-cran`.
- Update `cran-comments.md` only after the latest run is actually clean.
- Add win-builder and/or R-hub results before upload.

### 2. Remove generated `Rplots.pdf` from the source package

The source tarball includes:

```text
semanticfa/tests/testthat/Rplots.pdf
```

This is a generated artifact and should not ship. `.Rbuildignore` currently
ignores only a top-level `Rplots.pdf`, not nested plot files:

```text
^Rplots\.pdf$
```

Suggested action:

- Delete `tests/testthat/Rplots.pdf` from the working tree.
- Add a broader ignore rule, for example:

```text
^tests/testthat/Rplots\.pdf$
```

or, more generally:

```text
Rplots\.pdf$
```

Then rebuild and confirm:

```sh
tar -tzf semanticfa_0.1.0.tar.gz | grep Rplots
```

should return nothing.

### 3. Fix `DESCRIPTION` citations

The `Description` field cites methods like this:

```text
using published methods (Guenole et al., Pellert et al. 2026, Pokropek 2026)
```

CRAN policy says citations in the `Description` field should use author-year
style followed by a DOI/ISBN or URL. The current text has incomplete dates and
no persistent identifiers.

Suggested action:

- Either remove named citations from `Description` and leave them in Rd
  references/README, or rewrite them in CRAN-compliant form.
- If any cited work lacks a DOI, include a stable URL in angle brackets.
- Keep the `Description` concise; detailed bibliographic support belongs in
  function documentation and the vignette.

### 4. Make bundled-data provenance and licensing more explicit

`big5` is documented as public-domain IPIP item text plus embeddings generated
with `all-MiniLM-L6-v2`. That is directionally good, but CRAN policy asks for
clear ownership and rights for all package components, including data.

Suggested action:

- In `R/data.R`, document the embedding model more precisely:
  `sentence-transformers/all-MiniLM-L6-v2`, model-card URL, and license.
- Note the exact model/version or Hugging Face revision used, if available.
- Keep or improve a regeneration script in the repository so the `.rda` source
  path is auditable. The current `data-raw/big5.R` has a local absolute path,
  which is fine for a private workflow but weak as provenance.

### 5. Revisit default writes to the user cache

`sfa_embed()` defaults to `cache = TRUE` and writes `.rds` files under:

```r
tools::R_user_dir("semanticfa", "cache")
```

CRAN policy permits user-specific cache directories, but says default cache
sizes should be kept small and actively managed, including removal of outdated
material. The package has `sfa_clear_cache()`, but there is no automatic size
limit, expiry, or stale-cache management.

Suggested action:

- Prefer `cache = FALSE` by default, or
- Add active cache management: maximum size, age-based cleanup, and/or
  backend/model-aware invalidation.
- Document cache behavior clearly in `sfa_embed()` and `sfa_clear_cache()`.
- Include the backend and relevant backend arguments in the cache key, not just
  item text and model name.

### 6. Make Python provisioning unambiguously user-initiated

The package correctly keeps Python-backed examples under `\dontrun{}` and
skips the `.npz` round-trip on CRAN. However, the default embedding path still
does this on user calls:

```r
.sfa_py_require("sentence-transformers")
reticulate::import("sentence_transformers")
```

`sfa_install_python()` also installs unpinned Python packages:

```r
reticulate::py_install(packages, ...)
```

Suggested action:

- Ensure package installation, package loading, examples, tests, and vignettes
  never trigger Python provisioning or model downloads.
- Make documentation explicit that Python package/model downloads happen only
  after the user calls `sfa_install_python()` or chooses the `"sbert"` backend.
- Consider pinning minimum or exact Python package versions for reproducibility.
- Catch Python import/provisioning errors with concise messages that mention the
  precomputed-embedding alternative.

## Correctness And Robustness Improvements

### 7. Fix `similarity + calibrate = TRUE`

This call currently fails with a low-information error:

```r
sim <- sfa_similarity(big5$embeddings, scoring = big5$scoring)
sfa(big5$items, similarity = sim, nfactors = 5, calibrate = TRUE)
```

Observed result:

```text
invalid arguments
```

The precomputed-similarity path sets `embed_dim <- NA_integer_`, then
`calibrate = TRUE` reaches `.random_item_calibration()` and tries to generate a
random matrix with an `NA` dimension.

Suggested action:

- If calibration requires embeddings, error early with a clear message when
  `similarity` is supplied without embeddings.
- Alternatively, derive a defensible dimension from supplied embeddings only
  when they are available.

### 8. Avoid scoring messages when `similarity` is supplied

`sfa()` resolves scoring before branching into the precomputed-similarity path.
If the user supplies `similarity` and no scoring, they can still see:

```text
No scoring provided; defaulting to all +1 (equivalent to 'atomic' encoding).
```

That message is misleading because the supplied similarity matrix has already
encoded whatever scoring/keying convention was used.

Suggested action:

- Do not call `.resolve_scoring()` until the embedding-to-similarity path needs
  it.
- In the `similarity` path, ignore `encoding` and `scoring` silently or warn
  only when the arguments would otherwise imply work that is not being done.

### 9. Add stricter validation for matrices and numeric arguments

Several exported functions rely on downstream errors from `eigen()`,
`psych::fa()`, or arithmetic operations. That can produce cryptic messages.

Suggested action:

- In `sfa_similarity()`, validate that `embeddings` is a finite numeric matrix
  with at least two rows and no zero-width dimension.
- In the `sfa(similarity = ...)` path, validate numeric type, finite values,
  squareness, symmetry, and diagonal convention before PSD repair.
- Validate `nfactors <= n_items - 1` where required by the factor-analysis
  backend.
- Validate `parallel_iter`, `calibrate_iter`, `percentile`, and similar numeric
  controls before loops run.

### 10. Harden NLI classifier handling

`sfa_nli_matrix()` now checks row count, required columns, numeric type, and
finite values. It still does not check whether custom classifier probabilities
fall in `[0, 1]` or whether entailment/contradiction values are calibrated as
probabilities. The default cross-encoder also assumes the label order for the
chosen model.

Suggested action:

- For custom classifiers, validate probability bounds or document that raw
  scores are allowed and how they are interpreted.
- If arbitrary NLI model names are accepted, inspect the model label mapping
  where possible rather than assuming the default model's label order.
- Add tests for malformed probabilities and custom classifier edge cases.

### 11. Re-check PSD repair tests

`.check_psd()` now clips negative eigenvalues and rescales the diagonal, which
is the right direction. Add a regression test with a matrix whose minimum
eigenvalue is substantially negative, then assert the repaired result has no
materially negative eigenvalues and a unit diagonal.

This protects a critical path for user-supplied NLI or precomputed similarity
matrices.

## Documentation And Submission Polish

### 12. Reduce unnecessary `\dontrun{}` examples

Some examples genuinely need `\dontrun{}` because they require Python, network
access, API keys, or response data. Others could be made runnable with bundled
data and base/package dependencies. CRAN reviewers generally prefer runnable
examples when feasible.

Suggested action:

- Keep Python/OpenAI examples in `\dontrun{}`.
- Make at least one `sfa_itemplot(..., method = "pca")` example runnable, since
  it uses bundled data and no optional package.
- For optional dependencies, use `if (requireNamespace("pkg", quietly = TRUE))`
  rather than wrapping the whole example in `\dontrun{}`.

### 13. Align `sfa_itemplot()` default with dependency policy

`sfa_itemplot()` defaults to `method = "tsne"`, but `Rtsne` is in `Suggests`,
so the default call can fail after a minimal CRAN install. The error is
informative, but the default path depending on a suggested package is brittle.

Suggested action:

- Either move `Rtsne` to `Imports`, or change the default method to a path that
  works with required dependencies, such as `"pca"` or `"umap"`.
- If preserving the default matters for backward compatibility, make the Rd
  examples emphasize a guaranteed runnable method first.

### 14. Update `cran-comments.md` wording about dependencies

`cran-comments.md` says the core workflow depends only on Imports, then lists
`reticulate` among those imports while also saying core functionality needs no
Python. That is easy for a reviewer to read as contradictory.

Suggested action:

- Say more precisely that runnable CRAN examples/tests use precomputed
  embeddings and do not initialize Python.
- Avoid implying that `reticulate` itself is optional unless it is moved from
  `Imports` to `Suggests` and all calls are guarded.

### 15. Update release-facing metadata

Small polish items before submission:

- Update `README.md` installation instructions after CRAN acceptance, or mention
  both CRAN and GitHub development installation.
- If the CRAN release happens in 2026, update `inst/CITATION` from `year =
  "2025"` to the release year.
- Add final win-builder/R-hub results to `cran-comments.md`.
- Keep `NEWS.md` concise but aligned with the exact submitted version.

## Suggested Verification Commands

Run from a temporary directory so generated check files do not pollute the repo:

```sh
R CMD build /Users/devon7y/VS_Code/semanticfa
R CMD check --as-cran semanticfa_0.1.0.tar.gz
```

Check for unwanted source artifacts:

```sh
tar -tzf semanticfa_0.1.0.tar.gz | grep -E 'Rplots|method_papers|DASS|data-raw|semfa|feedback|paper_prompt'
```

Check CRAN/Bioconductor name availability:

```sh
Rscript -e 'ap <- available.packages(repos="https://cloud.r-project.org"); print("semanticfa" %in% rownames(ap))'
Rscript -e 'ap <- available.packages(contriburl="https://bioconductor.org/packages/3.22/bioc/src/contrib/PACKAGES"); print("semanticfa" %in% rownames(ap))'
```

Check data compression:

```sh
Rscript -e 'tools::checkRdaFiles("data")'
```

## Overall Assessment

The package is close enough to CRAN shape that the remaining work is mostly
submission hygiene, explicit provenance, and hardening existing edge cases. The
largest CRAN-facing risks are the inaccurate `cran-comments.md` check claim, the
generated `Rplots.pdf` in the source tarball, incomplete `DESCRIPTION`
citations, and cache/Python-download behavior that should be documented or
made more conservative.
