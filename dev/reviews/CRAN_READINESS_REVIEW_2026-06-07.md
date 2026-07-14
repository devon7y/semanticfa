# CRAN Readiness Review

Date: 2026-06-07

Scope: comprehensive review of `semanticfa` for CRAN publication readiness.
This report suggests fixes, hardening, documentation cleanup, and submission
hygiene only. It deliberately does not suggest new package features.

## Current Verdict

`semanticfa` is close to CRAN shape for the ordinary bundled-data workflow: the
test suite passes, examples pass in the no-vignette check, the built source
tarball is small, and the large local research materials are excluded from the
source package. The package should not be submitted yet. The main remaining
work is to get a fully clean `R CMD check --as-cran` in an environment with
Pandoc and TeX, make `cran-comments.md` truthful, and fix a small set of
release-facing robustness/documentation issues.

## Evidence Collected

- Reviewed package metadata and structure: `DESCRIPTION`, `NAMESPACE`,
  `.Rbuildignore`, `README.md`, `NEWS.md`, `cran-comments.md`, `inst/CITATION`,
  `vignettes/introduction.Rmd`, all `R/*.R`, and all `tests/testthat/*.R`.
- Checked current CRAN policy and submission checklist:
  - CRAN Repository Policy: https://cran.r-project.org/web/packages/policies.html
  - CRAN submission checklist: https://cran.r-project.org/web/packages/submission_checklist.html
  - R Packages guidance on release checks and `R CMD check`:
    https://r-pkgs.org/release.html and https://r-pkgs.org/R-CMD-check.html
- `R CMD build /Users/devon7y/VS_Code/semanticfa` failed before a tarball was
  produced because Pandoc is not installed locally.
- `R CMD build --no-build-vignettes /Users/devon7y/VS_Code/semanticfa`
  succeeded and produced a 146K source tarball.
- `R CMD check --as-cran semanticfa_0.1.0.tar.gz` on the no-vignette tarball
  completed with `1 ERROR | 3 WARNINGs | 4 NOTEs`.
- `devtools::test()` passed: `FAIL 0 | WARN 0 | SKIP 0 | PASS 206`.
- `tools::checkRdaFiles("data")` reported `data/big5.rda` as `xz` compressed,
  serialization version 3, size 70,452 bytes.
- Source-tarball artifact scan found no `Rplots.pdf`, `method_papers`,
  `DASS_walkthrough`, `data-raw`, `semfa`, `feedback`, `paper_prompt`, or CRAN
  readiness report files.
- `available.packages()` did not find `semanticfa` on CRAN or Bioconductor.
- `urlchecker::url_check()` could not run because Pandoc is not installed.

## Must Fix Before CRAN Submission

### 1. Produce a Real Clean `--as-cran` Result

CRAN expects packages to pass `R CMD check --as-cran` and the CRAN policy says
source submissions should be built by `R CMD build` and checked before upload.
The current local environment cannot complete that process:

```text
R CMD build /Users/devon7y/VS_Code/semanticfa
ERROR: Pandoc is required to build R Markdown vignettes but not available.
```

The no-vignette fallback check is useful but not submission-grade. It produced:

```text
Status: 1 ERROR, 3 WARNINGs, 4 NOTEs
```

The no-vignette check failures were toolchain-related:

- `ERROR`: `pdflatex is not available`
- `WARNING`: files in `vignettes/` but no built `inst/doc`
- `WARNING`: `Directory 'inst/doc' does not exist`
- `WARNING`: manual PDF creation failed
- `NOTE`: README/NEWS could not be checked without Pandoc
- `NOTE`: HTML validation skipped because recent HTML Tidy / `V8` unavailable
- `NOTE`: `semanticfa-manual.tex` left in the check directory after manual
  failure
- `NOTE`: expected new-submission note

Suggested changes:

- Run the final build/check on a machine or CI image with Pandoc, `pdflatex`,
  and preferably HTML Tidy plus `V8`.
- Run the exact CRAN-style sequence on the built tarball:

```sh
R CMD build /Users/devon7y/VS_Code/semanticfa
R CMD check --as-cran semanticfa_0.1.0.tar.gz
```

- Treat the no-vignette result as diagnostic only. Do not use it as the check
  result in `cran-comments.md`.

### 2. Fix `cran-comments.md`; It Currently Makes Claims That Are Not Verified

`cran-comments.md` says:

```text
0 errors | 0 warnings | 0 notes
```

That is not true for the checks run during this review. It also says the local
run was "without building the PDF reference manual" while still reporting zero
notes and zero warnings. The no-vignette/no-manual workaround is not equivalent
to CRAN's incoming check, and the full build did not complete here because
Pandoc is unavailable.

Suggested changes:

- Replace the check-results block only after a complete successful run.
- Include the unavoidable "New submission" note if it is the only remaining
  note.
- Add actual win-builder and/or R-hub results after they have completed.
- Remove future-tense wording such as "are run prior to submission" until those
  results are appended.

### 3. Verify the Vignette on a Clean Machine

The package has `VignetteBuilder: knitr` and `vignettes/introduction.Rmd`.
Because this local machine lacks Pandoc, I could not verify that the vignette
builds under the normal CRAN path. The vignette itself uses bundled data and
does not appear to require network access or Python, which is good.

Suggested changes:

- Run `R CMD build` with vignette building enabled on a Pandoc-equipped machine.
- Check that `inst/doc` is generated in the built package and that CRAN's
  vignette checks are clean.
- If any vignette chunks are slow, reduce iterations rather than disabling the
  vignette. The current `parallel_iter = 50` chunk is probably fine, but should
  be verified on Windows and Linux.

### 4. Run URL Checks After Installing Pandoc

`urlchecker::url_check()` failed immediately because Pandoc is missing:

```text
Error: pandoc is not installed and on the PATH
```

CRAN performs URL checks. The package contains several release-facing URLs:
GitHub URLs in `DESCRIPTION`, IPIP and Hugging Face URLs in `big5`
documentation, OpenAI API URL in code, ORCID, and DOI/arXiv references.

Suggested changes:

- Install Pandoc and run:

```r
urlchecker::url_check()
```

- Fix or document any transient URL failures before submission.

### 5. Revisit `DESCRIPTION` Wording for CRAN Human Review

The `DESCRIPTION` is valid and concise, but CRAN's checklist asks for careful,
informative `Title` and `Description` text. It also asks that external software
names be quoted and that relevant citations in the Description use
author-year style with persistent identifiers.

Current text says:

```text
Embeds item text with sentence transformers or other language models
```

and:

```text
The underlying methods are documented with full citations in the corresponding
function help pages.
```

Suggested changes:

- Quote external software/model-family names where appropriate, for example
  `'sentence-transformers'`.
- Either keep citations out of the `Description` entirely, as it currently
  mostly does, or add CRAN-compliant citations with DOI/arXiv/URL identifiers.
- Consider naming the specific statistical task more plainly for new CRAN users
  while staying concise. This is copyediting, not a new feature.

## Correctness And Robustness Fixes

### 6. `print.sfa()` Can Error on `NA`/`NaN` KMO Values

The precomputed-similarity path can fit an object whose KMO is `NaN`. Printing
then fails because `print.sfa()` tests `if (kmo_val >= 0.9)` without checking
that `kmo_val` is finite.

Reproducer:

```r
items <- paste0("item", 1:5)
S <- diag(5)
fit <- sfa(items, similarity = S, nfactors = 1)
print(fit)
```

Observed result:

```text
KMO:  NaN
Error: missing value where TRUE/FALSE needed
```

Relevant code: `R/class.R`, lines 63-74.

Suggested changes:

- Only apply KMO labels when `is.finite(kmo_val)`.
- Print a neutral label such as "unavailable" for `NA`/`NaN` diagnostics.
- Add a regression test using a diagonal or otherwise degenerate similarity
  matrix.

### 7. Zero-Vector Embeddings Produce `NaN` Similarities

`sfa_similarity()` validates that embeddings are finite, but it does not reject
all-zero rows. In `.apply_atomic_reversed()`, a zero row is detected and a
warning is issued, but the fallback uses the original zero row, so the norm is
still zero and the transformed embedding becomes `NaN`.

Reproducer:

```r
emb <- matrix(c(0, 0, 1, 0, 0, 1), nrow = 3, byrow = TRUE)
sfa_similarity(emb)
```

Observed result:

```text
1 item(s) have zero norm after sign-flipping; using original embeddings for
those items.
```

The returned matrix contains `NaN` off-diagonal values.

Relevant code: `R/similarity.R`, lines 139-150.

Suggested changes:

- Error early when any embedding row has zero norm for encodings that need
  normalization.
- For `squid` and `mean_centered_pearson`, decide whether a post-transform
  zero vector should error or be handled explicitly, then test it.
- After every transform, assert that the transformed embedding matrix and final
  similarity matrix are finite.

### 8. Numeric Controls Need Stricter Validation

Several numeric arguments can currently produce low-information warnings,
`NA` results, or downstream errors.

Examples observed:

```r
sfa_parallel(diag(3), diag(3), n_iter = 0)
```

returns an `sfa_parallel` object with `n_factors = NA`.

```r
sfa(letters[1:5], similarity = diag(5), nfactors = "x")
```

warns `NAs introduced by coercion`, then errors with:

```text
missing value where TRUE/FALSE needed
```

Relevant code:

- `R/retention.R`, lines 30-64
- `R/sfa.R`, lines 214-231

Suggested changes:

- Validate `nfactors` as one positive whole number before coercion.
- Validate `parallel_iter`, `calibrate_iter`, and `n_iter` as positive whole
  numbers.
- Validate `percentile` is a single finite number in `(0, 100)`.
- Add tests for invalid controls and for the expected error messages.

### 9. NLI Matrix Documentation and Validation Are Internally Inconsistent

`sfa_nli_matrix()` documentation says the returned matrix is in `[-1, 1]`, but
the classifier documentation says any finite numeric scores are accepted and
need not lie in `[0, 1]`. The implementation subtracts the two columns directly,
so custom scores can produce values far outside a correlation-like range.

Reproducer:

```r
clf <- function(p, h) {
  data.frame(entailment = rep(10, length(p)),
             contradiction = rep(-10, length(p)))
}
sfa_nli_matrix(c("a", "b", "c"), classifier = clf)
```

Observed off-diagonal values: `20`.

Relevant code: `R/nli.R`, lines 39-51 and 102-117.

Suggested changes:

- If the function is intended to return a correlation-like signed similarity,
  require entailment and contradiction to be probabilities in `[0, 1]`, or at
  least require their difference to be in `[-1, 1]`.
- If raw scores are intentionally allowed, update the return documentation and
  warn that such matrices may be unsuitable for `sfa(similarity = ...)`.
- Add tests for out-of-range classifier output.

### 10. Similarity-Matrix Validation Should Check Diagonal and Range

The `sfa(similarity = ...)` path checks dimensions, numeric/finite values, and
symmetry. It does not check that the diagonal is one or that off-diagonal
values are correlation-like. This matters because the matrix is passed to
`psych::fa()` as a correlation/similarity matrix after PSD repair.

Relevant code: `R/sfa.R`, lines 145-165.

Suggested changes:

- Require a unit diagonal or explicitly rescale/replace it with a message.
- Reject values outside a defensible range before PSD repair, unless the
  documentation clearly permits non-correlation similarities.
- Add tests for non-unit diagonal, out-of-range values, and asymmetric matrices.

### 11. Cache Behavior Should Be More Conservative or More Explicit

`sfa_embed()` defaults to `cache = TRUE` and writes to:

```r
tools::R_user_dir("semanticfa", "cache")
```

This is a proper user cache location, but CRAN policy expects user data/cache
behavior to be controlled and not surprising. `cran-comments.md` currently says
the cache is written only when caching is "explicitly enabled", but the exported
default is `cache = TRUE`.

Relevant code:

- `R/embed.R`, lines 34-48 and 73-96
- `cran-comments.md`, lines 54-57

Suggested changes:

- Either change the default to `cache = FALSE`, or keep `cache = TRUE` and
  document plainly that embedding calls write to the user cache by default.
- Add cache size/age management or document that users should call
  `sfa_clear_cache()`.
- Fix the `cran-comments.md` wording so it matches the actual default.

### 12. Python Provisioning Should Be Framed Carefully for CRAN

The examples and tests are careful to avoid Python/model downloads on CRAN, and
the bundled-data workflow runs without Python. The risk is mostly reviewer
interpretation: `reticulate` is in `Imports`, `sfa_embed()` defaults to the
`"sbert"` backend, and `reticulate::py_require()` can provision Python
requirements on first use.

Relevant code: `R/embed.R`, lines 129-177 and `R/nli.R`, lines 120-138.

Suggested changes:

- Keep all Python-backed examples under `\dontrun{}` or guarded examples.
- In `cran-comments.md`, say exactly that examples/tests use precomputed
  embeddings and do not initialize Python.
- Avoid implying `reticulate` is optional while it remains in `Imports`.
- Consider pinning or documenting Python package versions for reproducibility.

## Documentation and Release Polish

### 13. Improve Bundled-Data Provenance

The `big5` documentation now states that the IPIP items are public domain and
that embeddings were generated with `sentence-transformers/all-MiniLM-L6-v2`
under Apache 2.0. That is good. The regeneration script still reads a local
absolute path:

```r
"/Users/devon7y/VS_Code/LLM_Factor_Analysis/scale_items/Big5_items.csv"
```

Relevant files:

- `R/data.R`, lines 19-24
- `data-raw/big5.R`, lines 6-13

Suggested changes:

- Make the regeneration script reproducible from package-local or URL-based
  inputs.
- Record the model card URL and, if available, the exact Hugging Face revision
  used to generate the embeddings.
- Keep this provenance in `R/data.R` and/or a short note in `data-raw`.

### 14. Remove or Reconsider `@keywords internal` on Package-Level Help

`R/semanticfa-package.R` uses `@keywords internal` for the package overview.
That is common when authors want to hide package help from the index, but for a
new CRAN package the overview page is useful release-facing documentation.

Suggested change:

- Remove `@keywords internal` from the package overview unless hiding it is
  intentional.

### 15. Tighten README References

The README is concise and useful. Its reference list is currently abbreviated,
with several entries lacking DOI/arXiv/URL identifiers.

Suggested changes:

- Add persistent identifiers where available.
- Keep README references aligned with Rd references and `inst/CITATION`.
- After CRAN acceptance, add the CRAN install line above the GitHub install
  line:

```r
install.packages("semanticfa")
```

### 16. Verify Generated Rd After Any Roxygen Edits

The current check reported no code/documentation mismatches, so the generated
Rd files are currently synchronized. Any edits to roxygen comments for the
items above should be followed by:

```r
devtools::document()
devtools::test()
```

Then rebuild and run `R CMD check --as-cran`.

## Things That Look Good

- The source tarball excludes local-only materials via `.Rbuildignore`,
  including `method_papers/`, `data-raw/`, `semfa/`, `DASS_walkthrough.*`,
  `feedback.txt`, `paper_prompt.txt`, and CRAN review artifacts.
- The nested `tests/testthat/Rplots.pdf` is excluded by the unanchored
  `Rplots\.pdf$` rule.
- `DESCRIPTION` has a standard GPL license, maintainer metadata, ORCID,
  GitHub URL, bug-report URL, UTF-8 encoding, testthat edition, and
  `VignetteBuilder`.
- All declared R dependencies were installed in the local check environment.
- No current CRAN or Bioconductor package named `semanticfa` was found.
- `R CMD check` found no namespace issues, missing documentation entries, Rd
  usage mismatches, unstated dependencies in examples/tests/vignettes, S3
  registration problems, portable filename problems, non-ASCII R code, or data
  compression problems in the no-vignette check.
- The examples passed, including `--run-donttest`, in the no-vignette check.
- The main bundled-data workflow fits and prints correctly for ordinary input.
- The previous OpenAI-default-model issue appears fixed:
  `embed = "openai"` now resolves to `text-embedding-3-small` instead of the
  sentence-transformer default.
- The previous precomputed-similarity calibration issue appears fixed:
  `calibrate = TRUE` now warns and disables calibration when only a similarity
  matrix is supplied.
- The previous PSD repair concern appears fixed in principle:
  `.check_psd()` now floors eigenvalues and rescales the diagonal.
- `sfa_itemplot()` now has a runnable PCA example using bundled data.

## Suggested Pre-Submission Checklist

1. Fix the robustness issues in `print.sfa()`, zero-vector handling, numeric
   argument validation, NLI score validation/documentation, similarity-matrix
   validation, cache wording, and data provenance.
2. Re-run `devtools::document()` and `devtools::test()`.
3. Install Pandoc, TeX, recent HTML Tidy, and `V8`.
4. Run `urlchecker::url_check()`.
5. Run:

```sh
R CMD build /Users/devon7y/VS_Code/semanticfa
R CMD check --as-cran semanticfa_0.1.0.tar.gz
```

6. Run win-builder on R-devel and at least one R-hub multi-platform check.
7. Update `cran-comments.md` with the actual results.
8. Rebuild the source tarball and confirm local artifacts are excluded:

```sh
tar -tzf semanticfa_0.1.0.tar.gz | grep -E 'Rplots|method_papers|DASS|data-raw|semfa|feedback|paper_prompt|CRAN_READINESS'
```

The command should return no output.

