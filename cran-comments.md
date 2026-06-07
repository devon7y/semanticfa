# cran-comments

## Submission

This is a new submission (semanticfa 0.1.0).

## R CMD check results

`R CMD check --as-cran` on the built tarball (macOS, R release; full check with
PDF manual and vignette, Pandoc 2.12 + TeX Live):

```
0 errors | 0 warnings | 2 notes
```

* NOTE: "New submission" (this is the package's first submission to CRAN).
* NOTE: "checking HTML version of manual" -- the check skips HTML validation and
  math rendering because the local machine's HTML Tidy is too old and the `V8`
  package is unavailable. This is a property of the local check environment, not
  the package, and does not occur on CRAN's check machines.

`urlchecker::url_check()` reports all URLs correct. win-builder (R-devel and
R-release) results will be appended when available.

## Notes for the reviewer

* **Core workflow runs without initializing Python.** Turning a supplied
  embedding matrix or item-by-item similarity matrix into a factor solution with
  diagnostics uses only R code; the runnable examples and tests use the bundled
  precomputed embeddings and never load `reticulate`/Python. The package imports
  `reticulate (>= 1.41.0)` so it can declare Python requirements via
  `py_require()` for the optional on-device embedding backend, but Python is
  initialized only when the user explicitly calls a text-embedding path
  (`sfa_embed(embed = "sbert")`, `sfa_install_python()`, or the default
  `sfa_nli_matrix()` classifier).

* **Optional, gracefully-degrading functionality.** Some features require
  Suggests packages and check for them with `requireNamespace()`, erroring
  informatively when absent:
  - `EGAnet` — `sfa_dimselect()` (EGA-based embedding-dimension selection) and
    `n_factors_method = "EGA"` retention.
  - `httr2` — the OpenAI embedding backend.

* **Python-backed paths are not exercised on the check machines.** The `"sbert"`
  embedding backend and the default `sfa_nli_matrix()` classifier use
  `reticulate` with Python `sentence-transformers`. Because a working Python
  installation cannot be guaranteed during checking, every example and test
  that requires Python is wrapped in `\dontrun{}` or skipped via a mock
  classifier / `testthat::skip_if_not_installed()`. `reticulate (>= 1.41.0)` is
  used so that `reticulate::py_require()` can declare the Python requirements,
  which are then provisioned automatically on first use;
  `sfa_install_python()` provisions them ahead of time.

* **Bundled data.** The example dataset `big5` contains the public IPIP
  Big-Five 50-item markers (Open-Source Psychometrics Project) together with
  precomputed sentence-transformer embeddings, so all runnable examples and
  tests execute without network access or Python.

* **Embedding cache.** The cache uses `tools::R_user_dir("semanticfa", "cache")`.
  `sfa_embed()` caches by default (`cache = TRUE`); users can disable it with
  `cache = FALSE` or clear it with `sfa_clear_cache()`. No examples or tests
  write to the user's home filesystem, the package library, or other restricted
  locations (the bundled-data examples/tests do not embed text, so they do not
  write to the cache).
