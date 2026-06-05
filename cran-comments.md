# cran-comments

## Submission

This is a new submission (semanticfa 0.1.0).

## R CMD check results

Local `R CMD check --as-cran` (macOS, R release):

```
0 errors | 0 warnings | 0 notes
```

Cross-platform checks (win-builder R-devel/R-release and R-hub Windows / macOS /
Linux) are run prior to submission; results to be appended here.

## Notes for the reviewer

* **Core workflow needs no Python and no Suggests.** The package's central
  functionality — turning a supplied embedding matrix or item-by-item
  similarity matrix into a factor solution with diagnostics — depends only on
  the Imports (`psych`, `GPArotation`, `reticulate`, `withr`, `stats`,
  `utils`).

* **Optional, gracefully-degrading functionality.** Some features require
  Suggests packages and check for them with `requireNamespace()`, erroring
  informatively when absent:
  - `EGAnet` — `sfa_dimselect()` (Dynamic EGA dimension selection) and
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

* No examples write to the user's home filesystem, the package library, or
  other restricted locations; the embedding cache uses
  `tools::R_user_dir("semanticfa", "cache")` and is only written when caching
  is explicitly enabled.
