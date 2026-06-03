## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* local macOS (aarch64), R 4.4.x
* GitHub Actions: ubuntu-latest, macOS-latest, windows-latest (R release, R devel)

## Notes

* All examples and tests use precomputed embeddings bundled with the package.
  No network access or Python is required for core functionality.
* The `reticulate` and `httr2` packages are in Suggests and only needed
  for live embedding generation.
