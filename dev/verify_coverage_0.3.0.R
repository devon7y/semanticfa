# Verify sfa_coverage()'s v0.3.0 audit engine against the Python reference
# implementation (content_validity_geometry, audit_stats in
# scripts/make_figures.py) on IDENTICAL embeddings: 431PTQ (28 items) vs.
# the procrastination v2 region (944 texts, Qwen3-Embedding-8B,
# instruction-conditioned), no filters, one 95% convention.
#
# The two implementations share every definition but not an RNG, so the
# Monte Carlo nulls differ by draw. Expected agreement: radius within ~1%
# (its Monte Carlo CV at 20 draws is ~0.3%), coverage within ~0.01,
# critical count within a few counts, and the SAME flagged-item set (the
# count gap around the critical value is wide: 21 vs 46).
#
# Usage: Rscript dev/verify_coverage_0.3.0.R /path/to/scratchpad

args <- commandArgs(trailingOnly = TRUE)
dir <- if (length(args)) args[1] else "."
np <- reticulate::import("numpy")
C <- np$load(file.path(dir, "C.npy"))
S <- np$load(file.path(dir, "S.npy"))
ref <- jsonlite::fromJSON(file.path(dir, "reference.json"))

devtools::load_all("/Users/devon7y/VS_Code/semanticfa", quiet = TRUE)

norm_rows <- function(m) m / pmax(sqrt(rowSums(m^2)), 1e-12)
C <- norm_rows(C)
S <- norm_rows(S)

report <- function(name, r_val, py_val, tol) {
  ok <- abs(r_val - py_val) <= tol
  cat(sprintf("  %-16s R = %-9.4f Python = %-9.4f |diff| = %-8.4f %s\n",
              name, r_val, py_val, abs(r_val - py_val),
              if (ok) "OK" else "MISMATCH"))
  ok
}

# ---- Part A: the deterministic engine, at the Python radius. Given the
# same radius, coverage and corroboration counts have no randomness and
# must match exactly.
cat("Part A - deterministic engine at the reference radius:\n")
d_region <- semanticfa:::.cvg_nn_dist(C, S)
cov_fixed <- mean(d_region <= ref$radius)
corrob_fixed <- semanticfa:::.cvg_corroboration(C, S, ref$radius)
ok_a <- c(
  report("coverage", cov_fixed, ref$coverage, 1e-9),
  identical(as.integer(corrob_fixed), as.integer(ref$corrob))
)
cat("  corroboration counts identical: ", ok_a[2], "\n", sep = "")

# ---- Part B: the Monte Carlo calibration, R's own RNG. Radius within its
# Monte Carlo error; the decision layer (flag set, relevance) invariant.
cat("\nPart B - Monte Carlo calibration with R's own draws:\n")
cal <- semanticfa:::.cvg_calibrate(C, n_ref = nrow(S), draws = 20L,
                                   radius_q = 0.95, seed = 1L)
corrob <- semanticfa:::.cvg_corroboration(C, S, cal$radius)
null_counts <- semanticfa:::.cvg_null_counts(C, n_ref = nrow(S),
                                             radius = cal$radius,
                                             draws = 200L, seed = 1L)
p <- semanticfa:::.cvg_pval(corrob, null_counts)
relevant <- p > 0.05
critical <- stats::quantile(null_counts, 0.05, names = FALSE)
ideal_rel <- mean(semanticfa:::.cvg_pval(null_counts, null_counts) > 0.05)
flagged_r <- which(!relevant) - 1L   # 0-based, to match Python

ok_b <- c(
  report("radius",          cal$radius, ref$radius,          0.010),
  report("item relevance",  mean(relevant), ref$relevance,   1e-9),
  report("critical count",  critical,   ref$critical,        5),
  report("ideal relevance", ideal_rel,  ref$ideal_relevance, 0.02)
)
same_flags <- identical(sort(flagged_r), sort(as.integer(ref$flagged)))
cat("  flagged items    R = {", paste(sort(flagged_r), collapse = ", "),
    "}\n                   Py = {", paste(sort(ref$flagged), collapse = ", "),
    "}  ", if (same_flags) "OK" else "MISMATCH", "\n", sep = "")

if (all(ok_a) && all(ok_b) && same_flags) {
  cat("\nVERIFIED: deterministic engine exact; Monte Carlo calibration\n")
  cat("within its own error; identical verdicts on every item.\n")
} else {
  stop("Verification failed.", call. = FALSE)
}
