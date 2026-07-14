## Comparison Data (Ruscio & Roche, 2012) adapted to semanticfa's response-free
## setting: the "data matrix" is the transposed item-embedding matrix, so cases
## are embedding dimensions and variables are items. For the paper's main
## encoding (mean_centered_pearson), cor(t(E)) IS the analyzed similarity
## matrix, so classical CD applies without touching the eigen machinery.
##
## Datasets: Qwen3-Embedding 0.6B / 4B / 8B item embeddings + a human-response
## anchor (2,500-respondent subsample, reverse-scored, same cleaning as
## reproduce.R). Reference implementation: EFAtools::CD.
##
## Outputs: dev/cd_experiment/cd_results.rds (incremental) + console log.

suppressPackageStartupMessages({
  library(EFAtools)
  library(semanticfa)
  library(data.table)
})

out_rds <- "dev/cd_experiment/cd_results.rds"
results <- if (file.exists(out_rds)) readRDS(out_rds) else list()

## ---- sequential decision rule, re-derivable at any alpha from the RMSE
## distributions (Ruscio & Roche step 7: one-tailed Mann-Whitney, k vs k-1)
decide_from_rmse <- function(rmse, alpha) {
  K <- ncol(rmse)
  for (k in 2:K) {
    if (all(is.na(rmse[, k]))) return(k - 1L)  # EFAtools stopped before k
    p <- suppressWarnings(
      wilcox.test(rmse[, k], rmse[, k - 1], alternative = "less",
                  exact = FALSE)$p.value)
    if (is.na(p) || p >= alpha) return(k - 1L)
  }
  K
}

run_cd <- function(X, label) {
  if (!is.null(results[[label]])) {
    cat(sprintf("[%s] already done, skipping\n", label)); return(invisible())
  }
  cat(sprintf("\n==== %s: N=%d cases x %d variables ====\n",
              label, nrow(X), ncol(X)))
  t0 <- proc.time()["elapsed"]
  set.seed(42)
  cd <- EFAtools::CD(as.matrix(X), n_factors_max = 10,
                     N_pop = 10000, N_samples = 500, alpha = .30)
  el <- proc.time()["elapsed"] - t0
  res <- list(n_factors_alpha30 = cd$n_factors,
              eigenvalues = cd$eigenvalues, elapsed_s = unname(el))
  ## RMSE distributions, if the object carries them
  rm_slot <- Filter(function(n) grepl("RMSE", n, ignore.case = TRUE), names(cd))
  if (length(rm_slot)) {
    rmse <- cd[[rm_slot[1]]]
    if (is.matrix(rmse) && nrow(rmse) > 1) {
      res$rmse <- rmse
      res$median_rmse <- apply(rmse, 2, median, na.rm = TRUE)
      res$verdicts <- sapply(c(a30 = .30, a05 = .05, a01 = .01),
                             function(a) decide_from_rmse(rmse, a))
    }
  }
  if (is.null(res$verdicts)) {
    cat(sprintf("[%s] no RMSE distributions returned; rerunning at .05/.01\n",
                label))
    vs <- sapply(c(.05, .01), function(a) {
      set.seed(42)
      EFAtools::CD(as.matrix(X), n_factors_max = 10, N_pop = 10000,
                   N_samples = 500, alpha = a)$n_factors
    })
    res$verdicts <- c(a30 = cd$n_factors, a05 = vs[1], a01 = vs[2])
  }
  cat(sprintf("[%s] verdicts: alpha.30=%d  alpha.05=%d  alpha.01=%d  (%.0fs)\n",
              label, res$verdicts["a30"], res$verdicts["a05"],
              res$verdicts["a01"], el))
  if (!is.null(res$median_rmse))
    cat(sprintf("[%s] median RMSR by k: %s\n", label,
                paste(sprintf("%.4f", res$median_rmse), collapse = " ")))
  results[[label]] <<- res
  saveRDS(results, out_rds)
}

## ---- embedding datasets (dims as cases) -----------------------------------
load("data/big5.rda")
E8 <- big5$embeddings

## sanity: dims-as-cases correlation == package similarity (main encoding)
sim_pkg <- sfa_similarity(E8, "mean_centered_pearson")
gap <- max(abs(cor(t(E8)) - unclass(sim_pkg)))
cat(sprintf("sanity max|cor(t(E)) - sfa_similarity| = %.2e\n", gap))
stopifnot(gap < 1e-8)

embs <- list(
  emb_06B = t(as.matrix(fread("dev/cd_experiment/emb_06B.csv"))),
  emb_4B  = t(as.matrix(fread("dev/cd_experiment/emb_4B.csv"))),
  emb_8B  = t(E8))
for (nm in names(embs)) run_cd(embs[[nm]], nm)

## ---- human anchor: same cleaning + reverse-scoring as reproduce.R ---------
HUMAN_CSV <- Sys.getenv("BIG5_HUMAN_CSV",
  "/Users/devon7y/VS_Code/LLM_Factor_Analysis/scale_responses/Big5FM_data.csv")
human_raw <- fread(HUMAN_CSV, data.table = FALSE)
stopifnot(identical(names(human_raw), big5$codes))
keep <- rowSums(human_raw < 1 | human_raw > 5 | is.na(human_raw)) == 0
human <- human_raw[keep, ]
cat(sprintf("human rows after cleaning: %d\n", nrow(human)))
rev_idx <- which(big5$scoring == -1)
human[, rev_idx] <- 6 - human[, rev_idx]

set.seed(42)
sub <- human[sample(nrow(human), 2500), ]
run_cd(sub, "human_n2500")

cat("\nDONE\n")
print(sapply(results, function(r) r$verdicts))
