## Round 2: (a) human N-crossover (does the alpha rule recover 5 at the sample
## sizes CD was validated on, and saturate as N grows?); (b) dimension-
## subsampled CD on the embeddings (pseudo-N brought into CD's validated
## regime; replicates over random dimension subsets for stability).

suppressPackageStartupMessages({
  library(EFAtools)
  library(data.table)
})

out_rds <- "dev/cd_experiment/cd_results.rds"
results <- readRDS(out_rds)

decide_from_rmse <- function(rmse, alpha) {
  K <- ncol(rmse)
  for (k in 2:K) {
    col <- rmse[, k]
    if (all(is.na(col)) || all(col == 0)) return(k - 1L)
    p <- suppressWarnings(
      wilcox.test(col, rmse[, k - 1], alternative = "less",
                  exact = FALSE)$p.value)
    if (is.na(p) || p >= alpha) return(k - 1L)
  }
  K
}

run_cd <- function(X, label, seed = 42) {
  if (!is.null(results[[label]])) {
    cat(sprintf("[%s] already done, skipping\n", label)); return(invisible())
  }
  t0 <- proc.time()["elapsed"]
  set.seed(seed)
  cd <- EFAtools::CD(as.matrix(X), n_factors_max = 10,
                     N_pop = 10000, N_samples = 500, alpha = .30)
  el <- proc.time()["elapsed"] - t0
  rmse <- cd$RMSE_eigenvalues
  res <- list(n_factors_alpha30 = cd$n_factors, eigenvalues = cd$eigenvalues,
              elapsed_s = unname(el), rmse = rmse,
              median_rmse = apply(rmse, 2, function(z)
                if (all(z == 0)) NA_real_ else median(z)),
              verdicts = sapply(c(a30 = .30, a05 = .05, a01 = .01),
                                function(a) decide_from_rmse(rmse, a)))
  cat(sprintf("[%s] N=%d verdicts: .30=%d .05=%d .01=%d (%.0fs)\n",
              label, nrow(X), res$verdicts["a30"], res$verdicts["a05"],
              res$verdicts["a01"], el))
  cat(sprintf("[%s] median RMSR by k: %s\n", label,
              paste(sprintf("%.3f", res$median_rmse), collapse = " ")))
  results[[label]] <<- res
  saveRDS(results, out_rds)
}

## ---- (a) human N-crossover -------------------------------------------------
load("data/big5.rda")
HUMAN_CSV <- Sys.getenv("BIG5_HUMAN_CSV",
  "/Users/devon7y/VS_Code/LLM_Factor_Analysis/scale_responses/Big5FM_data.csv")
human_raw <- fread(HUMAN_CSV, data.table = FALSE)
keep <- rowSums(human_raw < 1 | human_raw > 5 | is.na(human_raw)) == 0
human <- human_raw[keep, ]
rev_idx <- which(big5$scoring == -1)
human[, rev_idx] <- 6 - human[, rev_idx]

for (n in c(250, 500, 1000)) {
  set.seed(42)
  run_cd(human[sample(nrow(human), n), ], sprintf("human_n%d", n))
}

## ---- (b) dimension-subsampled CD on the embeddings -------------------------
embs <- list(
  emb_06B = t(as.matrix(fread("dev/cd_experiment/emb_06B.csv"))),
  emb_4B  = t(as.matrix(fread("dev/cd_experiment/emb_4B.csv"))),
  emb_8B  = t(big5$embeddings))

## 8B: five replicates over disjoint-seeded random 500-dim subsets
for (r in 1:5) {
  set.seed(100 + r)
  dims <- sample(nrow(embs$emb_8B), 500)
  run_cd(embs$emb_8B[dims, ], sprintf("emb_8B_d500_r%d", r), seed = 100 + r)
}
## 0.6B and 4B: two replicates each
for (m in c("emb_06B", "emb_4B")) for (r in 1:2) {
  set.seed(200 + r)
  dims <- sample(nrow(embs[[m]]), 500)
  run_cd(embs[[m]][dims, ], sprintf("%s_d500_r%d", m, r), seed = 200 + r)
}

cat("\nDONE2\n")
v <- sapply(results, function(r) r$verdicts)
print(v[, order(colnames(v))])
