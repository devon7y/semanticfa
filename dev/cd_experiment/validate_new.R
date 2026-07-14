suppressPackageStartupMessages({library(data.table); devtools::load_all(".", quiet = TRUE)})
load("data/big5.rda")
embs <- list(`0.6B` = t(as.matrix(fread("dev/cd_experiment/emb_06B.csv"))),
             `4B`   = t(as.matrix(fread("dev/cd_experiment/emb_4B.csv"))),
             `8B`   = t(big5$embeddings))

cat("== EKC vs EFAtools (BvA2017) ==\n")
for (m in names(embs)) {
  X <- embs[[m]]; R <- cor(X)
  mine <- sfa_ekc(R, n = nrow(X))$n_factors
  ref  <- suppressMessages(EFAtools::EKC(R, N = nrow(X)))$n_factors_BvA2017
  cat(sprintf("  %5s: sfa_ekc=%d EFAtools=%d %s\n", m, mine, ref,
              ifelse(mine == ref, "OK", "MISMATCH")))
}

cat("== MAP vs psych::VSS (full scan) ==\n")
for (m in names(embs)) {
  X <- embs[[m]]; R <- cor(X)
  mine <- sfa_map(R)
  ref <- suppressWarnings(suppressMessages(
    psych::VSS(R, n = 48, n.obs = nrow(X), plot = FALSE, fm = "minres")))
  ref_k <- which.min(ref$map)
  cat(sprintf("  %5s: sfa_map=%d psych=%d %s  (curve cor %.4f over shared)\n",
              m, mine$n_factors, ref_k,
              ifelse(mine$n_factors == ref_k, "OK", "MISMATCH"),
              cor(mine$map[1:40], ref$map[1:40], use = "complete.obs")))
}

cat("== human EKC/MAP anchor (n=2500 subsample, seed 42) ==\n")
human_raw <- fread(Sys.getenv("BIG5_HUMAN_CSV",
  "/Users/devon7y/VS_Code/LLM_Factor_Analysis/scale_responses/Big5FM_data.csv"),
  data.table = FALSE)
keep <- rowSums(human_raw < 1 | human_raw > 5 | is.na(human_raw)) == 0
human <- human_raw[keep, ]; rev_idx <- which(big5$scoring == -1)
human[, rev_idx] <- 6 - human[, rev_idx]
set.seed(42); sub <- as.matrix(human[sample(nrow(human), 2500), ])
Rh <- cor(sub)
cat(sprintf("  EKC: mine=%d EFAtools=%d | MAP: mine=%d psych=%d\n",
    sfa_ekc(Rh, n = 2500)$n_factors,
    suppressMessages(EFAtools::EKC(Rh, N = 2500))$n_factors_BvA2017,
    sfa_map(Rh)$n_factors,
    which.min(suppressWarnings(suppressMessages(
      psych::VSS(Rh, n = 30, n.obs = 2500, plot = FALSE)))$map)))

cat("== sfa_cd smoke: 8B profile (fast settings) ==\n")
t0 <- proc.time()["elapsed"]
cd8 <- sfa_cd(big5$embeddings, n_factors_max = 8, n_samples = 150,
              n_pop = 5000, gen_iter = 6, seed = 42)
cat(sprintf("  8B norm profile: %s (%.0fs)\n",
    paste(sprintf("%.3f", cd8$profile), collapse = " "),
    proc.time()["elapsed"] - t0))
cat(sprintf("  monotone: %s, verdict NA: %s\n",
    all(diff(cd8$median_rmsr) < 0), is.na(cd8$n_factors)))

cat("== sfa_cd human n=500 with alpha=.30 (EFAtools said 7) ==\n")
set.seed(42); sub5 <- as.matrix(human[sample(nrow(human), 500), ])
cdh <- sfa_cd(sub5, input = "data", n_factors_max = 10, n_samples = 500,
              alpha = .30, seed = 42)
cat(sprintf("  verdict=%d | norm profile: %s\n", cdh$n_factors,
    paste(sprintf("%.3f", cdh$profile), collapse = " ")))
