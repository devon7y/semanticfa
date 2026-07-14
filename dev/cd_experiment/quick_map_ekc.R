suppressPackageStartupMessages({library(semanticfa); library(data.table)})
load("data/big5.rda")
embs <- list(
  `0.6B` = t(as.matrix(fread("dev/cd_experiment/emb_06B.csv"))),
  `4B`   = t(as.matrix(fread("dev/cd_experiment/emb_4B.csv"))),
  `8B`   = t(big5$embeddings))
for (m in names(embs)) {
  X <- embs[[m]]; R <- cor(X); N <- nrow(X)
  map <- suppressWarnings(suppressMessages(
    psych::VSS(R, n = 15, n.obs = N, plot = FALSE, fm = "minres")))
  ekc <- suppressWarnings(suppressMessages(EFAtools::EKC(R, N = N)))
  cat(sprintf("%5s (N=%d dims): MAP=%d  EKC=%d\n", m, N,
              which.min(map$map), ekc$n_factors_BvA2017))
}
