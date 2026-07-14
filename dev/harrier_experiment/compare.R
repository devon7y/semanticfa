## Harrier-OSS-27B vs Qwen3-Embedding-8B on the 50 IPIP Big-Five items.
## Main encoding (mean_centered_pearson) throughout, mirroring the paper.
## Battery: eigen structure, retention votes (5 criteria + MAP), CD profile,
## similarity-space separation, 5-factor fit quality (NMI/ARI vs theory,
## omega, Phi, DAAL), and human-benchmark congruence (matched Tucker phi,
## pair-level r).

suppressPackageStartupMessages({library(semanticfa); library(data.table)})
load("data/big5.rda")

read_npz_emb <- function(path) {
  o <- sfa_load_npz(path)
  o$embeddings
}
EH <- read_npz_emb("dev/harrier_experiment/Big5_items_Harrier27B.npz")
models <- list(`Qwen3-8B` = big5$embeddings, `Harrier-27B` = EH)
cat("dims:", sapply(models, ncol), "\n")

big5_df <- data.frame(code = big5$codes, item = big5$items,
                      factor = big5$factors, scoring = big5$scoring)

## ---- human benchmark (same cleaning as the paper) --------------------------
human_raw <- fread(Sys.getenv("BIG5_HUMAN_CSV",
  "/Users/devon7y/VS_Code/LLM_Factor_Analysis/scale_responses/Big5FM_data.csv"),
  data.table = FALSE)
keep <- rowSums(human_raw < 1 | human_raw > 5 | is.na(human_raw)) == 0
human <- human_raw[keep, ]
rev_idx <- which(big5$scoring < 0)
human[, rev_idx] <- 6 - human[, rev_idx]
R_human <- cor(human)
human_fa <- psych::fa(R_human, nfactors = 5, n.obs = nrow(human),
                      rotate = "oblimin", fm = "minres")
cat("human n:", nrow(human), "\n")

res <- list()
for (m in names(models)) {
  E <- models[[m]]
  sim <- sfa_similarity(E, "mean_centered_pearson",
                        factors = big5$factors, codes = big5$codes)
  eigs <- sort(eigen(sim, symmetric = TRUE, only.values = TRUE)$values,
               decreasing = TRUE)

  nf <- sfa_nfactors(sim, E,
                     methods = c("parallel", "kaiser", "TEFI", "EGA", "EKC"))
  mp <- sfa_map(sim)
  cd <- sfa_cd(E, n_factors_max = 10, seed = 42)

  ## similarity-space separation: within- vs between-domain pairs
  fmat <- outer(big5$factors, big5$factors, "==")
  lt <- lower.tri(sim)
  win <- sim[lt & fmat]; btw <- sim[lt & !fmat]
  sep_d <- (mean(win) - mean(btw)) /
    sqrt(((length(win) - 1) * var(win) + (length(btw) - 1) * var(btw)) /
         (length(win) + length(btw) - 2))

  fit <- sfa(big5_df, embeddings = E, encoding = "mean_centered_pearson",
             nfactors = 5)
  cong_theory <- sfa_congruence(fit, target = big5$factors,
                                metrics = c("nmi", "ari"))
  phi_f <- fit$Phi
  phi_off <- abs(phi_f[lower.tri(phi_f)])
  om <- fit$omega
  daal <- fit$daal
  daal_gap <- if (!is.null(daal)) {
    dg <- apply(daal, 2, max) - apply(daal, 2, function(z) max(z[-which.max(z)]))
    mean(dg)
  } else NA_real_

  tuck <- abs(psych::factor.congruence(fit$loadings, human_fa$loadings))
  tuck_matched <- mean(apply(tuck, 1, max))
  pair_r <- cor(sim[lt], R_human[lt])

  res[[m]] <- list(
    dim = ncol(E), eig1 = eigs[1], eig26 = eigs[2:6],
    gap56 = eigs[5] / eigs[6],
    votes = setNames(nf$methods$n_factors, nf$methods$method),
    consensus = nf$consensus, map = mp$n_factors,
    cd_profile = cd$profile, cd_improvement = cd$improvement,
    sep_within = mean(win), sep_between = mean(btw), sep_d = sep_d,
    nmi = cong_theory$nmi, ari = cong_theory$ari,
    kmo = fit$kmo$total, rmsr = fit$rmsr,
    omega_assigned = mean(om$omega_assigned),
    phi_off_mean = mean(phi_off), phi_off_max = max(phi_off),
    daal_gap = daal_gap,
    tucker_matched = tuck_matched, pair_r = pair_r)

  cat(sprintf("\n==== %s (dim %d) ====\n", m, ncol(E)))
  cat("votes: ", paste(names(res[[m]]$votes), res[[m]]$votes,
                       collapse = "  "), " | consensus", nf$consensus,
      "| MAP", mp$n_factors, "\n")
  cat(sprintf("eig1 %.2f | eig2-6 %s | gap l5/l6 %.3f\n", eigs[1],
              paste(sprintf("%.2f", eigs[2:6]), collapse = " "),
              res[[m]]$gap56))
  cat(sprintf("CD improvement by k: %s\n",
              paste(sprintf("%.1f", 100 * cd$improvement), collapse = " ")))
  cat(sprintf("separation: within %.3f between %.3f d %.2f\n",
              mean(win), mean(btw), sep_d))
  cat(sprintf("theory: NMI %.3f ARI %.3f | KMO %.3f RMSR %.3f\n",
              res[[m]]$nmi, res[[m]]$ari, res[[m]]$kmo, res[[m]]$rmsr))
  cat(sprintf("omega(assigned) %.3f | Phi off mean %.3f max %.3f | DAAL gap %.3f\n",
              res[[m]]$omega_assigned, mean(phi_off), max(phi_off),
              daal_gap))
  cat(sprintf("human: matched Tucker %.3f | pair-level r %.3f\n",
              tuck_matched, pair_r))
}

saveRDS(res, "dev/harrier_experiment/comparison.rds")
cat("\nDONE\n")
