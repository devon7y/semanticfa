## make_ci_assets.R -- delete-one-item jackknife intervals for the manuscript's
## headline agreement statistics.
##
## Run from the reproduce/ directory, AFTER reproduce.R and BEFORE
## make_paper_assets.R (which hard-requires the CSV this script writes):
##   Rscript make_ci_assets.R
##   Rscript make_paper_assets.R
##
## Reads   output/results.rds, data(big5), and the human response file
##         (BIG5_HUMAN_CSV, default data/Big5FM_data.csv -- same convention
##         as reproduce.R; the file is not redistributed, see reproduce.R)
## Writes  output/ci_jackknife.csv
##
## Why an item-level jackknife: the 1,225 item pairs are not independent
## (every item participates in 49 pairs), so a pair-level interval would be
## anticonservative, and with N = 874,434 respondents the human-side
## sampling error is negligible relative to item-level uncertainty. Deleting
## one item at a time and recomputing the statistic on the remaining 49
## respects the dependence structure. The pair-level correlation is
## jackknifed on the Fisher-z scale and back-transformed; the mean matched
## Tucker phi is jackknifed on the raw scale, re-fitting BOTH factor
## solutions (semantic and human) and re-matching factors per replicate so
## label switching cannot bias the interval.
##
## This script deliberately does NOT touch reproduce.R or reproduce.Rout:
## the transcript is byte-checked against the manuscript's verbatim excerpts.

suppressMessages(library(semanticfa))
suppressMessages(library(psych))

res <- readRDS("output/results.rds")
data(big5)

HUMAN_CSV <- Sys.getenv("BIG5_HUMAN_CSV", "data/Big5FM_data.csv")

big5_df <- data.frame(code = big5$codes, item = big5$items,
                      factor = big5$factors, scoring = big5$scoring)
E8 <- big5$embeddings

## --- human correlation matrices (mirrors reproduce.R section 10) ------------

human_raw <- read.csv(HUMAN_CSV)
stopifnot(identical(names(human_raw), big5$codes))
keep  <- rowSums(human_raw < 1 | human_raw > 5 | is.na(human_raw)) == 0
human <- human_raw[keep, ]
n_human <- nrow(human)
stopifnot(n_human == res$n_human)

human_keyed <- human
rev_idx <- which(big5$scoring < 0)
human_keyed[, rev_idx] <- 6 - human_keyed[, rev_idx]
R_human     <- cor(human_keyed)
R_human_raw <- cor(human)
rm(human_raw, human, human_keyed)

## --- full-sample statistics (must reproduce the stored values) --------------

sim_mcp <- sfa_similarity(E8, "mean_centered_pearson",
                          factors = big5$factors, codes = big5$codes)
lt <- lower.tri(sim_mcp)
r_full <- cor(sim_mcp[lt], R_human_raw[lt])
stopifnot(abs(r_full - res$pair_level_r["mean_centered_pearson", "r_raw"]) < 1e-10)

fit_full <- sfa(big5_df, embeddings = E8, encoding = "mean_centered_pearson",
                nfactors = 5)
hfa_full <- psych::fa(R_human, nfactors = 5, n.obs = n_human,
                      rotate = "oblimin", fm = "minres")
matched_mean <- function(sem_load, hum_load) {
  phi <- psych::factor.congruence(sem_load, hum_load)
  mean(apply(abs(phi), 1, max))
}
phimean_full <- matched_mean(fit_full$loadings, hfa_full$loadings)
phimean_stored <- mean(apply(abs(res$tucker_matrix), 1, max))
stopifnot(abs(phimean_full - phimean_stored) < 5e-4)

## --- delete-one-item jackknife ----------------------------------------------

n_items <- nrow(E8)
z_i   <- numeric(n_items)
phi_i <- numeric(n_items)
for (i in seq_len(n_items)) {
  sim49 <- sim_mcp[-i, -i]
  lt49  <- lower.tri(sim49)
  z_i[i] <- atanh(cor(sim49[lt49], R_human_raw[-i, -i][lt49]))
  fit49  <- sfa(big5_df[-i, ], embeddings = E8[-i, , drop = FALSE],
                encoding = "mean_centered_pearson", nfactors = 5)
  hfa49  <- psych::fa(R_human[-i, -i], nfactors = 5, n.obs = n_human,
                      rotate = "oblimin", fm = "minres")
  phi_i[i] <- matched_mean(fit49$loadings, hfa49$loadings)
  cat(sprintf("jackknife %2d/%d: r = %.4f, mean matched phi = %.4f\n",
              i, n_items, tanh(z_i[i]), phi_i[i]))
}

jack_se <- function(theta) sqrt((length(theta) - 1) / length(theta) *
                                sum((theta - mean(theta))^2))
z_se    <- jack_se(z_i)
r_lo    <- tanh(atanh(r_full) - 1.96 * z_se)
r_hi    <- tanh(atanh(r_full) + 1.96 * z_se)
phi_se  <- jack_se(phi_i)
phi_lo  <- phimean_full - 1.96 * phi_se
phi_hi  <- phimean_full + 1.96 * phi_se

out <- data.frame(
  quantity = c("pair_r_mcp_raw", "pair_r_mcp_raw_lo", "pair_r_mcp_raw_hi",
               "phi_mean_matched", "phi_mean_matched_lo", "phi_mean_matched_hi",
               "n_items_jackknife"),
  value = c(r_full, r_lo, r_hi, phimean_full, phi_lo, phi_hi, n_items))
write.csv(out, "output/ci_jackknife.csv", row.names = FALSE)
cat(sprintf("r = %.4f, 95%% CI [%.4f, %.4f]\n", r_full, r_lo, r_hi))
cat(sprintf("mean matched phi = %.4f, 95%% CI [%.4f, %.4f]\n",
            phimean_full, phi_lo, phi_hi))
cat("Wrote output/ci_jackknife.csv\n")
