## ===========================================================================
## reproduce.R — full reproduction script for the semanticfa paper
##
## "semanticfa: An R Package for Semantic Factor Analysis of Psychometric
##  Scale Items with Language-Model Embeddings"
##  Yanitski & Westbury, University of Alberta
##
## This script regenerates every number, table, and figure in the paper's
## demonstration (Pillar 3) from the inputs in data/. Run it from the
## reproduce/ directory, e.g.:
##
##     R CMD BATCH --no-save --no-timing reproduce.R
##
## Inputs:
##   data(big5)           - bundled with the package: the 50 IPIP Big-Five
##                          Factor Marker items with precomputed
##                          Qwen/Qwen3-Embedding-8B embeddings (50 x 4096;
##                          last-token pooling, no instruction prefix). The
##                          main analyses run on this bundled matrix.
##   Big5FM_aux_8B.npz    - Qwen3-Embedding-8B vectors for construct names and
##                          pole words, extracted from a precomputed 1M-word
##                          lexicon (see data-raw/extract_aux_8B.py). Needed so
##                          construct labels / projection poles live in the
##                          same embedding space as the 8B item vectors.
##   Big5_items_0.6B.npz  - reference embeddings of the same items generated
##                          offline with the package's DEFAULT on-device model
##                          (Qwen/Qwen3-Embedding-0.6B; 50 x 1024). Used to
##                          verify the live sfa_embed() backend (Section 4)
##                          and as the matching space for vetting new
##                          candidate items (Section 8).
##   Big5FM_data.csv      - human responses to the same 50 items from the
##                          Open-Source Psychometrics Project "BIG5" dataset
##                          (https://openpsychometrics.org/_rawdata/), columns
##                          E1..O50, 5-point Likert, 0 = missed. Used ONLY as
##                          the validation target for the semantic structure;
##                          no response data enters the semantic analysis.
##
## Requirements: semanticfa (>= 0.1.0), psych, EGAnet, Rtsne, uwot, and a
## Python with numpy (for reading .npz) and sentence-transformers (only for
## the live-embedding demos in Sections 4 and 5; reticulate provisions this
## automatically on first use).
## ===========================================================================

library(semanticfa)
set.seed(42)

AUX_NPZ   <- "data/Big5FM_aux_8B.npz"
REF06_NPZ <- "data/Big5_items_0.6B.npz"
HUMAN_CSV <- Sys.getenv("BIG5_HUMAN_CSV", "data/Big5FM_data.csv")

dir.create("output",  showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

results <- list()   # machine-readable copy of every key number in the paper

banner <- function(...) cat("\n\n=====", ..., "=====\n\n")

## ---------------------------------------------------------------------------
## 1. Load the items and the embeddings
## ---------------------------------------------------------------------------
banner("1. Load items and embeddings")

data(big5)   # 50 IPIP Big-Five Factor Markers + bundled Qwen3-Embedding-8B
str(big5, max.level = 1)

big5_df <- data.frame(code = big5$codes, item = big5$items,
                      factor = big5$factors, scoring = big5$scoring)

## The high-fidelity item embeddings ship with the package: precomputed
## offline with Qwen/Qwen3-Embedding-8B (50 x 4096; last-token pooling, no
## instruction prefix). The scoring follows the official IPIP key with the
## Emotional Stability items keyed in the Neuroticism direction (18
## reverse-keyed items in total).
E8 <- big5$embeddings    # 50 x 4096

## Auxiliary 8B vectors: construct names + pole words in the same space
aux <- sfa_load_npz(AUX_NPZ, codes_key = "codes")
aux_vec <- function(w) aux$embeddings[match(w, aux$codes), , drop = TRUE]

## ---------------------------------------------------------------------------
## 2. Four similarity encodings from one embedding matrix
## ---------------------------------------------------------------------------
banner("2. sfa_similarity: four encodings")

sim_at  <- sfa_similarity(E8, "atomic",
                          factors = big5$factors, codes = big5$codes)
sim_ar  <- sfa_similarity(E8, "atomic_reversed", scoring = big5$scoring,
                          factors = big5$factors, codes = big5$codes)
sim_sq  <- sfa_similarity(E8, "squid",
                          factors = big5$factors, codes = big5$codes)
sim_mcp <- sfa_similarity(E8, "mean_centered_pearson",
                          factors = big5$factors, codes = big5$codes)

enc_range <- t(sapply(list(atomic = sim_at, atomic_reversed = sim_ar,
                           squid = sim_sq, mean_centered_pearson = sim_mcp),
                      function(s) round(range(s[lower.tri(s)]), 3)))
colnames(enc_range) <- c("min", "max")
enc_range
results$encoding_ranges <- enc_range

## ---------------------------------------------------------------------------
## 3. Signed similarity from natural-language inference
## ---------------------------------------------------------------------------
banner("3. sfa_nli_matrix: entailment - contradiction")

t_nli <- system.time(
  sim_nli <- sfa_nli_matrix(big5$items)   # cross-encoder/nli-deberta-v3-base
)
cat("NLI matrix:", nrow(sim_nli), "x", ncol(sim_nli),
    "in", round(t_nli["elapsed"], 1), "s\n")
dimnames(sim_nli) <- list(big5$codes, big5$codes)

## the canonical example: an extraversion item vs. its reverse-keyed sibling
round(sim_nli[c("E1", "E2", "E5"), c("E1", "E2", "E5")], 2)
round(sim_ar[c("E1", "E2", "E5"), c("E1", "E2", "E5")], 2)
results$nli_example <- round(sim_nli[c("E1", "E2", "E5"),
                                     c("E1", "E2", "E5")], 2)

## ---------------------------------------------------------------------------
## 4. sfa_embed: the on-device default backend (bundled-data provenance)
## ---------------------------------------------------------------------------
banner("4. sfa_embed: on-device embedding with the default model")

## The package's default on-device model is Qwen/Qwen3-Embedding-0.6B.
## Re-embed the items here to demonstrate the live backend and check
## agreement with reference vectors generated offline with the same model
## (loaded from a .npz, which also demonstrates sfa_load_npz on item
## embeddings). Clearing the cache first ensures the timing reflects real
## on-device embedding.
ref06 <- sfa_load_npz(REF06_NPZ)
ref06
stopifnot(identical(ref06$codes, big5$codes))

sfa_clear_cache()
t_emb <- system.time(emb06 <- sfa_embed(big5$items))
cat("sfa_embed:", nrow(emb06), "x", ncol(emb06),
    "in", round(t_emb["elapsed"], 1), "s\n")
cos_live_ref <- sapply(seq_len(nrow(emb06)), function(i) {
  a <- emb06[i, ]; b <- ref06$embeddings[i, ]
  sum(a * b) / sqrt(sum(a^2) * sum(b^2))
})
cat("cosine(live re-embedding, offline reference) per item:\n")
print(summary(round(cos_live_ref, 4)))
results$embed_live_vs_reference <- summary(cos_live_ref)

## ---------------------------------------------------------------------------
## 5. How many factors? sfa_parallel / sfa_nfactors / sfa_dimselect
## ---------------------------------------------------------------------------
banner("5. Factor retention")

pa <- sfa_parallel(sim_mcp, E8)
pa
results$parallel <- pa

nf <- sfa_nfactors(sim_mcp, embeddings = E8,
                   methods = c("parallel", "kaiser", "TEFI", "EGA"))
nf
results$nfactors <- nf

t_ds <- system.time(
  ds <- sfa_dimselect(E8, factors = big5$factors,
                      encoding = "mean_centered_pearson")
)
ds
cat("sfa_dimselect elapsed:", round(t_ds["elapsed"], 1), "s\n")
results$dimselect <- ds

## ---------------------------------------------------------------------------
## 6. The fitted model: sfa() end to end
## ---------------------------------------------------------------------------
banner("6. sfa(): the one-call pipeline")

## Main fit: the keying-free mean-centered Pearson encoding — it yields a
## genuine correlation matrix and, in the encoding comparison of Section 11,
## matches both the theoretical partition and the human-data structure as well
## as or better than every alternative. nfactors is left to parallel analysis
## first, then fixed at 5.
fit_auto <- sfa(big5_df, embeddings = E8, encoding = "mean_centered_pearson")
cat("Auto-retained factors:", fit_auto$factors, "\n")

fit <- sfa(big5_df, embeddings = E8, encoding = "mean_centered_pearson",
           nfactors = 5)
fit
summary(fit)
results$fit_diagnostics <- fit[c("kmo", "tefi", "rmsr", "caf", "omega", "daal")]

## Monte-Carlo null calibration of the diagnostics (Pokropek-style)
t_cal <- system.time(
  fit_cal <- sfa(big5_df, embeddings = E8, encoding = "mean_centered_pearson",
                 nfactors = 5, calibrate = TRUE, calibrate_iter = 100)
)
cat("calibrate=TRUE elapsed:", round(t_cal["elapsed"], 1), "s\n")
fit_cal$calibration
results$calibration <- fit_cal$calibration

## hand-off to the psych ecosystem
ps <- as_psych(fit)
print(psych::fa.sort(unclass(ps$loadings)), digits = 2, cutoff = 0.3)

## ---------------------------------------------------------------------------
## 7. Interpretation: sfa_anchor, sfa_project, sfa_congruence (theory)
## ---------------------------------------------------------------------------
banner("7. Interpretation")

anc <- sfa_anchor(fit, anchor = "centroid")
anc

## label anchors need construct-name embeddings from the SAME (8B) space
lab_names <- c("Extraversion", "Neuroticism", "Agreeableness",
               "Conscientiousness", "Openness")
lab_emb <- rbind(Extraversion      = aux_vec("extraversion"),
                 Neuroticism       = aux_vec("neuroticism"),
                 Agreeableness     = aux_vec("agreeableness"),
                 Conscientiousness = aux_vec("conscientiousness"),
                 Openness          = aux_vec("openness"))
anc_lab <- sfa_anchor(fit, anchor = "label", label_embeddings = lab_emb)
anc_lab
results$anchor_centroid <- anc$centroid
results$anchor_label <- anc_lab$label

## bipolar semantic axes (Grand et al.-style projection), 8B pole words
poles <- list(
  stability   = list(low = aux_vec("anxious"),  high = aux_vec("calm")),
  sociability = list(low = aux_vec("solitary"), high = aux_vec("sociable")))
prj <- sfa_project(fit,
                   axes = list(stability   = c(low = "anxious",  high = "calm"),
                               sociability = c(low = "solitary", high = "sociable")),
                   pole_embeddings = poles)
prj
cat("\nMost anxious-pole items (stability axis):\n")
print(round(head(sort(prj$scores[, "stability"]), 5), 2))
cat("\nMost calm-pole items:\n")
print(round(tail(sort(prj$scores[, "stability"]), 5), 2))
cat("\nMost sociable-pole items (sociability axis):\n")
print(round(tail(sort(prj$scores[, "sociability"]), 5), 2))
results$projection <- prj$scores

## congruence with the theoretical Big-Five partition
cong_theory <- sfa_congruence(fit, target = big5$factors,
                              metrics = c("nmi", "ari"))
cong_theory
results$congruence_theory <- cong_theory

## jingle/jangle screen across the five subscales (8B item + label vectors)
sub_items <- split(big5$items, big5$factors)
sub_emb   <- lapply(split(seq_len(50), big5$factors),
                    function(i) E8[i, , drop = FALSE])
jj_lab_emb <- rbind(
  Agreeableness     = aux_vec("agreeableness"),
  Conscientiousness = aux_vec("conscientiousness"),
  Extraversion      = aux_vec("extraversion"),
  Neuroticism       = aux_vec("neuroticism"),
  Openness          = aux_vec("openness"))[names(sub_items), ]
jj <- sfa_jinglejangle(sub_items, item_embeddings = sub_emb,
                       label_embeddings = jj_lab_emb)
jj
results$jinglejangle <- jj

## ---------------------------------------------------------------------------
## 8. Refinement before any data collection
## ---------------------------------------------------------------------------
banner("8. Refinement: redundancy, short form, candidate items")

red_uva <- sfa_redundancy(fit)                     # UVA / wTO (EGAnet)
red_uva
red_cos <- sfa_redundancy(fit, method = "cosine", threshold = 0.85)
red_cos
results$redundancy_uva <- red_uva
results$redundancy_cosine <- red_cos

short <- sfa_simplify(fit, target_n = 5, method = "anchor")   # 50 -> 25 items
short
short$keep
results$shortform <- short

## Vetting brand-new candidate items embeds NEW text, so it runs in the
## 0.6B space (the package's on-device default model), where the matching
## model is available locally; the item vectors are the offline 0.6B
## reference loaded in Section 4. The fit is built WITHOUT the scoring column:
## sfa_item_fit() sign-flips reverse-keyed items before forming construct
## centroids, and — as the encoding comparison shows — a sign-flipped
## embedding is an anti-topic vector, so flipped items cancel a centroid
## rather than sharpen it. Omitting scoring keeps the centroids as pure
## topic centroids. (The similarity side is unaffected: the
## mean_centered_pearson encoding is keying-free.)
fit06 <- sfa(big5_df[, c("code", "item", "factor")],
             embeddings = ref06$embeddings,
             encoding = "mean_centered_pearson", nfactors = 5)
cand <- c("I make friends easily.",
          "I am the life of every party.",
          "I rarely feel anxious or depressed.")
## the fit was built from precomputed embeddings, so name the model the
## candidates should be embedded with (the same model as the bundled vectors)
ifit <- sfa_item_fit(fit06, cand, model = "Qwen/Qwen3-Embedding-0.6B",
                     redundancy_cutoff = 0.85)
ifit
results$item_fit <- ifit

## ---------------------------------------------------------------------------
## 9. Visualization (figures used in the paper)
## ---------------------------------------------------------------------------
banner("9. Figures")

pdf("figures/fig_corplot.pdf", width = 7.5, height = 7.5)
sfa_corplot(fit, order = c("E", "N", "A", "C", "O"))
dev.off()

## the signed NLI matrix makes the block structure (and the negative
## relations of reverse-keyed items) far more visible than raw cosine
pdf("figures/fig_corplot_nli.pdf", width = 7.5, height = 7.5)
sfa_corplot(sim_nli, factors = big5$factors,
            order = c("E", "N", "A", "C", "O"))
dev.off()

pdf("figures/fig_itemmap.pdf", width = 8, height = 8)
op <- par(mfrow = c(2, 2), mar = c(3.5, 3.5, 2.5, 1))
for (m in c("tsne", "umap", "pca", "mds")) {
  sfa_itemplot(fit, method = m, seed = 42, legend = (m == "tsne"))
}
par(op)
dev.off()

pdf("figures/fig_scree.pdf", width = 7, height = 5)
plot(fit, type = "scree")
dev.off()

pdf("figures/fig_loadings.pdf", width = 7, height = 8)
plot(fit, type = "loadings")
dev.off()

## ---------------------------------------------------------------------------
## 9b. Model size and structural clarity: 0.6B vs 4B vs 8B
## ---------------------------------------------------------------------------
banner("9b. Model-size comparison")

## The same 50 items embedded with three sizes of the same model family
## (Qwen3-Embedding-0.6B / 4B / 8B), all under the main keying-free encoding.
## The 0.6B vectors were loaded in Section 4; the 8B vectors are the bundled
## ones used throughout.
emb_by_model <- list(
  `0.6B` = ref06$embeddings,
  `4B`   = sfa_load_npz("data/Big5_items_4B.npz")$embeddings,
  `8B`   = E8)

model_nf <- list()
for (m in names(emb_by_model)) {
  sim_m <- sfa_similarity(emb_by_model[[m]], "mean_centered_pearson",
                          factors = big5$factors, codes = big5$codes)
  pdf(sprintf("figures/fig_corplot_%s.pdf", sub("0.6B", "06B", m)),
      width = 7.5, height = 7.5)
  sfa_corplot(sim_m, order = c("E", "N", "A", "C", "O"))
  dev.off()
  nf_m <- sfa_nfactors(sim_m, embeddings = emb_by_model[[m]],
                       methods = c("parallel", "kaiser", "TEFI", "EGA"))
  cat(sprintf("\nQwen3-Embedding-%s (%d dims):\n", m, ncol(emb_by_model[[m]])))
  print(nf_m)
  model_nf[[m]] <- nf_m
}
results$model_comparison <- lapply(model_nf, function(x)
  list(methods = x$methods, consensus = x$consensus))

## ---------------------------------------------------------------------------
## 10. The human benchmark: conventional EFA on real responses
## ---------------------------------------------------------------------------
banner("10. Human-response EFA (validation target only)")

## ~1M respondents, Open-Source Psychometrics Project. 0 codes a skipped item.
human_raw <- read.csv(HUMAN_CSV)
stopifnot(identical(names(human_raw), big5$codes))
keep <- rowSums(human_raw < 1 | human_raw > 5 | is.na(human_raw)) == 0
human <- human_raw[keep, ]
n_human <- nrow(human)
cat("Respondents after cleaning:", n_human, "of", nrow(human_raw), "\n")

## reverse-score the 18 reverse-keyed items (the standard scored direction),
## then factor the correlations; the raw-response correlations are kept too
human_keyed <- human
rev_idx <- which(big5$scoring < 0)
human_keyed[, rev_idx] <- 6 - human_keyed[, rev_idx]
R_human       <- cor(human_keyed)
R_human_raw   <- cor(human)        # unkeyed, for the keying-free encodings

human_fa <- psych::fa(R_human, nfactors = 5, n.obs = n_human,
                      rotate = "oblimin", fm = "minres")
print(psych::fa.sort(unclass(human_fa$loadings)), digits = 2, cutoff = 0.3)
results$n_human <- n_human

## ---------------------------------------------------------------------------
## 11. Semantic-behavioral coherence: SFA vs. the human structure
## ---------------------------------------------------------------------------
banner("11. Semantic vs. human structure")

## (a) structural congruence of the main semantic fit vs. the human EFA.
##     (The disattenuated metric is computed separately in (b): it needs two
##     same-size correlation-like matrices, and its split-half reliability is
##     undefined for sign-flipped similarity matrices — see the note there.)
cong_human <- sfa_congruence(fit, target = human_fa,
                             metrics = c("tucker", "nmi", "ari", "frobenius"))
cong_human
results$congruence_human <- cong_human

## factor-by-factor Tucker phi after matching
phi <- psych::factor.congruence(fit$loadings, human_fa$loadings)
round(phi, 2)
results$tucker_matrix <- phi

## (b) item-pair-level agreement: semantic similarity vs. human inter-item r.
##     Two human matrices give the two keying conventions: R_human (reverse
##     items rescored — the standard scored direction) and R_human_raw
##     (verbatim responses). Each encoding is correlated with both.
##     The disattenuated correlation divides the observed r by the split-half
##     reliabilities of the two matrices; for the sign-flipped
##     atomic_reversed similarity the split-half reliability of the semantic
##     matrix is negative (the keying flip imposes a checkerboard pattern), so
##     the metric is undefined there and reported as NA.
lt <- lower.tri(sim_ar)
sims <- list(atomic = sim_at, atomic_reversed = sim_ar, squid = sim_sq,
             mean_centered_pearson = sim_mcp, nli = sim_nli)
pair_tab <- t(sapply(sims, function(s) c(
  r_keyed = cor(s[lt], R_human[lt]),
  r_raw   = cor(s[lt], R_human_raw[lt]))))
round(pair_tab, 3)
results$pair_level_r <- pair_tab

## (c) encoding comparison: fit each encoding (and the NLI matrix), score it
##     against theory (NMI/ARI) and against the human EFA (mean matched
##     Tucker phi, pair-level r, disattenuated r where defined)
fits <- list(
  atomic                = sfa(big5_df, embeddings = E8, encoding = "atomic",
                              nfactors = 5),
  atomic_reversed       = sfa(big5_df, embeddings = E8,
                              encoding = "atomic_reversed", nfactors = 5),
  squid                 = sfa(big5_df, embeddings = E8, encoding = "squid",
                              nfactors = 5),
  mean_centered_pearson = fit,
  nli                   = sfa(big5_df, similarity = sim_nli, nfactors = 5))

## mean Tucker phi after matching each semantic factor to its best human
## counterpart (|phi|: oblimin factors are sign-indeterminate)
matched_tucker <- function(phi) mean(apply(abs(phi), 2, max))

enc_table <- t(sapply(names(fits), function(nm) {
  f  <- fits[[nm]]
  ct <- sfa_congruence(f, target = big5$factors, metrics = c("nmi", "ari"))
  ch <- sfa_congruence(f, target = human_fa, metrics = "tucker")
  dis <- tryCatch(
    sfa_congruence(f, target = R_human, metrics = "disattenuated")$disattenuated,
    error = function(e) NA_real_, warning = function(w) NA_real_)
  c(NMI_theory = ct$nmi, ARI_theory = ct$ari,
    Tucker_human = matched_tucker(ch$tucker),
    r_keyed = pair_tab[nm, "r_keyed"], r_raw = pair_tab[nm, "r_raw"],
    Disattenuated_keyed = dis,
    KMO = f$kmo$total, TEFI = f$tefi, RMSR = f$rmsr, CAF = f$caf)
}))
round(enc_table, 3)
results$encoding_table <- enc_table

## (d) figure: item-pair semantic similarity vs. human correlation, for the
##     main (keying-free) encoding against the raw-response correlations —
##     like with like: neither side uses the scoring key
pdf("figures/fig_semantic_vs_human.pdf", width = 6.5, height = 6.5)
plot(R_human_raw[lt], sim_mcp[lt],
     xlab = sprintf("Human inter-item correlation, raw responses (N = %s)",
                    format(n_human, big.mark = ",")),
     ylab = "Semantic similarity (Qwen3-Embedding-8B, mean-centered Pearson)",
     pch = 19, cex = 0.55, col = grDevices::adjustcolor("steelblue4", 0.55))
abline(lm(sim_mcp[lt] ~ R_human_raw[lt]), lwd = 2, col = "firebrick")
legend("topleft", bty = "n",
       legend = sprintf("r = %.2f  (1,225 item pairs)",
                        cor(sim_mcp[lt], R_human_raw[lt])))
dev.off()

## (e) human-side retention, for context
human_pa <- psych::fa.parallel(R_human, n.obs = n_human, fa = "fa",
                               plot = FALSE)
cat("Human parallel analysis suggests", human_pa$nfact, "factors\n")
results$human_parallel_nfact <- human_pa$nfact

## ---------------------------------------------------------------------------
## 12. Save everything
## ---------------------------------------------------------------------------
banner("12. Save outputs")

saveRDS(results, "output/results.rds")
write.csv(round(enc_table, 4), "output/encoding_table.csv")
write.csv(round(phi, 3), "output/tucker_matrix.csv")
write.csv(data.frame(code = big5$codes, factor = big5$factors,
                     round(unclass(fit$loadings), 3)),
          "output/semantic_loadings.csv", row.names = FALSE)
write.csv(data.frame(code = big5$codes, factor = big5$factors,
                     round(unclass(human_fa$loadings), 3)),
          "output/human_loadings.csv", row.names = FALSE)
write.csv(round(results$anchor_centroid, 3), "output/anchor_centroid.csv")
cat("Wrote output/results.rds and CSV tables.\n")

sessionInfo()
