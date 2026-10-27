## ===========================================================================
## reproduce.R — full reproduction script for the semanticfa paper
##
## "semanticfa: An R Package for Response-Free Semantic Factor Analysis of
##  Psychometric Scales"
##  Yanitski & Westbury, University of Alberta
##
## This script regenerates every number, table, and figure in the paper's
## worked example from the inputs in data/. Run it from the reproduce/
## directory, e.g.:
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
##   Big5FM_cand_8B.npz   - Qwen3-Embedding-8B vectors for the three new
##                          candidate items vetted in Section 8, generated
##                          offline with the same settings as the item vectors
##                          (see data-raw/embed_candidates_8B.py).
##   Big5_items_0.6B.npz, Big5_items_4B.npz
##                        - the same 50 items embedded offline with the
##                          smaller models of the same family
##                          (Qwen/Qwen3-Embedding-0.6B, 50 x 1024; -4B,
##                          50 x 2560), for the model-size comparison
##                          (Section 9b).
##   DASS_items_0.6B.npz, DASS_items_4B.npz, DASS_items_8B.npz
##                        - the 42 items of the Depression Anxiety Stress
##                          Scales embedded offline with the three sizes and
##                          the same settings, for the model-size comparison
##                          with a second scale (Section 11c).
##   Big5FM_data.csv      - human responses to the same 50 items from the
##                          Open-Source Psychometrics Project "BIG5" dataset
##                          (https://openpsychometrics.org/_rawdata/), columns
##                          E1..O50, 5-point Likert, 0 = missed. Used ONLY as
##                          the validation target for the semantic structure;
##                          no response data enters the semantic analysis.
##
## Requirements: semanticfa (>= 0.5.1), psych, EGAnet, Rtsne, uwot, and a
## Python with numpy (for reading .npz) and sentence-transformers (only for
## the NLI matrix in Section 3; reticulate provisions this automatically on
## first use, and the cross-encoder is downloaded on first run).
## ===========================================================================

library(semanticfa)
set.seed(42)

AUX_NPZ   <- "data/Big5FM_aux_8B.npz"
CAND_NPZ  <- "data/Big5FM_cand_8B.npz"
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
## instruction prefix). The scoring key follows the benchmark administration
## (Goldberg's Emotional Stability domain relabeled Neuroticism and keyed in
## that direction; 18 reverse-keyed items in total).
E8 <- big5$embeddings    # 50 items x 4096 embedding dimensions

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

sim_nli <- sfa_nli_matrix(big5$items)   # cross-encoder/nli-deberta-v3-base
dimnames(sim_nli) <- list(big5$codes, big5$codes)

## the canonical example: an extraversion item vs. its reverse-keyed sibling
round(sim_nli[c("E1", "E2", "E5"), c("E1", "E2", "E5")], 2)
round(sim_ar[c("E1", "E2", "E5"), c("E1", "E2", "E5")], 2)
results$nli_example <- round(sim_nli[c("E1", "E2", "E5"),
                                     c("E1", "E2", "E5")], 2)

## ---------------------------------------------------------------------------
## 5. How many factors? sfa_parallel / sfa_nfactors / sfa_dimselect
## ---------------------------------------------------------------------------
banner("5. Factor retention")

pa <- sfa_parallel(sim_mcp, E8)
pa
results$parallel <- pa

nf <- sfa_nfactors(sim_mcp, embeddings = E8,
                   methods = c("parallel", "kaiser", "TEFI", "EGA", "EKC"))
nf
results$nfactors <- nf

## The empirical Kaiser criterion on its own, for its reference series
ekc <- sfa_ekc(sim_mcp, E8)
ekc
results$ekc <- ekc

## Velicer's MAP, available but not a default consensus vote: on embedding
## similarity matrices it tracks reliable minor structure well past the
## interpretable factor count (see ?sfa_map)
mp <- sfa_map(sim_mcp)
mp
results$map <- mp

ds <- sfa_dimselect(E8, factors = big5$factors,
                    encoding = "mean_centered_pearson")
ds
## the same EGA criterion at the full embedding, for comparison with the
## selected depth
ds$trajectory[ds$trajectory$depth == max(ds$trajectory$depth), ]
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

## which factor each item loads on most strongly (largest absolute loading),
## cross-tabulated with its theoretical domain
L_fit <- unclass(fit$loadings)
assign_fit <- table(factor = colnames(L_fit)[apply(abs(L_fit), 1, which.max)],
                    domain = big5$factors)
assign_fit
results$assignment <- unclass(assign_fit)

## Monte-Carlo null calibration of the diagnostics (Pokropek-style)
fit_cal <- sfa(big5_df, embeddings = E8, encoding = "mean_centered_pearson",
               nfactors = 5, calibrate = TRUE, calibrate_iter = 100)
fit_cal$calibration
cat("Calibration replicates retained:", length(fit_cal$calibration$rmsr),
    "of 100\n")
summary(warnings())
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

## all items whose leave-one-out own-construct similarity is exceeded by
## another construct (the printout above lists only the lowest eight), and
## how many items are most similar to their OWN construct's name
own_col <- match(big5$factors, colnames(anc$centroid))
own_c   <- anc$centroid[cbind(seq_len(50), own_col)]
rival_c <- sapply(seq_len(50), function(i) max(anc$centroid[i, -own_col[i]]))
closer_other <- data.frame(item = big5$codes, own = round(own_c, 3),
                           rival = round(rival_c, 3))[rival_c > own_c, ]
closer_other
own_name_top <- sum(apply(anc_lab$label, 1, which.max) ==
                    match(big5$factors, colnames(anc_lab$label)))
cat("Items most similar to their own construct's name:", own_name_top,
    "of 50\n")
results$anchor_closer_other <- closer_other
results$anchor_own_name_top <- own_name_top

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

## chance reference for NMI and ARI: the same comparison with the domain
## labels randomly permuted across items (1,000 permutations)
perm_na <- withr::with_seed(42, t(replicate(1000, {
  cp <- sfa_congruence(fit, target = sample(big5$factors),
                       metrics = c("nmi", "ari"))
  c(nmi = cp$nmi, ari = cp$ari)
})))
round(apply(perm_na, 2, quantile, probs = c(.50, .95, 1)), 3)
results$congruence_theory_null <- perm_na

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
jj$flags
results$jinglejangle <- jj

## ---------------------------------------------------------------------------
## 8. Refinement before any data collection
## ---------------------------------------------------------------------------
banner("8. Refinement: redundancy, short form, candidate items")

red_uva <- sfa_redundancy(fit)                     # UVA / wTO (EGAnet)
red_uva
red_uva$pairs                                      # every flagged pair
red_cos <- sfa_redundancy(fit, method = "cosine", threshold = 0.85)
red_cos
results$redundancy_uva <- red_uva
results$redundancy_cosine <- red_cos

short <- sfa_simplify(fit, target_n = 5, method = "anchor")   # 50 -> 25 items
short
short$keep
L_sf <- unclass(short$reduced_fit$loadings)
table(factor = colnames(L_sf)[apply(abs(L_sf), 1, which.max)],
      domain = short$reduced_fit$item_data$factor)
results$shortform <- short

## Vetting brand-new candidate items runs in the SAME 8B space as every
## other analysis: the three candidates were embedded offline with
## Qwen/Qwen3-Embedding-8B (identical settings to the item vectors; see
## data-raw/embed_candidates_8B.py) and ship in the reproduction archive,
## and the five construct-name vectors are the auxiliary 8B vectors loaded
## in Section 1. Both sets are written to the package's embedding cache,
## in a temporary directory rather than the user's own cache, so that the
## sfa_item_fit() call below, written as a user would write it, reads them
## from the cache instead of running the model (no live model, no Python).
cand8 <- sfa_load_npz(CAND_NPZ)
constructs8 <- unique(big5$factors)
Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "sfa_cache"))
cache8 <- tools::R_user_dir("semanticfa", "cache")
dir.create(cache8, recursive = TRUE, showWarnings = FALSE)
cache_file8 <- function(texts)
  file.path(cache8, paste0(semanticfa:::.cache_key(texts, "Qwen/Qwen3-Embedding-8B",
                                                   "sbert"), ".rds"))
saveRDS(cand8$embeddings, cache_file8(cand8$items))
saveRDS(aux$embeddings[match(tolower(constructs8), tolower(aux$codes)), , drop = FALSE],
        cache_file8(constructs8))
## sfa_item_fit() uses only the item embeddings, their theoretical labels,
## and the construct-name vectors, so this reference fit is equivalent to
## the main one; the scoring column is left out because the keying-free
## mean_centered_pearson encoding does not use it.
fit_ns <- sfa(big5_df[, c("code", "item", "factor")],
              embeddings = E8,
              encoding = "mean_centered_pearson", nfactors = 5)
## two planted flaws (a cross-loader and a near-duplicate) plus one
## genuine addition expected to pass
cand <- c("I make friends easily.",
          "I am the life of every party.",
          "I finish what I start.")
ifit <- sfa_item_fit(fit_ns, cand, model = "Qwen/Qwen3-Embedding-8B",
                     redundancy_cutoff = 0.85)
ifit
## check: the call read exactly the archived vectors (the cache keys match
## the candidates and the construct names it embeds), and passing the same
## vectors through a custom embed function gives the same report
stopifnot(identical(cand8$items, cand),
          identical(unique(fit_ns$item_data$factor), constructs8),
          file.exists(cache_file8(cand)), file.exists(cache_file8(constructs8)))
ifit_chk <- sfa_item_fit(fit_ns, cand, redundancy_cutoff = 0.85,
  embed = function(texts) rbind(cand8$embeddings, aux$embeddings)[
    match(tolower(texts), tolower(c(cand8$items, aux$codes))), , drop = FALSE])
stopifnot(identical(capture.output(print(ifit)), capture.output(print(ifit_chk))))
results$item_fit <- ifit

## ---------------------------------------------------------------------------
## 9. Visualization (figures used in the paper)
## ---------------------------------------------------------------------------
banner("9. Figures")

## the all-positive similarity heatmaps use a sequential scale over the
## observed range (a -1 to 1 diverging scale leaves every cell pale)
seq_gr <- grDevices::colorRampPalette(c("white", "#2196F3", "#0D47A1"))
## group = TRUE is passed explicitly: otherwise R partially matches gr = to
## sfa_corplot()'s group argument, which turns grouping off and drops the palette
pdf("figures/fig_corplot.pdf", width = 7.5, height = 7.5)
sfa_corplot(sim_at, factors = big5$factors,
            order = c("O", "C", "E", "A", "N"),
            main = "Cosine similarity (atomic)", zlim = c(0.3, 1),
            group = TRUE, gr = seq_gr, n.legend = 7)
dev.off()

## the signed NLI matrix makes the block structure (and the negative
## relations of reverse-keyed items) far more visible than raw cosine
pdf("figures/fig_corplot_nli.pdf", width = 7.5, height = 7.5)
sfa_corplot(sim_nli, factors = big5$factors,
            order = c("O", "C", "E", "A", "N"),
            main = "Signed NLI similarity", cex.axis = 0.6)
dev.off()

## the four item maps as a user draws them; their coordinates feed the
## neighbor shares below (drawn to a null device: the paper's figure is
## drawn next, from the same maps)
pdf(NULL)
maps <- list()
for (m in c("tsne", "umap", "pca", "mds")) {
  maps[[m]] <- sfa_itemplot(fit, method = m, seed = 42)$Y
}
dev.off()

## Figure 5: the same maps redrawn for the paper.
## larger panels and smaller labels limit overprinting; xpd = NA keeps labels
## at the panel edges from being clipped; one shared legend sits below the
## panels (sfa_itemplot colors domains in alphabetical order with "Dark 3").
## The linear maps (PCA, MDS) crowd most items into one region, so their
## points are colored by domain without item labels.
## In the nonlinear maps the item codes are placed by place_labels(), a small
## helper that tries eight positions around each point at growing distances
## and keeps the first that touches no other label and no other point; a
## thin line joins a label that had to move away to its point.
place_labels <- function(xy, labels, col, cex = 0.6) {
  n <- nrow(xy)
  w <- graphics::strwidth(labels, cex = cex) * 1.08
  h <- graphics::strheight(labels, cex = cex) * 1.4
  pr <- c(graphics::strwidth("M", cex = 0.75), graphics::strheight("M", cex = 0.75)) * 0.55
  usr <- graphics::par("usr")
  box <- matrix(NA_real_, n, 4)
  pos <- matrix(NA_real_, n, 2)
  moved <- logical(n)
  placed <- integer(0)
  clash <- function(b, k) {
    for (j in placed)
      if (b[1] < box[j, 2] && b[2] > box[j, 1] && b[3] < box[j, 4] && b[4] > box[j, 3])
        return(TRUE)
    for (j in seq_len(n)[-k])
      if (xy[j, 1] + pr[1] > b[1] && xy[j, 1] - pr[1] < b[2] &&
          xy[j, 2] + pr[2] > b[3] && xy[j, 2] - pr[2] < b[4])
        return(TRUE)
    FALSE
  }
  dirs <- rbind(c(0, 1), c(1, 0), c(-1, 0), c(0, -1),
                c(1, 1), c(-1, 1), c(1, -1), c(-1, -1))
  crowd <- vapply(seq_len(n), function(i)
    sum(abs(xy[, 1] - xy[i, 1]) < 3 * max(w) & abs(xy[, 2] - xy[i, 2]) < 3 * max(h)),
    numeric(1))
  for (k in order(-crowd)) {
    found <- FALSE
    for (r in c(1, 1.8, 2.7, 3.8, 5.2, 7)) {
      for (d in seq_len(nrow(dirs))) {
        cx <- xy[k, 1] + dirs[d, 1] * (w[k] / 2 + pr[1]) * r
        cy <- xy[k, 2] + dirs[d, 2] * (h[k] / 2 + pr[2]) * r
        b <- c(cx - w[k] / 2, cx + w[k] / 2, cy - h[k] / 2, cy + h[k] / 2)
        if (b[1] < usr[1] || b[2] > usr[2] || b[3] < usr[3] || b[4] > usr[4]) next
        if (!clash(b, k)) {
          box[k, ] <- b
          pos[k, ] <- c(cx, cy)
          moved[k] <- r > 1
          found <- TRUE
          break
        }
      }
      if (found) break
    }
    if (!found) {
      pos[k, ] <- c(xy[k, 1], xy[k, 2] + h[k] / 2 + pr[2])
      box[k, ] <- c(pos[k, 1] - w[k] / 2, pos[k, 1] + w[k] / 2,
                    pos[k, 2] - h[k] / 2, pos[k, 2] + h[k] / 2)
    }
    placed <- c(placed, k)
  }
  for (k in which(moved)) {
    ex <- min(max(xy[k, 1], box[k, 1]), box[k, 2])
    ey <- min(max(xy[k, 2], box[k, 3]), box[k, 4])
    graphics::segments(xy[k, 1], xy[k, 2], ex, ey, col = "grey60", lwd = 0.6)
  }
  graphics::text(pos, labels = labels, cex = cex, col = col)
  invisible(pos)
}
dom_cols <- grDevices::hcl.colors(5, "Dark 3")[as.integer(factor(big5$factors))]
pdf("figures/fig_itemmap.pdf", width = 9.5, height = 10)
op <- par(mfrow = c(2, 2), mar = c(4.1, 4.1, 2.5, 1), mgp = c(2.4, 0.8, 0),
          oma = c(2.2, 0, 0, 0), xpd = NA)
for (m in c("tsne", "umap", "pca", "mds")) {
  mp <- sfa_itemplot(fit, method = m, seed = 42, legend = FALSE,
                     cex = 0.75, labels = rep("", 50))
  stopifnot(isTRUE(all.equal(unname(mp$Y), unname(maps[[m]]))))
  if (m %in% c("tsne", "umap")) place_labels(mp$Y, big5$codes, dom_cols)
}
dom_lv <- levels(factor(big5$factors))
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot.new()
legend("bottom", legend = dom_lv, horiz = TRUE, bty = "n", pch = 19,
       col = grDevices::hcl.colors(length(dom_lv), "Dark 3"), cex = 0.9,
       text.width = max(graphics::strwidth(dom_lv, cex = 0.9)))
par(op)
dev.off()
## share of variance on the two principal components the PCA map shows
## (the maps use the encoded item embeddings: centered, unit length)
pc_var <- summary(stats::prcomp(fit$transformed_embeddings))$importance[2, 1:2]
round(pc_var, 3)
results$itemmap_pc_variance <- pc_var
## separation of the domains in each map: the share of each item's five
## nearest neighbors in the two-dimensional map that come from its own
## domain (chance is 9/49, about .18, because each domain has nine other
## items among the 49)
map_purity <- sapply(maps, function(Y) {
  d <- as.matrix(stats::dist(Y))
  diag(d) <- Inf
  own <- sapply(seq_len(50), function(i)
    mean(big5$factors[order(d[i, ])[1:5]] == big5$factors[i]))
  c(tapply(own, big5$factors, mean), overall = mean(own))
})
round(map_purity, 2)
results$itemmap_purity <- map_purity
results$itemmap_coords <- maps

## scree plot of the similarity-matrix eigenvalues (the values sfa_nfactors
## prints) against the parallel-analysis threshold, the 95th percentile of
## the eigenvalues of 100 random-embedding similarity matrices; log scale so
## the first eigenvalue and the threshold region are both readable
ev_mcp <- eigen(sim_mcp, symmetric = TRUE, only.values = TRUE)$values
pdf("figures/fig_scree.pdf", width = 7, height = 5)
op <- par(mar = c(4.1, 4.4, 1, 1))
plot(seq_along(ev_mcp), ev_mcp, log = "y", type = "b", pch = 19, cex = 0.6,
     xlab = "Eigenvalue number (50 items)", ylab = "Eigenvalue (log scale)")
lines(seq_along(pa$percentiles), pa$percentiles, lty = 2, lwd = 2,
      col = "firebrick")
abline(h = 1, lty = 3, col = "grey40")
legend("topright", bty = "n", lty = c(1, 2, 3), pch = c(19, NA, NA),
       col = c("black", "firebrick", "grey40"),
       legend = c("Observed eigenvalues",
                  "Parallel-analysis threshold (95th percentile)",
                  "Eigenvalue of 1"))
par(op)
dev.off()

## Loadings heatmap with items in the conventional O-C-E-A-N theoretical
## order (plot(fit, type = "loadings") draws the same map sorted by
## dominant factor; same palette, scale, and axes as the package plot).
dom_ord <- order(match(big5$factors,
                       c("Openness", "Conscientiousness", "Extraversion",
                         "Agreeableness", "Neuroticism")))
ld <- unclass(fit$loadings)[dom_ord, ]
ld <- ld[, order(-colSums(ld^2))]        # factors by explained variance
## the color scale covers every loading (one oblique pattern loading,
## N16 on the Neuroticism factor, slightly exceeds 1); a key is drawn at the
## right, and lines separate the five domains
zl <- max(1, ceiling(max(abs(ld)) * 20) / 20)
ld_cols <- grDevices::hcl.colors(50, "RdBu")
## below each factor label: the domain the factor matches best (the domain
## on whose items its mean absolute loading is largest, as in Table 1)
dom_of <- setNames(fit$omega$matched_theoretical, fit$omega$factor)
pdf("figures/fig_loadings.pdf", width = 8, height = 8)
graphics::layout(matrix(1:2, 1), widths = c(6.3, 1.5))
op <- par(mar = c(5.6, 4.1, 2.5, 1))
graphics::image(seq_len(ncol(ld)), seq_len(nrow(ld)), t(ld[nrow(ld):1, ]),
                col = ld_cols, zlim = c(-zl, zl),
                xlab = "", ylab = "Item", axes = FALSE,
                main = "Factor loadings")
graphics::axis(1, at = seq_len(ncol(ld)), labels = colnames(ld))
graphics::axis(1, at = seq_len(ncol(ld)), labels = dom_of[colnames(ld)],
               line = 1.1, tick = FALSE, cex.axis = 0.75)
graphics::mtext("Factor (matched domain)", side = 1, line = 4.3)
graphics::axis(2, at = seq_len(nrow(ld)), labels = rev(rownames(ld)),
               las = 2, cex.axis = 0.6)
graphics::abline(h = 50 - cumsum(table(factor(big5$factors[dom_ord],
                   levels = unique(big5$factors[dom_ord]))))[-5] + 0.5,
                 col = "grey30")
par(mar = c(5.6, 0.5, 2.5, 3))
key_z <- seq(-zl, zl, length.out = 50)
graphics::image(1, key_z, matrix(key_z, 1), col = ld_cols, axes = FALSE,
                xlab = "", ylab = "", main = "Loading", cex.main = 0.9)
graphics::axis(4, las = 1)
par(op)
graphics::layout(1)
dev.off()

## ---------------------------------------------------------------------------
## 9b. Model size and structural clarity: 0.6B vs 4B vs 8B
## ---------------------------------------------------------------------------
banner("9b. Model-size comparison")

## The same 50 items embedded with three sizes of the same model family
## (Qwen3-Embedding-0.6B / 4B / 8B), all under the main keying-free encoding.
## The 0.6B and 4B vectors come from the reproduction archive; the 8B vectors
## are the bundled ones used throughout.
emb_by_model <- list(
  `0.6B` = sfa_load_npz(REF06_NPZ)$embeddings,
  `4B`   = sfa_load_npz("data/Big5_items_4B.npz")$embeddings,
  `8B`   = E8)

## one shared color range for the three heatmaps: from the lowest off-
## diagonal similarity across the three sizes (rounded down) to 1
ms_floor <- floor(10 * min(sapply(emb_by_model, function(e) {
  sm <- sfa_similarity(e, "mean_centered_pearson")
  min(sm[lower.tri(sm)])
}))) / 10
cat("Model-size heatmaps share the color range", ms_floor, "to 1\n")

## the panels are printed small, so the axes mark the five domains (O, C, E,
## A, N, at the middle of each block) instead of the 50 item codes
dom_axis <- rep("", 50)
for (d in unique(big5$factors)) {
  dom_axis[which(big5$factors == d)[5]] <- substr(d, 1, 1)
}

model_nf <- list()
model_sep <- list()
for (m in names(emb_by_model)) {
  sim_m <- sfa_similarity(emb_by_model[[m]], "mean_centered_pearson",
                          factors = big5$factors, codes = big5$codes)
  pdf(sprintf("figures/fig_corplot_%s.pdf", sub("0.6B", "06B", m)),
      width = 7.5, height = 7.5)
  sfa_corplot(sim_m, order = c("O", "C", "E", "A", "N"),
              main = sprintf("Qwen3-Embedding-%s", m), labels = dom_axis,
              cex.axis = 1.6, xlas = 1, zlim = c(ms_floor, 1),
              n.legend = round(10 * (1 - ms_floor)), group = TRUE, gr = seq_gr)
  dev.off()
  ## within- versus between-domain separation of the plotted similarities:
  ## group means plus the point-biserial correlation between sharing a
  ## domain and the pair's similarity, over all 1,225 item pairs
  same_dom <- outer(big5$factors, big5$factors, "==")
  ut <- upper.tri(sim_m)
  model_sep[[m]] <- c(within  = mean(sim_m[same_dom & ut]),
                      between = mean(sim_m[!same_dom & ut]),
                      rpb     = cor(sim_m[ut], as.numeric(same_dom[ut])))
  cat(sprintf("%s within = %.3f | between = %.3f | point-biserial = %.3f\n",
              m, model_sep[[m]]["within"], model_sep[[m]]["between"],
              model_sep[[m]]["rpb"]))
  nf_m <- sfa_nfactors(sim_m, embeddings = emb_by_model[[m]],
                       methods = c("parallel", "kaiser", "TEFI", "EGA", "EKC"))
  cat(sprintf("\nQwen3-Embedding-%s (%d dims):\n", m, ncol(emb_by_model[[m]])))
  print(nf_m)
  model_nf[[m]] <- nf_m
}
results$model_comparison <- lapply(model_nf, function(x)
  list(methods = x$methods, consensus = x$consensus))
results$modelsize_separation <- do.call(rbind, model_sep)
## item-level bootstrap intervals for the point-biserial separation at each
## size (items resampled with replacement, self-pairs dropped, as in the
## pair-level bootstrap of Section 11), and the within- minus between-domain
## gap of each domain on its own
sep_boot <- sapply(names(emb_by_model), function(m) {
  sim_m <- sfa_similarity(emb_by_model[[m]], "mean_centered_pearson")
  withr::with_seed(42, replicate(1000, {
    s <- sample(50, replace = TRUE)
    keep_p <- upper.tri(diag(50)) & outer(s, s, "!=")
    same_b <- outer(big5$factors[s], big5$factors[s], "==")[keep_p]
    cor(sim_m[s, s][keep_p], as.numeric(same_b))
  }))
})
sep_ci <- apply(sep_boot, 2, quantile, c(.025, .975))
round(sep_ci, 3)
results$modelsize_separation_ci <- sep_ci
sep_domain <- sapply(names(emb_by_model), function(m) {
  sim_m <- sfa_similarity(emb_by_model[[m]], "mean_centered_pearson")
  ut <- upper.tri(sim_m)
  sapply(sort(unique(big5$factors)), function(d) {
    in_d <- big5$factors == d
    mean(sim_m[outer(in_d, in_d, "&") & ut]) - mean(sim_m[outer(in_d, !in_d, "&")])
  })
})
round(sep_domain, 3)
results$modelsize_domain_gap <- sep_domain

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
summary(warnings())
print(psych::fa.sort(unclass(human_fa$loadings)), digits = 2, cutoff = 0.3)
results$n_human <- n_human

## ---------------------------------------------------------------------------
## 10b. Comparison-data misfit profiles: crisp versus graded structure
## ---------------------------------------------------------------------------
banner("10b. Comparison-data profiles (sfa_cd)")

## For each candidate factor count k, the comparison-data profile records how
## well bootstrap samples from a k-factor comparison population reproduce the
## observed eigenvalue spectrum (Ruscio & Roche 2012, adapted; see ?sfa_cd).
## Response data with a real factor boundary show a sharp elbow at that
## boundary; embedding similarity matrices decline smoothly instead. The
## sequential significance rule is run at its conventional alpha = .30 here
## deliberately, to document that it saturates at the cap on embedding
## matrices (each added factor keeps reproducing the anisotropic spectrum
## reliably better) and therefore carries no retention information for them.

cd_emb <- list()
for (m in names(emb_by_model)) {
  cd_m <- sfa_cd(emb_by_model[[m]], n_factors_max = 10, alpha = .30)
  cat(sprintf("\nQwen3-Embedding-%s comparison-data profile:\n", m))
  print(cd_m)
  cd_emb[[m]] <- cd_m
}

## The human anchor: the same profile on real responses (n = 2,500 keyed
## subsample), plus the sequential rule across subsample sizes. Within the
## sample sizes where the comparison-data method was validated the rule
## terminates; as n grows the test has power against any nonzero improvement
## and the verdict inflates, mirroring the full-sample parallel analysis of
## the human benchmark reported in Section 11.
set.seed(42)
cd_human <- sfa_cd(as.matrix(human_keyed[sample(n_human, 2500), ]),
                   input = "data", n_factors_max = 10, alpha = .30)
cat("\nHuman responses (n = 2500) comparison-data profile:\n")
print(cd_human)

cd_cross <- vapply(c(250, 500, 1000), function(n_sub) {
  set.seed(42)
  sfa_cd(as.matrix(human_keyed[sample(n_human, n_sub), ]), input = "data",
         n_factors_max = 10, alpha = .30)$n_factors
}, integer(1))
names(cd_cross) <- c("n250", "n500", "n1000")
cd_cross <- c(cd_cross, n2500 = cd_human$n_factors)
cat("\nSequential-rule verdicts on human subsamples (alpha = .30):\n")
print(cd_cross)

results$cd <- list(
  embeddings = lapply(cd_emb, function(x)
    x[c("median_rmsr", "profile", "improvement", "n_factors", "n")]),
  human = cd_human[c("median_rmsr", "profile", "improvement",
                     "n_factors", "n")],
  crossover = cd_cross)

## profile table + two-panel figure
cd_tab <- do.call(rbind, c(
  lapply(names(cd_emb), function(m) data.frame(
    dataset = m, k = seq_along(cd_emb[[m]]$median_rmsr),
    median_rmsr = cd_emb[[m]]$median_rmsr,
    normalized = cd_emb[[m]]$profile,
    improvement = c(NA, cd_emb[[m]]$improvement))),
  list(data.frame(
    dataset = "human", k = seq_along(cd_human$median_rmsr),
    median_rmsr = cd_human$median_rmsr,
    normalized = cd_human$profile,
    improvement = c(NA, cd_human$improvement)))))
write.csv(cd_tab, "output/cd_profiles.csv", row.names = FALSE)

pdf("figures/fig_cd_profiles.pdf", width = 9, height = 4.2)
par(mfrow = c(1, 2), mar = c(4.2, 4.4, 2.4, 1))
cd_cols <- c(`0.6B` = "#8ecae6", `4B` = "#219ebc", `8B` = "#023047",
             human = "#d62828")
cd_lty <- c(`0.6B` = 3, `4B` = 2, `8B` = 1, human = 1)
cd_pch <- c(`0.6B` = 1, `4B` = 2, `8B` = 19, human = 15)
## the profile is the eigenvalue RMSR of the comparison-data method (root mean
## square difference between eigenvalue sets), not the residual RMSR of sfa()
plot(NULL, xlim = c(1, 10), ylim = c(0, 1), xlab = "Factors (k)",
     ylab = "Eigenvalue RMSR relative to k = 1", main = "Misfit profile")
for (d in names(cd_cols)) {
  y <- if (d == "human") cd_human$profile else cd_emb[[d]]$profile
  lines(seq_along(y), y, type = "b", pch = cd_pch[d], lty = cd_lty[d],
        lwd = 2, col = cd_cols[d])
}
legend("topright", legend = c("Qwen3-Embedding-0.6B", "Qwen3-Embedding-4B",
                              "Qwen3-Embedding-8B", "Human (n = 2,500)"),
       col = cd_cols, lwd = 2, lty = cd_lty, pch = cd_pch, bty = "n")
plot(NULL, xlim = c(2, 10), ylim = c(0, 62), xlab = "Factors (k)",
     ylab = "Improvement over k - 1 (%)", main = "Relative improvement")
for (d in names(cd_cols)) {
  y <- if (d == "human") cd_human$improvement else cd_emb[[d]]$improvement
  lines(1 + seq_along(y), 100 * y, type = "b", pch = cd_pch[d],
        lty = cd_lty[d], lwd = 2, col = cd_cols[d])
}
dev.off()

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

## the NLI matrix before and after the positive-definite repair: how many
## eigenvalues were negative, and how closely the repaired item-pair values
## track the original ones (the pair-level r below uses the original matrix)
ev_nli <- eigen(sim_nli, symmetric = TRUE, only.values = TRUE)$values
nli_repair <- c(negative_eigenvalues = sum(ev_nli < 0),
                min_eigenvalue       = min(ev_nli),
                r_original_repaired  = cor(sim_nli[lt],
                                           fits$nli$sim_matrix[lt]))
round(nli_repair, 3)
results$nli_repair <- nli_repair

## mean Tucker phi after matching each semantic factor (row) to its best
## human counterpart (|phi|: oblimin factors are sign-indeterminate)
matched_tucker <- function(phi) mean(apply(abs(phi), 1, max))

enc_table <- t(sapply(names(fits), function(nm) {
  f  <- fits[[nm]]
  ct <- sfa_congruence(f, target = big5$factors, metrics = c("nmi", "ari"))
  ch <- sfa_congruence(f, target = human_fa, metrics = "tucker")
  dis <- tryCatch(
    sfa_congruence(f, target = R_human, metrics = "disattenuated")$disattenuated,
    error = function(e) NA_real_,
    warning = function(w) { cat(nm, ":", conditionMessage(w), "\n"); NA_real_ })
  c(NMI_theory = ct$nmi, ARI_theory = ct$ari,
    Tucker_human = matched_tucker(ch$tucker),
    r_keyed = pair_tab[nm, "r_keyed"], r_raw = pair_tab[nm, "r_raw"],
    Disattenuated_keyed = dis,
    KMO = f$kmo$total, TEFI = f$tefi, RMSR = f$rmsr, CAF = f$caf)
}))
round(enc_table, 3)
results$encoding_table <- enc_table

## (d) the shape of the item-pair relation, and its figure. Semantic
##     similarity rises steeply toward the positive end of the human-correlation
##     axis and only slightly toward the negative end, where most pairs join an
##     item and a reverse-keyed item of the same domain. A quadratic fit
##     describes the curvature; the linear r is kept as the headline because it
##     is the conventional summary. Main (keying-free) encoding against the
##     raw-response correlations, like with like: neither side uses the
##     scoring key.
x_h <- R_human_raw[lt]
y_s <- sim_mcp[lt]
fit_lin  <- lm(y_s ~ x_h)
fit_quad <- lm(y_s ~ x_h + I(x_h^2))
opp_same <- (outer(big5$scoring, big5$scoring) < 0 &
             outer(big5$factors, big5$factors, "=="))[lt]
pair_shape <- c(
  r_linear           = cor(y_s, x_h),
  r_absolute         = cor(y_s, abs(x_h)),
  r2_linear          = summary(fit_lin)$r.squared,
  r2_quadratic       = summary(fit_quad)$r.squared,
  n_below_m20        = sum(x_h < -.20),
  n_below_m20_opp    = sum(x_h < -.20 & opp_same),
  mean_sim_below_m20 = mean(y_s[x_h < -.20]),
  mean_sim_near_zero = mean(y_s[abs(x_h) < .10]),
  mean_sim_above_20  = mean(y_s[x_h > .20]))
round(pair_shape, 3)
results$pair_level_shape <- pair_shape

pdf("figures/fig_semantic_vs_human.pdf", width = 6.5, height = 6.5)
plot(x_h, y_s,
     xlab = sprintf("Human inter-item correlation, raw responses (N = %s)",
                    format(n_human, big.mark = ",")),
     ylab = "Semantic similarity (Qwen3-Embedding-8B, mean-centered Pearson)",
     pch = 19, cex = 0.55, col = grDevices::adjustcolor("steelblue4", 0.55))
abline(fit_lin, lwd = 2, col = "firebrick")
xs <- seq(min(x_h), max(x_h), length.out = 200)
lines(xs, predict(fit_quad, data.frame(x_h = xs)), lwd = 2, lty = 2,
      col = "black")
legend("topleft", bty = "n", lwd = 2, lty = c(1, 2),
       col = c("firebrick", "black"),
       legend = c(sprintf("linear: r = %.2f, R2 = %.2f",
                          pair_shape["r_linear"], pair_shape["r2_linear"]),
                  sprintf("quadratic: R2 = %.2f", pair_shape["r2_quadratic"])),
       title = "1,225 item pairs", title.adj = 0)
dev.off()

## (e) human-side retention, for context
human_pa <- psych::fa.parallel(R_human, n.obs = n_human, fa = "fa",
                               plot = FALSE)
cat("Human parallel analysis suggests", human_pa$nfact, "factors\n")
## the comparison is between common-factor eigenvalues: observed versus the
## mean eigenvalues of random data of the same size
round(rbind(observed = human_pa$fa.values[1:12],
            random   = human_pa$fa.sim[1:12]), 3)
results$human_parallel_nfact <- human_pa$nfact

## ---------------------------------------------------------------------------
## 11b. Supporting checks for the reported comparisons
## ---------------------------------------------------------------------------
## Each check below runs after every reported analysis and under its own seed
## (withr::with_seed), so it changes none of the numbers printed above.
banner("11b. Supporting checks")

## (a) atomic versus mean-centered Pearson, entry by entry
enc_equiv <- c(max_abs_diff = max(abs(sim_at[lt] - sim_mcp[lt])),
               r = cor(sim_at[lt], sim_mcp[lt]))
signif(enc_equiv, 4)
results$encoding_equivalence <- enc_equiv

## (b) parallel-analysis thresholds (95th percentile of 100 random-embedding
##     eigenvalues) and observed eigenvalues, first eight, for each model size
pa_by_model <- lapply(names(emb_by_model), function(m) {
  sm <- sfa_similarity(emb_by_model[[m]], "mean_centered_pearson")
  pm <- sfa_parallel(sm, emb_by_model[[m]])
  rbind(observed  = pm$observed[1:8], threshold = pm$percentiles[1:8])
})
names(pa_by_model) <- names(emb_by_model)
for (m in names(pa_by_model)) {
  cat(sprintf("\nQwen3-Embedding-%s (%d dims):\n", m, ncol(emb_by_model[[m]])))
  print(round(pa_by_model[[m]], 3))
}
results$pa_thresholds <- pa_by_model

## (c) the calibration replicates again, with the same seed and draws as
##     sfa(calibrate = TRUE), recording which oblimin rotations converged; the
##     RMSR values must reproduce the calibration exactly
cal_rep <- withr::with_seed(42, t(vapply(seq_len(100), function(i) {
  rand <- matrix(stats::rnorm(50 * ncol(E8)), nrow = 50)
  rand <- rand / sqrt(rowSums(rand^2))
  sm <- tcrossprod(rand)
  diag(sm) <- 1
  converged <- TRUE
  n_warn <- 0
  fa_r <- withCallingHandlers(
    psych::fa(sm, nfactors = 5, rotate = "oblimin", fm = "minres",
              n.obs = NA, warnings = FALSE),
    warning = function(w) {
      if (grepl("Convergence not obtained", conditionMessage(w))) {
        converged <<- FALSE
        n_warn <<- n_warn + 1
      }
      invokeRestart("muffleWarning")
    })
  L <- unclass(fa_r$loadings)
  Phi <- if (is.null(fa_r$Phi)) diag(ncol(L)) else fa_r$Phi
  res_r <- sm - (L %*% Phi %*% t(L) + diag(fa_r$uniquenesses))
  c(rmsr = sqrt(mean(res_r[upper.tri(res_r)]^2)), converged = converged,
    n_warnings = n_warn)
}, numeric(3))))
cal_match <- isTRUE(all.equal(unname(cal_rep[, "rmsr"]),
                              unname(fit_cal$calibration$rmsr), tolerance = 1e-8))
cat("Replicated RMSR values match the calibration:", cal_match, "\n")
results$calibration_replication_matches <- cal_match
cal_tab <- data.frame(replicate = seq_len(100),
                      rmsr = fit_cal$calibration$rmsr,
                      caf  = fit_cal$calibration$caf,
                      tefi = fit_cal$calibration$tefi,
                      rotation_converged = as.logical(cal_rep[, "converged"]),
                      rotation_warnings = cal_rep[, "n_warnings"])
cal_summary <- rbind(
  mean     = sapply(cal_tab[, c("rmsr", "caf", "tefi")], mean),
  sd       = sapply(cal_tab[, c("rmsr", "caf", "tefi")], sd),
  min_converged = sapply(cal_tab[cal_tab$rotation_converged, c("rmsr", "caf", "tefi")], min),
  max_converged = sapply(cal_tab[cal_tab$rotation_converged, c("rmsr", "caf", "tefi")], max))
cat("Calibration fits whose rotation converged:",
    sum(cal_tab$rotation_converged), "of 100\n")
## psych::fa can repeat a failed oblimin rotation, so one fit can issue the
## convergence warning several times
cat("Rotation warnings by replicate:\n")
print(cal_tab[cal_tab$rotation_warnings > 0, c("replicate", "rotation_warnings")])
round(cal_summary, 4)
write.csv(cal_tab, "output/calibration_null.csv", row.names = FALSE)
results$calibration_summary <- cal_summary
results$calibration_converged <- sum(cal_tab$rotation_converged)
results$calibration_warnings <- cal_tab$rotation_warnings

## (d) item-level bootstrap for the item-pair r: resample the 50 items with
##     replacement and recompute r over the pairs of distinct items
boot_pair_r <- withr::with_seed(42, replicate(1000, {
  s <- sample(50, replace = TRUE)
  keep_p <- upper.tri(diag(50)) & outer(s, s, "!=")
  cor(sim_mcp[s, s][keep_p], R_human_raw[s, s][keep_p])
}))
pair_r_ci <- quantile(boot_pair_r, c(.025, .975))
round(pair_r_ci, 3)
results$pair_level_r_ci <- pair_r_ci

## (e) the negative end of the item-pair relation: pairs whose human
##     correlation is below -.20, split by whether they join an item and a
##     reverse-keyed item of the same domain
neg <- x_h < -.20
pair_neg <- c(mean_sim_opp_same  = mean(y_s[neg & opp_same]),
              mean_sim_other_neg = mean(y_s[neg & !opp_same]),
              n_other_neg        = sum(neg & !opp_same))
round(pair_neg, 3)
results$pair_level_negative <- pair_neg

## (f) is the short form's gain more than selection alone gives? The 25 kept
##     items, and 200 random sets of five items per domain, each fitted with
##     five factors and scored against the theoretical domains
items_df <- big5_df[, c("code", "item", "factor")]
sf_idx <- match(short$keep, big5$codes)
fit_sf5 <- sfa(items_df[sf_idx, ], embeddings = E8[sf_idx, ],
               encoding = "mean_centered_pearson", nfactors = 5)
cg_sf5 <- sfa_congruence(fit_sf5, target = big5$factors[sf_idx],
                         metrics = c("nmi", "ari"))
sf_random <- withr::with_seed(42, t(replicate(200, {
  idx <- unlist(lapply(split(seq_len(50), big5$factors), sample, 5))
  f <- sfa(items_df[idx, ], embeddings = E8[idx, ],
           encoding = "mean_centered_pearson", nfactors = 5)
  cg <- sfa_congruence(f, target = big5$factors[idx], metrics = c("nmi", "ari"))
  c(nmi = cg$nmi, ari = cg$ari)
})))
sf_null <- rbind(short_form_5 = c(nmi = cg_sf5$nmi, ari = cg_sf5$ari),
                 random_median = apply(sf_random, 2, median),
                 random_95th   = apply(sf_random, 2, quantile, .95),
                 random_max    = apply(sf_random, 2, max))
round(sf_null, 3)
results$shortform_null <- list(short_form_5 = c(nmi = cg_sf5$nmi, ari = cg_sf5$ari),
                               random = sf_random)

## (g) the NLI matrix: how far the two orders of each pair disagree before
##     averaging, and how its scores fall by keying and domain
sim_nli_dir <- sfa_nli_matrix(big5$items, symmetric = FALSE)
nli_sym_diff <- max(abs((sim_nli_dir + t(sim_nli_dir)) / 2 - unname(sim_nli)))
cat("Largest difference between the averaged orders and sim_nli:",
    signif(nli_sym_diff, 3), "\n")
results$nli_symmetrization_diff <- nli_sym_diff
ut_nli <- upper.tri(sim_nli_dir)
same_dom_all <- outer(big5$factors, big5$factors, "==")
same_key_all <- outer(big5$scoring, big5$scoring) > 0
nli_struct <- c(
  r_orders            = cor(sim_nli_dir[ut_nli], t(sim_nli_dir)[ut_nli]),
  mean_abs_order_diff = mean(abs(sim_nli_dir[ut_nli] - t(sim_nli_dir)[ut_nli])),
  mean_within_same_key = mean(sim_nli[ut_nli & same_dom_all & same_key_all]),
  mean_within_opp_key  = mean(sim_nli[ut_nli & same_dom_all & !same_key_all]),
  share_cross_negative = mean(sim_nli[ut_nli & !same_dom_all] < 0),
  share_cross_below_m50 = mean(sim_nli[ut_nli & !same_dom_all] < -.5))
round(nli_struct, 3)
results$nli_structure <- nli_struct

## (h) where E1-E2 falls among the sign-flipped cosines, and the opposite-keyed
##     same-domain pairs as a whole
opp_lt <- (outer(big5$scoring, big5$scoring) < 0 & same_dom_all)[lt]
ar_lt <- sim_ar[lt]
ar_opp <- c(e1e2 = sim_ar["E1", "E2"],
            e1e2_percentile = mean(ar_lt <= sim_ar["E1", "E2"]),
            mean_opp_same = mean(ar_lt[opp_lt]),
            min_all = min(ar_lt))
round(ar_opp, 3)
results$atomic_reversed_opposites <- ar_opp

## (i) the comparison-data rule on more human subsamples: five independent
##     draws at n = 250 and at n = 500
cd_reps <- t(sapply(1:5, function(s) vapply(c(250, 500), function(n_sub) {
  set.seed(s)
  sfa_cd(as.matrix(human_keyed[sample(n_human, n_sub), ]), input = "data",
         n_factors_max = 10, alpha = .30)$n_factors
}, integer(1))))
dimnames(cd_reps) <- list(paste0("seed", 1:5), c("n250", "n500"))
cd_reps
results$cd_replicates <- cd_reps

## (j) depth selection: the range of NMI and the grid over depths
ds_tr <- ds$trajectory
c(n_depths = nrow(ds_tr), min_depth = min(ds_tr$depth),
  step = unname(stats::median(diff(ds_tr$depth))),
  nmi_min = min(ds_tr$nmi), nmi_max = max(ds_tr$nmi))
results$dimselect_grid <- ds_tr
## the grid: every 28th depth from 3, plus the full embedding at the end
tail(ds_tr$depth, 3)

## depth trajectory figure: NMI with the theoretical domains and TEFI at each
## depth, with the selected depth marked
pdf("figures/fig_depth.pdf", width = 9, height = 4)
op <- par(mfrow = c(1, 2), mar = c(4.2, 4.4, 2.4, 1))
plot(ds_tr$depth, ds_tr$nmi, type = "l", lwd = 2, xlab = "Depth (leading coordinates)",
     ylab = "NMI with the theoretical domains", main = "Agreement with theory")
abline(v = ds$optimal_depth, lty = 2, col = "firebrick")
plot(ds_tr$depth, ds_tr$tefi, type = "l", lwd = 2, xlab = "Depth (leading coordinates)",
     ylab = "TEFI (lower is cleaner)", main = "Entropy fit")
abline(v = ds$optimal_depth, lty = 2, col = "firebrick")
par(op)
dev.off()

## (k) the item pairs by domain and keying: same domain and keyed alike, same
##     domain and keyed oppositely, different domains. Mean semantic similarity
##     (main encoding) and mean raw-response correlation in each group, and the
##     share of each group whose raw-response correlation is negative
same_dom_lt <- same_dom_all[lt]
same_key_lt <- same_key_all[lt]
pair_groups <- list(same_domain_same_key = same_dom_lt & same_key_lt,
                    same_domain_opp_key  = same_dom_lt & !same_key_lt,
                    cross_domain         = !same_dom_lt)
pair_by_group <- t(sapply(pair_groups, function(g)
  c(n = sum(g), mean_sim = mean(y_s[g]), mean_human_r = mean(x_h[g]),
    share_negative = mean(x_h[g] < 0))))
round(pair_by_group, 3)
results$pair_by_group <- pair_by_group

## (l) item-level bootstrap, as in (d), for the curvature (quadratic minus
##     linear R2), for the similarity gap between same-domain pairs keyed
##     alike and keyed oppositely, and for the gap between opposite-keyed
##     same-domain pairs and cross-domain pairs
boot_shape <- withr::with_seed(42, t(replicate(1000, {
  s <- sample(50, replace = TRUE)
  keep_p <- upper.tri(diag(50)) & outer(s, s, "!=")
  xb <- R_human_raw[s, s][keep_p]
  yb <- sim_mcp[s, s][keep_p]
  sd_b <- outer(big5$factors[s], big5$factors[s], "==")[keep_p]
  sk_b <- (outer(big5$scoring[s], big5$scoring[s]) > 0)[keep_p]
  c(r2_gain = summary(lm(yb ~ xb + I(xb^2)))$r.squared -
              summary(lm(yb ~ xb))$r.squared,
    same_minus_opp = mean(yb[sd_b & sk_b]) - mean(yb[sd_b & !sk_b]),
    opp_minus_cross = mean(yb[sd_b & !sk_b]) - mean(yb[!sd_b]))
})))
shape_ci <- apply(boot_shape, 2, quantile, c(.025, .975), na.rm = TRUE)
round(shape_ci, 3)
results$pair_shape_ci <- shape_ci

## (m) the parallel-analysis verdict for each model size under ten further
##     seeds (each 100 random-embedding matrices)
pa_seeds <- sapply(names(emb_by_model), function(m) {
  sm <- sfa_similarity(emb_by_model[[m]], "mean_centered_pearson")
  vapply(1:10, function(s)
    sfa_parallel(sm, emb_by_model[[m]], seed = s)$n_factors, integer(1))
})
rownames(pa_seeds) <- paste0("seed", 1:10)
pa_seeds
results$pa_seeds <- pa_seeds

## (n) the human subsamples of the comparison-data analysis are drawn after
##     set.seed(42), so each smaller one is the start of the larger ones
cd_nested <- sapply(c(250, 500, 1000), function(n_sub) {
  set.seed(42); a <- sample(n_human, n_sub)
  set.seed(42); b <- sample(n_human, 2500)
  identical(a, b[seq_len(n_sub)])
})
cd_nested
results$cd_nested <- all(cd_nested)

## (o) the redundancy flags at other nominal sample sizes: sfa_redundancy()
##     estimates its network with a fixed nominal size of 1,000, so the same
##     network and weighted topological overlap are recomputed at 500 and
##     2,000, and the number of pairs at or above the 0.25 cutoff is counted
wto_pairs_at <- function(n) {
  net <- suppressWarnings(suppressMessages(EGAnet::network.estimation(
    data = fit$sim_matrix, n = n, model = "glasso", verbose = FALSE)))
  w <- abs(as.matrix(suppressWarnings(suppressMessages(EGAnet::wto(net)))))
  w <- (w + t(w)) / 2
  sum(w[upper.tri(w)] >= 0.25)
}
red_by_n <- sapply(c(n500 = 500, n1000 = 1000, n2000 = 2000), wto_pairs_at)
red_by_n
stopifnot(red_by_n[["n1000"]] == nrow(red_uva$pairs))
results$redundancy_by_n <- red_by_n

## ---------------------------------------------------------------------------
## 11c. Model size with a second scale: the DASS
## ---------------------------------------------------------------------------
banner("11c. Model-size comparison with a second scale (DASS)")

## The 42 items of the Depression Anxiety Stress Scales (14 per scale),
## embedded offline with the same three model sizes and the same settings as
## the Big Five items (data-raw/embed_items.py re-embeds them and compares).
## The statistics are those of Section 9b. This section comes last so that
## it changes none of the numbers printed above.
dass_npz <- lapply(c(`0.6B` = "0.6B", `4B` = "4B", `8B` = "8B"), function(m)
  sfa_load_npz(sprintf("data/DASS_items_%s.npz", m)))
dass_f <- dass_npz[["8B"]]$factors
stopifnot(all(sapply(dass_npz, function(z) identical(z$codes, dass_npz[["8B"]]$codes) &&
                                             identical(z$factors, dass_f))))
table(dass_f)
dass_emb <- lapply(dass_npz, `[[`, "embeddings")
dass_sim <- lapply(dass_emb, function(e) sfa_similarity(e, "mean_centered_pearson"))
n_dass <- length(dass_f)

## within- and between-scale means and the point-biserial, as in Section 9b
dass_same <- outer(dass_f, dass_f, "==")
dass_ut <- upper.tri(dass_same)
dass_sep <- t(sapply(dass_sim, function(sm)
  c(within  = mean(sm[dass_same & dass_ut]),
    between = mean(sm[!dass_same & dass_ut]),
    rpb     = cor(sm[dass_ut], as.numeric(dass_same[dass_ut])))))
round(dass_sep, 3)
## the within- minus between-scale gap of each scale on its own
dass_domain <- sapply(dass_sim, function(sm)
  sapply(sort(unique(dass_f)), function(d) {
    in_d <- dass_f == d
    mean(sm[outer(in_d, in_d, "&") & dass_ut]) - mean(sm[outer(in_d, !in_d, "&")])
  }))
round(dass_domain, 3)

## item-level bootstrap (items resampled with replacement, self-pairs
## dropped, as in Section 9b). The same resampled items serve all three
## sizes, so the differences between sizes are paired. The Big Five items
## are resampled the same way for comparison.
size_boot <- function(sims, f) {
  n <- length(f)
  withr::with_seed(42, replicate(1000, {
    s <- sample(n, replace = TRUE)
    keep_p <- upper.tri(diag(n)) & outer(s, s, "!=")
    same_b <- outer(f[s], f[s], "==")[keep_p]
    unlist(lapply(sims, function(sm) {
      y <- sm[s, s][keep_p]
      c(rpb = cor(y, as.numeric(same_b)), gap = mean(y[same_b]) - mean(y[!same_b]))
    }))
  }))
}
size_ci <- function(b) {
  d <- rbind(b,
             `8B-0.6B.rpb` = b["8B.rpb", ] - b["0.6B.rpb", ],
             `8B-0.6B.gap` = b["8B.gap", ] - b["0.6B.gap", ],
             `4B-0.6B.gap` = b["4B.gap", ] - b["0.6B.gap", ],
             `8B-4B.gap`   = b["8B.gap", ] - b["4B.gap", ])
  t(apply(d, 1, quantile, c(.025, .975)))
}
dass_ci <- size_ci(size_boot(dass_sim, dass_f))
round(dass_ci, 3)
big5_sim <- lapply(emb_by_model, function(e) sfa_similarity(e, "mean_centered_pearson"))
big5_ci <- size_ci(size_boot(big5_sim, big5$factors))
round(big5_ci, 3)
## the Big Five point-biserial intervals repeat those of Section 9b
stopifnot(isTRUE(all.equal(unname(big5_ci[c("0.6B.rpb", "4B.rpb", "8B.rpb"), ]),
                           unname(t(sep_ci)))))

## retention at each size, with the criteria of Section 9b
dass_nf <- lapply(names(dass_emb), function(m) {
  nf_m <- sfa_nfactors(dass_sim[[m]], embeddings = dass_emb[[m]],
                       methods = c("parallel", "kaiser", "TEFI", "EGA", "EKC"))
  cat(sprintf("\nDASS, Qwen3-Embedding-%s (%d dims):\n", m, ncol(dass_emb[[m]])))
  print(nf_m)
  nf_m
})
names(dass_nf) <- names(dass_emb)

## three-factor solutions at each size: agreement of each item's dominant
## factor with its scale (NMI, ARI), against 1,000 permutations of the labels.
## psych starts the oblimin rotation from several random orientations, so
## each fit has its own seed (the seed check below refits under 40 others).
dass_fit <- lapply(dass_npz, function(z) withr::with_seed(42,
  sfa(z, nfactors = 3, encoding = "mean_centered_pearson")))
dass_agree <- t(sapply(names(dass_fit), function(m) {
  cg <- sfa_congruence(dass_fit[[m]], target = dass_f, metrics = c("nmi", "ari"))
  c(nmi = cg$nmi, ari = cg$ari)
}))
round(dass_agree, 3)
dass_perm <- withr::with_seed(42, t(replicate(1000, {
  cp <- sfa_congruence(dass_fit[["8B"]], target = sample(dass_f),
                       metrics = c("nmi", "ari"))
  c(nmi = cp$nmi, ari = cp$ari)
})))
round(apply(dass_perm, 2, max), 3)
## the same agreement for the five-factor Big Five solutions at each size
## (the 8B value repeats the main fit's)
big5_agree <- t(sapply(names(emb_by_model), function(m) {
  f5 <- sfa(big5_df, embeddings = emb_by_model[[m]],
            encoding = "mean_centered_pearson", nfactors = 5)
  cg <- sfa_congruence(f5, target = big5$factors, metrics = c("nmi", "ari"))
  c(nmi = cg$nmi, ari = cg$ari)
}))
round(big5_agree, 3)
stopifnot(isTRUE(all.equal(unname(big5_agree["8B", ]),
                           unname(c(cong_theory$nmi, cong_theory$ari)))))
dass_tab <- lapply(dass_fit, function(fm) {
  L <- unclass(fm$loadings)
  table(scale = dass_f, factor = colnames(L)[apply(abs(L), 1, which.max)])
})
for (m in names(dass_tab)) {
  cat(sprintf("\nDASS, %s: scale by dominant factor\n", m))
  print(dass_tab[[m]])
}

## seed check: each three-factor DASS fit and each five-factor Big Five fit
## refitted under seeds 1 to 40. Each distinct solution is listed with the
## number of seeds that gave it, its agreement with the scales, and the
## oblimin criterion (gamma = 0) that the rotation minimizes, lower being
## better. The keying warning of the Big Five fits, shown above, is
## suppressed here.
oblimin_crit <- function(L) {
  L2 <- unclass(L)^2
  sum(L2 * (L2 %*% (1 - diag(ncol(L2))))) / 4
}
seed_sols <- function(fit_fun, target) {
  fits <- lapply(1:40, function(s) withr::with_seed(s, fit_fun()))
  r <- t(sapply(fits, function(fm) {
    cg <- sfa_congruence(fm, target = target, metrics = c("nmi", "ari"))
    c(nmi = cg$nmi, ari = cg$ari, criterion = oblimin_crit(fm$loadings))
  }))
  key <- sprintf("%.3f %.3f", r[, "nmi"], r[, "ari"])
  first <- match(unique(key), key)
  list(solutions = data.frame(r[first, , drop = FALSE],
                              seeds = as.vector(table(key)[unique(key)]),
                              row.names = NULL),
       fits = fits[first])
}
seed_check <- c(
  lapply(setNames(names(dass_npz), paste("DASS", names(dass_npz))), function(m)
    seed_sols(function() sfa(dass_npz[[m]], nfactors = 3,
                             encoding = "mean_centered_pearson"), dass_f)),
  lapply(setNames(names(emb_by_model), paste("Big Five", names(emb_by_model))), function(m)
    seed_sols(function() suppressWarnings(
      sfa(big5_df, embeddings = emb_by_model[[m]],
          encoding = "mean_centered_pearson", nfactors = 5)), big5$factors)))
for (nm in names(seed_check)) {
  cat(sprintf("\n%s: distinct solutions under 40 seeds\n", nm))
  print(round(seed_check[[nm]]$solutions, 4))
}
## each 0.6B DASS solution, in the order listed, with the scale by dominant factor
dass_alt_tab <- lapply(seed_check[["DASS 0.6B"]]$fits, function(fm) {
  L <- unclass(fm$loadings)
  table(scale = dass_f, factor = colnames(L)[apply(abs(L), 1, which.max)])
})
for (i in seq_along(dass_alt_tab)) {
  cat(sprintf("\nDASS, 0.6B, solution %d: scale by dominant factor\n", i))
  print(dass_alt_tab[[i]])
}
cat(sprintf("\nDASS, 0.6B, seed 42 (reported): oblimin criterion %.4f\n",
            oblimin_crit(dass_fit[["0.6B"]]$loadings)))

## heatmaps for the Supplement: items grouped by scale, one shared color
## range from the lowest off-diagonal similarity (rounded down) to 1
dass_floor <- floor(10 * min(sapply(dass_sim, function(sm) min(sm[lower.tri(sm)])))) / 10
dass_axis <- rep("", n_dass)
for (d in unique(dass_f)) dass_axis[which(dass_f == d)[7]] <- substr(d, 1, 1)
for (m in names(dass_sim)) {
  pdf(sprintf("figures/fig_dass_corplot_%s.pdf", sub("0.6B", "06B", m)),
      width = 7.5, height = 7.5)
  sfa_corplot(dass_sim[[m]], factors = dass_f, order = c("D", "A", "S"),
              main = sprintf("Qwen3-Embedding-%s", m), labels = dass_axis,
              cex.axis = 1.6, xlas = 1, zlim = c(dass_floor, 1),
              n.legend = round(10 * (1 - dass_floor)), group = TRUE, gr = seq_gr)
  dev.off()
}
cat("DASS heatmaps share the color range", dass_floor, "to 1\n")

results$dass_modelsize <- list(
  separation = dass_sep, domain_gap = dass_domain, ci = dass_ci,
  big5_ci = big5_ci, agreement = dass_agree, big5_agreement = big5_agree,
  assignment = lapply(dass_tab, unclass),
  seed_check = lapply(seed_check, `[[`, "solutions"),
  small_alt_assignment = lapply(dass_alt_tab, unclass),
  small_criterion = oblimin_crit(dass_fit[["0.6B"]]$loadings),
  perm_max = apply(dass_perm, 2, max), color_floor = dass_floor,
  retention = lapply(dass_nf, function(x)
    list(methods = x$methods, consensus = x$consensus)))

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
write.csv(round(sim_nli, 6), "output/sim_nli.csv")   # the signed NLI matrix, before repair
cat("Wrote output/results.rds and CSV tables.\n")

## the Python side of the run (NumPy for the .npz archives, the NLI
## cross-encoder) and the cached cross-encoder revision
np_mod <- reticulate::import("numpy")
py_versions <- c(
  python = as.character(reticulate::import("platform")$python_version()),
  numpy = as.character(np_mod$`__version__`),
  torch = as.character(reticulate::import("torch")$`__version__`),
  transformers = as.character(reticulate::import("transformers")$`__version__`),
  sentence_transformers =
    as.character(reticulate::import("sentence_transformers")$`__version__`))
print(py_versions)
## the hardware the run used: whether torch could see a GPU (the NLI
## cross-encoder runs on it if so), and the worker count that
## parallel::mclapply() uses in psych::fa.parallel() (its mc.cores default)
cat("torch CUDA available:", reticulate::import("torch")$cuda$is_available(),
    "| mc.cores option:", getOption("mc.cores", 2L),
    "| cores detected:", parallel::detectCores(), "\n")
nli_snap <- file.path(Sys.getenv("HF_HOME", file.path(path.expand("~"), ".cache",
                      "huggingface")), "hub",
                      "models--cross-encoder--nli-deberta-v3-base", "snapshots")
cat("NLI cross-encoder revision:", list.files(nli_snap), "\n")

sessionInfo()
