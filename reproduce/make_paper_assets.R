## make_paper_assets.R -- stage the manuscript's numbers, tables, and figures
## from the reproduction outputs.
##
## Run from the reproduce/ directory, AFTER reproduce.R:
##   R CMD BATCH --no-save --no-timing reproduce.R
##   Rscript make_paper_assets.R
##
## Reads   output/results.rds, output/*.csv, reproduce.Rout, data(big5)
## Writes  ../latex/generated/values.tex               one \newcommand per
##                                                     manuscript-reported number
##         ../latex/generated/table_tucker_body.tex    body of Table (Tucker phi)
##         ../latex/generated/table_encodings_body.tex body of Table (encodings)
##         ../latex/generated/table_semantic_loadings.tex   Supplementary Table S1
##         ../latex/generated/table_human_loadings.tex      Supplementary Table S2
##         ../latex/figures/*.pdf                      canonical manuscript figures
##
## The manuscript is a view over these outputs: every data-derived number in the
## prose is a \val... macro defined here, and every figure path points at a file
## this script stages, so reproduce.R + this script + latexmk refreshes the whole
## PDF with no hand edits. Verbatim transcript excerpts quote reproduce.Rout
## directly and are checked against it, not macro-substituted.

suppressMessages(library(semanticfa))

res  <- readRDS("output/results.rds")
rout <- readLines("reproduce.Rout", warn = FALSE)
data(big5)

gen_dir <- file.path("..", "latex", "generated")
fig_dir <- file.path("..", "latex", "figures")
dir.create(gen_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

## --- formatting helpers (precision decided here, never in the prose) --------

strip0 <- function(s) sub("^(-?)0\\.", "\\1.", s)
r2  <- function(x) strip0(sprintf("%.2f", x))     # .92
r3  <- function(x) strip0(sprintf("%.3f", x))     # .527
r4  <- function(x) strip0(sprintf("%.4f", x))     # .0119
f2  <- function(x) sprintf("%.2f", x)             # -27.71 / 29.63
cm  <- function(n) {                              # 874{,}434 (LaTeX thin comma)
  s <- formatC(as.integer(n), big.mark = ",", format = "d")
  gsub(",", "{,}", s, fixed = TRUE)
}
words <- c("one","two","three","four","five","six","seven","eight","nine","ten",
           "eleven","twelve","thirteen","fourteen","fifteen")
wd  <- function(n) if (n >= 1 && n <= length(words)) words[n] else as.character(n)
texneg <- function(s) gsub("-", "$-$", s, fixed = TRUE)  # table cells: $-$.001

## --- values parsed from the console transcript (reproduce.Rout) -------------

grab1 <- function(pattern, transform = identity) {
  hit <- grep(pattern, rout, value = TRUE)
  if (length(hit) == 0) stop("reproduce.Rout: no line matching ", pattern)
  transform(hit[1])
}

n_line     <- grab1("^Respondents after cleaning:")
n_human    <- as.integer(sub(".*cleaning: (\\d+) of.*", "\\1", n_line))
n_raw      <- as.integer(sub(".*of (\\d+).*", "\\1", n_line))
stopifnot(n_human == res$n_human)

nli_mineig <- sub(".*min eigenvalue = (-?[0-9.]+)\\).*", "\\1",
                  grab1("min eigenvalue = "))

## factor-correlation range: first printed Phi block of the main fit
phi_at  <- grep("^Factor correlations \\(Phi\\):", rout)[1]
phi_rows <- rout[(phi_at + 2):(phi_at + 6)]
phi_num <- lapply(strsplit(trimws(phi_rows), "\\s+"),
                  function(v) as.numeric(v[-1]))
phi_mat <- do.call(rbind, phi_num)
phi_off <- phi_mat[upper.tri(phi_mat)]

## communality range: first printed Communalities block (50 items)
comm_at   <- grep("^Communalities:", rout)[1]
comm_rows <- rout[(comm_at + 2):(comm_at + 51)]
comm_sp   <- strsplit(trimws(comm_rows), "\\s+")
comm      <- setNames(vapply(comm_sp, function(v) as.numeric(v[2]), 0),
                      vapply(comm_sp, `[`, "", 1))

## --- values recomputed from the bundled data (not stored in results.rds) ----

E8n    <- big5$embeddings / sqrt(rowSums(big5$embeddings^2))
E8ar   <- E8n * big5$scoring
ar_e1e2 <- sum(E8ar["E1", ] * E8ar["E2", ])       # sign-flipped E1-E2 cosine

## --- convenience handles on results.rds --------------------------------------

enc_rng <- res$encoding_ranges
nf      <- res$nfactors
nfm     <- setNames(nf$methods$n_factors, nf$methods$method)
ds      <- res$dimselect
ds_opt  <- ds$trajectory[ds$trajectory$depth == ds$optimal_depth, ][1, ]
fitd    <- res$fit_diagnostics
omega   <- fitd$omega
om      <- function(f, col) omega[omega$factor == f, col]
cal     <- res$calibration
anc_c   <- res$anchor_centroid
anc_l   <- res$anchor_label
prj     <- res$projection
uva     <- res$redundancy_uva
rcos    <- res$redundancy_cosine
sf      <- res$shortform$fidelity
ifit    <- res$item_fit
ch      <- res$congruence_human
tuck    <- res$tucker_matrix
prr     <- res$pair_level_r
enc     <- res$encoding_table
mc      <- res$model_comparison

## per-size retention (0.6B and 4B reported separately: under the sequential
## parallel-analysis rule their retained counts differ)
mc_get <- function(size, method)
  mc[[size]]$methods$n_factors[mc[[size]]$methods$method == method]

## semantic factor -> domain (via matched omega) and human factor -> domain
sem_dom <- setNames(omega$matched_theoretical, omega$factor)
hload   <- read.csv("output/human_loadings.csv", check.names = FALSE)
hum_mr  <- setdiff(names(hload), c("code", "factor"))
hum_dom <- vapply(hum_mr, function(m) {
  daal <- tapply(abs(hload[[m]]), hload$factor, mean)
  names(which.max(daal))
}, "")
abbr <- c(Extraversion = "E", Neuroticism = "N", Agreeableness = "A",
          Conscientiousness = "C", Openness = "O")

## matched Tucker phi per domain (row-wise best match, absolute values)
tuck_abs <- abs(tuck)
matched  <- apply(tuck_abs, 1, max)
phi_dom  <- setNames(matched, abbr[sem_dom[rownames(tuck)]])

## --- values.tex ---------------------------------------------------------------

L <- character(0)
add <- function(name, value, src) {
  L[[length(L) + 1]] <<- sprintf("\\newcommand{\\%s}{%s}%% %s", name, value, src)
}

L[[1]] <- "% values.tex -- AUTOGENERATED by reproduce/make_paper_assets.R. Do not edit."
L[[2]] <- "% Every data-derived number in the manuscript prose is one of these macros."

## samples and counts
add("valNHuman",     cm(n_human), "reproduce.Rout, respondents after cleaning")
add("valNHumanRaw",  cm(n_raw),   "reproduce.Rout, raw response records")
add("valNHumanRemovedPct", sprintf("%.1f", 100 * (n_raw - n_human) / n_raw),
    "reproduce.Rout, percentage of raw records removed by cleaning")
add("valNPairs",     cm(choose(nrow(big5$embeddings), 2)), "choose(50, 2) item pairs")
add("valNRev",       sum(big5$scoring < 0), "data(big5), reverse-keyed items")
## encodings
add("valRangeCosMin", r3(enc_rng["atomic", "min"]), "results$encoding_ranges atomic min")
add("valRangeCosMax", r3(enc_rng["atomic", "max"]), "results$encoding_ranges atomic max")
add("valRangeArMin",  r3(enc_rng["atomic_reversed", "min"]), "results$encoding_ranges atomic_reversed min")
add("valRangeSqMin",  r3(enc_rng["squid", "min"]), "results$encoding_ranges squid min")
add("valNliOpp",      r2(res$nli_example["E1", "E2"]), "results$nli_example E1-E2")
add("valArOpp",       r2(ar_e1e2), "recomputed sign-flipped E1-E2 cosine, data(big5)")
add("valAtOpp",       r2(-ar_e1e2), "unflipped (atomic) E1-E2 cosine = minus the sign-flipped value")
stopifnot(-ar_e1e2 > 0)
## retention
add("valEigFirst",   f2(nf$eigenvalues[1]), "results$nfactors eigenvalue 1")
add("valEigFirstPct", sprintf("%.0f", 100 * nf$eigenvalues[1] / length(nf$eigenvalues)),
    "results$nfactors eigenvalue 1 as a percentage of the total (number of items)")
## prose guard: the consensus is a 2-2 tie between five (parallel, EGA) and
## six (Kaiser, EKC), broken toward the smaller number by sfa_nfactors()
votes <- table(unlist(nfm[c("parallel", "kaiser", "TEFI", "EGA", "EKC")]))
stopifnot(votes[["5"]] == 2, votes[["6"]] == 2, nf$consensus == 5,
          nfm[["kaiser"]] == nfm[["EKC"]])
add("valPaK",        nfm[["parallel"]], "results$nfactors parallel")
add("valKaiserK",    nfm[["kaiser"]],   "results$nfactors kaiser")
add("valTefiK",      nfm[["TEFI"]],     "results$nfactors TEFI")
add("valEgaK",       nfm[["EGA"]],      "results$nfactors EGA")
add("valConsensusK", nf$consensus,      "results$nfactors consensus")
add("valPaKWord",        wd(nfm[["parallel"]]), "as word")
add("valKaiserKWord",    wd(nfm[["kaiser"]]),   "as word")
add("valTefiKWord",      wd(nfm[["TEFI"]]),     "as word")
add("valEgaKWord",       wd(nfm[["EGA"]]),      "as word")
add("valConsensusKWord", wd(nf$consensus),      "as word")
add("valDepthOpt",   cm(ds$optimal_depth), "results$dimselect optimal_depth")
add("valDepthFull",  cm(ds$full_dim),      "results$dimselect full_dim")
add("valDepthNmi",   r3(ds_opt$nmi),       "results$dimselect trajectory nmi at optimum")
add("valDepthNDimWord", wd(ds_opt$n_dim),  "results$dimselect n_dim at optimum")
## model-size retention, per size
add("valPaKSmallWord",    wd(mc_get("0.6B", "parallel")), "results$model_comparison 0.6B parallel")
add("valPaKMediumWord",   wd(mc_get("4B", "parallel")),   "results$model_comparison 4B parallel")
add("valEgaKSmallWord",   wd(mc_get("0.6B", "EGA")),      "results$model_comparison 0.6B EGA")
add("valEgaKMediumWord",  wd(mc_get("4B", "EGA")),        "results$model_comparison 4B EGA")
add("valConsSmallWord",   wd(mc[["0.6B"]]$consensus),     "results$model_comparison 0.6B consensus")
add("valConsMediumWord",  wd(mc[["4B"]]$consensus),       "results$model_comparison 4B consensus")
add("valEkcK",           nfm[["EKC"]],     "results$nfactors EKC")
add("valEkcKWord",       wd(nfm[["EKC"]]), "as word")
add("valEkcKSmallWord",  wd(mc_get("0.6B", "EKC")), "results$model_comparison 0.6B EKC")
add("valEkcKMediumWord", wd(mc_get("4B", "EKC")),   "results$model_comparison 4B EKC")
add("valEkcKLargeWord",  wd(mc_get("8B", "EKC")),   "results$model_comparison 8B EKC")
# prose says EKC (like kaiser and TEFI) is constant "at every size"
stopifnot(mc_get("0.6B", "EKC") == mc_get("8B", "EKC"),
          mc_get("4B", "EKC") == mc_get("8B", "EKC"))
stopifnot(mc_get("0.6B", "kaiser") == mc_get("8B", "kaiser"),
          mc_get("4B", "kaiser") == mc_get("8B", "kaiser"),
          mc_get("0.6B", "TEFI") == mc_get("8B", "TEFI"),
          mc_get("4B", "TEFI") == mc_get("8B", "TEFI"))
add("valMapK",           res$map$n_factors, "results$map n_factors")
## comparison-data misfit profiles (Section 10b of reproduce.R)
cdp    <- res$cd
cd_hum <- cdp$human
pct1 <- function(x) sprintf("%.1f", 100 * x)
add("valCdHumanN",       cm(cd_hum$n), "results$cd human n")
add("valCdHumanImpFive", pct1(cd_hum$improvement[4]), "results$cd human improvement into k=5")
add("valCdHumanImpSix",  pct1(cd_hum$improvement[5]), "results$cd human improvement into k=6")
add("valCdHumanNormFive", sprintf("%.0f\\%%", 100 * cd_hum$profile[5]),
    "results$cd human normalized RMSR at k=5")
emb_n5  <- vapply(cdp$embeddings, function(x) x$profile[5], 0)
add("valCdEmbNormFiveRange",
    if (diff(range(round(100 * emb_n5))) < 1) sprintf("%.0f\\%%", 100 * min(emb_n5))
    else sprintf("%.0f--%.0f\\%%", 100 * min(emb_n5), 100 * max(emb_n5)),
    "results$cd embeddings normalized RMSR at k=5, range")
emb_imp <- unlist(lapply(cdp$embeddings, function(x) x$improvement))
add("valCdEmbImpLow",  sprintf("%.0f", 100 * min(emb_imp)), "results$cd embeddings min step improvement")
add("valCdEmbImpHigh", sprintf("%.0f", 100 * max(emb_imp)), "results$cd embeddings max step improvement")
stopifnot(max(emb_imp) < cd_hum$improvement[4] / 2)   # prose: under half the human peak
add("valCdCrossTwoFiftyWord", wd(cdp$crossover[["n250"]]),
    "results$cd crossover verdict n=250")
# prose says the sequential rule saturates to the cap from n = 500 up
stopifnot(cdp$crossover[["n500"]] == 10L, cdp$crossover[["n1000"]] == 10L,
          cdp$crossover[["n2500"]] == 10L)
add("valCdCrossSatWord", wd(cdp$crossover[["n2500"]]),
    "results$cd crossover verdicts n>=500 (at the cap)")
emb_verd <- vapply(cdp$embeddings, function(x) x$n_factors, 0L)
stopifnot(all(emb_verd == 10L))  # prose claims saturation at the ten-factor cap
add("valCdEmbVerdictWord", wd(emb_verd[[1]]), "results$cd embeddings sequential-rule verdict (cap)")
## main fit
add("valKmo",     r3(fitd$kmo$total), "results$fit_diagnostics kmo total")
add("valTefiMain", f2(fitd$tefi),     "results$fit_diagnostics tefi")
add("valRmsr",    r3(fitd$rmsr),      "results$fit_diagnostics rmsr")
add("valCaf",     r3(fitd$caf),       "results$fit_diagnostics caf")
add("valPhiLo",   r2(min(phi_off)), "reproduce.Rout, factor-correlation minimum")
add("valPhiHi",   r2(max(phi_off)), "reproduce.Rout, factor-correlation maximum")
add("valCommMin", r3(min(comm)),    "reproduce.Rout, communality minimum")
add("valCommMinItem", names(which.min(comm)), "reproduce.Rout, item at communality minimum")
add("valCommMax", r3(max(comm)),    "reproduce.Rout, communality maximum")
add("valCommMaxItem", names(which.max(comm)), "reproduce.Rout, item at communality maximum")
mrA <- omega$factor[omega$matched_theoretical == "Agreeableness"][1]
mrE <- omega$factor[omega$matched_theoretical == "Extraversion"][1]
add("valMrA", mrA, "results omega, factor matched to Agreeableness")
add("valMrE", mrE, "results omega, factor matched to Extraversion")
add("valMrN", omega$factor[omega$matched_theoretical == "Neuroticism"][1], "results omega, factor matched to Neuroticism")
add("valMrC", omega$factor[omega$matched_theoretical == "Conscientiousness"][1], "results omega, factor matched to Conscientiousness")
add("valMrO", omega$factor[omega$matched_theoretical == "Openness"][1], "results omega, factor matched to Openness")
add("valOmegaAsgnA",  r3(om(mrA, "omega_assigned")),    "results omega, A-matched factor assigned")
add("valOmegaTheoA",  r3(om(mrA, "omega_theoretical")), "results omega, A-matched factor theoretical")
add("valOmegaAsgnE",  r3(om(mrE, "omega_assigned")),    "results omega, E-matched factor assigned")
add("valOmegaTheoE",  r3(om(mrE, "omega_theoretical")), "results omega, E-matched factor theoretical")
## calibration nulls
add("valCalRmsrMin", r4(min(cal$rmsr)), "results$calibration rmsr min")
add("valCalRmsrMax", r4(max(cal$rmsr)), "results$calibration rmsr max")
add("valCalCafMin",  r3(min(cal$caf)),  "results$calibration caf min")
add("valCalCafMax",  r3(max(cal$caf)),  "results$calibration caf max")
add("valCalTefiMin", f2(min(cal$tefi)), "results$calibration tefi min")
add("valCalTefiMax", f2(max(cal$tefi)), "results$calibration tefi max")
## interpretation
add("valAnchorNTwOwn",   r2(anc_c["N12", "Neuroticism"]),  "results$anchor_centroid N12 own")
add("valAnchorNTwRival", r2(anc_c["N12", "Extraversion"]), "results$anchor_centroid N12 rival")
a23_gap <- anc_c["A23", "Extraversion"] - anc_c["A23", "Agreeableness"]
stopifnot(a23_gap > 0, a23_gap < 0.01)   # prose: A23 is closer to Extraversion "by less than .01"
add("valAnchorATwThreeGap", r3(a23_gap), "results$anchor_centroid A23 Extraversion minus own (Agreeableness)")
add("valAnchorATwFiveOwn",  r2(anc_l["A25", "Agreeableness"]), "results$anchor_label A25 own")
add("valAnchorATwFiveNeur", r2(anc_l["A25", "Neuroticism"]),   "results$anchor_label A25 Neuroticism")
add("valProjNTwCalm", r2(prj["N12", "stability"]), "results$projection N12 stability")
add("valNmiTheory", r3(ch$nmi), "results$congruence_human nmi (= vs theory)")
add("valAriTheory", r3(ch$ari), "results$congruence_human ari (= vs theory)")
## refinement
add("valUvaPairsWord",    wd(nrow(uva$pairs)),     "results$redundancy_uva pairs")
add("valUvaClustersWord", wd(length(uva$clusters)), "results$redundancy_uva clusters")
## the redundancy count at other nominal sample sizes (results$redundancy_by_n):
## the prose says it is the same at 500, 1,000 and 2,000
rbn <- res$redundancy_by_n
stopifnot(length(unique(rbn)) == 1, rbn[["n1000"]] == nrow(uva$pairs))
add("valWtoTop",          r3(max(uva$pairs$overlap)), "results$redundancy_uva top overlap")
add("valCosPairsWord",    wd(nrow(rcos$pairs)),     "results$redundancy_cosine pairs")
add("valCosClustersWord", wd(length(rcos$clusters)), "results$redundancy_cosine clusters")
add("valCosTopA", r3(sort(rcos$pairs$overlap, decreasing = TRUE)[1]), "results$redundancy_cosine top pair")
add("valCosTopB", r3(sort(rcos$pairs$overlap, decreasing = TRUE)[2]), "results$redundancy_cosine 2nd pair")
add("valSfNmiFull", r3(sf$nmi_full),    "results$shortform nmi full")
add("valSfNmiRed",  r3(sf$nmi_reduced), "results$shortform nmi reduced")
add("valSfAriFull", r3(sf$ari_full),    "results$shortform ari full")
add("valSfAriRed",  r3(sf$ari_reduced), "results$shortform ari reduced")
add("valSfKFullWord", wd(sf$nfactors_full),    "results$shortform factors full")
add("valSfKRedWord",  wd(sf$nfactors_reduced), "results$shortform factors reduced")
uva_pairs <- res$redundancy_uva$pairs
## every flagged pair joins two items of one domain (codes start with the domain letter)
stopifnot(all(substr(uva_pairs$item_i, 1, 1) == substr(uva_pairs$item_j, 1, 1)))
pkey <- function(d) paste(pmin(d$item_i, d$item_j), pmax(d$item_i, d$item_j))
cos_pairs  <- res$redundancy_cosine$pairs
cos_in_uva <- sum(pkey(cos_pairs) %in% pkey(uva_pairs))
add("valCosInUvaWord", wd(cos_in_uva),
    "results$redundancy_cosine pairs also flagged by results$redundancy_uva")
sf_red_kept <- sum(uva_pairs$item_i %in% res$shortform$keep &
                   uva_pairs$item_j %in% res$shortform$keep)
stopifnot(sf_red_kept >= 1, sf_red_kept < nrow(uva_pairs))
## prose: the short form's factors keep Neuroticism alone and merge
## Conscientiousness with Openness and Agreeableness with Extraversion
L_sf   <- unclass(res$shortform$reduced_fit$loadings)
sf_tab <- table(colnames(L_sf)[apply(abs(L_sf), 1, which.max)],
                res$shortform$reduced_fit$item_data$factor)
sf_home <- function(d) { f <- rownames(sf_tab)[sf_tab[, d] == 5]; if (length(f) == 1) f else NA }
stopifnot(ncol(L_sf) == 3, nrow(sf_tab) == 3,
          !is.na(sf_home("Openness")), identical(sf_home("Conscientiousness"), sf_home("Openness")),
          !is.na(sf_home("Agreeableness")), identical(sf_home("Agreeableness"), sf_home("Extraversion")),
          !is.na(sf_home("Neuroticism")), sum(sf_tab[sf_home("Neuroticism"), ]) == 5)
add("valSfRedKeptWord", wd(sf_red_kept),
    "results$redundancy_uva pairs with both items kept in results$shortform$keep")
add("valIfGap",     r2(ifit$summary$gap[1]),         "results$item_fit candidate 1 gap")
add("valIfNearest", r2(ifit$summary$nearest_sim[2]), "results$item_fit candidate 2 nearest")
add("valIfGoodGap", r2(ifit$summary$gap[3]),         "results$item_fit candidate 3 gap")
stopifnot(grepl("^cross-loads", ifit$summary$verdict[1]),
          grepl("^redundant",   ifit$summary$verdict[2]),
          grepl("^good fit",    ifit$summary$verdict[3]))
## human comparison
add("valHumanPaKWord", wd(res$human_parallel_nfact), "results$human_parallel_nfact")
add("valPhiO", r2(phi_dom[["O"]]), "results$tucker_matrix matched Openness")
add("valPhiC", r2(phi_dom[["C"]]), "results$tucker_matrix matched Conscientiousness")
add("valPhiN", r2(phi_dom[["N"]]), "results$tucker_matrix matched Neuroticism")
add("valPhiE", r2(phi_dom[["E"]]), "results$tucker_matrix matched Extraversion")
add("valPhiA", r2(phi_dom[["A"]]), "results$tucker_matrix matched Agreeableness")
add("valPhiEA", r2(tuck_abs[names(sem_dom)[sem_dom == "Extraversion"],
                            names(hum_dom)[hum_dom == "Agreeableness"]]),
    "results$tucker_matrix semantic E vs human A")
add("valPhiMean", r3(mean(matched)), "results$tucker_matrix mean matched phi")
add("valFrob",    r3(ch$frobenius),  "results$congruence_human frobenius")
add("valNliMinEig", nli_mineig, "reproduce.Rout, NLI PSD projection message")
add("valPairR",     r2(prr["mean_centered_pearson", "r_raw"]), "results$pair_level_r mcp raw (2 dp)")
add("valMcpRRaw",   r3(prr["mean_centered_pearson", "r_raw"]), "results$pair_level_r mcp raw (3 dp)")
add("valMcpRKeyed", r3(prr["mean_centered_pearson", "r_keyed"]), "results$pair_level_r mcp keyed (3 dp)")

## shape of the item-pair relation (section 11d): the quadratic fit, the
## correlation with the absolute human correlations, and the negative-r pairs
psh <- res$pair_level_shape
stopifnot(!is.null(psh),
          abs(psh[["r_linear"]] - prr["mean_centered_pearson", "r_raw"]) < 1e-12,
          psh[["r2_quadratic"]] > psh[["r2_linear"]],
          psh[["r_absolute"]] > psh[["r_linear"]],
          psh[["mean_sim_below_m20"]] > psh[["mean_sim_near_zero"]],
          psh[["n_below_m20_opp"]] > psh[["n_below_m20"]] / 2)   # prose: "mostly"
add("valPairRAbs",     r2(psh[["r_absolute"]]),  "results$pair_level_shape r with |human r|")
add("valPairRsqLin",   r2(psh[["r2_linear"]]),   "results$pair_level_shape linear R2")
add("valPairRsqQuad",  r2(psh[["r2_quadratic"]]), "results$pair_level_shape quadratic R2")
add("valPairNegN",     as.character(psh[["n_below_m20"]]),     "results$pair_level_shape pairs with human r < -.20")
add("valPairNegOppN",  as.character(psh[["n_below_m20_opp"]]), "results$pair_level_shape of those, same-domain opposite-keyed pairs")
add("valPairSimNeg",   r2(psh[["mean_sim_below_m20"]]), "results$pair_level_shape mean similarity, human r < -.20")
add("valPairSimZero",  r2(psh[["mean_sim_near_zero"]]), "results$pair_level_shape mean similarity, |human r| < .10")
add("valAtRKeyed",  r3(enc["atomic", "r_keyed"]),          "results$encoding_table atomic r_keyed")
add("valArRKeyed",  r3(enc["atomic_reversed", "r_keyed"]), "results$encoding_table atomic_reversed r_keyed")
add("valArRRaw",    r3(enc["atomic_reversed", "r_raw"]),   "results$encoding_table atomic_reversed r_raw")
add("valNliNmi",    r3(enc["nli", "NMI_theory"]),  "results$encoding_table nli NMI")
add("valNliAri",    r3(enc["nli", "ARI_theory"]),  "results$encoding_table nli ARI")
add("valNliRRaw",   r3(enc["nli", "r_raw"]),       "results$encoding_table nli r_raw")
add("valNliRKeyed", r3(enc["nli", "r_keyed"]),     "results$encoding_table nli r_keyed")
add("valNliPhi",    r3(enc["nli", "Tucker_human"]), "results$encoding_table nli Tucker")
add("valSqRKeyed",  r3(enc["squid", "r_keyed"]),   "results$encoding_table squid r_keyed")
add("valSqRRaw",    r3(enc["squid", "r_raw"]), "results$encoding_table squid r_raw")
## prose guards for the encoding comparison (like with like): NLI agrees
## best with the raw correlations, then SQuID, then the cosine encodings;
## sign-flipping lowers keyed agreement and does not raise raw agreement
stopifnot(enc["nli", "r_raw"] > enc["squid", "r_raw"],
          enc["squid", "r_raw"] > enc["mean_centered_pearson", "r_raw"],
          enc["atomic_reversed", "r_raw"] <= enc["atomic", "r_raw"],
          enc["atomic_reversed", "r_keyed"] < enc["atomic", "r_keyed"],
          enc["atomic_reversed", "NMI_theory"] == enc["atomic", "NMI_theory"])
## prose guards for the congruence benchmarks (Lorenzo-Seva & ten Berge,
## 2006: .85-.94 fair similarity): O in the fair range, C and N just below
stopifnot(phi_dom[["O"]] >= .85, phi_dom[["O"]] < .95,
          phi_dom[["C"]] >= .75, phi_dom[["C"]] < .85,
          phi_dom[["N"]] >= .75, phi_dom[["N"]] < .85)
add("valSqDis",     r3(enc["squid", "Disattenuated_keyed"]), "results$encoding_table squid disattenuated")
add("valSqKmo",     r3(enc["squid", "KMO"]),       "results$encoding_table squid KMO")
add("valNmiGapNli", r3(enc["nli", "NMI_theory"] - enc["mean_centered_pearson", "NMI_theory"]),
    "results$encoding_table NMI gap, nli minus mcp")

## model-size domain separation (results$modelsize_separation): point-biserial
## correlation between sharing a domain and pair similarity, per model size.
## The prose claims substantial separation at EVERY size (not a monotone
## gradient - the 2026-09 quantification showed 4B highest), so assert that.
msep <- res$modelsize_separation
stopifnot(!is.null(msep), identical(rownames(msep), c("0.6B", "4B", "8B")),
          all(msep[, "rpb"] > 0.30))
add("valSepSmall",  r3(msep["0.6B", "rpb"]), "results$modelsize_separation 0.6B point-biserial")
add("valSepMedium", r3(msep["4B", "rpb"]),   "results$modelsize_separation 4B point-biserial")
add("valSepLarge",  r3(msep["8B", "rpb"]),   "results$modelsize_separation 8B point-biserial")
sep_gap <- msep[, "within"] - msep[, "between"]
stopifnot(diff(range(sep_gap)) < 0.01)   # prose: the within-minus-between gap is nearly constant
add("valSepGapSmall",  r3(sep_gap[["0.6B"]]), "results$modelsize_separation 0.6B within minus between")
add("valSepGapMedium", r3(sep_gap[["4B"]]),   "results$modelsize_separation 4B within minus between")
add("valSepGapLarge",  r3(sep_gap[["8B"]]),   "results$modelsize_separation 8B within minus between")
## item-level bootstrap intervals for the point-biserial separation
sci_ms <- res$modelsize_separation_ci
for (m in c("0.6B", "4B", "8B")) {
  X <- c("0.6B" = "Small", "4B" = "Medium", "8B" = "Large")[[m]]
  add(sprintf("valSepCi%sLo", X), r3(sci_ms[1, m]), sprintf("results$modelsize_separation_ci %s 2.5th percentile", m))
  add(sprintf("valSepCi%sHi", X), r3(sci_ms[2, m]), sprintf("results$modelsize_separation_ci %s 97.5th percentile", m))
}
## the prose says the three intervals overlap
stopifnot(max(sci_ms[1, ]) < min(sci_ms[2, ]))
## per-domain within-minus-between gap: the prose says every gap is positive
## and Neuroticism's is the largest at each size
dgap <- res$modelsize_domain_gap
stopifnot(!is.null(dgap), all(dgap > 0),
          all(apply(dgap, 2, which.max) == which(rownames(dgap) == "Neuroticism")))
for (m in c("0.6B", "4B", "8B")) {
  X <- c("0.6B" = "Small", "4B" = "Medium", "8B" = "Large")[[m]]
  add(sprintf("valDomGapN%s", X), r3(dgap["Neuroticism", m]), sprintf("results$modelsize_domain_gap Neuroticism %s", m))
}
add("valDomGapOtherMax", r3(max(dgap[rownames(dgap) != "Neuroticism", ])),
    "results$modelsize_domain_gap largest gap of the other four domains at any size")

## --- outputs added for the 2026-10 validation round ---------------------------
add("valCalReps", as.character(length(res$calibration$rmsr)),
    "results$calibration replicates retained")
## calibration fits whose oblimin rotation hit its iteration limit (warning summary
## printed right after the calibration call)
cal_i   <- grep("^Calibration replicates retained", rout)
rot_hit <- grep("^\\s*\\d+x : In engine\\(A, Tmat", rout)
rot_hit <- rot_hit[rot_hit > cal_i][1]
stopifnot(length(cal_i) == 1, !is.na(rot_hit), rot_hit - cal_i < 6,
          grepl("Convergence not obtained in GPFoblq", rout[rot_hit + 1]))
add("valCalRotWarnN", sub("^\\s*(\\d+)x : .*", "\\1", rout[rot_hit]),
    "reproduce.Rout, warnings that an oblimin rotation did not converge (counted per warning, not per fit)")
stopifnot(!is.null(res$anchor_closer_other), !is.null(res$anchor_own_name_top))
add("valAnchorCloserN", wd(nrow(res$anchor_closer_other)),
    "results$anchor_closer_other, items closer to another centroid (word)")
add("valAnchorOwnNameN", as.character(res$anchor_own_name_top),
    "results$anchor_own_name_top, items most similar to their own construct name")
pn <- res$congruence_theory_null
add("valNmiNullMax", r3(max(pn[, "nmi"])), "results$congruence_theory_null max NMI (1,000 permutations)")
add("valAriNullMax", r3(max(pn[, "ari"])), "results$congruence_theory_null max ARI (1,000 permutations)")
stopifnot(max(pn[, "nmi"]) < mean(c(res$congruence_theory$nmi)),
          max(pn[, "ari"]) < res$congruence_theory$ari)
jjf <- res$jinglejangle$flags
stopifnot(nrow(jjf) == 10, all(jjf$type == "-"))
add("valJjMaxDiv", r2(abs(jjf$divergence[1])),
    "results$jinglejangle$flags largest |content - label| divergence")
add("valJjMaxPair", sprintf("%s and %s", jjf$scale_a[1], jjf$scale_b[1]),
    "results$jinglejangle$flags pair with the largest divergence")
jj_ae <- jjf[paste(pmin(jjf$scale_a, jjf$scale_b), pmax(jjf$scale_a, jjf$scale_b)) ==
             "Agreeableness Extraversion", ]
stopifnot(nrow(jj_ae) == 1)
## prose: the largest divergence is the Agreeableness-Extraversion pair
stopifnot(identical(rownames(jj_ae), rownames(jjf)[1]))
add("valJjAEDiv", r2(abs(jj_ae$divergence)),
    "results$jinglejangle$flags Agreeableness-Extraversion |divergence|")
add("valJjAEContent", r2(jj_ae$content_sim),
    "results$jinglejangle$flags Agreeableness-Extraversion content similarity")
trj <- res$dimselect$trajectory
fullrow <- trj[trj$depth == max(trj$depth), ]
add("valDepthFullNmi", r3(fullrow$nmi), "results$dimselect trajectory nmi at the full embedding")
add("valDepthFullNDimWord", wd(fullrow$n_dim), "results$dimselect trajectory n_dim at the full embedding")
nr <- res$nli_repair
add("valNliNegEig", wd(nr[["negative_eigenvalues"]]), "results$nli_repair negative eigenvalues (word)")
add("valNliRepairR", r2(nr[["r_original_repaired"]]), "results$nli_repair r between original and repaired pair values")

## items per semantic factor (each item to its largest-absolute-loading
## factor, from the loadings CSV): size, own-domain items, reverse-keyed items
dom_full <- c("Openness", "Conscientiousness", "Extraversion",
              "Agreeableness", "Neuroticism")
sl     <- read.csv("output/semantic_loadings.csv", check.names = FALSE)
sl_mr  <- setdiff(names(sl), c("code", "factor"))
sl_top <- sl_mr[apply(abs(as.matrix(sl[, sl_mr])), 1, which.max)]
sl_rev <- big5$scoring[match(sl$code, big5$codes)] < 0
stopifnot(all(as.vector(table(factor(sl_top, levels = rownames(res$assignment)))) ==
              rowSums(res$assignment)))
asg_rev <- setNames(vapply(names(sem_dom), function(f) sum(sl_top == f & sl_rev), 0),
                    names(sem_dom))
for (d in dom_full) {
  f  <- names(sem_dom)[sem_dom == d]
  on <- sl_top == f
  X  <- abbr[[d]]
  add(sprintf("valAsgn%sSize", X), wd(sum(on)),            sprintf("items assigned to the %s-matched factor (word)", d))
  add(sprintf("valAsgn%sOwn",  X), wd(sum(on & sl$factor == d)), sprintf("of those, items from %s (word)", d))
  add(sprintf("valAsgn%sRev",  X), wd(sum(on & sl_rev)),   sprintf("of those, reverse-keyed items (word)"))
}
## prose guards: the Agreeableness-matched factor holds only reverse-keyed
## items ("all of them reverse-keyed") drawn from several domains, and the
## Extraversion-matched factor holds only forward-keyed items
onA <- sl_top == names(sem_dom)[sem_dom == "Agreeableness"]
onE <- sl_top == names(sem_dom)[sem_dom == "Extraversion"]
stopifnot(all(sl_rev[onA]), !any(sl_rev[onE]))
add("valAsgnADomainsWord", wd(length(unique(sl$factor[onA]))),
    "domains represented on the Agreeableness-matched factor (word)")
add("valAsgnRevOff", wd(sum(sl_rev & !onA)),
    "reverse-keyed items assigned to factors other than the Agreeableness-matched factor (word)")
## where the Agreeableness and Extraversion items went: the two blended
## factors (matched to A and E) and the Neuroticism-matched factor
onN <- sl_top == names(sem_dom)[sem_dom == "Neuroticism"]
add("valAsgnEOnBlend", wd(sum((onA | onE) & sl$factor == "Extraversion")),
    "Extraversion items assigned to the two blended factors (word)")
add("valAsgnAOnBlend", wd(sum((onA | onE) & sl$factor == "Agreeableness")),
    "Agreeableness items assigned to the two blended factors (word)")
add("valAsgnAOnN", wd(sum(onN & sl$factor == "Agreeableness")),
    "Agreeableness items assigned to the Neuroticism-matched factor (word)")
add("valAsgnBlendOther", wd(sum((onA | onE) & !(sl$factor %in% c("Agreeableness", "Extraversion")))),
    "items from other domains on the two blended factors (word)")
## the communality maximum item's loading on its factor slightly exceeds 1
stopifnot(max(abs(as.matrix(sl[, sl_mr]))) > 1, max(abs(as.matrix(sl[, sl_mr]))) < 1.05)

## --- supporting checks (reproduce.R section 11b) ------------------------------
ms_line <- grab1("^Model-size heatmaps share the color range")
add("valMsFloor", sub(".*range ([0-9.]+) to 1.*", "\\1", ms_line),
    "reproduce.Rout, shared lower end of the model-size heatmap color scale")
## (a) atomic versus mean-centered Pearson, entry by entry
ee <- res$encoding_equivalence
stopifnot(!is.null(ee), ee[["max_abs_diff"]] < 0.01, ee[["r"]] > 0.999)
add("valEncMaxDiff", strip0(formatC(ee[["max_abs_diff"]], format = "f", digits = 4)),
    "results$encoding_equivalence largest |atomic - mean-centered Pearson|")
add("valEncR", r3(ee[["r"]]), "results$encoding_equivalence r over the pairs")
## (b) parallel-analysis thresholds per model size (first, and eighth for 8B)
pat <- res$pa_thresholds
stopifnot(identical(names(pat), c("0.6B", "4B", "8B")),
          all(sapply(pat, function(x) all(diff(x["threshold", ]) < 0))))
add("valPaThrSmall",  r2(pat[["0.6B"]]["threshold", 1]), "results$pa_thresholds 0.6B first")
add("valPaThrMedium", r2(pat[["4B"]]["threshold", 1]),   "results$pa_thresholds 4B first")
add("valPaThrLarge",  r2(pat[["8B"]]["threshold", 1]),   "results$pa_thresholds 8B first")
add("valPaThrEighthLarge", r2(pat[["8B"]]["threshold", 8]), "results$pa_thresholds 8B eighth")
stopifnot(abs(pat[["8B"]]["threshold", 1] - res$parallel$percentiles[1]) < 1e-12)
## (c) calibration: replication of the rotation-convergence flags
stopifnot(isTRUE(res$calibration_replication_matches))
cs <- res$calibration_summary
add("valCalConvN", as.character(res$calibration_converged),
    "results$calibration_converged, fits whose oblimin rotation converged")
stopifnot(res$calibration_converged < 100)
add("valCalNonConvWord", wd(100 - res$calibration_converged),
    "results$calibration_converged, fits whose rotation did not converge (word)")
add("valCalRmsrMean", r4(cs["mean", "rmsr"]), "results$calibration_summary mean RMSR")
add("valCalCafMean",  r3(cs["mean", "caf"]),  "results$calibration_summary mean CAF")
add("valCalCafSd",    r3(cs["sd", "caf"]),    "results$calibration_summary SD CAF")
add("valCalTefiMean", f2(cs["mean", "tefi"]), "results$calibration_summary mean TEFI")
add("valCalTefiSd",   f2(cs["sd", "tefi"]),   "results$calibration_summary SD TEFI")
add("valCalTefiConvMin", f2(cs["min_converged", "tefi"]), "results$calibration_summary min TEFI, converged fits")
add("valCalTefiConvMax", f2(cs["max_converged", "tefi"]), "results$calibration_summary max TEFI, converged fits")
add("valCalCafConvMax", r3(cs["max_converged", "caf"]), "results$calibration_summary max CAF, converged fits")
## the prose says the overall CAF maximum comes from the non-converged fit
stopifnot(max(cal$caf) > cs["max_converged", "caf"])
## (d) item-level bootstrap interval for the item-pair r
pci <- res$pair_level_r_ci
stopifnot(pci[[1]] < res$pair_level_shape[["r_linear"]],
          pci[[2]] > res$pair_level_shape[["r_linear"]])
add("valPairRLo", r2(pci[[1]]), "results$pair_level_r_ci 2.5th percentile (1,000 item resamples)")
add("valPairRHi", r2(pci[[2]]), "results$pair_level_r_ci 97.5th percentile")
## (e) the negative end of the item-pair relation
pn2 <- res$pair_level_negative
add("valPairSimPos", r2(res$pair_level_shape[["mean_sim_above_20"]]),
    "results$pair_level_shape mean similarity, human r > .20")
add("valPairSimNegOpp",   r2(pn2[["mean_sim_opp_same"]]),  "results$pair_level_negative opposite-keyed same-domain pairs")
add("valPairSimNegOther", r2(pn2[["mean_sim_other_neg"]]), "results$pair_level_negative other negative pairs")
add("valPairNegOtherN", as.character(pn2[["n_other_neg"]]), "results$pair_level_negative count of other negative pairs")
stopifnot(pn2[["n_other_neg"]] + res$pair_level_shape[["n_below_m20_opp"]] ==
          res$pair_level_shape[["n_below_m20"]])
## (f) the short form against random five-per-domain sets
sfn <- res$shortform_null
sfr <- sfn$random
add("valSfFiveNmi", r3(sfn$short_form_5[["nmi"]]), "results$shortform_null short form fitted with five factors, NMI")
add("valSfFiveAri", r3(sfn$short_form_5[["ari"]]), "results$shortform_null short form fitted with five factors, ARI")
add("valSfRandN", as.character(nrow(sfr)), "results$shortform_null number of random sets")
add("valSfRandNmiMed", r3(median(sfr[, "nmi"])), "results$shortform_null random sets median NMI")
add("valSfRandNmiHi",  r3(quantile(sfr[, "nmi"], .95)), "results$shortform_null random sets 95th percentile NMI")
add("valSfRandNmiMax", r3(max(sfr[, "nmi"])), "results$shortform_null random sets max NMI")
add("valSfRandAriMed", r3(median(sfr[, "ari"])), "results$shortform_null random sets median ARI")
add("valSfRandAriHi",  r3(quantile(sfr[, "ari"], .95)), "results$shortform_null random sets 95th percentile ARI")
add("valSfRandAriMax", r3(max(sfr[, "ari"])), "results$shortform_null random sets max ARI")
# the prose says the short form beat every random set on both metrics
stopifnot(sfn$short_form_5[["nmi"]] > max(sfr[, "nmi"]),
          sfn$short_form_5[["ari"]] > max(sfr[, "ari"]))
## (g) NLI structure
ns <- res$nli_structure
add("valNliOrdersR", r2(ns[["r_orders"]]), "results$nli_structure r between the two orders of each pair")
add("valNliSameKey", r2(ns[["mean_within_same_key"]]), "results$nli_structure mean, within-domain same-keyed pairs")
add("valNliOppKey",  r2(ns[["mean_within_opp_key"]]),  "results$nli_structure mean, within-domain opposite-keyed pairs")
add("valNliCrossNegPct", sprintf("%.0f", 100 * ns[["share_cross_negative"]]), "results$nli_structure percent of cross-domain pairs negative")
add("valNliCrossStrongPct", sprintf("%.0f", 100 * ns[["share_cross_below_m50"]]), "results$nli_structure percent of cross-domain pairs below -.5")
stopifnot(res$nli_symmetrization_diff < 1e-6)
## (h) the sign-flipped cosines of opposite-keyed pairs
ao <- res$atomic_reversed_opposites
add("valArOppPct", sprintf("%.0f", 100 * ao[["e1e2_percentile"]]), "results$atomic_reversed_opposites percentile of E1-E2")
add("valArOppMean", r2(ao[["mean_opp_same"]]), "results$atomic_reversed_opposites mean of opposite-keyed same-domain pairs")
## (i) the comparison-data rule over five subsamples at n = 250 and n = 500
cdr <- res$cd_replicates
add("valCdRepLowMin", wd(min(cdr[, "n250"])), "results$cd_replicates n = 250 minimum (word)")
add("valCdRepLowMax", wd(max(cdr[, "n250"])), "results$cd_replicates n = 250 maximum (word)")
add("valCdRepMidMin", wd(min(cdr[, "n500"])), "results$cd_replicates n = 500 minimum (word)")
add("valCdRepMidMax", wd(max(cdr[, "n500"])), "results$cd_replicates n = 500 maximum (word)")
## (j) the depth grid
dg <- res$dimselect_grid
add("valDepthNEval", as.character(nrow(dg)), "results$dimselect_grid number of depths evaluated")
add("valDepthStep",  as.character(stats::median(diff(dg$depth))), "results$dimselect_grid grid spacing")
add("valDepthNmiMin", r3(min(dg$nmi)), "results$dimselect_grid minimum NMI over depths")
add("valDepthNmiMax", r3(max(dg$nmi)), "results$dimselect_grid maximum NMI over depths")
stopifnot(abs(max(dg$nmi) - ds_opt$nmi) < 1e-9)
## the grid is every valDepthStep-th depth from the smallest, plus the full
## embedding appended at the end
stopifnot(max(dg$depth) == ds$full_dim,
          all(diff(dg$depth)[-(nrow(dg) - 1)] == stats::median(diff(dg$depth))))
add("valDepthMin", as.character(min(dg$depth)), "results$dimselect_grid smallest depth")
add("valDepthGridN", as.character(nrow(dg) - 1), "results$dimselect_grid depths on the regular grid (full embedding excluded)")
add("valDepthGridLast", cm(dg$depth[nrow(dg) - 1]), "results$dimselect_grid largest depth on the regular grid")

## --- checks added after validation round 3 ------------------------------------
## calibration: rotation warnings counted per replicate in the replication
cw <- res$calibration_warnings
stopifnot(!is.null(cw), length(cw) == 100)
add("valCalWarnReps", wd(sum(cw > 0)), "results$calibration_warnings replicates with any rotation warning (word)")
add("valCalWarnTotal", as.character(sum(cw)), "results$calibration_warnings total rotation warnings in the replication")
stopifnot(sum(cw > 0) == 100 - res$calibration_converged)
## prose guard: every transcript warning is accounted for by the replication
stopifnot(sum(cw) == as.integer(sub("^\\s*(\\d+)x : .*", "\\1", rout[rot_hit])))
## (k) item pairs by domain and keying
pg <- res$pair_by_group
stopifnot(identical(rownames(pg), c("same_domain_same_key", "same_domain_opp_key", "cross_domain")),
          sum(pg[, "n"]) == 1225)
add("valPairSameKeyN",   as.character(pg["same_domain_same_key", "n"]), "results$pair_by_group same domain, keyed alike: pairs")
add("valPairSameKeySim", r2(pg["same_domain_same_key", "mean_sim"]), "results$pair_by_group same domain, keyed alike: mean similarity")
add("valPairSameKeyR",   r2(pg["same_domain_same_key", "mean_human_r"]), "results$pair_by_group same domain, keyed alike: mean raw-response r")
add("valPairOppKeyN",    as.character(pg["same_domain_opp_key", "n"]), "results$pair_by_group same domain, keyed oppositely: pairs")
add("valPairOppKeySim",  r2(pg["same_domain_opp_key", "mean_sim"]), "results$pair_by_group same domain, keyed oppositely: mean similarity")
add("valPairOppKeyR",    r2(pg["same_domain_opp_key", "mean_human_r"]), "results$pair_by_group same domain, keyed oppositely: mean raw-response r")
add("valPairOppKeyNegPct", sprintf("%.0f", 100 * pg["same_domain_opp_key", "share_negative"]), "results$pair_by_group same domain, keyed oppositely: percent with negative raw-response r")
add("valPairCrossN",     cm(pg["cross_domain", "n"]), "results$pair_by_group different domains: pairs")
add("valPairCrossSim",   r2(pg["cross_domain", "mean_sim"]), "results$pair_by_group different domains: mean similarity")
add("valPairCrossR",     r2(pg["cross_domain", "mean_human_r"]), "results$pair_by_group different domains: mean raw-response r")
## prose guards: opposite-keyed same-domain pairs are about as similar as pairs
## from different domains, clearly less similar than same-domain pairs keyed
## alike, and correlate negatively in the responses
stopifnot(abs(pg["same_domain_opp_key", "mean_sim"] - pg["cross_domain", "mean_sim"]) < .03,
          pg["same_domain_same_key", "mean_sim"] - pg["same_domain_opp_key", "mean_sim"] > .05,
          pg["same_domain_opp_key", "mean_human_r"] < 0)
## (l) item-level bootstrap intervals for the curvature and the keying gap
sci <- res$pair_shape_ci
psh2 <- res$pair_level_shape
add("valPairGain",   r2(psh2[["r2_quadratic"]] - psh2[["r2_linear"]]), "results$pair_level_shape quadratic minus linear R2")
add("valPairGainLo", r2(sci[1, "r2_gain"]), "results$pair_shape_ci curvature gain 2.5th percentile")
add("valPairGainHi", r2(sci[2, "r2_gain"]), "results$pair_shape_ci curvature gain 97.5th percentile")
add("valPairGap",    r2(pg["same_domain_same_key", "mean_sim"] - pg["same_domain_opp_key", "mean_sim"]),
    "results$pair_by_group same-domain similarity, keyed alike minus keyed oppositely")
add("valPairGapLo",  r2(sci[1, "same_minus_opp"]), "results$pair_shape_ci keying gap 2.5th percentile")
add("valPairGapHi",  r2(sci[2, "same_minus_opp"]), "results$pair_shape_ci keying gap 97.5th percentile")
## opposite-keyed same-domain pairs minus cross-domain pairs: point and interval
add("valPairOppCrossGap", r3(pg["same_domain_opp_key", "mean_sim"] - pg["cross_domain", "mean_sim"]),
    "results$pair_by_group opposite-keyed minus cross-domain mean similarity")
add("valPairOppCrossLo", r3(sci[1, "opp_minus_cross"]), "results$pair_shape_ci opposite-keyed minus cross-domain 2.5th percentile")
add("valPairOppCrossHi", r3(sci[2, "opp_minus_cross"]), "results$pair_shape_ci opposite-keyed minus cross-domain 97.5th percentile")
## the prose says this interval includes zero
stopifnot(sci[1, "opp_minus_cross"] < 0, sci[2, "opp_minus_cross"] > 0)
## (m) parallel-analysis verdicts under ten further seeds
pas <- res$pa_seeds
reported_pa <- sapply(c("0.6B", "4B", "8B"), function(m) mc_get(m, "parallel"))
for (m in c("0.6B", "4B", "8B")) {
  tag <- c(`0.6B` = "Small", `4B` = "Medium", `8B` = "Large")[[m]]
  add(paste0("valPaSeed", tag, "Min"), wd(min(pas[, m])), sprintf("results$pa_seeds %s minimum verdict over ten seeds (word)", m))
  add(paste0("valPaSeed", tag, "Max"), wd(max(pas[, m])), sprintf("results$pa_seeds %s maximum verdict over ten seeds (word)", m))
  add(paste0("valPaSeed", tag, "Same"), wd(sum(pas[, m] == reported_pa[[m]])), sprintf("results$pa_seeds %s seeds giving the reported verdict (word)", m))
}
## prose guard: "the same verdict in every run" for each model size
stopifnot(all(sapply(c("0.6B", "4B", "8B"), function(m) all(pas[, m] == reported_pa[[m]]))))
add("valPaSmallFourObs", r3(pat[["0.6B"]]["observed", 4]), "results$pa_thresholds 0.6B fourth observed eigenvalue")
add("valPaSmallFourThr", r3(pat[["0.6B"]]["threshold", 4]), "results$pa_thresholds 0.6B fourth threshold")
stopifnot(pat[["0.6B"]]["observed", 4] <= pat[["0.6B"]]["threshold", 4])
## (n) nested human subsamples
stopifnot(isTRUE(res$cd_nested))
## item map: share of variance on the two principal components shown
pcv <- res$itemmap_pc_variance
add("valPcaVarOne", sprintf("%.0f", 100 * pcv[[1]]), "results$itemmap_pc_variance PC1 percent")
add("valPcaVarTwo", sprintf("%.0f", 100 * pcv[[2]]), "results$itemmap_pc_variance PC2 percent")
## separation of the domains in the item maps: share of each item's five
## nearest neighbors from its own domain (results$itemmap_purity)
pur <- res$itemmap_purity
stopifnot(isTRUE(all.equal(pur[, "pca"], pur[, "mds"])))   # the linear maps are mirror images
add("valPurityTsne", r2(pur["overall", "tsne"]), "results$itemmap_purity t-SNE, all items")
add("valPurityUmap", r2(pur["overall", "umap"]), "results$itemmap_purity UMAP, all items")
add("valPurityLin",  r2(pur["overall", "pca"]),  "results$itemmap_purity PCA and MDS, all items")
add("valPurityNTsne", r2(pur["Neuroticism", "tsne"]), "results$itemmap_purity t-SNE, Neuroticism items")
add("valPurityNUmap", r2(pur["Neuroticism", "umap"]), "results$itemmap_purity UMAP, Neuroticism items")
add("valPurityNLin",  r2(pur["Neuroticism", "pca"]),  "results$itemmap_purity PCA and MDS, Neuroticism items")
add("valPurityChance", r2(9 / 49), "share expected by chance: nine other items of the domain among 49")
## the prose says Neuroticism is the most separated domain in the linear maps
## and that the nonlinear maps separate the domains more than the linear ones
stopifnot(which.max(pur[rownames(pur) != "overall", "pca"]) ==
            which(rownames(pur)[rownames(pur) != "overall"] == "Neuroticism"),
          min(pur["overall", c("tsne", "umap")]) > pur["overall", "pca"])
## omega for the Openness-, Neuroticism-, and Conscientiousness-matched factors
mrO <- omega$factor[omega$matched_theoretical == "Openness"][1]
mrN <- omega$factor[omega$matched_theoretical == "Neuroticism"][1]
mrC <- omega$factor[omega$matched_theoretical == "Conscientiousness"][1]
add("valOmegaAsgnO", r3(om(mrO, "omega_assigned")),    "results omega, O-matched factor assigned")
add("valOmegaTheoO", r3(om(mrO, "omega_theoretical")), "results omega, O-matched factor theoretical")
add("valOmegaAsgnN", r3(om(mrN, "omega_assigned")),    "results omega, N-matched factor assigned")
add("valOmegaTheoN", r3(om(mrN, "omega_theoretical")), "results omega, N-matched factor theoretical")
add("valOmegaAsgnC", r3(om(mrC, "omega_assigned")),    "results omega, C-matched factor assigned")
add("valOmegaTheoC", r3(om(mrC, "omega_theoretical")), "results omega, C-matched factor theoretical")
## the prose says the theoretical Openness set scores slightly higher
stopifnot(om(mrO, "omega_theoretical") > om(mrO, "omega_assigned"))
## near ties in the item-to-factor assignment (largest minus second-largest
## absolute loading below .03)
sl_csv <- read.csv("output/semantic_loadings.csv", check.names = FALSE)
sl_abs <- abs(as.matrix(sl_csv[, setdiff(names(sl_csv), c("code", "factor"))]))
sl_margin <- apply(sl_abs, 1, function(v) { v <- sort(v, decreasing = TRUE); v[1] - v[2] })
add("valAsgnNearTieN", wd(sum(sl_margin < .03)), "output/semantic_loadings.csv items whose two largest |loadings| differ by < .03 (word)")
## the prose names the near-tie items
stopifnot(identical(sort(sl_csv$code[sl_margin < .03]), c("A24", "E10", "N14", "O46")))
## margins behind the domain labels of the two blended factors: mean absolute
## loading on the matched domain's items minus that on the runner-up domain,
## which the prose says is the other blended domain
for (d in c("Agreeableness", "Extraversion")) {
  f <- names(sem_dom)[sem_dom == d]
  v <- sort(tapply(abs(sl_csv[[f]]), sl_csv$factor, mean), decreasing = TRUE)
  stopifnot(names(v)[1] == d,
            names(v)[2] == setdiff(c("Agreeableness", "Extraversion"), d))
  add(sprintf("valLabelMargin%s", abbr[[d]]), r3(v[[1]] - v[[2]]),
      sprintf("mean |loading| margin behind the %s label of %s", d, f))
}
## the short form: random five-per-domain sets already exceed the full form
stopifnot(median(sfr[, "nmi"]) > sf$nmi_full)
## Python side of the run and the NLI cross-encoder revision, from the transcript
pv_i <- grep("^> print\\(py_versions\\)", rout)
stopifnot(length(pv_i) == 1)
pv_end <- pv_i + which(grepl("^[>+] ", rout[(pv_i + 1):length(rout)]))[1] - 1
pv_txt <- rout[(pv_i + 1):pv_end]
pv_vals <- gsub('"', "", unlist(regmatches(pv_txt, gregexpr('"[^"]+"', pv_txt))))
stopifnot(length(pv_vals) == 5)
add("valPyPython", pv_vals[1], "reproduce.Rout Python version")
add("valPyNumpy", pv_vals[2], "reproduce.Rout numpy version")
add("valPyTorch", pv_vals[3], "reproduce.Rout torch version")
add("valPyTransformers", pv_vals[4], "reproduce.Rout transformers version")
add("valPySentTrans", pv_vals[5], "reproduce.Rout sentence-transformers version")
nli_rev <- sub("^NLI cross-encoder revision: *([0-9a-f]+).*", "\\1", grab1("^NLI cross-encoder revision:"))
stopifnot(grepl("^[0-9a-f]{40}$", nli_rev))
add("valNliRevision", nli_rev, "reproduce.Rout NLI cross-encoder Hugging Face revision")
## item-embedding provenance: output of data-raw/embed_items.py for the three
## sizes (archived as output/embed_items_check.txt), with the model revisions
## and the cosine between each re-embedded and stored item vector
ec <- readLines("output/embed_items_check.txt", warn = FALSE)
ec_rev <- sub(".*Qwen3-Embedding-([0-9.]+B) loaded from local snapshot, revision ([0-9a-f]{40}).*",
              "\\1 \\2", grep("loaded from local snapshot, revision", ec, value = TRUE))
ec_rev <- setNames(sub("^\\S+ ", "", ec_rev), sub(" .*$", "", ec_rev))
stopifnot(identical(sort(names(ec_rev)), c("0.6B", "4B", "8B")),
          all(grepl("^[0-9a-f]{40}$", ec_rev)))
add("valRevSmall",  ec_rev[["0.6B"]], "output/embed_items_check.txt Qwen3-Embedding-0.6B revision")
add("valRevMedium", ec_rev[["4B"]],   "output/embed_items_check.txt Qwen3-Embedding-4B revision")
add("valRevLarge",  ec_rev[["8B"]],   "output/embed_items_check.txt Qwen3-Embedding-8B revision")
ec_cos <- as.numeric(sub(".*cosine\\(new, stored\\): min ([0-9.]+),.*", "\\1",
                         grep("cosine\\(new, stored\\): min", ec, value = TRUE)))
stopifnot(length(ec_cos) == 3, all(ec_cos > 0.9999))
add("valEmbCheckMinCos", strip0(sprintf("%.6f", floor(min(ec_cos) * 1e6) / 1e6)),
    "output/embed_items_check.txt smallest cosine between re-embedded and stored item vectors (rounded down)")

## --- the DASS as a second scale for the model-size comparison (Section 11c) --
dm <- res$dass_modelsize
stopifnot(!is.null(dm), identical(rownames(dm$separation), c("0.6B", "4B", "8B")))
SZ <- c("0.6B" = "Small", "4B" = "Medium", "8B" = "Large")
DS <- c(Dep = "Depression", Anx = "Anxiety", Str = "Stress")
apa_n <- function(n) if (n < 10) wd(n) else as.character(n)   # APA: 10 and above as digits
dass_gap <- dm$separation[, "within"] - dm$separation[, "between"]
for (m in names(SZ)) {
  X <- SZ[[m]]
  add(sprintf("valDassRpb%s", X), r3(dm$separation[m, "rpb"]), sprintf("results$dass_modelsize separation %s point-biserial", m))
  add(sprintf("valDassRpbCi%sLo", X), r3(dm$ci[paste0(m, ".rpb"), 1]), sprintf("results$dass_modelsize ci %s rpb 2.5th percentile", m))
  add(sprintf("valDassRpbCi%sHi", X), r3(dm$ci[paste0(m, ".rpb"), 2]), sprintf("results$dass_modelsize ci %s rpb 97.5th percentile", m))
  add(sprintf("valDassGap%s", X), r3(dass_gap[[m]]), sprintf("results$dass_modelsize separation %s within minus between", m))
  add(sprintf("valDassNmi%s", X), r3(dm$agreement[m, "nmi"]), sprintf("results$dass_modelsize agreement %s NMI", m))
  add(sprintf("valDassAri%s", X), r3(dm$agreement[m, "ari"]), sprintf("results$dass_modelsize agreement %s ARI", m))
  add(sprintf("valBigNmi%s", X), r3(dm$big5_agreement[m, "nmi"]), sprintf("results$dass_modelsize big5_agreement %s NMI", m))
  add(sprintf("valBigAri%s", X), r3(dm$big5_agreement[m, "ari"]), sprintf("results$dass_modelsize big5_agreement %s ARI", m))
  for (k in names(DS))
    add(sprintf("valDass%sGap%s", k, X), r3(dm$domain_gap[DS[[k]], m]),
        sprintf("results$dass_modelsize domain_gap %s %s", DS[[k]], m))
  rt <- dm$retention[[m]]
  add(sprintf("valDassPaK%sWord", X), wd(rt$methods$n_factors[rt$methods$method == "parallel"]),
      sprintf("results$dass_modelsize retention %s parallel", m))
  add(sprintf("valDassCons%sWord", X), wd(rt$consensus), sprintf("results$dass_modelsize retention %s consensus", m))
  ## each scale's items on the factor that holds most of them
  tb <- dm$assignment[[m]]
  for (k in names(DS))
    add(sprintf("valDass%s%sModal", X, k), apa_n(max(tb[DS[[k]], ])),
        sprintf("results$dass_modelsize assignment %s %s on its modal factor", m, DS[[k]]))
}
## the 8B main fit's agreement repeats the main analysis
stopifnot(isTRUE(all.equal(dm$big5_agreement["8B", "nmi"], ch$nmi)))
## differences between sizes (paired item-level bootstrap)
add("valDassGapDiff",   r3(dass_gap[["8B"]] - dass_gap[["0.6B"]]), "results$dass_modelsize 8B minus 0.6B gap")
add("valDassGapDiffLo", r3(dm$ci["8B-0.6B.gap", 1]), "results$dass_modelsize ci 8B-0.6B gap 2.5th percentile")
add("valDassGapDiffHi", r3(dm$ci["8B-0.6B.gap", 2]), "results$dass_modelsize ci 8B-0.6B gap 97.5th percentile")
add("valDassRpbDiffLo", r3(dm$ci["8B-0.6B.rpb", 1]), "results$dass_modelsize ci 8B-0.6B rpb 2.5th percentile")
add("valDassRpbDiffHi", r3(dm$ci["8B-0.6B.rpb", 2]), "results$dass_modelsize ci 8B-0.6B rpb 97.5th percentile")
add("valBigGapDiffLo",  r3(dm$big5_ci["8B-0.6B.gap", 1]), "results$dass_modelsize big5_ci 8B-0.6B gap 2.5th percentile")
add("valBigGapDiffHi",  r3(dm$big5_ci["8B-0.6B.gap", 2]), "results$dass_modelsize big5_ci 8B-0.6B gap 97.5th percentile")
add("valDassPermNmi", r3(dm$perm_max[["nmi"]]), "results$dass_modelsize perm_max NMI (1,000 permutations)")
add("valDassPermAri", r3(dm$perm_max[["ari"]]), "results$dass_modelsize perm_max ARI (1,000 permutations)")
add("valDassFloor", sprintf("%.1f", dm$color_floor), "results$dass_modelsize color_floor (same form as valMsFloor)")
add("valDassNPairs", cm(choose(sum(dm$assignment[["8B"]]), 2)), "choose(42, 2) DASS item pairs")
## the 0.6B solution's shared factor: the Stress items on the factor that
## holds most Anxiety items
tb06 <- dm$assignment[["0.6B"]]
add("valDassSmallStrOnAnx", apa_n(tb06["Stress", which.max(tb06["Anxiety", ])]),
    "results$dass_modelsize assignment 0.6B Stress items on the modal Anxiety factor")
## seed check: the 0.6B DASS rotation has a second solution; the reported one
## (seed 42) has the lower oblimin criterion, and every other fit has one
sc <- dm$seed_check
sc06 <- sc[["DASS 0.6B"]]
stopifnot(all(sapply(sc, function(x) sum(x$seeds)) == 40),
          nrow(sc06) == 2, all(sapply(sc[names(sc) != "DASS 0.6B"], nrow) == 1))
rep06 <- which(abs(sc06$nmi - dm$agreement["0.6B", "nmi"]) < 1e-9)
alt06 <- setdiff(1:2, rep06)
alt_tab <- dm$small_alt_assignment[[alt06]]
stopifnot(length(rep06) == 1,
          abs(sc06$criterion[rep06] - dm$small_criterion) < 1e-4,
          sc06$criterion[rep06] < sc06$criterion[alt06],
          ## every other fit gave the reported solution under all seeds
          isTRUE(all.equal(sapply(paste("DASS", c("4B", "8B")), function(k) sc[[k]]$nmi),
                           unname(dm$agreement[c("4B", "8B"), "nmi"]), check.attributes = FALSE)),
          isTRUE(all.equal(sapply(paste("Big Five", names(SZ)), function(k) sc[[k]]$nmi),
                           unname(dm$big5_agreement[names(SZ), "nmi"]), check.attributes = FALSE)))
add("valDassSeedN", sum(sc06$seeds), "results$dass_modelsize seed_check seeds per fit")
add("valDassSmallSeedsRep", apa_n(sc06$seeds[rep06]),
    "results$dass_modelsize seed_check DASS 0.6B seeds giving the reported solution")
add("valDassSmallSeedsAlt", apa_n(sc06$seeds[alt06]),
    "results$dass_modelsize seed_check DASS 0.6B seeds giving the second solution")
add("valDassCritSmall", sprintf("%.3f", sc06$criterion[rep06]),
    "results$dass_modelsize seed_check DASS 0.6B oblimin criterion, reported solution")
add("valDassCritAlt", sprintf("%.3f", sc06$criterion[alt06]),
    "results$dass_modelsize seed_check DASS 0.6B oblimin criterion, second solution")
add("valDassAltNmi", r3(sc06$nmi[alt06]), "results$dass_modelsize seed_check DASS 0.6B second solution NMI")
add("valDassAltAri", r3(sc06$ari[alt06]), "results$dass_modelsize seed_check DASS 0.6B second solution ARI")
add("valDassAltAnxModal", apa_n(max(alt_tab["Anxiety", ])),
    "results$dass_modelsize small_alt_assignment Anxiety on its modal factor")
add("valDassAltStrOnAnx", apa_n(alt_tab["Stress", which.max(alt_tab["Anxiety", ])]),
    "results$dass_modelsize small_alt_assignment Stress items on the modal Anxiety factor")
## prose guards for the Supplement's DASS section and the main-text mention
dsep <- dm$separation
stopifnot(
  all(diff(dass_gap) > 0),                                   # the gap rose with size
  dm$ci["8B-0.6B.gap", 1] <= 0, dm$ci["8B-0.6B.gap", 1] > -0.01,   # only just includes zero
  dm$big5_ci["8B-0.6B.gap", 1] < 0, dm$big5_ci["8B-0.6B.gap", 2] > 0,
  all(diff(dsep[, "rpb"]) > 0), diff(range(dsep[, "rpb"])) < 0.05,  # rose only slightly
  max(dm$ci[paste0(names(SZ), ".rpb"), 1]) < min(dm$ci[paste0(names(SZ), ".rpb"), 2]),
  all(diff(dm$domain_gap["Depression", ]) > 0), all(diff(dm$domain_gap["Stress", ]) > 0),
  all(diff(dm$domain_gap["Anxiety", ]) < 0),
  all(diff(dm$agreement[, "nmi"]) > 0), all(diff(dm$agreement[, "ari"]) > 0),
  min(dm$agreement[, "nmi"]) > dm$perm_max[["nmi"]], min(dm$agreement[, "ari"]) > dm$perm_max[["ari"]],
  which.max(dm$big5_agreement[, "nmi"]) == 2, which.max(dm$big5_agreement[, "ari"]) == 2,
  length(unique(sapply(dm$retention, function(r) r$methods$n_factors[r$methods$method == "parallel"]))) == 1,
  all(sapply(dm$retention, function(r) r$consensus != 3 &&
                 r$methods$n_factors[r$methods$method == "parallel"] != 3)),
  ## 0.6B: the modal Anxiety factor, which holds most Anxiety items, also
  ## holds all 14 Stress items; 8B: three distinct factors
  max(tb06["Anxiety", ]) > 7, tb06["Stress", which.max(tb06["Anxiety", ])] == 14,
  length(unique(apply(dm$assignment[["8B"]], 1, which.max))) == 3,
  ## the second 0.6B solution: all Anxiety items on one factor with most
  ## Stress items, agreement higher than the reported 0.6B solution but below 4B
  max(alt_tab["Anxiety", ]) == 14, which.max(alt_tab["Anxiety", ]) == which.max(alt_tab["Stress", ]),
  alt_tab["Stress", which.max(alt_tab["Anxiety", ])] > 7,
  sc06$nmi[alt06] > sc06$nmi[rep06], sc06$ari[alt06] > sc06$ari[rep06],
  sc06$nmi[alt06] < dm$agreement["4B", "nmi"], sc06$ari[alt06] < dm$agreement["4B", "ari"])
## provenance of the DASS vectors: data-raw/embed_items.py at the same revisions
ecd <- readLines("output/embed_items_check_dass.txt", warn = FALSE)
ecd_rev <- sub(".*Qwen3-Embedding-([0-9.]+B) loaded from local snapshot, revision ([0-9a-f]{40}).*",
               "\\1 \\2", grep("loaded from local snapshot, revision", ecd, value = TRUE))
ecd_rev <- setNames(sub("^\\S+ ", "", ecd_rev), sub(" .*$", "", ecd_rev))
stopifnot(identical(ecd_rev[names(ec_rev)], ec_rev))
ecd_cos <- as.numeric(sub(".*cosine\\(new, stored\\): min ([0-9.]+),.*", "\\1",
                          grep("cosine\\(new, stored\\): min", ecd, value = TRUE)))
stopifnot(length(ecd_cos) == 3, all(ecd_cos > 0.9999))
## the prose says every cosine is 1 to six decimal places
stopifnot(round(min(ecd_cos), 6) == 1)
add("valDassEmbCheckCos", sprintf("%.6f", round(min(ecd_cos), 6)),
    "output/embed_items_check_dass.txt smallest cosine between re-embedded and stored DASS item vectors (six decimals)")

writeLines(unlist(L), file.path(gen_dir, "values.tex"))

## --- item-to-factor assignment by theoretical domain (main fit) --------------
dom_full <- c("Openness", "Conscientiousness", "Extraversion",
              "Agreeableness", "Neuroticism")
asg <- res$assignment
fac_order <- names(sem_dom)[match(dom_full, sem_dom)]
stopifnot(!anyNA(fac_order), setequal(fac_order, rownames(asg)))
asg <- asg[fac_order, dom_full, drop = FALSE]
asg_rows <- vapply(seq_len(nrow(asg)), function(i) {
  cells <- as.character(asg[i, ])
  cells[i] <- sprintf("\\textbf{%s}", cells[i])
  sprintf("%s (%s) & %s & %d & %d \\\\", rownames(asg)[i], abbr[sem_dom[rownames(asg)[i]]],
          paste(cells, collapse = " & "), sum(asg[i, ]), asg_rev[[rownames(asg)[i]]])
}, "")
writeLines(c("% AUTOGENERATED by reproduce/make_paper_assets.R. Do not edit.",
             "\\begin{tabular}{lrrrrrrr}", "\\toprule",
             "& \\multicolumn{5}{c}{Theoretical domain} & & \\\\",
             "\\cmidrule(lr){2-6}",
             "Factor (matched domain) & O & C & E & A & N & Items & Reverse-keyed \\\\",
             "\\midrule", asg_rows, "\\bottomrule", "\\end{tabular}"),
           file.path(gen_dir, "table_assignment_body.tex"))
cat("values.tex:", length(L) - 2, "macros\n")

## --- Table: Tucker congruence (body) -----------------------------------------

## columns in the conventional O C E A N domain order, rows matched to it
dom_order <- c("Openness", "Conscientiousness", "Extraversion",
               "Agreeableness", "Neuroticism")
stopifnot(identical(hum_mr, colnames(tuck)))
col_order <- hum_mr[match(dom_order, hum_dom[hum_mr])]
stopifnot(!anyNA(col_order))
tuck    <- tuck[, col_order]
hum_mr  <- col_order
hum_dom <- hum_dom[col_order]
hdr <- paste0("Semantic factor & ",
              paste(sprintf("%s (%s)", abbr[hum_dom], hum_mr), collapse = " & "),
              " \\\\")
## rows emitted in the COLUMN-domain order, so each semantic factor's best
## match (bold) sits on the visual diagonal of the cross-matrix
row_order <- rownames(tuck)[match(abbr[hum_dom], abbr[sem_dom[rownames(tuck)]])]
stopifnot(!anyNA(row_order))
tuck_rows <- vapply(row_order, function(rn) {
  vals <- tuck[rn, ]
  cells <- vapply(seq_along(vals), function(j) {
    cell <- texneg(r2(vals[j]))
    if (abs(vals[j]) == max(abs(vals))) cell <- sprintf("\\textbf{%s}", cell)
    cell
  }, "")
  sprintf("%s (%s) & %s \\\\", abbr[sem_dom[rn]], rn, paste(cells, collapse = " & "))
}, "")
writeLines(c(
  "% AUTOGENERATED by reproduce/make_paper_assets.R from output/results.rds. Do not edit.",
  "\\begin{tabular}{lrrrrr}",
  "\\toprule",
  "& \\multicolumn{5}{c}{Human factor} \\\\",
  "\\cmidrule(lr){2-6}",
  hdr,
  "\\midrule",
  tuck_rows,
  "\\bottomrule",
  "\\end{tabular}"), file.path(gen_dir, "table_tucker_body.tex"))

## --- Table: encoding comparison (body) ---------------------------------------

enc_names <- c(atomic = "atomic", atomic_reversed = "atomic\\_reversed",
               squid = "squid", mean_centered_pearson = "mean\\_centered\\_pearson",
               nli = "nli")
enc_cell <- function(x, fmt) if (is.na(x)) "--" else fmt(x)
enc_rows <- vapply(rownames(enc), function(rn) {
  e <- enc[rn, ]
  sprintf("%s & %s & %s & %s & %s & %s & %s & %s & $%s$ & %s & %s \\\\",
          enc_names[[rn]],
          r3(e[["NMI_theory"]]), r3(e[["ARI_theory"]]), r3(e[["Tucker_human"]]),
          r3(e[["r_keyed"]]), r3(e[["r_raw"]]),
          enc_cell(e[["Disattenuated_keyed"]], r3),
          r3(e[["KMO"]]), f2(e[["TEFI"]]), r3(e[["RMSR"]]), r3(e[["CAF"]]))
}, "")
writeLines(c(
  "% AUTOGENERATED by reproduce/make_paper_assets.R from output/results.rds. Do not edit.",
  "\\begin{tabular}{lrrrrrrrrrr}",
  "\\toprule",
  "& \\multicolumn{2}{c}{Theory} & \\multicolumn{4}{c}{Human data} &",
  "\\multicolumn{4}{c}{Diagnostics} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-7} \\cmidrule(lr){8-11}",
  "Encoding or matrix & NMI & ARI & $\\bar{\\phi}$ & $r_{\\text{keyed}}$ &",
  "$r_{\\text{raw}}$ & $r_{\\text{dis}}$ & KMO & TEFI & RMSR & CAF \\\\",
  "\\midrule",
  enc_rows,
  "\\bottomrule",
  "\\end{tabular}"), file.path(gen_dir, "table_encodings_body.tex"))

## --- Supplementary table: model size for the Big Five and the DASS (body) -----

ms_row <- function(scale, m, rpb, lo, hi, gap, nmi, ari, cons)
  sprintf("%s & %s & %s & [%s, %s] & %s & %s & %s & %s \\\\",
          scale, m, rpb, lo, hi, gap, nmi, ari, cons)
big5_sep <- res$modelsize_separation
big5_rows <- vapply(names(SZ), function(m) ms_row(
  if (m == "0.6B") "Big Five" else "", m,
  r3(big5_sep[m, "rpb"]), r3(dm$big5_ci[paste0(m, ".rpb"), 1]), r3(dm$big5_ci[paste0(m, ".rpb"), 2]),
  r3(big5_sep[m, "within"] - big5_sep[m, "between"]),
  r3(dm$big5_agreement[m, "nmi"]), r3(dm$big5_agreement[m, "ari"]),
  mc[[m]]$consensus), "")
dass_rows <- vapply(names(SZ), function(m) ms_row(
  if (m == "0.6B") "DASS" else "", m,
  r3(dm$separation[m, "rpb"]), r3(dm$ci[paste0(m, ".rpb"), 1]), r3(dm$ci[paste0(m, ".rpb"), 2]),
  r3(dass_gap[[m]]), r3(dm$agreement[m, "nmi"]), r3(dm$agreement[m, "ari"]),
  dm$retention[[m]]$consensus), "")
writeLines(c(
  "% AUTOGENERATED by reproduce/make_paper_assets.R from output/results.rds. Do not edit.",
  "\\begin{tabular}{llrcrrrr}",
  "\\toprule",
  "Scale & Model & $r_{\\text{pb}}$ & 95\\% interval & Gap & NMI & ARI & Consensus \\\\",
  "\\midrule",
  big5_rows,
  "\\midrule",
  dass_rows,
  "\\bottomrule",
  "\\end{tabular}"), file.path(gen_dir, "table_modelsize_body.tex"))

## --- Supplementary longtables: full loading matrices --------------------------

loading_longtable <- function(csv, caption, label, note) {
  d  <- read.csv(csv, check.names = FALSE)
  mr <- setdiff(names(d), c("code", "factor"))
  rows <- vapply(seq_len(nrow(d)), function(i) {
    cells <- vapply(mr, function(m) texneg(r3(d[i, m])), "")
    top <- which.max(abs(unlist(d[i, mr])))
    cells[top] <- sprintf("\\textbf{%s}", cells[top])
    sprintf("%s & %s & %s \\\\", d$code[i], d$factor[i],
            paste(cells, collapse = " & "))
  }, "")
  c("% AUTOGENERATED by reproduce/make_paper_assets.R. Do not edit.",
    "{\\renewcommand{\\baselinestretch}{1}\\footnotesize",
    "\\begin{longtable}{llrrrrr}",
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{%s}\\\\", label),
    "\\toprule",
    sprintf("Item & Domain & %s \\\\", paste(mr, collapse = " & ")),
    "\\midrule",
    "\\endfirsthead",
    sprintf("\\multicolumn{7}{l}{Table~\\ref{%s} (continued)}\\\\", label),
    "\\toprule",
    sprintf("Item & Domain & %s \\\\", paste(mr, collapse = " & ")),
    "\\midrule",
    "\\endhead",
    "\\bottomrule",
    "\\endfoot",
    "\\bottomrule",
    sprintf("\\multicolumn{7}{p{0.9\\linewidth}}{\\textit{Note}. %s}\\\\", note),
    "\\endlastfoot",
    rows,
    "\\end{longtable}}")
}

loading_note <- function(dom_map) paste(
  "Entries are oblimin pattern loadings, and the factors are allowed to",
  "correlate. Bold marks each item's largest absolute loading. Item codes",
  "give the item's domain initial and its position in the 50-item",
  "inventory, and Domain is the domain the inventory assigns it to.",
  "MR = minimum residual, the prefix of the psych package's automatic",
  "factor labels. Each factor is matched to the domain on whose items its",
  "mean absolute loading is largest (columns in table order):",
  paste0(paste(sprintf("%s = %s", names(dom_map), dom_map), collapse = ", "), "."))

writeLines(loading_longtable(
  "output/semantic_loadings.csv",
  paste("Factor loadings of the 50 International Personality Item Pool",
        "Big-Five Factor Marker items in the five-factor semantic",
        "solution (mean-centered Pearson encoding of the Qwen3-Embedding-8B",
        "embeddings, minimum residual extraction, oblimin rotation)"),
  "tab:s-semantic-loadings", loading_note(sem_dom[sl_mr])),
  file.path(gen_dir, "table_semantic_loadings.tex"))

writeLines(loading_longtable(
  "output/human_loadings.csv",
  paste("Factor loadings of the 50 International Personality Item Pool",
        "Big-Five Factor Marker items in the five-factor solution of the",
        "human responses (correlations of reverse-scored responses, minimum",
        "residual extraction, oblimin rotation,",
        sprintf("$N = %s$ respondents after exclusions)", cm(n_human))),
  "tab:s-human-loadings", loading_note(hum_dom[setdiff(names(hload), c("code", "factor"))])),
  file.path(gen_dir, "table_human_loadings.tex"))

## --- figures: stage under the manuscript's canonical names -------------------

fig_map <- c(fig_corplot.pdf          = "corplot.pdf",
             fig_corplot_nli.pdf      = "corplot_nli.pdf",
             fig_corplot_06B.pdf      = "corplot_06b.pdf",
             fig_corplot_4B.pdf       = "corplot_4b.pdf",
             fig_corplot_8B.pdf       = "corplot_8b.pdf",
             fig_itemmap.pdf          = "itemmap.pdf",
             fig_scree.pdf            = "scree.pdf",
             fig_loadings.pdf         = "loadings.pdf",
             fig_semantic_vs_human.pdf = "semantic_vs_human.pdf",
             fig_cd_profiles.pdf      = "cd_profiles.pdf",
             fig_depth.pdf            = "depth.pdf",
             fig_dass_corplot_06B.pdf = "dass_corplot_06b.pdf",
             fig_dass_corplot_4B.pdf  = "dass_corplot_4b.pdf",
             fig_dass_corplot_8B.pdf  = "dass_corplot_8b.pdf")
for (src in names(fig_map)) {
  from <- file.path("figures", src)
  if (!file.exists(from)) stop("missing figure: ", from)
  file.copy(from, file.path(fig_dir, fig_map[[src]]), overwrite = TRUE)
}
cat("Staged", length(fig_map), "figures into", fig_dir, "\n")
cat("Done.\n")
