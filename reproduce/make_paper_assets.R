## make_paper_assets.R -- stage the manuscript's numbers, tables, and figures
## from the reproduction outputs.
##
## Run from the reproduce/ directory, AFTER reproduce.R:
##   R CMD BATCH --vanilla reproduce.R
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

t_nli  <- sub(".* in ([0-9.]+) s.*", "\\1", grab1("^NLI matrix: 50 x 50 in"))
t_ds   <- sub(".*elapsed: ([0-9.]+) s.*", "\\1", grab1("^sfa_dimselect elapsed:"))
t_cal  <- sub(".*elapsed: ([0-9.]+) s.*", "\\1", grab1("^calibrate=TRUE elapsed:"))
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
add("valNPairs",     cm(choose(nrow(big5$embeddings), 2)), "choose(50, 2) item pairs")
add("valNRev",       sum(big5$scoring < 0), "data(big5), reverse-keyed items")
## encodings
add("valRangeCosMin", r3(enc_rng["atomic", "min"]), "results$encoding_ranges atomic min")
add("valRangeCosMax", r3(enc_rng["atomic", "max"]), "results$encoding_ranges atomic max")
add("valRangeArMin",  r3(enc_rng["atomic_reversed", "min"]), "results$encoding_ranges atomic_reversed min")
add("valRangeSqMin",  r3(enc_rng["squid", "min"]), "results$encoding_ranges squid min")
add("valNliOpp",      r2(res$nli_example["E1", "E2"]), "results$nli_example E1-E2")
add("valArOpp",       r2(ar_e1e2), "recomputed sign-flipped E1-E2 cosine, data(big5)")
add("valLiveCosMin",  r4(min(res$embed_live_vs_reference)), "results$embed_live_vs_reference min")
add("valTimeNli",     t_nli, "reproduce.Rout, NLI matrix timing (s)")
## retention
add("valEigFirst",   f2(nf$eigenvalues[1]), "results$nfactors eigenvalue 1")
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
add("valDepthEval",  nrow(ds$trajectory),  "results$dimselect depths evaluated")
add("valTimeDimselect", t_ds, "reproduce.Rout, sfa_dimselect timing (s)")
## model-size retention, per size
add("valPaKSmallWord",    wd(mc_get("0.6B", "parallel")), "results$model_comparison 0.6B parallel")
add("valPaKMediumWord",   wd(mc_get("4B", "parallel")),   "results$model_comparison 4B parallel")
add("valEgaKSmallWord",   wd(mc_get("0.6B", "EGA")),      "results$model_comparison 0.6B EGA")
add("valEgaKMediumWord",  wd(mc_get("4B", "EGA")),        "results$model_comparison 4B EGA")
add("valConsSmallWord",   wd(mc[["0.6B"]]$consensus),     "results$model_comparison 0.6B consensus")
add("valConsMediumWord",  wd(mc[["4B"]]$consensus),       "results$model_comparison 4B consensus")
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
add("valTimeCal",    t_cal, "reproduce.Rout, calibrate=TRUE timing (s)")
## interpretation
add("valAnchorNTwOwn",   r2(anc_c["N12", "Neuroticism"]),  "results$anchor_centroid N12 own")
add("valAnchorNTwRival", r2(anc_c["N12", "Extraversion"]), "results$anchor_centroid N12 rival")
add("valAnchorATwFiveOwn",  r2(anc_l["A25", "Agreeableness"]), "results$anchor_label A25 own")
add("valAnchorATwFiveNeur", r2(anc_l["A25", "Neuroticism"]),   "results$anchor_label A25 Neuroticism")
add("valProjNTwCalm", r2(prj["N12", "stability"]), "results$projection N12 stability")
add("valNmiTheory", r3(ch$nmi), "results$congruence_human nmi (= vs theory)")
add("valAriTheory", r3(ch$ari), "results$congruence_human ari (= vs theory)")
## refinement
add("valUvaPairsWord",    wd(nrow(uva$pairs)),     "results$redundancy_uva pairs")
add("valUvaClustersWord", wd(length(uva$clusters)), "results$redundancy_uva clusters")
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
add("valIfGap",     r2(ifit$summary$gap[1]),         "results$item_fit candidate 1 gap")
add("valIfNearest", r2(ifit$summary$nearest_sim[2]), "results$item_fit candidate 2 nearest")
add("valIfWeakSim", r2(ifit$summary$sim_items[3]),   "results$item_fit candidate 3 sim")
add("valIfWeakAvg", r2(ifit$avg_item_fit[[ifit$summary$best[3]]]), "results$item_fit candidate 3 domain avg")
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
add("valAtRKeyed",  r3(enc["atomic", "r_keyed"]),          "results$encoding_table atomic r_keyed")
add("valArRKeyed",  r3(enc["atomic_reversed", "r_keyed"]), "results$encoding_table atomic_reversed r_keyed")
add("valArRRaw",    r3(enc["atomic_reversed", "r_raw"]),   "results$encoding_table atomic_reversed r_raw")
add("valNliNmi",    r3(enc["nli", "NMI_theory"]),  "results$encoding_table nli NMI")
add("valNliAri",    r3(enc["nli", "ARI_theory"]),  "results$encoding_table nli ARI")
add("valNliRRaw",   r3(enc["nli", "r_raw"]),       "results$encoding_table nli r_raw")
add("valNliRKeyed", r3(enc["nli", "r_keyed"]),     "results$encoding_table nli r_keyed")
add("valNliPhi",    r3(enc["nli", "Tucker_human"]), "results$encoding_table nli Tucker")
add("valSqRKeyed",  r3(enc["squid", "r_keyed"]),   "results$encoding_table squid r_keyed")
add("valSqDis",     sprintf("%.3f", enc["squid", "Disattenuated_keyed"]), "results$encoding_table squid disattenuated")
add("valSqKmo",     r3(enc["squid", "KMO"]),       "results$encoding_table squid KMO")
add("valNmiGapNli", r3(enc["nli", "NMI_theory"] - enc["mean_centered_pearson", "NMI_theory"]),
    "results$encoding_table NMI gap, nli minus mcp")

writeLines(unlist(L), file.path(gen_dir, "values.tex"))
cat("values.tex:", length(L) - 2, "macros\n")

## --- Table: Tucker congruence (body) -----------------------------------------

hdr <- paste0("Semantic factor & ",
              paste(sprintf("%s (%s)", abbr[hum_dom], hum_mr), collapse = " & "),
              " \\\\")
stopifnot(identical(hum_mr, colnames(tuck)))
tuck_rows <- vapply(rownames(tuck), function(rn) {
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
enc_cell <- function(x, fmt) if (is.na(x)) "---" else fmt(x)
enc_rows <- vapply(rownames(enc), function(rn) {
  e <- enc[rn, ]
  sprintf("%s & %s & %s & %s & %s & %s & %s & %s & $%s$ & %s & %s \\\\",
          enc_names[[rn]],
          r3(e[["NMI_theory"]]), r3(e[["ARI_theory"]]), r3(e[["Tucker_human"]]),
          r3(e[["r_keyed"]]), r3(e[["r_raw"]]),
          enc_cell(e[["Disattenuated_keyed"]],
                   function(x) sprintf("%.3f", round(x, 3))),
          r3(e[["KMO"]]), f2(e[["TEFI"]]), r3(e[["RMSR"]]), r3(e[["CAF"]]))
}, "")
writeLines(c(
  "% AUTOGENERATED by reproduce/make_paper_assets.R from output/results.rds. Do not edit.",
  "\\begin{tabular}{lrrrrrrrrrr}",
  "\\toprule",
  "& \\multicolumn{2}{c}{Theory} & \\multicolumn{4}{c}{Human data} &",
  "\\multicolumn{4}{c}{Diagnostics} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-7} \\cmidrule(lr){8-11}",
  "Encoding & NMI & ARI & $\\bar{\\phi}$ & $r_{\\text{keyed}}$ &",
  "$r_{\\text{raw}}$ & $r_{\\text{dis}}$ & KMO & TEFI & RMSR & CAF \\\\",
  "\\midrule",
  enc_rows,
  "\\bottomrule",
  "\\end{tabular}"), file.path(gen_dir, "table_encodings_body.tex"))

## --- Supplementary longtables: full loading matrices --------------------------

loading_longtable <- function(csv, caption, label) {
  d  <- read.csv(csv, check.names = FALSE)
  mr <- setdiff(names(d), c("code", "factor"))
  rows <- vapply(seq_len(nrow(d)), function(i) {
    cells <- vapply(mr, function(m) texneg(r3(d[i, m])), "")
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
    rows,
    "\\end{longtable}}")
}

writeLines(loading_longtable(
  "output/semantic_loadings.csv",
  paste("Semantic factor loadings for the 50 IPIP Big-Five Factor Marker",
        "items (mean-centered Pearson encoding, minres extraction, oblimin",
        "rotation)"),
  "tab:s-semantic-loadings"),
  file.path(gen_dir, "table_semantic_loadings.tex"))

writeLines(loading_longtable(
  "output/human_loadings.csv",
  paste("Human-response factor loadings for the 50 IPIP Big-Five Factor",
        "Marker items (keyed correlations, minres extraction, oblimin",
        "rotation, $N = 874{,}434$)"),
  "tab:s-human-loadings"),
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
             fig_semantic_vs_human.pdf = "semantic_vs_human.pdf")
for (src in names(fig_map)) {
  from <- file.path("figures", src)
  if (!file.exists(from)) stop("missing figure: ", from)
  file.copy(from, file.path(fig_dir, fig_map[[src]]), overwrite = TRUE)
}
cat("Staged", length(fig_map), "figures into", fig_dir, "\n")
cat("Done.\n")
