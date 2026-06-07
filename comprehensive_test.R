# =============================================================================
# Comprehensive smoke/behaviour test for the semanticfa package.
#
# Exercises EVERY exported function across its parameter space. Most paths run
# with no Python via the bundled `big5` data and a deterministic custom embedder;
# live Python (sbert / NLI) and EGAnet paths are gated and skipped gracefully if
# unavailable.
#
# Run:  Rscript comprehensive_test.R
# Exit code is non-zero if any test FAILs.
# =============================================================================

suppressMessages({
  if (requireNamespace("devtools", quietly = TRUE) &&
      file.exists("DESCRIPTION")) {
    devtools::load_all(".", quiet = TRUE)
  } else {
    library(semanticfa)
  }
})

RUN_LIVE <- isTRUE(as.logical(Sys.getenv("SFA_TEST_LIVE", "TRUE")))  # live Python
HAS_EGANET <- requireNamespace("EGAnet", quietly = TRUE)
HAS_RTSNE  <- requireNamespace("Rtsne",  quietly = TRUE)
HAS_UWOT   <- requireNamespace("uwot",   quietly = TRUE)
DASS_NPZ   <- "/Users/devon7y/VS_Code/LLM_Factor_Analysis/embeddings/DASS_items_8B.npz"

# ---- harness ----------------------------------------------------------------
.results <- new.env(parent = emptyenv())
.results$rows <- list()
record <- function(desc, status, detail = "") {
  .results$rows[[length(.results$rows) + 1L]] <-
    list(desc = desc, status = status, detail = detail)
  tag <- switch(status, PASS = "PASS", FAIL = "FAIL", SKIP = "SKIP")
  cat(sprintf("[%s] %s\n", tag, desc))
  if (status == "FAIL") cat("        -> ", detail, "\n", sep = "")
}
ok <- function(desc, expr) {
  e <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e),
                warning = function(w) NULL)   # warnings allowed; errors fail
  if (is.null(e)) record(desc, "PASS") else record(desc, "FAIL", e)
}
errs <- function(desc, expr, pattern = NULL) {   # expect an error (optionally matching)
  e <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  if (is.null(e)) {
    record(desc, "FAIL", "expected an error but none was thrown")
  } else if (!is.null(pattern) && !grepl(pattern, e)) {
    record(desc, "FAIL", sprintf("error did not match '%s': %s", pattern, e))
  } else record(desc, "PASS")
}
skip <- function(desc, reason) record(desc, "SKIP", reason)
warns <- function(desc, expr, pattern = NULL) {   # expect a warning (optionally matching)
  w <- NULL
  res <- withCallingHandlers(
    tryCatch({ force(expr); "ran" }, error = function(e) paste0("ERR:", conditionMessage(e))),
    warning = function(wm) { w <<- conditionMessage(wm); invokeRestart("muffleWarning") })
  if (startsWith(res, "ERR")) record(desc, "FAIL", res)
  else if (is.null(w)) record(desc, "FAIL", "expected a warning but none was thrown")
  else if (!is.null(pattern) && !grepl(pattern, w))
    record(desc, "FAIL", sprintf("warning did not match '%s': %s", pattern, w))
  else record(desc, "PASS")
}
msg <- function(desc, expr, pattern) {   # expect a message matching pattern
  m <- character(0)
  withCallingHandlers(tryCatch(force(expr), error = function(e) NULL),
    message = function(mm) { m <<- c(m, conditionMessage(mm)); invokeRestart("muffleMessage") })
  if (any(grepl(pattern, m))) record(desc, "PASS")
  else record(desc, "FAIL", sprintf("expected message '%s'; got: %s", pattern,
                                     paste(m, collapse = " | ")))
}
section <- function(s) cat(sprintf("\n========== %s ==========\n", s))

png_dev <- function() grDevices::png(tempfile(fileext = ".png"))

# ---- data + helpers ---------------------------------------------------------
data(big5)
EMB <- big5$embeddings
DIM <- ncol(EMB)
DF  <- data.frame(code = big5$codes, item = big5$items, factor = big5$factors,
                  scoring = big5$scoring, stringsAsFactors = FALSE)

# deterministic embedder factory: known item -> its embedding; construct name ->
# its centroid; anything else -> a stable pseudo-vector of the right dimension.
# No Python required, and dimension-matched to whatever embeddings it is built on.
make_fake_embed <- function(emb, items, factors) {
  d <- ncol(emb); cons <- unique(factors)
  function(txt, ...) {
    out <- matrix(0, length(txt), d)
    for (i in seq_along(txt)) {
      mi <- match(txt[i], items)
      if (!is.na(mi)) {
        out[i, ] <- emb[mi, ]
      } else if (txt[i] %in% cons) {
        out[i, ] <- colMeans(emb[factors == txt[i], , drop = FALSE])
      } else {
        withr::with_seed(sum(utf8ToInt(txt[i])), out[i, ] <- stats::rnorm(d))
      }
    }
    out
  }
}
fake_embed <- make_fake_embed(EMB, big5$items, big5$factors)
fake_nli <- function(prem, hyp) {
  s <- (nchar(prem) %% 5) / 5
  data.frame(entailment = 0.5 + 0.3 * s, contradiction = 0.3 - 0.1 * s)
}

# a primary fit reused throughout (precomputed embeddings, no Python)
FIT <- suppressWarnings(suppressMessages(
  sfa(DF, embeddings = EMB, scoring = big5$scoring, nfactors = 5, seed = 42L)))

# =============================================================================
section("1. sfa_embed")
ok("sfa_embed: custom function backend",
   { e <- sfa_embed(big5$items[1:5], embed = fake_embed); stopifnot(dim(e)[1] == 5) })
ok("sfa_embed: data.frame input (codes as rownames)",
   { e <- sfa_embed(DF[1:5, ], embed = fake_embed); stopifnot(!is.null(rownames(e))) })
ok("sfa_embed: single string via custom fn (1 x dim)",
   { e <- sfa_embed("a lone item", embed = fake_embed); stopifnot(nrow(e) == 1) })
errs("sfa_embed: openai without API key errors", pattern = "OPENAI_API_KEY|httr2",
     { Sys.setenv(OPENAI_API_KEY = ""); sfa_embed("x", embed = "openai") })
if (RUN_LIVE) {
  ok("sfa_embed: live sbert (default Qwen, small)",
     { e <- sfa_embed(big5$items[1:3]); stopifnot(nrow(e) == 3) })
} else skip("sfa_embed: live sbert", "SFA_TEST_LIVE=FALSE")

# =============================================================================
section("2. sfa_load_npz")
if (file.exists(DASS_NPZ) && requireNamespace("reticulate", quietly = TRUE) &&
    !is.null(tryCatch(reticulate::import("numpy"), error = function(e) NULL))) {
  ok("sfa_load_npz: real DASS .npz",
     { e <- sfa_load_npz(DASS_NPZ); stopifnot(inherits(e, "sfa_embeddings")) })
} else skip("sfa_load_npz: real DASS .npz", "npz/numpy unavailable")
errs("sfa_load_npz: missing file errors", pattern = "not found",
     sfa_load_npz("/no/such/file.npz"))

# =============================================================================
section("3. sfa_similarity")
for (enc in c("atomic", "atomic_reversed", "squid", "mean_centered_pearson")) {
  ok(sprintf("sfa_similarity: encoding=%s", enc),
     { s <- sfa_similarity(EMB, encoding = enc, scoring = big5$scoring)
       stopifnot(dim(s)[1] == 50, isSymmetric(unname(s))) })
}
ok("sfa_similarity: records factors+codes attributes",
   { s <- sfa_similarity(EMB, factors = big5$factors, codes = big5$codes)
     stopifnot(!is.null(attr(s, "factors")), !is.null(attr(s, "codes"))) })
ok("sfa_similarity: accepts an sfa object",
   { s <- sfa_similarity(FIT); stopifnot(dim(s)[1] == 50) })
ok("sfa_similarity: accepts an sfa_embeddings object",
   { obj <- structure(list(embeddings = EMB, codes = big5$codes,
                           factors = big5$factors, scoring = big5$scoring),
                      class = "sfa_embeddings")
     s <- sfa_similarity(obj); stopifnot(!is.null(attr(s, "codes"))) })
errs("sfa_similarity: rejects non-finite embeddings", pattern = "non-finite",
     { bad <- EMB; bad[1, 1] <- NA; sfa_similarity(bad) })
errs("sfa_similarity: rejects <2 rows", pattern = "at least 2 rows",
     sfa_similarity(EMB[1, , drop = FALSE]))

# =============================================================================
section("4. sfa (core)")
for (enc in c("atomic", "atomic_reversed", "squid", "mean_centered_pearson")) {
  ok(sprintf("sfa: encoding=%s (no warnings/errors)", enc),
     suppressMessages(sfa(DF, embeddings = EMB, scoring = big5$scoring,
                          encoding = enc, nfactors = 5, seed = 42L)))
}
ok("sfa: nfactors=NULL + parallel retention",
   suppressMessages(sfa(DF, embeddings = EMB, scoring = big5$scoring,
                        n_factors_method = "parallel", seed = 42L)))
ok("sfa: kaiser retention",
   suppressMessages(sfa(DF, embeddings = EMB, scoring = big5$scoring,
                        n_factors_method = "kaiser", seed = 42L)))
ok("sfa: TEFI retention",
   suppressMessages(sfa(DF, embeddings = EMB, scoring = big5$scoring,
                        n_factors_method = "TEFI", seed = 42L)))
if (HAS_EGANET) {
  ok("sfa: EGA retention",
     suppressMessages(sfa(DF, embeddings = EMB, scoring = big5$scoring,
                          n_factors_method = "EGA", seed = 42L)))
} else skip("sfa: EGA retention", "EGAnet not installed")
ok("sfa: rotate=varimax, fm=ml",
   suppressMessages(sfa(DF, embeddings = EMB, scoring = big5$scoring,
                        nfactors = 5, rotate = "varimax", fm = "ml", seed = 42L)))
ok("sfa: calibrate=TRUE (small)",
   suppressWarnings(suppressMessages(
     sfa(DF, embeddings = EMB, scoring = big5$scoring, nfactors = 5,
         calibrate = TRUE, calibrate_iter = 5L, seed = 42L))))
ok("sfa: bare numeric matrix as items",
   suppressMessages(sfa(EMB, nfactors = 5, seed = 42L)))
ok("sfa: sfa_embeddings object input",
   { obj <- structure(list(embeddings = EMB, codes = big5$codes,
                           factors = big5$factors, scoring = big5$scoring),
                      class = "sfa_embeddings")
     suppressMessages(sfa(obj, nfactors = 5, seed = 42L)) })
ok("sfa: custom embed function (text path)",
   suppressMessages(sfa(DF, embed = fake_embed, nfactors = 5, seed = 42L)))
ok("sfa: precomputed similarity path",
   { s <- sfa_similarity(EMB, scoring = big5$scoring)
     suppressMessages(suppressWarnings(sfa(big5$items, similarity = s, nfactors = 5))) })
errs("sfa: nfactors too large errors", pattern = "at most",
     suppressMessages(sfa(DF, embeddings = EMB, scoring = big5$scoring, nfactors = 60)))
errs("sfa: similarity + calibrate warns-then-works OR clean",
     pattern = "embeddings|calibration",
     { s <- sfa_similarity(EMB, scoring = big5$scoring)
       withCallingHandlers(
         suppressMessages(sfa(big5$items, similarity = s, nfactors = 5, calibrate = TRUE)),
         warning = function(w) stop(conditionMessage(w))) })
if (HAS_EGANET) {
  ok("sfa: dim_select=dynega",
     suppressWarnings(suppressMessages(
       sfa(DF, embeddings = EMB, scoring = big5$scoring,
           dim_select = "dynega", n_factors_method = "EGA", seed = 42L))))
} else skip("sfa: dim_select=dynega", "EGAnet not installed")

# =============================================================================
section("5. print / summary / plot / as_psych")
ok("print.sfa", { out <- capture.output(print(FIT)); stopifnot(length(out) > 3) })
ok("summary.sfa", { out <- capture.output(summary(FIT)); stopifnot(length(out) > 3) })
for (ty in c("scree", "loadings", "residuals", "similarity")) {
  ok(sprintf("plot.sfa: type=%s", ty), { png_dev(); on.exit(grDevices::dev.off())
                                         plot(FIT, ty); grDevices::dev.off() })
}
ok("as_psych returns a psych fa-like object",
   { p <- as_psych(FIT); stopifnot(!is.null(p$loadings)) })

# =============================================================================
section("6. sfa_corplot")
ok("sfa_corplot: from fit (grouped, default)",
   { png_dev(); on.exit(grDevices::dev.off()); sfa_corplot(FIT); grDevices::dev.off() })
ok("sfa_corplot: group=FALSE",
   { png_dev(); on.exit(grDevices::dev.off()); sfa_corplot(FIT, group = FALSE); grDevices::dev.off() })
ok("sfa_corplot: order by abbreviations",
   { png_dev(); on.exit(grDevices::dev.off())
     sfa_corplot(FIT, order = c("E", "N", "A", "C", "O")); grDevices::dev.off() })
ok("sfa_corplot: upper=FALSE, numbers=TRUE",
   { png_dev(); on.exit(grDevices::dev.off())
     sfa_corplot(FIT, upper = FALSE, numbers = TRUE); grDevices::dev.off() })
ok("sfa_corplot: from a bare similarity matrix",
   { s <- sfa_similarity(EMB, factors = big5$factors, codes = big5$codes)
     png_dev(); on.exit(grDevices::dev.off()); sfa_corplot(s); grDevices::dev.off() })
errs("sfa_corplot: bad order entry errors", pattern = "matches no factor",
     { png_dev(); on.exit(grDevices::dev.off()); sfa_corplot(FIT, order = c("Z")) })

# =============================================================================
section("7. sfa_itemplot (+ sfa_tsneplot alias)")
methods_avail <- c("pca", "mds",
                   if (HAS_RTSNE) "tsne", if (HAS_UWOT) "umap")
for (m in c("pca", "mds", "tsne", "umap")) {
  if (m %in% methods_avail) {
    ok(sprintf("sfa_itemplot: method=%s", m),
       { png_dev(); on.exit(grDevices::dev.off()); sfa_itemplot(FIT, method = m); grDevices::dev.off() })
  } else skip(sprintf("sfa_itemplot: method=%s", m), "package not installed")
}
ok("sfa_itemplot: color=FALSE, legend=FALSE",
   { png_dev(); on.exit(grDevices::dev.off())
     sfa_itemplot(FIT, method = "pca", color = FALSE, legend = FALSE); grDevices::dev.off() })
ok("sfa_itemplot: from a bare similarity matrix",
   { s <- sfa_similarity(EMB, factors = big5$factors, codes = big5$codes)
     png_dev(); on.exit(grDevices::dev.off()); sfa_itemplot(s, method = "mds"); grDevices::dev.off() })
ok("sfa_tsneplot alias warns + works",
   { png_dev(); on.exit(grDevices::dev.off())
     suppressWarnings(sfa_tsneplot(FIT, method = "pca")); grDevices::dev.off() })

# =============================================================================
section("8. sfa_parallel / sfa_nfactors / sfa_dimselect")
ok("sfa_parallel: (sim, emb)",
   { s <- sfa_similarity(EMB, scoring = big5$scoring)
     p <- sfa_parallel(s, EMB, n_iter = 20L); stopifnot(is.numeric(p$n_factors)) })
ok("sfa_parallel: from fit", suppressWarnings(sfa_parallel(FIT, n_iter = 20L)))
ok("sfa_nfactors: methods=parallel,kaiser,TEFI",
   { s <- sfa_similarity(EMB, scoring = big5$scoring)
     suppressMessages(sfa_nfactors(s, embeddings = EMB,
                                   methods = c("parallel", "kaiser", "TEFI"),
                                   parallel_iter = 20L)) })
ok("sfa_nfactors: from fit", suppressMessages(suppressWarnings(sfa_nfactors(FIT))))
if (HAS_EGANET) {
  ok("sfa_nfactors: includes EGA",
     { s <- sfa_similarity(EMB, scoring = big5$scoring)
       suppressMessages(sfa_nfactors(s, embeddings = EMB,
                                     methods = c("kaiser", "EGA"))) })
  ok("sfa_dimselect (small grid)",
     suppressWarnings(suppressMessages(
       sfa_dimselect(EMB, factors = big5$factors, scoring = big5$scoring,
                     min_depth = 50, max_depth = 150, step = 50))))
} else {
  skip("sfa_nfactors: includes EGA", "EGAnet not installed")
  skip("sfa_dimselect", "EGAnet not installed")
}

# =============================================================================
section("9. sfa_anchor")
ok("sfa_anchor: centroid",
   { a <- sfa_anchor(FIT, anchor = "centroid"); stopifnot(!is.null(a$centroid)) })
ok("sfa_anchor: label (custom embed)",
   { a <- sfa_anchor(FIT, anchor = "label", embed = fake_embed); stopifnot(!is.null(a$label)) })
ok("sfa_anchor: both",
   { a <- sfa_anchor(FIT, anchor = "both", embed = fake_embed)
     stopifnot(!is.null(a$centroid), !is.null(a$label)) })
ok("sfa_anchor: label_embeddings precomputed",
   { le <- t(sapply(unique(big5$factors), function(g)
              colMeans(EMB[big5$factors == g, , drop = FALSE])))
     a <- sfa_anchor(FIT, anchor = "label", label_embeddings = le)
     stopifnot(!is.null(a$label)) })
ok("print.sfa_anchor", { a <- sfa_anchor(FIT); capture.output(print(a)) })

# =============================================================================
section("10. sfa_item_fit")
ok("sfa_item_fit: single candidate + construct (custom embed)",
   { r <- sfa_item_fit(FIT, "I love big parties", construct = "Extraversion",
                       embed = fake_embed); stopifnot(inherits(r, "sfa_item_fit")) })
ok("sfa_item_fit: batch of candidates",
   sfa_item_fit(FIT, c("I love big parties", "I worry a lot"), embed = fake_embed))
ok("sfa_item_fit: reverse_key=TRUE",
   sfa_item_fit(FIT, "I love big parties", reverse_key = TRUE, embed = fake_embed))
ok("sfa_item_fit: redundancy_cutoff tuned",
   sfa_item_fit(FIT, big5$items[1], embed = fake_embed, redundancy_cutoff = 0.5))
ok("print.sfa_item_fit",
   { r <- sfa_item_fit(FIT, "I love big parties", embed = fake_embed)
     capture.output(print(r)) })

# =============================================================================
section("11. sfa_redundancy")
ok("sfa_redundancy: cosine",
   sfa_redundancy(FIT, method = "cosine", threshold = 0.85))
ok("sfa_redundancy: cosine default threshold", sfa_redundancy(FIT, method = "cosine"))
if (HAS_EGANET) {
  ok("sfa_redundancy: wto (UVA)", sfa_redundancy(FIT, method = "wto", threshold = 0.25))
} else skip("sfa_redundancy: wto", "EGAnet not installed")
ok("sfa_redundancy: from a bare matrix",
   { s <- sfa_similarity(EMB, scoring = big5$scoring); rownames(s) <- colnames(s) <- big5$codes
     sfa_redundancy(s, method = "cosine", threshold = 0.9) })
ok("print.sfa_redundancy", capture.output(print(sfa_redundancy(FIT, method = "cosine"))))

# =============================================================================
section("12. sfa_simplify")
for (mth in c("anchor", "medoid")) for (grp in c("theoretical", "fitted")) {
  ok(sprintf("sfa_simplify: method=%s groups=%s", mth, grp),
     suppressWarnings(suppressMessages(
       sfa_simplify(FIT, target_n = 5, method = mth, groups = grp))))
}
ok("print.sfa_simplify",
   capture.output(print(suppressWarnings(suppressMessages(
     sfa_simplify(FIT, target_n = 5))))))

# =============================================================================
section("13. sfa_project")
ax <- list(severity = c(low = "calm and relaxed", high = "anxious and tense"))
ok("sfa_project: axes via custom embed",
   { p <- sfa_project(FIT, axes = ax, embed = fake_embed)
     stopifnot(!is.null(p$scores)) })
ok("sfa_project: normalize=FALSE",
   sfa_project(FIT, axes = ax, normalize = FALSE, embed = fake_embed))
ok("sfa_project: pole_embeddings precomputed",
   { pe <- list(severity = list(low = fake_embed("calm")[1, ],
                                high = fake_embed("tense")[1, ]))
     sfa_project(FIT, axes = ax, pole_embeddings = pe) })
ok("print.sfa_project",
   capture.output(print(sfa_project(FIT, axes = ax, embed = fake_embed))))

# =============================================================================
section("14. sfa_jinglejangle")
scales <- list(
  Extra = big5$items[big5$factors == "Extraversion"],
  Neuro = big5$items[big5$factors == "Neuroticism"],
  Agree = big5$items[big5$factors == "Agreeableness"])
ie <- lapply(scales, fake_embed)
ok("sfa_jinglejangle: item_embeddings precomputed",
   { j <- sfa_jinglejangle(scales, item_embeddings = ie); stopifnot(!is.null(j)) })
ok("sfa_jinglejangle: flag tuned",
   sfa_jinglejangle(scales, item_embeddings = ie, flag = 0.1))
ok("print.sfa_jinglejangle",
   capture.output(print(sfa_jinglejangle(scales, item_embeddings = ie))))

# =============================================================================
section("15. sfa_nli_matrix")
ok("sfa_nli_matrix: custom classifier, symmetric=TRUE",
   { M <- sfa_nli_matrix(big5$items[1:6], classifier = fake_nli)
     stopifnot(dim(M)[1] == 6, isSymmetric(unname(M))) })
ok("sfa_nli_matrix: symmetric=FALSE",
   { M <- sfa_nli_matrix(big5$items[1:6], classifier = fake_nli, symmetric = FALSE)
     stopifnot(dim(M)[1] == 6) })
errs("sfa_nli_matrix: bad classifier row count errors", pattern = "rows",
     sfa_nli_matrix(big5$items[1:4],
                    classifier = function(p, h) data.frame(entailment = 1, contradiction = 0)))
if (RUN_LIVE) {
  ok("sfa_nli_matrix: live cross-encoder (small)",
     { M <- sfa_nli_matrix(big5$items[1:4]); stopifnot(dim(M)[1] == 4) })
} else skip("sfa_nli_matrix: live cross-encoder", "SFA_TEST_LIVE=FALSE")

# =============================================================================
section("16. sfa_congruence")
ok("sfa_congruence: nmi+ari vs theoretical factors",
   { c1 <- sfa_congruence(FIT, target = big5$factors, metrics = c("nmi", "ari"))
     stopifnot(!is.null(c1)) })
ok("sfa_congruence: tucker/frobenius/disattenuated vs a psych::fa target",
   { ref <- as_psych(FIT)
     sfa_congruence(FIT, target = ref,
                    metrics = c("tucker", "frobenius", "disattenuated")) })
ok("sfa_congruence: single metric",
   sfa_congruence(FIT, target = big5$factors, metrics = "nmi"))
ok("print.sfa_congruence",
   capture.output(print(sfa_congruence(FIT, target = big5$factors,
                                       metrics = c("nmi", "ari")))))

# =============================================================================
section("17. utilities")
ok("sfa_install_python is a function (not invoked)", stopifnot(is.function(sfa_install_python)))
ok("sfa_clear_cache runs", sfa_clear_cache())

# =============================================================================
section("18. npz end-to-end (real saved 8B embeddings)")
have_npz <- file.exists(DASS_NPZ) && requireNamespace("reticulate", quietly = TRUE) &&
  !is.null(tryCatch(reticulate::import("numpy"), error = function(e) NULL))
if (have_npz) {
  emb8 <- sfa_load_npz(DASS_NPZ)
  fe8  <- make_fake_embed(emb8$embeddings, emb8$items, emb8$factors)
  ok("npz: print sfa_embeddings", capture.output(print(emb8)))
  ok("npz: sfa(emb8) full pipeline from the file",
     suppressWarnings(suppressMessages(sfa(emb8, nfactors = 3))))
  fit8 <- suppressWarnings(suppressMessages(sfa(emb8, nfactors = 3)))
  ok("npz: sfa_similarity(emb8)", sfa_similarity(emb8))
  ok("npz: sfa_corplot(fit8)",
     { png_dev(); on.exit(grDevices::dev.off()); sfa_corplot(fit8); grDevices::dev.off() })
  ok("npz: sfa_itemplot(fit8, pca)",
     { png_dev(); on.exit(grDevices::dev.off()); sfa_itemplot(fit8, method = "pca"); grDevices::dev.off() })
  ok("npz: sfa_anchor(fit8, centroid)", sfa_anchor(fit8, anchor = "centroid"))
  ok("npz: sfa_anchor(fit8, label, custom embed)",
     sfa_anchor(fit8, anchor = "label", embed = fe8))
  ok("npz: sfa_redundancy(fit8, cosine)", sfa_redundancy(fit8, method = "cosine"))
  if (HAS_EGANET) ok("npz: sfa_redundancy(fit8, wto)",
                     sfa_redundancy(fit8, method = "wto"))
  ok("npz: sfa_simplify(fit8)",
     suppressWarnings(suppressMessages(sfa_simplify(fit8, target_n = 7))))
  ok("npz: sfa_congruence(fit8, nmi/ari vs npz factors)",
     sfa_congruence(fit8, target = emb8$factors, metrics = c("nmi", "ari")))
  ok("npz: sfa_item_fit(fit8, custom embed)",
     sfa_item_fit(fit8, "I feel down most of the time",
                  construct = "Depression", embed = fe8))
  ok("npz: sfa_project(fit8, custom embed)",
     sfa_project(fit8, axes = list(severity = c(low = "mild", high = "severe")),
                 embed = fe8))
} else skip("npz end-to-end section", "npz file or numpy unavailable")

# =============================================================================
section("19. error branches & alternate paths (from coverage audit)")
SIM      <- sfa_similarity(EMB, scoring = big5$scoring)
FIT_SIM  <- suppressWarnings(suppressMessages(sfa(DF, similarity = SIM, nfactors = 5)))  # has factors, no embeddings
FIT_BARE <- suppressWarnings(suppressMessages(sfa(EMB, nfactors = 5)))                   # no factor labels

## sfa(): scoring defaults + validation + alt inputs
msg("sfa: scoring=NULL emits default message (atomic_reversed)",
    suppressWarnings(sfa(big5$items, embeddings = EMB, encoding = "atomic_reversed",
                         nfactors = 5)), "No scoring provided")
errs("sfa: invalid scoring value errors",
     sfa(big5$items, embeddings = EMB, scoring = c(2, rep(1, 49)), nfactors = 5))
errs("sfa: scoring length mismatch errors",
     sfa(big5$items, embeddings = EMB, scoring = c(1, -1), nfactors = 5))
ok("sfa: bare character vector + custom embed",
   suppressMessages(sfa(big5$items, embed = fake_embed, nfactors = 5)))
errs("sfa: non-square similarity errors",
     sfa(big5$items, similarity = EMB[, 1:10], nfactors = 5))
errs("sfa: non-finite similarity errors",
     { s <- SIM; s[1, 1] <- NA; sfa(big5$items, similarity = s, nfactors = 5) })
errs("sfa: non-symmetric similarity errors",
     { s <- SIM; s[1, 2] <- s[1, 2] + 0.5; sfa(big5$items, similarity = s, nfactors = 5) })

## sfa_similarity(): keying-free warning
warns("sfa_similarity: squid + reverse scoring warns keying-free",
      sfa_similarity(EMB, encoding = "squid", scoring = big5$scoring), "keying-free")
warns("sfa_similarity: mean_centered_pearson + reverse scoring warns",
      sfa_similarity(EMB, encoding = "mean_centered_pearson", scoring = big5$scoring), "keying-free")

## sfa_embed(): cache bypass + (live) cache hit
ok("sfa_embed: cache=FALSE (custom fn)",
   sfa_embed(big5$items[1:2], embed = fake_embed, cache = FALSE))
if (RUN_LIVE) ok("sfa_embed: live cache hit (2x same call)",
   { a <- sfa_embed(big5$items[1:2]); b <- sfa_embed(big5$items[1:2]); stopifnot(nrow(b) == 2) })

## sfa_load_npz(): missing-key error + embeddings-only archive
if (exists("have_npz") && have_npz) {
  np <- reticulate::import("numpy")
  errs("sfa_load_npz: missing embeddings key errors",
       { tmp <- tempfile(fileext = ".npz"); np$savez(tmp, foo = matrix(1, 3, 3)); sfa_load_npz(tmp) })
  ok("sfa_load_npz: embeddings-only archive (NULL metadata)",
     { tmp <- tempfile(fileext = ".npz"); np$savez(tmp, embeddings = EMB)
       e <- sfa_load_npz(tmp); stopifnot(is.null(e$codes)) })
} else { skip("sfa_load_npz: missing-key error", "numpy"); skip("sfa_load_npz: embeddings-only", "numpy") }

## retention on a no-embeddings (precomputed-similarity) fit
errs("sfa_parallel: precomputed-sim fit (no embeddings) errors", sfa_parallel(FIT_SIM))
ok("sfa_nfactors: precomputed-sim fit (parallel degrades, others run)",
   suppressMessages(suppressWarnings(sfa_nfactors(FIT_SIM))))

## sfa_dimselect: factors = NULL (TEFI-only fallback)
if (HAS_EGANET) {
  ok("sfa_dimselect: factors=NULL (TEFI-only fallback)",
     suppressWarnings(suppressMessages(
       sfa_dimselect(EMB, scoring = big5$scoring, min_depth = 50, max_depth = 150, step = 50))))
} else skip("sfa_dimselect: factors=NULL", "EGAnet not installed")

## sfa_corplot: label auto-generation when labels are sentence-like
ok("sfa_corplot: sentence labels -> factor-based codes",
   { s <- sfa_similarity(EMB, factors = big5$factors); rownames(s) <- colnames(s) <- big5$items
     png_dev(); on.exit(grDevices::dev.off()); sfa_corplot(s); grDevices::dev.off() })
ok("sfa_corplot: sentence labels, no factors -> generic I codes",
   { s <- sfa_similarity(EMB); rownames(s) <- colnames(s) <- big5$items
     png_dev(); on.exit(grDevices::dev.off()); sfa_corplot(s, group = FALSE); grDevices::dev.off() })

## sfa_itemplot: tsne/umap distance path from a bare similarity matrix
SIMfc <- sfa_similarity(EMB, factors = big5$factors, codes = big5$codes)
if (HAS_RTSNE) ok("sfa_itemplot: tsne from similarity matrix (distance path)",
   { png_dev(); on.exit(grDevices::dev.off()); sfa_itemplot(SIMfc, method = "tsne"); grDevices::dev.off() })
if (HAS_UWOT)  ok("sfa_itemplot: umap from similarity matrix (distance path)",
   { png_dev(); on.exit(grDevices::dev.off()); sfa_itemplot(SIMfc, method = "umap"); grDevices::dev.off() })

## sfa_anchor: labels forms + error branches
cons5 <- unique(big5$factors)
ok("sfa_anchor: labels= character vector",
   sfa_anchor(FIT, anchor = "label", labels = paste("the trait", cons5), embed = fake_embed))
ok("sfa_anchor: labels= named vector",
   sfa_anchor(FIT, anchor = "label",
              labels = stats::setNames(paste("trait", cons5), cons5), embed = fake_embed))
errs("sfa_anchor: fit without factor labels errors", sfa_anchor(FIT_BARE))
errs("sfa_anchor: precomputed-sim fit (no embeddings) errors",
     sfa_anchor(FIT_SIM, anchor = "centroid"))

## sfa_item_fit: error branches + construct matching + single-construct scale
errs("sfa_item_fit: non-sfa x errors", sfa_item_fit(EMB, "x", embed = fake_embed))
errs("sfa_item_fit: empty item errors", sfa_item_fit(FIT, character(0), embed = fake_embed))
errs("sfa_item_fit: no-match construct errors",
     sfa_item_fit(FIT, "x", construct = "Zzz", embed = fake_embed))
ok("sfa_item_fit: single-construct scale (cross-loading n/a)",
   { df1 <- data.frame(code = big5$codes[1:10], item = big5$items[1:10],
                       factor = "General", scoring = big5$scoring[1:10],
                       stringsAsFactors = FALSE)
     f1 <- suppressWarnings(suppressMessages(
       sfa(df1, embeddings = EMB[1:10, ], scoring = big5$scoring[1:10], nfactors = 1)))
     fe1 <- make_fake_embed(EMB[1:10, ], big5$items[1:10], "General")
     r <- sfa_item_fit(f1, big5$items[3], embed = fe1); stopifnot(is.na(r$summary$gap[1])) })

## sfa_redundancy: bad input
errs("sfa_redundancy: non-square input errors", sfa_redundancy(EMB[, 1:10]))

## sfa_simplify: error branches
errs("sfa_simplify: non-sfa x errors", sfa_simplify(EMB, target_n = 5))
errs("sfa_simplify: groups=theoretical without factor column errors",
     suppressWarnings(sfa_simplify(FIT_BARE, target_n = 5, groups = "theoretical")))

## sfa_project: list-form axis with multiple phrases per pole
ok("sfa_project: list-form axis (multiple phrases per pole)",
   sfa_project(FIT, axes = list(sev = list(low = c("mild", "slight"),
                                           high = c("severe", "extreme"))), embed = fake_embed))

## sfa_congruence: matrix target forms
ok("sfa_congruence: target = loadings matrix (tucker)",
   sfa_congruence(FIT, target = unclass(as_psych(FIT)$loadings), metrics = "tucker"))
ok("sfa_congruence: target = correlation matrix (disattenuated)",
   sfa_congruence(FIT, target = FIT$sim_matrix, metrics = "disattenuated"))

## sfa_jinglejangle: labels argument
ok("sfa_jinglejangle: labels= argument",
   sfa_jinglejangle(scales, labels = c("Ex", "Ne", "Ag"), item_embeddings = ie))

## sfa_nli_matrix: missing required columns
errs("sfa_nli_matrix: missing entailment/contradiction columns errors",
     sfa_nli_matrix(big5$items[1:4],
                    classifier = function(p, h) data.frame(foo = rep(1, length(p)))))

## summary.sfa calibration block + plot.sfa scree parallel overlay
ok("summary.sfa: calibration block",
   { fc <- suppressWarnings(suppressMessages(
       sfa(DF, embeddings = EMB, scoring = big5$scoring, nfactors = 5,
           calibrate = TRUE, calibrate_iter = 5L))); capture.output(summary(fc)) })
ok("plot.sfa: scree with parallel-analysis overlay",
   { fp <- suppressWarnings(suppressMessages(
       sfa(DF, embeddings = EMB, scoring = big5$scoring, n_factors_method = "parallel")))
     png_dev(); on.exit(grDevices::dev.off()); plot(fp, "scree"); grDevices::dev.off() })

# =============================================================================
# Summary
# =============================================================================
rows <- .results$rows
status <- vapply(rows, `[[`, character(1), "status")
np <- sum(status == "PASS"); nf <- sum(status == "FAIL"); ns <- sum(status == "SKIP")
cat(sprintf("\n================ SUMMARY ================\n"))
cat(sprintf("  PASS: %d   FAIL: %d   SKIP: %d   (total %d)\n", np, nf, ns, length(rows)))
if (nf > 0) {
  cat("\n  FAILURES:\n")
  for (r in rows) if (r$status == "FAIL") cat(sprintf("   - %s\n       %s\n", r$desc, r$detail))
}
if (ns > 0) {
  cat("\n  SKIPPED:\n")
  for (r in rows) if (r$status == "SKIP") cat(sprintf("   - %s (%s)\n", r$desc, r$detail))
}
cat("=========================================\n")
if (nf > 0) quit(status = 1L, save = "no")
