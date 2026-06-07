test_that("KMO computes on a correlation matrix", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "atomic_reversed",
                        scoring = big5$scoring)
  kmo <- semanticfa:::.compute_kmo(sim)
  expect_true(kmo$total > 0 && kmo$total <= 1)
  expect_length(kmo$per_item, 50)
})

test_that("CAF is non-degenerate (not pinned at 0) on an embedding matrix", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "atomic_reversed",
                        scoring = big5$scoring)
  fa5 <- suppressWarnings(psych::fa(sim, nfactors = 5, rotate = "oblimin",
                                    fm = "minres", warnings = FALSE))
  rc <- semanticfa:::.compute_rmsr_caf(sim, fa5)
  expect_true(is.finite(rc$caf))
  expect_gt(rc$caf, 0.1)          # would be exactly 0 under the old (buggy) code
  expect_lt(rc$caf, 1)
})

test_that("CAF matches the EFAtools reference on psych::bfi", {
  skip_if_not_installed("EFAtools")
  data(bfi, package = "psych")
  R <- stats::cor(bfi[, 1:25], use = "pairwise")
  fa5 <- suppressWarnings(psych::fa(R, nfactors = 5, rotate = "oblimin",
                                    fm = "ml", warnings = FALSE))
  caf <- semanticfa:::.compute_rmsr_caf(R, fa5)$caf
  ref <- suppressWarnings(suppressMessages(
    EFAtools::EFA(R, n_factors = 5, N = 2800, method = "ML",
                  rotation = "oblimin")$fit_indices$CAF))
  expect_equal(caf, ref, tolerance = 0.1)   # ours ~0.40 vs EFAtools ~0.42
})

test_that("TEFI (partition-based) matches the EGAnet reference", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "atomic_reversed",
                        scoring = big5$scoring)
  dimnames(sim) <- list(big5$codes, big5$codes)
  tefi <- semanticfa:::.compute_tefi(sim, big5$factors)
  expect_true(is.finite(tefi))
  skip_if_not_installed("EGAnet")
  ref <- suppressWarnings(suppressMessages(
    EGAnet::tefi(sim, structure = big5$factors)$VN.Entropy.Fit))
  expect_equal(tefi, as.numeric(ref), tolerance = 1e-6)
})

test_that("DAAL matrix has correct dimensions", {
  data(big5)
  df <- data.frame(code = big5$codes, item = big5$items,
                   factor = big5$factors, scoring = big5$scoring,
                   stringsAsFactors = FALSE)
  fit <- sfa(df, nfactors = 5, embeddings = big5$embeddings)
  expect_equal(nrow(fit$daal), 5)
  expect_equal(ncol(fit$daal), 5)
  expect_true(all(fit$daal >= 0))
})

test_that("omega returns one row per factor", {
  data(big5)
  df <- data.frame(code = big5$codes, item = big5$items,
                   factor = big5$factors, scoring = big5$scoring,
                   stringsAsFactors = FALSE)
  fit <- sfa(df, nfactors = 5, embeddings = big5$embeddings)
  expect_equal(nrow(fit$omega), 5)
  expect_true("omega_assigned" %in% names(fit$omega))
})

test_that(".check_psd repairs a strongly indefinite matrix to PSD with unit diagonal", {
  M <- matrix(c(1, 0.9, 0.9, 0.9, 1, 0.9, 0.9, 0.9, 1), 3)
  M[1, 3] <- M[3, 1] <- -0.95
  expect_lt(min(eigen(M, symmetric = TRUE, only.values = TRUE)$values), -0.5)
  fixed <- suppressMessages(semanticfa:::.check_psd(M))
  expect_gt(min(eigen(fixed, symmetric = TRUE, only.values = TRUE)$values), -1e-8)
  expect_equal(unname(diag(fixed)), rep(1, 3))
})

test_that("sfa() errors clearly on similarity + calibrate, and stays quiet on scoring", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings, scoring = big5$scoring)
  attr(sim, "transformed_embeddings") <- NULL
  # #7: calibrate with a precomputed matrix warns and proceeds (no crash)
  expect_warning(
    fit <- suppressMessages(sfa(big5$items, similarity = sim, nfactors = 5,
                                calibrate = TRUE)),
    "calibration needs item embeddings")
  expect_s3_class(fit, "sfa")
  # #8: no spurious "No scoring provided" message in the similarity path
  expect_no_message(suppressWarnings(
    sfa(big5$items, similarity = sim, nfactors = 5)))
})

test_that("print.sfa handles non-finite KMO without erroring (degenerate similarity)", {
  fit <- suppressWarnings(suppressMessages(
    sfa(paste0("i", 1:5), similarity = diag(5), nfactors = 1)))
  expect_output(print(fit), "KMO")   # must not error even when KMO is NaN
})

test_that("zero-norm embedding rows yield finite (cosine 0) similarities, not NaN", {
  emb <- matrix(c(0, 0, 1, 0, 0, 1), nrow = 3, byrow = TRUE)  # row 1 all-zero
  s <- suppressWarnings(sfa_similarity(emb))
  expect_false(any(is.nan(s)))
  expect_true(all(is.finite(s)))
})

test_that("invalid numeric controls error with clear messages", {
  expect_error(sfa(letters[1:5], similarity = diag(5), nfactors = "x"), "whole number")
  expect_error(sfa(letters[1:5], similarity = diag(5), nfactors = 0), "whole number")
  expect_error(sfa_parallel(diag(5), diag(5), n_iter = 0), "whole number")
  expect_error(sfa_parallel(diag(5), diag(5), percentile = 150), "0, 100")
})
