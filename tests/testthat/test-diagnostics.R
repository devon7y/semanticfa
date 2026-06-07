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
