test_that("atomic_reversed sign-flips reverse-keyed items", {
  set.seed(1)
  emb <- matrix(rnorm(20), nrow = 4, ncol = 5)
  scoring <- c(1, -1, 1, -1)
  sim <- sfa_similarity(emb, encoding = "atomic_reversed", scoring = scoring)

  expect_true(is.matrix(sim))
  expect_equal(nrow(sim), 4)
  expect_equal(diag(sim), rep(1, 4))
  expect_true(isSymmetric(sim))
})

test_that("atomic encoding ignores scoring", {
  set.seed(1)
  emb <- matrix(rnorm(20), nrow = 4, ncol = 5)
  sim_atomic <- sfa_similarity(emb, encoding = "atomic")
  sim_ar_all1 <- sfa_similarity(emb, encoding = "atomic_reversed",
                                 scoring = rep(1, 4))
  expect_equal(sim_atomic, sim_ar_all1, tolerance = 1e-10)
})

test_that("squid recovers negative off-diagonals via centering (keying-free)", {
  data(big5)
  # SQuID (Pellert et al. 2026) recovers negatives from the centering alone,
  # without any scoring/sign-flip
  sim <- sfa_similarity(big5$embeddings, encoding = "squid")
  off_diag <- sim[lower.tri(sim)]
  expect_true(any(off_diag < 0))
})

test_that("squid is keying-free: scoring does not change it, and warns", {
  data(big5)
  sim_plain <- sfa_similarity(big5$embeddings, encoding = "squid")
  expect_warning(
    sim_scored <- sfa_similarity(big5$embeddings, encoding = "squid",
                                 scoring = big5$scoring),
    "keying-free")
  expect_equal(sim_plain, sim_scored, tolerance = 1e-12)
})

test_that("mean_centered_pearson yields a proper correlation matrix", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings,
                        encoding = "mean_centered_pearson")
  expect_equal(unname(diag(sim)), rep(1, 50))
  expect_true(all(sim >= -1 - 1e-10))
  expect_true(all(sim <= 1 + 1e-10))
})

test_that("scoring = NULL defaults to all +1 with message", {
  set.seed(1)
  emb <- matrix(rnorm(20), nrow = 4, ncol = 5)
  expect_message(
    sfa_similarity(emb, encoding = "atomic_reversed"),
    "No scoring provided"
  )
})
