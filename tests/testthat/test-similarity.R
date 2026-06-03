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

test_that("squid recovers negative off-diagonals with reverse items", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "squid",
                        scoring = big5$scoring)
  off_diag <- sim[lower.tri(sim)]
  expect_true(any(off_diag < 0))
})

test_that("mean_centered_pearson yields a proper correlation matrix", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings,
                        encoding = "mean_centered_pearson",
                        scoring = big5$scoring)
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
