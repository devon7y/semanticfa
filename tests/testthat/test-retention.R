test_that("sfa_parallel returns valid structure", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "atomic_reversed",
                        scoring = big5$scoring)
  pa <- sfa_parallel(sim, big5$embeddings, n_iter = 20, seed = 42)

  expect_s3_class(pa, "sfa_parallel")
  expect_true(pa$n_factors >= 1)
  expect_length(pa$observed, 50)
  expect_length(pa$percentiles, 50)
})

test_that("EGA retention runs on a response-free matrix (no sample size)", {
  skip_if_not_installed("EGAnet")
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "atomic_reversed",
                        scoring = big5$scoring)
  nf <- semanticfa:::.retention_ega(sim)
  expect_true(is.integer(nf) && length(nf) == 1L && nf >= 1L)
})

test_that("sfa_nfactors returns consensus", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "atomic_reversed",
                        scoring = big5$scoring)
  nf <- sfa_nfactors(sim, big5$embeddings,
                     methods = c("parallel", "kaiser"),
                     parallel_iter = 20, seed = 42)

  expect_s3_class(nf, "sfa_nfactors")
  expect_true(!is.na(nf$consensus))
  expect_equal(nrow(nf$methods), 2)
})

test_that("seed produces reproducible parallel analysis", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "atomic_reversed",
                        scoring = big5$scoring)
  pa1 <- sfa_parallel(sim, big5$embeddings, n_iter = 10, seed = 123)
  pa2 <- sfa_parallel(sim, big5$embeddings, n_iter = 10, seed = 123)
  expect_identical(pa1$n_factors, pa2$n_factors)
  expect_equal(pa1$percentiles, pa2$percentiles)
})

test_that("parallel analysis does not alter global RNG", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "atomic_reversed",
                        scoring = big5$scoring)
  set.seed(999)
  before <- runif(1)
  set.seed(999)
  sfa_parallel(sim, big5$embeddings, n_iter = 10, seed = 42)
  after <- runif(1)
  expect_equal(before, after)
})
