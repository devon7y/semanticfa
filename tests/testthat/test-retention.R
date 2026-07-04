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

test_that("sfa_ekc matches the Braeken & van Assen reference implementation", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "mean_centered_pearson")
  ekc <- sfa_ekc(sim, big5$embeddings)

  expect_s3_class(ekc, "sfa_ekc")
  expect_true(ekc$n_factors >= 1L)
  expect_length(ekc$references, 50)
  expect_equal(ekc$n, ncol(big5$embeddings))
  # first reference is the Marchenko-Pastur upper edge
  expect_equal(ekc$references[1],
               (1 + sqrt(50 / ncol(big5$embeddings)))^2)
  # references never drop below one
  expect_true(all(ekc$references >= 1))

  skip_if_not_installed("EFAtools")
  ref <- suppressWarnings(suppressMessages(
    EFAtools::EKC(sim, N = ncol(big5$embeddings))))
  expect_equal(ekc$n_factors, ref$n_factors_BvA2017)
})

test_that("sfa_ekc requires a dimension source and accepts explicit n", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "mean_centered_pearson")
  expect_error(sfa_ekc(sim), "embeddings")
  expect_identical(sfa_ekc(sim, n = ncol(big5$embeddings))$n_factors,
                   sfa_ekc(sim, big5$embeddings)$n_factors)
})

test_that("sfa_map returns a valid minimum and tracks psych's MAP", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "mean_centered_pearson")
  mp <- sfa_map(sim)

  expect_s3_class(mp, "sfa_map")
  expect_true(mp$n_factors >= 1L)
  expect_true(is.finite(mp$map0))
  expect_true(min(mp$map, na.rm = TRUE) < mp$map0)

  ref <- suppressWarnings(suppressMessages(
    psych::VSS(sim, n = 20, n.obs = ncol(big5$embeddings), plot = FALSE)))
  shared <- seq_len(20)
  expect_gt(cor(mp$map[shared], ref$map[shared], use = "complete.obs"), 0.999)
})

test_that("sfa_nfactors runs EKC and MAP methods and default includes EKC", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "mean_centered_pearson")
  nf <- sfa_nfactors(sim, big5$embeddings,
                     methods = c("kaiser", "EKC", "MAP"),
                     parallel_iter = 10, seed = 42)
  expect_equal(nf$methods$method, c("kaiser", "EKC", "MAP"))
  expect_true(all(nf$methods$n_factors >= 1L))
  expect_true("EKC" %in% eval(formals(sfa_nfactors)$methods))
  expect_false("MAP" %in% eval(formals(sfa_nfactors)$methods))
})
