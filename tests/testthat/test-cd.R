test_that("sfa_cd recovers a crisp factor count on conventional data", {
  # three orthogonal factors, strong loadings: CD's validated regime
  set.seed(7)
  n <- 300; per <- 4
  lam <- matrix(0, 3 * per, 3)
  for (f in 1:3) lam[(f - 1) * per + 1:per, f] <- 0.8
  X <- matrix(rnorm(n * 3), n) %*% t(lam) +
    matrix(rnorm(n * 3 * per, sd = sqrt(1 - 0.64)), n)
  # alpha = .05 per the hyperparameter study of Goretzko & Ruscio (2024);
  # the liberal .30 default of Ruscio & Roche takes its documented one-step
  # Type I overextraction on this fixture
  cd <- sfa_cd(X, input = "data", n_factors_max = 4, n_samples = 100,
               n_pop = 2000, gen_iter = 5, alpha = .05, seed = 42)

  expect_s3_class(cd, "sfa_cd")
  expect_equal(cd$n_factors, 3L)
  expect_length(cd$median_rmsr, 4)
  expect_equal(cd$profile[1], 1)
})

test_that("sfa_cd profiles embeddings without a verdict by default", {
  data(big5)
  cd <- sfa_cd(big5$embeddings[, 1:400], n_factors_max = 3, n_samples = 40,
               n_pop = 1000, gen_iter = 3, seed = 42)
  expect_true(is.na(cd$n_factors))
  expect_true(all(diff(cd$median_rmsr) < 0))
  expect_equal(dim(cd$rmsr), c(40L, 3L))
  expect_equal(cd$n, 400)
})

test_that("sfa_cd is reproducible under a seed and validates inputs", {
  data(big5)
  a <- sfa_cd(big5$embeddings[, 1:200], n_factors_max = 2, n_samples = 20,
              n_pop = 500, gen_iter = 2, seed = 99)
  b <- sfa_cd(big5$embeddings[, 1:200], n_factors_max = 2, n_samples = 20,
              n_pop = 500, gen_iter = 2, seed = 99)
  expect_identical(a$median_rmsr, b$median_rmsr)
  expect_error(sfa_cd(big5$embeddings, input = "data"), "more cases")
  expect_error(sfa_cd(big5$embeddings, alpha = 2), "alpha")
})
