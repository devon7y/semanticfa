test_that("KMO computes on a correlation matrix", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "atomic_reversed",
                        scoring = big5$scoring)
  kmo <- semanticfa:::.compute_kmo(sim)
  expect_true(kmo$total > 0 && kmo$total <= 1)
  expect_length(kmo$per_item, 50)
})

test_that("TEFI returns a finite value", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "atomic_reversed",
                        scoring = big5$scoring)
  tefi <- semanticfa:::.compute_tefi(sim)
  expect_true(is.finite(tefi))
  expect_true(tefi > 0)
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
