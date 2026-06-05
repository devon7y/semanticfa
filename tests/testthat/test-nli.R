# A deterministic mock NLI classifier so the tests need no Python/model.
mock_nli <- function(prem, hyp) {
  same_first <- substr(prem, 1, 3) == substr(hyp, 1, 3)
  data.frame(entailment   = ifelse(same_first, 0.80, 0.10),
             contradiction = ifelse(same_first, 0.05, 0.55))
}

test_that("sfa_nli_matrix builds a signed symmetric matrix", {
  data(big5)
  items <- big5$items[1:6]
  M <- sfa_nli_matrix(items, classifier = mock_nli)

  expect_equal(dim(M), c(6L, 6L))
  expect_true(all(diag(M) == 1))
  expect_equal(M, t(M))
  expect_true(all(M >= -1 & M <= 1))
})

test_that("classifier output is validated", {
  bad <- function(p, h) data.frame(foo = rep(0, length(p)))
  expect_error(sfa_nli_matrix(c("a", "b", "c"), classifier = bad),
               "entailment")
})

test_that("an NLI matrix can drive sfa() via similarity=", {
  data(big5)
  items <- big5$items[1:8]
  M <- sfa_nli_matrix(items, classifier = mock_nli)
  fit <- suppressWarnings(suppressMessages(sfa(items, similarity = M, nfactors = 2)))
  expect_s3_class(fit, "sfa")
  expect_equal(fit$embed_method, "precomputed_similarity")
  expect_equal(ncol(fit$loadings), 2L)
})

test_that("sfa() validates similarity-matrix dimensions", {
  data(big5)
  expect_error(
    suppressMessages(sfa(big5$items[1:5], similarity = matrix(0, 4, 4))),
    "matching the items")
})
