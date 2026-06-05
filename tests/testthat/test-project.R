make_fit <- function() {
  data(big5)
  df <- data.frame(code = big5$codes, item = big5$items, factor = big5$factors,
                   scoring = big5$scoring, stringsAsFactors = FALSE)
  suppressWarnings(suppressMessages(
    sfa(df, embeddings = big5$embeddings, scoring = big5$scoring,
        nfactors = 5, seed = 42L)))
}

test_that("sfa_project returns an item-by-axis matrix with precomputed poles", {
  data(big5)
  fit <- make_fit()
  axes <- list(NtoE = c(low = "neurotic", high = "extraverted"))
  poles <- list(NtoE = list(
    low  = big5$embeddings[big5$factors == "Neuroticism", , drop = FALSE],
    high = big5$embeddings[big5$factors == "Extraversion", , drop = FALSE]))
  pr <- sfa_project(fit, axes = axes, pole_embeddings = poles)

  expect_s3_class(pr, "sfa_projection")
  expect_equal(dim(pr$scores), c(50L, 1L))
  expect_equal(colnames(pr$scores), "NtoE")
  # normalized: Extraversion items should sit higher on the N->E axis than Neuroticism items
  e <- mean(pr$scores[big5$factors == "Extraversion", 1])
  n <- mean(pr$scores[big5$factors == "Neuroticism", 1])
  expect_gt(e, n)
})

test_that("pole/item dimension mismatch errors", {
  fit <- make_fit()
  bad <- list(NtoE = list(low = matrix(0, 1, 10), high = matrix(1, 1, 10)))
  expect_error(
    sfa_project(fit, axes = list(NtoE = c(low = "a", high = "b")),
                pole_embeddings = bad),
    "dim")
})

test_that("axes must be a named list", {
  fit <- make_fit()
  expect_error(sfa_project(fit, axes = list(c(low = "a", high = "b"))),
               "named list")
})
