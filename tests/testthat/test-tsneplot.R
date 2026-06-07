test_that("sfa_tsneplot returns 2-D coordinates for every item", {
  skip_if_not_installed("Rtsne")
  data(big5)
  fit <- suppressWarnings(suppressMessages(sfa(
    data.frame(code = big5$codes, item = big5$items,
               factor = big5$factors, scoring = big5$scoring),
    embeddings = big5$embeddings, scoring = big5$scoring,
    nfactors = 5, seed = 42L)))

  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  res <- sfa_tsneplot(fit)
  grDevices::dev.off()
  unlink(tmp)

  expect_type(res, "list")
  expect_equal(dim(res$Y), c(50L, 2L))
  expect_equal(res$labels, big5$codes)
})

test_that("sfa_tsneplot accepts a similarity matrix and errors on tiny n", {
  skip_if_not_installed("Rtsne")
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "atomic_reversed",
                        scoring = big5$scoring, factors = big5$factors,
                        codes = big5$codes)
  attr(sim, "transformed_embeddings") <- NULL

  tmp <- tempfile(fileext = ".png"); grDevices::png(tmp)
  res <- sfa_tsneplot(sim)
  grDevices::dev.off(); unlink(tmp)
  expect_equal(dim(res$Y), c(50L, 2L))

  expect_error(sfa_tsneplot(sim[1:3, 1:3]), "at least 5")
})
