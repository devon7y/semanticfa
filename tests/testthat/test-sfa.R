test_that("sfa() runs end-to-end with bundled data", {
  data(big5)
  fit <- sfa(big5$items, nfactors = 5, embeddings = big5$embeddings,
             scoring = big5$scoring)

  expect_s3_class(fit, "sfa")
  expect_equal(fit$factors, 5)
  expect_s3_class(fit$loadings, "loadings")
  expect_equal(nrow(unclass(fit$loadings)), 50)
  expect_equal(ncol(unclass(fit$loadings)), 5)
  expect_length(fit$communality, 50)
  expect_length(fit$uniquenesses, 50)
  expect_true(!is.null(fit$Phi))
  expect_true(!is.null(fit$Vaccounted))
  expect_true(is.finite(fit$kmo$total))
  expect_true(is.finite(fit$tefi))
  expect_true(is.finite(fit$rmsr))
})

test_that("sfa() works with data.frame input", {
  data(big5)
  df <- data.frame(
    code = big5$codes,
    item = big5$items,
    factor = big5$factors,
    scoring = big5$scoring,
    stringsAsFactors = FALSE
  )
  fit <- sfa(df, nfactors = 5, embeddings = big5$embeddings)
  expect_s3_class(fit, "sfa")
  expect_equal(rownames(unclass(fit$loadings))[1], "E1")
  expect_true(!is.null(fit$daal))
})

test_that("sfa() works with named character vector", {
  data(big5)
  items <- stats::setNames(big5$items, big5$codes)
  fit <- sfa(items, nfactors = 5, embeddings = big5$embeddings,
             scoring = big5$scoring)
  expect_equal(rownames(unclass(fit$loadings))[1], "E1")
})

test_that("as_psych returns a psych fa object", {
  data(big5)
  fit <- sfa(big5$items, nfactors = 5, embeddings = big5$embeddings,
             scoring = big5$scoring)
  fa_obj <- as_psych(fit)
  expect_true(inherits(fa_obj, "psych") || inherits(fa_obj, "fa"))
})

test_that("print.sfa works without error", {
  data(big5)
  fit <- sfa(big5$items, nfactors = 5, embeddings = big5$embeddings,
             scoring = big5$scoring)
  expect_output(print(fit), "Semantic Factor Analysis")
})

test_that("summary.sfa works without error", {
  data(big5)
  fit <- sfa(big5$items, nfactors = 5, embeddings = big5$embeddings,
             scoring = big5$scoring)
  expect_output(summary(fit), "omega")
})

test_that("plot.sfa scree works", {
  data(big5)
  fit <- sfa(big5$items, nfactors = 5, embeddings = big5$embeddings,
             scoring = big5$scoring)
  expect_silent(plot(fit, type = "scree"))
})

test_that("Heywood cases are detected", {
  data(big5)
  fit <- sfa(big5$items, nfactors = 5, embeddings = big5$embeddings,
             scoring = big5$scoring)
  expect_true(is.logical(fit$heywood))
  expect_length(fit$heywood, 50)
})

test_that("calibration runs when requested", {
  data(big5)
  fit <- sfa(big5$items, nfactors = 5, embeddings = big5$embeddings,
             scoring = big5$scoring, calibrate = TRUE, calibrate_iter = 5)
  expect_true(!is.null(fit$calibration))
  expect_true("rmsr" %in% names(fit$calibration))
})
