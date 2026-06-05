make_big5_fit <- function() {
  data(big5)
  df <- data.frame(code = big5$codes, item = big5$items,
                   factor = big5$factors, scoring = big5$scoring,
                   stringsAsFactors = FALSE)
  suppressWarnings(suppressMessages(
    sfa(df, embeddings = big5$embeddings, scoring = big5$scoring,
        nfactors = 5, seed = 42L)))
}

test_that("sfa_simplify(anchor) keeps target_n per construct and reports fidelity", {
  fit <- make_big5_fit()
  s <- sfa_simplify(fit, target_n = 5, method = "anchor")

  expect_s3_class(s, "sfa_simplify")
  expect_length(s$keep, 25L)                      # 5 constructs x 5
  expect_equal(nrow(s$drop), 25L)
  expect_true(all(s$fidelity$per_construct == 5L))
  expect_true(is.finite(s$fidelity$nmi_reduced))
  expect_s3_class(s$reduced_fit, "sfa")
})

test_that("anchor-based pruning does not discard all reverse-keyed items", {
  data(big5)
  fit <- make_big5_fit()
  s <- sfa_simplify(fit, target_n = 5, method = "anchor")
  kept_scoring <- big5$scoring[match(s$keep, big5$codes)]
  # the sign-alignment fix means reverse items survive on merit
  expect_gt(sum(kept_scoring == -1), 0L)
})

test_that("medoid method also returns target_n per construct", {
  fit <- make_big5_fit()
  s <- sfa_simplify(fit, target_n = 4, method = "medoid")
  expect_true(all(s$fidelity$per_construct == 4L))
  expect_length(s$keep, 20L)
})

test_that("constructs with <= target_n items are kept whole", {
  fit <- make_big5_fit()
  s <- sfa_simplify(fit, target_n = 100, method = "anchor")
  expect_length(s$keep, 50L)
  expect_equal(nrow(s$drop), 0L)
})

test_that("sfa_simplify validates inputs", {
  fit <- make_big5_fit()
  expect_error(sfa_simplify(fit, target_n = 0), "positive integer")
  expect_error(sfa_simplify("not an sfa", target_n = 5), "'sfa' object")
})

test_that("groups = 'fitted' uses the extracted factors and needs no theory key", {
  data(big5)
  fit_no_theory <- suppressWarnings(suppressMessages(
    sfa(big5$items, embeddings = big5$embeddings, scoring = big5$scoring,
        nfactors = 5, seed = 42L)))            # character items -> no factor column
  # theoretical grouping unavailable -> errors
  expect_error(sfa_simplify(fit_no_theory, target_n = 5, groups = "theoretical"),
               "theoretical")
  # fitted grouping works without a key
  s <- sfa_simplify(fit_no_theory, target_n = 5, groups = "fitted")
  expect_s3_class(s, "sfa_simplify")
  expect_equal(s$groups, "fitted")
  expect_length(s$keep, 25L)
})
