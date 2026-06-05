test_that("sfa_anchor returns a centroid belonging matrix with expected shape", {
  data(big5)
  df <- data.frame(code = big5$codes, item = big5$items,
                   factor = big5$factors, scoring = big5$scoring,
                   stringsAsFactors = FALSE)
  fit <- suppressWarnings(suppressMessages(
    sfa(df, embeddings = big5$embeddings, scoring = big5$scoring,
        nfactors = 5, seed = 42L)))

  an <- sfa_anchor(fit, anchor = "centroid")
  expect_s3_class(an, "sfa_anchor")
  expect_equal(dim(an$centroid), c(50L, 5L))
  expect_setequal(colnames(an$centroid), unique(big5$factors))
})

test_that("sign-alignment makes reverse-keyed items belong to their construct", {
  data(big5)
  df <- data.frame(code = big5$codes, item = big5$items,
                   factor = big5$factors, scoring = big5$scoring,
                   stringsAsFactors = FALSE)
  fit <- suppressWarnings(suppressMessages(
    sfa(df, embeddings = big5$embeddings, scoring = big5$scoring,
        nfactors = 5, seed = 42L)))

  an <- sfa_anchor(fit, anchor = "centroid")
  own <- an$centroid[cbind(seq_len(50),
                           match(big5$factors, colnames(an$centroid)))]
  rev_items <- big5$scoring == -1

  # reverse-keyed items must NOT be systematically negative on their construct
  expect_gt(mean(own[rev_items]), 0.2)
  # forward and reverse items should have comparable belonging strength
  expect_lt(abs(mean(own[rev_items]) - mean(own[!rev_items])), 0.15)
})

test_that("label anchor accepts precomputed label embeddings of matching dim", {
  data(big5)
  df <- data.frame(code = big5$codes, item = big5$items,
                   factor = big5$factors, scoring = big5$scoring,
                   stringsAsFactors = FALSE)
  fit <- suppressWarnings(suppressMessages(
    sfa(df, embeddings = big5$embeddings, scoring = big5$scoring,
        nfactors = 5, seed = 42L)))

  constructs <- unique(big5$factors)
  lab <- t(vapply(constructs,
                  function(g) colMeans(big5$embeddings[big5$factors == g, ]),
                  numeric(ncol(big5$embeddings))))
  rownames(lab) <- constructs

  an <- sfa_anchor(fit, anchor = "label", label_embeddings = lab)
  expect_equal(dim(an$label), c(50L, 5L))

  # wrong dimension should error
  expect_error(sfa_anchor(fit, anchor = "label",
                          label_embeddings = lab[, 1:10]),
               "dimension")
})

test_that("label anchor without a backend errors helpfully", {
  data(big5)
  df <- data.frame(code = big5$codes, item = big5$items,
                   factor = big5$factors, scoring = big5$scoring,
                   stringsAsFactors = FALSE)
  fit <- suppressWarnings(suppressMessages(
    sfa(df, embeddings = big5$embeddings, scoring = big5$scoring,
        nfactors = 5, seed = 42L)))
  expect_error(sfa_anchor(fit, anchor = "label"), "precomputed embeddings")
})

test_that("sfa_anchor requires factor labels", {
  data(big5)
  fit <- suppressWarnings(suppressMessages(
    sfa(big5$items, embeddings = big5$embeddings, scoring = big5$scoring,
        nfactors = 5, seed = 42L)))   # no factor column
  expect_error(sfa_anchor(fit), "factor labels")
})
