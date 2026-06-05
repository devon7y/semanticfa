test_that("sfa_redundancy flags an injected duplicate pair", {
  data(big5)
  emb <- big5$embeddings
  emb[2, ] <- emb[1, ]                         # make items 1 and 2 identical
  df <- data.frame(code = big5$codes, item = big5$items, factor = big5$factors,
                   scoring = big5$scoring, stringsAsFactors = FALSE)
  fit <- suppressWarnings(suppressMessages(
    sfa(df, embeddings = emb, scoring = big5$scoring, nfactors = 5, seed = 42L)))

  rd <- sfa_redundancy(fit, threshold = 0.95, method = "cosine")
  expect_s3_class(rd, "sfa_redundancy")
  pair_codes <- c(rd$pairs$item_i, rd$pairs$item_j)
  expect_true(all(big5$codes[1:2] %in% pair_codes))
  # one of the twins should be suggested for removal
  expect_true(any(big5$codes[1:2] %in% rd$suggest_remove))
})

test_that("wto and cosine methods both run and return clusters", {
  fit <- local({
    data(big5)
    df <- data.frame(code = big5$codes, item = big5$items, factor = big5$factors,
                     scoring = big5$scoring, stringsAsFactors = FALSE)
    suppressWarnings(suppressMessages(
      sfa(df, embeddings = big5$embeddings, scoring = big5$scoring,
          nfactors = 5, seed = 42L)))
  })
  rc <- sfa_redundancy(fit, threshold = 0.5, method = "cosine")
  rw <- sfa_redundancy(fit, threshold = 0.5, method = "wto")
  expect_true(is.list(rc$clusters))
  expect_true(is.list(rw$clusters))
  expect_true(is.data.frame(rc$pairs))
})

test_that("a square similarity matrix can be passed directly", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "atomic_reversed",
                        scoring = big5$scoring)
  attr(sim, "transformed_embeddings") <- NULL
  rownames(sim) <- colnames(sim) <- big5$codes
  rd <- sfa_redundancy(sim, threshold = 0.7, method = "cosine")
  expect_s3_class(rd, "sfa_redundancy")
})
