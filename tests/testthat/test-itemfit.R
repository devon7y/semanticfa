make_big5_fit <- function() {
  data(big5)
  suppressWarnings(suppressMessages(sfa(
    data.frame(code = big5$codes, item = big5$items,
               factor = big5$factors, scoring = big5$scoring),
    embeddings = big5$embeddings, scoring = big5$scoring,
    nfactors = 5, seed = 42L)))
}

# embed lookup: existing item text -> its raw embedding; construct name -> the
# construct's raw centroid. Lets us test without a live embedding backend.
big5_lookup <- function(txt, ...) {
  data(big5)
  out <- matrix(0, length(txt), ncol(big5$embeddings))
  for (i in seq_along(txt)) {
    mi <- match(txt[i], big5$items)
    out[i, ] <- if (!is.na(mi)) big5$embeddings[mi, ]
                else colMeans(big5$embeddings[big5$factors == txt[i], , drop = FALSE])
  }
  out
}

test_that("an existing item lands in its own construct and is flagged redundant", {
  data(big5)
  fit <- make_big5_fit()
  fwd <- which(big5$scoring == 1)[1]            # a forward-keyed item
  res <- sfa_item_fit(fit, big5$items[fwd], embed = big5_lookup, model = "x")

  expect_s3_class(res, "sfa_item_fit")
  expect_equal(res$summary$best[1], big5$factors[fwd])    # own construct
  expect_equal(res$summary$nearest[1], big5$codes[fwd])   # nearest = itself
  expect_gt(res$summary$nearest_sim[1], 0.99)             # ~ identical
  expect_match(res$summary$verdict[1], "redundant")
})

test_that("reverse_key flips the candidate (own-construct similarity negates)", {
  data(big5)
  fit <- make_big5_fit()
  fwd <- which(big5$scoring == 1)[1]
  f <- sfa_item_fit(fit, big5$items[fwd], embed = big5_lookup, model = "x")
  r <- sfa_item_fit(fit, big5$items[fwd], embed = big5_lookup, model = "x",
                    reverse_key = TRUE)
  own <- big5$factors[fwd]
  expect_gt(f$similarity_to_items[1, own], 0)
  expect_lt(r$similarity_to_items[1, own], 0)             # flipped
})

test_that("sfa_item_fit errors without an embedding model", {
  fit <- make_big5_fit()                                   # precomputed, no model
  expect_error(sfa_item_fit(fit, "some new item"), "embedding model")
})

test_that(".match_construct accepts prefixes and errors clearly", {
  data(big5)
  expect_equal(semanticfa:::.match_construct("Extra", unique(big5$factors)),
               "Extraversion")
  expect_error(semanticfa:::.match_construct("Zzz", unique(big5$factors)),
               "matches no factor")
})

test_that("a single-construct scale does not crash sfa_item_fit or its print", {
  data(big5)
  sub <- 1:10
  df <- data.frame(code = big5$codes[sub], item = big5$items[sub],
                   factor = rep("General", length(sub)),
                   scoring = big5$scoring[sub], stringsAsFactors = FALSE)
  fit <- suppressWarnings(suppressMessages(
    sfa(df, embeddings = big5$embeddings[sub, ], scoring = big5$scoring[sub],
        nfactors = 1, seed = 42L)))
  lk <- function(txt, ...) {
    out <- matrix(0, length(txt), ncol(big5$embeddings))
    for (i in seq_along(txt)) {
      mi <- match(txt[i], big5$items[sub])
      out[i, ] <- if (!is.na(mi)) big5$embeddings[sub, ][mi, ]
                  else colMeans(big5$embeddings[sub, , drop = FALSE])
    }
    out
  }
  res <- sfa_item_fit(fit, big5$items[sub][3], embed = lk, model = "x")
  expect_s3_class(res, "sfa_item_fit")
  expect_true(is.na(res$summary$gap[1]))                 # no 2nd construct
  expect_output(print(res), "single construct")          # print must not crash
})

test_that("a singleton (one-item) construct does not crash sfa_item_fit", {
  data(big5)
  idx <- 1:6
  df <- data.frame(code = big5$codes[idx], item = big5$items[idx],
                   factor = c(rep("Big", 5), "Solo"),
                   scoring = big5$scoring[idx], stringsAsFactors = FALSE)
  fit <- suppressWarnings(suppressMessages(
    sfa(df, embeddings = big5$embeddings[idx, ], scoring = big5$scoring[idx],
        nfactors = 1, seed = 42L)))
  lk <- function(txt, ...) {
    out <- matrix(0, length(txt), ncol(big5$embeddings))
    for (i in seq_along(txt)) {
      mi <- match(txt[i], big5$items[idx])
      out[i, ] <- if (!is.na(mi)) big5$embeddings[idx, ][mi, ]
                  else if (identical(txt[i], "Solo")) big5$embeddings[idx[6], ]
                  else colMeans(big5$embeddings[idx[1:5], , drop = FALSE])
    }
    out
  }
  # candidate == the solo item -> best construct is the singleton (NaN baseline)
  res <- sfa_item_fit(fit, big5$items[idx[6]], embed = lk, model = "x")
  expect_s3_class(res, "sfa_item_fit")
  expect_false(is.finite(res$avg_item_fit[["Solo"]]))    # NaN baseline, handled
  expect_output(print(res))                              # must not crash
})
