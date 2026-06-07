test_that("sfa_similarity and sfa accept an sfa_embeddings object", {
  data(big5)
  emb_obj <- structure(
    list(embeddings = big5$embeddings, codes = big5$codes,
         items = big5$items, factors = big5$factors, scoring = big5$scoring),
    class = "sfa_embeddings")

  sim <- sfa_similarity(emb_obj)                 # pulls scoring/factors/codes
  expect_equal(dim(sim), c(50L, 50L))
  expect_equal(attr(sim, "factors"), big5$factors)
  expect_equal(attr(sim, "codes"), big5$codes)

  fit <- suppressWarnings(suppressMessages(sfa(emb_obj, nfactors = 5)))
  expect_s3_class(fit, "sfa")
  expect_equal(fit$item_data$factor, big5$factors)   # theoretical grouping carried in
  expect_equal(fit$item_data$code, big5$codes)
})

test_that("sfa_load_npz round-trips a .npz archive", {
  skip_on_cran()                                   # importing numpy may provision Python
  skip_if_not_installed("reticulate")
  np <- tryCatch(reticulate::import("numpy"), error = function(e) NULL)
  skip_if(is.null(np), "numpy not available")

  data(big5)
  tmp <- tempfile(fileext = ".npz")
  np$savez(tmp, embeddings = big5$embeddings,
           codes = big5$codes, factors = big5$factors,
           scoring = as.integer(big5$scoring))

  e <- sfa_load_npz(tmp)
  expect_s3_class(e, "sfa_embeddings")
  expect_equal(dim(e$embeddings), c(50L, 384L))
  expect_equal(e$codes, big5$codes)
  expect_equal(e$factors, big5$factors)
  expect_equal(e$scoring, as.integer(big5$scoring))
  expect_equal(rownames(e$embeddings), big5$codes)
})

test_that("sfa_load_npz errors clearly on a missing file / missing key", {
  expect_error(sfa_load_npz("/no/such/file.npz"), "not found")
})

test_that("sfa_corplot 'order' accepts abbreviations and errors on no-match", {
  data(big5)
  emb_obj <- structure(
    list(embeddings = big5$embeddings, codes = big5$codes,
         factors = big5$factors, scoring = big5$scoring),
    class = "sfa_embeddings")
  sim <- sfa_similarity(emb_obj)

  lv <- semanticfa:::.resolve_group_order(c("E", "N", "A", "C", "O"),
                                          unique(big5$factors))
  expect_equal(lv[1], "Extraversion")
  expect_equal(lv[2], "Neuroticism")
  expect_setequal(lv, unique(big5$factors))

  # unmentioned factors are appended
  lv2 <- semanticfa:::.resolve_group_order("Open", unique(big5$factors))
  expect_equal(lv2[1], "Openness")
  expect_length(lv2, length(unique(big5$factors)))

  # no-match errors
  expect_error(semanticfa:::.resolve_group_order("Zzz", unique(big5$factors)),
               "matches no factor")
})
