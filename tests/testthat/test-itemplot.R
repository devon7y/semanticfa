make_big5_mapfit <- function() {
  data(big5)
  suppressWarnings(suppressMessages(sfa(
    data.frame(code = big5$codes, item = big5$items,
               factor = big5$factors, scoring = big5$scoring),
    embeddings = big5$embeddings, scoring = big5$scoring,
    nfactors = 5, seed = 42L)))
}

draw <- function(expr) {
  tmp <- tempfile(fileext = ".png"); grDevices::png(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) })
  force(expr)
}

test_that("PCA and MDS need no extra package and return 2-D coords", {
  data(big5)
  fit <- make_big5_mapfit()
  for (m in c("pca", "mds")) {
    res <- NULL
    draw(res <- sfa_itemplot(fit, method = m))
    expect_equal(dim(res$Y), c(50L, 2L))
    expect_equal(res$method, m)
    expect_equal(res$labels, big5$codes)
  }
})

test_that("t-SNE (default) returns 2-D coords for every item", {
  skip_if_not_installed("Rtsne")
  data(big5)
  fit <- make_big5_mapfit()
  res <- NULL
  draw(res <- sfa_itemplot(fit))
  expect_equal(dim(res$Y), c(50L, 2L))
  expect_equal(res$method, "tsne")
})

test_that("UMAP returns 2-D coords for every item", {
  skip_if_not_installed("uwot")
  data(big5)
  fit <- make_big5_mapfit()
  res <- NULL
  draw(res <- sfa_itemplot(fit, method = "umap"))
  expect_equal(dim(res$Y), c(50L, 2L))
  expect_equal(res$method, "umap")
})

test_that("a similarity matrix works and errors are clear", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "atomic_reversed",
                        scoring = big5$scoring, factors = big5$factors,
                        codes = big5$codes)
  attr(sim, "transformed_embeddings") <- NULL

  res <- NULL
  draw(res <- sfa_itemplot(sim, method = "mds"))     # base-R path, no deps
  expect_equal(dim(res$Y), c(50L, 2L))

  # tsne/umap need >= 5 items; pca/mds only need >= 3
  expect_error(sfa_itemplot(sim[1:4, 1:4], method = "tsne"), "at least 5")
  expect_error(sfa_itemplot(sim[1:2, 1:2], method = "pca"), "at least 3")
})

test_that("sfa_tsneplot is a deprecated alias that still works", {
  data(big5)
  fit <- make_big5_mapfit()
  res <- NULL
  draw(expect_warning(res <- sfa_tsneplot(fit, method = "pca"), "deprecated|sfa_itemplot"))
  expect_equal(dim(res$Y), c(50L, 2L))
  expect_equal(res$method, "pca")
})

test_that("degenerate inputs do not crash any method (pad-to-2)", {
  data(big5)
  # 1-D embeddings: PCA used to crash on $x[,1:2]
  df <- data.frame(code = big5$codes[1:6], item = big5$items[1:6],
                   factor = big5$factors[1:6], scoring = big5$scoring[1:6],
                   stringsAsFactors = FALSE)
  fit1d <- suppressWarnings(suppressMessages(
    sfa(df, embeddings = big5$embeddings[1:6, 1, drop = FALSE],
        scoring = big5$scoring[1:6], nfactors = 1, seed = 42L)))
  res <- NULL
  draw(res <- sfa_itemplot(fit1d, method = "pca"))
  expect_equal(dim(res$Y), c(6L, 2L))

  # fully degenerate distances: cmdscale returns 0 columns -> MDS used to crash
  sim1 <- matrix(1, 5, 5); diag(sim1) <- 1
  draw(res <- suppressWarnings(sfa_itemplot(sim1, method = "mds")))
  expect_equal(dim(res$Y), c(5L, 2L))
})

test_that("labels fall back to similarity-matrix rownames", {
  data(big5)
  sim <- sfa_similarity(big5$embeddings, encoding = "atomic_reversed",
                        scoring = big5$scoring)
  attr(sim, "transformed_embeddings") <- NULL
  attr(sim, "codes") <- NULL
  rownames(sim) <- colnames(sim) <- big5$codes
  res <- NULL
  draw(res <- sfa_itemplot(sim, method = "pca"))
  expect_equal(res$labels, big5$codes)
})
