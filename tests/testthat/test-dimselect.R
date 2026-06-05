test_that("sfa_dimselect returns a valid landscape and depth", {
  skip_if_not_installed("EGAnet")
  data(big5)
  # keep it quick: cap the depth range and use a coarse step
  ds <- sfa_dimselect(big5$embeddings, factors = big5$factors,
                      scoring = big5$scoring, encoding = "atomic_reversed",
                      max_depth = 150L, step = 15L)

  expect_s3_class(ds, "sfa_dimselect")
  expect_true(ds$optimal_depth >= 3L && ds$optimal_depth <= 150L)
  expect_true(all(c("depth", "n_dim", "nmi", "tefi", "composite") %in%
                    names(ds$trajectory)))
  # composite optimum matches the reported depth
  fin <- ds$trajectory[is.finite(ds$trajectory$composite), ]
  expect_equal(ds$optimal_depth,
               fin$depth[which.max(fin$composite)])
  # NMI term was used (labels supplied)
  expect_true(ds$used_nmi)
})

test_that("composite weights are honored and NMI is in [0,1]", {
  skip_if_not_installed("EGAnet")
  data(big5)
  ds <- sfa_dimselect(big5$embeddings, factors = big5$factors,
                      scoring = big5$scoring, max_depth = 120L, step = 20L,
                      weights = c(nmi = 0.5, tefi = 0.5))
  expect_equal(unname(ds$weights[["nmi"]]), 0.5)
  nmi <- ds$trajectory$nmi
  nmi <- nmi[is.finite(nmi)]
  expect_true(all(nmi >= 0 & nmi <= 1))
})

test_that("TEFI-only fallback warns when no labels are supplied", {
  skip_if_not_installed("EGAnet")
  data(big5)
  expect_warning(
    sfa_dimselect(big5$embeddings, scoring = big5$scoring,
                  max_depth = 80L, step = 20L),
    "TEFI"
  )
})

test_that("sfa(dim_select='dynega') reduces the embedding and records it", {
  skip_if_not_installed("EGAnet")
  data(big5)
  df <- data.frame(code = big5$codes, item = big5$items, factor = big5$factors,
                   scoring = big5$scoring, stringsAsFactors = FALSE)
  fit <- suppressWarnings(suppressMessages(
    sfa(df, embeddings = big5$embeddings, scoring = big5$scoring,
        dim_select = "dynega", seed = 42L)))

  expect_s3_class(fit$dim_select, "sfa_dimselect")
  expect_equal(fit$embedding_dim, fit$dim_select$optimal_depth)
  expect_lte(fit$embedding_dim, ncol(big5$embeddings))
})
