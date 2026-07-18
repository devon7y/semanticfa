# sem-k retention: argument validation runs everywhere; the numerical
# parity test needs Python + the model artifact, so it is skipped on CRAN
# and on machines without the stack.

.semk_ready <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) return(FALSE)
  art <- tryCatch(semanticfa:::.semk_artifact(download = FALSE, quiet = TRUE),
                  error = function(e) NULL)
  if (is.null(art)) return(FALSE)
  # exercises the real bridge: py_require declarations + vendored import
  mod <- tryCatch(semanticfa:::.semk_py(), error = function(e) NULL)
  !is.null(mod)
}

test_that("sfa_semk validates its inputs", {
  expect_error(sfa_semk(), "'embeddings' is required")
  expect_error(sfa_semk(embeddings = matrix(rnorm(20), 5, 4)),
               "at least 8 items")
  expect_error(
    sfa_semk(embeddings = matrix(rnorm(80), 10, 8), floor = "high"),
    "'floor' must be a single finite number")
})

test_that("uncached artifact without download permission errors cleanly", {
  withr::local_options(semanticfa.semk_artifact = NULL)
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  expect_error(semanticfa:::.semk_artifact(download = FALSE),
               "not cached yet")
})

test_that("artifact checksum mismatch is caught and file removed", {
  withr::local_options(semanticfa.semk_artifact = NULL)
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  dest <- file.path(semanticfa:::.semk_dir(), semanticfa:::.SFA_SEMK_FILE)
  writeLines("not a joblib", dest)
  expect_error(semanticfa:::.semk_artifact(download = FALSE),
               "checksum mismatch")
  expect_false(file.exists(dest))
})

test_that("sem-k reproduces the published Big Five verdict", {
  skip_on_cran()
  skip_if_not(.semk_ready(), "python stack or sem-k artifact unavailable")

  data(big5, package = "semanticfa")
  sim <- sfa_similarity(big5$embeddings, "mean_centered_pearson")
  res <- sfa_semk(sim, big5$embeddings, download = FALSE)

  expect_s3_class(res, "sfa_semk")
  expect_identical(res$n_factors, 5L)
  expect_true(res$lo90 >= 1L && res$lo90 <= res$n_factors)
  expect_true(res$hi90 >= res$n_factors)
  expect_named(res$battery, c("kaiser", "pa_iso", "ekc", "map"))
  expect_output(print(res), "sem-k calibrated semantic factor retention")
})

test_that("sem-k verdicts are seed-invariant on big5", {
  skip_on_cran()
  skip_if_not(.semk_ready(), "python stack or sem-k artifact unavailable")

  data(big5, package = "semanticfa")
  r42 <- sfa_semk(embeddings = big5$embeddings, seed = 42L,
                  download = FALSE)
  r43 <- sfa_semk(embeddings = big5$embeddings, seed = 43L,
                  download = FALSE)
  expect_identical(r42$n_factors, r43$n_factors)
})

test_that("sfa_nfactors dispatches semk and reports it", {
  skip_on_cran()
  skip_if_not(.semk_ready(), "python stack or sem-k artifact unavailable")

  data(big5, package = "semanticfa")
  sim <- sfa_similarity(big5$embeddings, "mean_centered_pearson")
  nf <- sfa_nfactors(sim, big5$embeddings, methods = c("kaiser", "semk"))
  expect_true("semk" %in% nf$methods$method)
  expect_identical(nf$methods$n_factors[nf$methods$method == "semk"], 5L)
})
