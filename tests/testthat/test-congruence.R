test_that("NMI returns 1 for identical partitions", {
  labels <- c("A", "A", "B", "B", "C", "C")
  expect_equal(semanticfa:::.compute_nmi(labels, labels), 1, tolerance = 1e-10)
})

test_that("ARI returns 1 for identical partitions", {
  labels <- c("A", "A", "B", "B", "C", "C")
  expect_equal(semanticfa:::.compute_ari(labels, labels), 1, tolerance = 1e-10)
})

test_that("Frobenius similarity of identical matrices is 1", {
  m <- matrix(c(1, 0.3, 0.3, 1), 2, 2)
  expect_equal(semanticfa:::.compute_frobenius(m, m), 1, tolerance = 1e-10)
})

test_that("sfa_congruence works with a psych fa object", {
  data(big5)
  fit <- sfa(big5$items, nfactors = 5, embeddings = big5$embeddings,
             scoring = big5$scoring)
  cong <- sfa_congruence(fit, as_psych(fit))
  expect_s3_class(cong, "sfa_congruence")
  expect_true(!is.null(cong$tucker))
  expect_true(!is.null(cong$nmi))
  expect_true(!is.null(cong$ari))
})

test_that("sfa_congruence works with factor labels", {
  data(big5)
  fit <- sfa(big5$items, nfactors = 5, embeddings = big5$embeddings,
             scoring = big5$scoring)
  cong <- sfa_congruence(fit, big5$factors,
                         metrics = c("nmi", "ari"))
  expect_true(is.numeric(cong$nmi))
  expect_true(is.numeric(cong$ari))
})

test_that("disattenuated metric is computed", {
  data(big5)
  fit <- sfa(big5$items, nfactors = 5, embeddings = big5$embeddings,
             scoring = big5$scoring)
  # self-comparison
  cong <- sfa_congruence(fit, fit$sim_matrix,
                         metrics = "disattenuated")
  expect_true(is.numeric(cong$disattenuated))
  expect_true(cong$disattenuated >= 0.9)
})
