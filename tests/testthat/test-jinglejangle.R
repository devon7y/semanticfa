test_that("sfa_jinglejangle flags a same-content / different-name pair (jangle)", {
  data(big5)
  ext <- big5$items[big5$factors == "Extraversion"]
  neu <- big5$items[big5$factors == "Neuroticism"]
  scales <- list(Extraversion = ext, Sociability = ext, Neuroticism = neu)

  ie <- list(
    Extraversion = big5$embeddings[big5$factors == "Extraversion", , drop = FALSE],
    Sociability  = big5$embeddings[big5$factors == "Extraversion", , drop = FALSE],
    Neuroticism  = big5$embeddings[big5$factors == "Neuroticism", , drop = FALSE])
  # distinct label vectors so Extraversion vs Sociability share content but not label
  le <- big5$embeddings[match(c("E1", "C31", "N11"), big5$codes), , drop = FALSE]

  jj <- sfa_jinglejangle(scales, item_embeddings = ie, label_embeddings = le,
                         flag = 0.15)
  expect_s3_class(jj, "sfa_jinglejangle")
  expect_equal(dim(jj$content_sim), c(3L, 3L))

  pair <- jj$flags[(jj$flags$scale_a == "Extraversion" &
                      jj$flags$scale_b == "Sociability"), ]
  expect_equal(nrow(pair), 1L)
  expect_identical(pair$type, "jangle")          # identical content, different label
})

test_that("at least two named scales are required", {
  expect_error(sfa_jinglejangle(list(A = c("x", "y"))), "at least two")
  expect_error(sfa_jinglejangle(list(c("x"), c("y"))), "named list")
})
