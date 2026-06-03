test_that("custom embedding function works", {
  items <- c("I am happy", "I am sad", "I am neutral")
  custom_fn <- function(text, ...) {
    matrix(rnorm(length(text) * 10), nrow = length(text), ncol = 10)
  }
  emb <- sfa_embed(items, embed = custom_fn)
  expect_true(is.matrix(emb))
  expect_equal(nrow(emb), 3)
  expect_equal(ncol(emb), 10)
})

test_that("sbert backend errors informatively without reticulate", {
  skip_if(requireNamespace("reticulate", quietly = TRUE))
  expect_error(sfa_embed("test", embed = "sbert"), "reticulate")
})

test_that("openai backend errors without API key", {
  skip_if(!requireNamespace("httr2", quietly = TRUE))
  withr::with_envvar(c(OPENAI_API_KEY = ""), {
    expect_error(sfa_embed("test", embed = "openai"), "OPENAI_API_KEY")
  })
})

test_that("cache key is deterministic", {
  key1 <- semanticfa:::.cache_key(c("a", "b"), "model1")
  key2 <- semanticfa:::.cache_key(c("a", "b"), "model1")
  expect_identical(key1, key2)

  key3 <- semanticfa:::.cache_key(c("a", "c"), "model1")
  expect_false(key1 == key3)
})
