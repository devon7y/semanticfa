# Offline unit tests for the naming machinery. No Python, no downloads:
# pools are plain R matrices with hand-constructed geometry so every
# expected outcome is checkable by eye.

# tiny helper: unit-normalize rows
unit <- function(m) m / sqrt(rowSums(m^2))

# A 6-word pool in 4 dimensions with two word families and a tier-1 flag
# layout chosen to exercise the gate and the label rule.
toy_pool <- function() {
  words <- data.frame(
    word   = c("anxiety", "anxiousness", "racing heart", "calmness",
               "depression", "sadness"),
    family = c("anxiety", "anxiety", "racing heart", "calmness",
               "depression", "sadness"),
    tier1  = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  emb <- unit(rbind(
    c(1.00, 0.10, 0, 0),   # anxiety
    c(0.99, 0.12, 0, 0),   # anxiousness (same family as anxiety)
    c(0.98, 0.20, 0, 0),   # racing heart (closest to target, not tier1)
    c(-1.0, 0.05, 0, 0),   # calmness
    c(0, 0, 1.00, 0.1),    # depression
    c(0, 0, 0.95, 0.2)     # sadness
  ))
  list(words = words, emb = emb, dim = 4L, model = "toy", precision = "fp16")
}

test_that("gate walk deduplicates families and keeps ranking order", {
  pool <- toy_pool()
  # ranked indices: racing heart, anxiety, anxiousness, calmness
  gate <- semanticfa:::.sfa_gate(pool$words, idx = c(3L, 1L, 2L, 4L),
                                 score = c(.99, .98, .97, .10))
  expect_equal(gate$word[1:2], c("racing heart", "anxiety"))
  # anxiousness (same family as anxiety) must be skipped
  expect_false("anxiousness" %in% gate$word)
  expect_equal(gate$word[3], "calmness")
})

test_that("label rule prefers the first tier-1 candidate, else top word", {
  pool <- toy_pool()
  gate <- semanticfa:::.sfa_gate(pool$words, idx = c(3L, 1L, 4L),
                                 score = c(.99, .98, .10))
  pick <- semanticfa:::.sfa_pick(gate, n_candidates = 5L)
  expect_equal(pick$label, "anxiety")     # racing heart outranks but isn't tier1
  expect_equal(pick$rule, "tier1")

  gate2 <- gate[gate$word == "racing heart", , drop = FALSE]
  pick2 <- semanticfa:::.sfa_pick(gate2, n_candidates = 5L)
  expect_equal(pick2$label, "racing heart")
  expect_equal(pick2$rule, "top1")
})

test_that("naming targets: pole restriction, weighting, grand-mean contrast", {
  # four items: three load + on F1, one loads - (reverse item); the reverse
  # item must be excluded from the centroid.
  q <- unit(rbind(
    c(1, 0, 0, 0),
    c(0.9, 0.1, 0, 0),
    c(0.8, 0.2, 0, 0),
    c(-1, 0, 0, 0)
  ))
  L <- cbind(F1 = c(0.8, 0.7, 0.6, -0.5))
  fit <- structure(list(loadings = L), class = "sfa")
  tg <- semanticfa:::.sfa_name_targets(fit, q)
  expect_equal(tg$n_items, 3L)            # reverse item dropped
  expect_equal(dim(tg$targets), c(1L, 4L))
  expect_equal(sqrt(sum(tg$targets^2)), 1, tolerance = 1e-8)
  # the grand mean (which includes the reverse item) was subtracted:
  # the target must not simply be the centroid direction
  cen <- colSums(q[1:3, ] * (c(.8, .7, .6) / sum(c(.8, .7, .6))))
  cen <- cen / sqrt(sum(cen^2))
  expect_gt(sum((tg$targets[1, ] - cen)^2), 1e-6)
})

test_that("blocked top-k retrieval matches direct computation", {
  set.seed(1)
  emb <- unit(matrix(rnorm(200 * 8), 200))
  pool <- list(words = data.frame(word = paste0("w", 1:200),
                                  family = paste0("w", 1:200),
                                  tier1 = TRUE),
               emb = emb, dim = 8L)
  targets <- unit(matrix(rnorm(3 * 8), 3))
  top <- semanticfa:::.sfa_pool_top(pool, targets, k = 10L, block_size = 37L)
  for (j in 1:3) {
    direct <- order(emb %*% targets[j, ], decreasing = TRUE)[1:10]
    expect_equal(top$idx[[j]], direct)
  }
})

test_that("keeper resolves duplicate labels toward the closer factor", {
  # two factors, both would pick family "anxiety"; F2 is closer
  gate1 <- data.frame(word = c("anxiety", "worry"), family = c("anxiety", "worry"),
                      tier1 = c(TRUE, TRUE), score = c(0.80, 0.70),
                      row = 1:2, stringsAsFactors = FALSE)
  gate2 <- data.frame(word = c("anxiety", "panic"), family = c("anxiety", "panic"),
                      tier1 = c(TRUE, TRUE), score = c(0.90, 0.60),
                      row = c(1L, 3L), stringsAsFactors = FALSE)
  picks <- list(semanticfa:::.sfa_pick(gate1), semanticfa:::.sfa_pick(gate2))
  res <- semanticfa:::.sfa_keeper(picks, list(gate1, gate2),
                                  loo = list(NULL, NULL), n_candidates = 5L)
  expect_equal(res$picks[[2]]$label, "anxiety")   # higher score keeps it
  expect_equal(res$picks[[1]]$label, "worry")     # loser re-picks
  expect_true(res$moved[1])
  expect_false(res$moved[2])
})

test_that("keeper prefers the loser's LOO-set members when re-picking", {
  gate1 <- data.frame(word = c("anxiety", "worry", "dread"),
                      family = c("anxiety", "worry", "dread"),
                      tier1 = c(TRUE, TRUE, TRUE),
                      score = c(0.80, 0.70, 0.65),
                      row = 1:3, stringsAsFactors = FALSE)
  gate2 <- data.frame(word = "anxiety", family = "anxiety", tier1 = TRUE,
                      score = 0.90, row = 1L, stringsAsFactors = FALSE)
  picks <- list(semanticfa:::.sfa_pick(gate1), semanticfa:::.sfa_pick(gate2))
  # loser's LOO set contains dread but not worry -> dread must win
  res <- semanticfa:::.sfa_keeper(picks, list(gate1, gate2),
                                  loo = list(c("anxiety", "dread"), NULL),
                                  n_candidates = 5L)
  expect_equal(res$picks[[1]]$label, "dread")
})

test_that("LOO candidate sets surface near-ties and stay stable when sharp", {
  # Factor with 3 items all pointing the same way -> singleton set.
  q <- unit(rbind(c(1, 0, 0, 0), c(0.95, 0.05, 0, 0), c(0.9, 0.1, 0, 0),
                  c(0, 1, 0, 0)))   # 4th item on another factor
  L <- cbind(F1 = c(.8, .7, .6, .0), F2 = c(0, 0, 0, .9))
  fit <- structure(list(loadings = L), class = "sfa")
  tg <- semanticfa:::.sfa_name_targets(fit, q)
  pool <- toy_pool()
  top <- semanticfa:::.sfa_pool_top(pool, tg$targets, k = 6L, block_size = 3L)
  gate <- semanticfa:::.sfa_gate(pool$words, top$idx[[1]], top$score[[1]])
  loo <- semanticfa:::.sfa_loo(fit, q, tg, 1L, pool, gate)
  expect_true(length(loo) >= 1L)
  expect_true(all(loo %in% gate$word))
})

test_that("sfa_name end-to-end on a synthetic fit and toy pool", {
  # two clean factors in item space; naming embeddings put F1 near the
  # anxiety cluster and F2 near the depression cluster
  q <- unit(rbind(
    c(1, 0.05, 0, 0), c(0.97, 0.08, 0, 0), c(0.94, 0.02, 0, 0),
    c(0, 0, 1, 0.05), c(0, 0, 0.96, 0.08)
  ))
  L <- cbind(F1 = c(.8, .75, .7, .02, .01),
             F2 = c(.01, .02, .03, .82, .78))
  fit <- structure(list(
    loadings = L,
    item_data = data.frame(item = paste("item", 1:5)),
    embed_model = "toy"
  ), class = "sfa")

  # stub the embedding step: return q for any input
  local_mocked_bindings(
    sfa_embed = function(items, ...) q,
    .package = "semanticfa"
  )
  labels <- sfa_name(fit, model = "toy", pool = toy_pool(),
                     loo_sets = TRUE, collision = TRUE)
  expect_s3_class(labels, "sfa_labels")
  expect_equal(nrow(labels), 2L)
  expect_equal(labels$label[1], "anxiety")
  expect_equal(labels$label[2], "depression")
  expect_false(any(labels$collision_moved))
})
