# Synthetic ground-truth tests for sfa_build_region() / sfa_coverage().
# No encoder runs here: embeddings are deterministic fakes keyed by text.

set.seed(42)

.dim <- 16L

.norm_rows <- function(m) m / pmax(sqrt(rowSums(m^2)), 1e-12)

.make_cloud <- function(n, center, sd = 0.15) {
  .norm_rows(matrix(stats::rnorm(n * .dim, mean = rep(center, each = n),
                                 sd = sd), nrow = n))
}

.centers <- diag(.dim)[1:3, ] # three well-separated cluster directions

# cluster-specific vocabulary so gap labeling has signal to find
.vocab <- list(
  c("delay", "postpone", "deadline", "tomorrow", "avoid"),
  c("guilt", "shame", "regret", "remorse", "sorry"),
  c("email", "meeting", "workplace", "office", "manager")
)

.make_sentences <- function(cluster, n) {
  vapply(seq_len(n), function(i) {
    w <- sample(.vocab[[cluster]], 3, replace = TRUE)
    paste0("People often ", w[1], " and ", w[2], " things with ", w[3],
           " in daily life episode ", cluster, "-", i, ".")
  }, character(1))
}

# ---- a fake region + a fake embedder keyed by exact text ------------------

.region_n <- c(150L, 150L, 150L)
.region_emb <- rbind(.make_cloud(150, .centers[1, ]),
                     .make_cloud(150, .centers[2, ]),
                     .make_cloud(150, .centers[3, ]))
.region_text <- c(.make_sentences(1, 150), .make_sentences(2, 150),
                  .make_sentences(3, 150))

.lookup <- new.env(parent = emptyenv())

.register <- function(texts, emb) {
  for (i in seq_along(texts)) assign(texts[i], emb[i, ], envir = .lookup)
}
.register(.region_text, .region_emb)

.fake_embed <- function(texts, ...) {
  t(vapply(texts, function(tx) {
    if (!exists(tx, envir = .lookup)) {
      stop("fake embedder has no vector for: ", tx)
    }
    get(tx, envir = .lookup)
  }, numeric(.dim)))
}

.fake_region <- function() {
  structure(list(
    construct = "procrastination",
    definition = "Delaying things despite expecting costs.",
    sentences = data.frame(text = .region_text,
                           source = "test", stringsAsFactors = FALSE),
    embeddings = .region_emb,
    corpus = "synthetic test cloud",
    docs_streamed = 450L, target = 450L, max_docs = Inf,
    variants = "procrastination",
    encoder = "fake-encoder", instruction = NULL,
    extracted = Sys.time(), semanticfa = "test"
  ), class = "sfa_region")
}

.seed_text <- "Delaying things despite expecting costs."
.register(.seed_text, matrix(colMeans(.centers), nrow = 1))

.new_items <- function(prefix, emb) {
  texts <- paste0(prefix, " item number ", seq_len(nrow(emb)),
                  " about the construct.")
  .register(texts, emb)
  texts
}

# ---------------------------------------------------------------- audits

test_that("an ideal item set scores near the null quantile", {
  items <- .new_items("ideal", rbind(.make_cloud(8, .centers[1, ]),
                                     .make_cloud(8, .centers[2, ]),
                                     .make_cloud(8, .centers[3, ])))
  audit <- sfa_coverage(items, .fake_region(), embed = .fake_embed,
                        model = "fake-encoder", cache = FALSE,
                        sense_gate = FALSE, trim = 0, n_boot = 20)
  expect_s3_class(audit, "sfa_coverage")
  expect_gte(audit$coverage, 0.75)
  expect_gte(audit$item_relevance, 0.85)
  expect_gt(audit$auc, 0.30)
  expect_lt(audit$auc, 0.80)
  # the calibration promise: an ideal scale judged by its own null ~ 1 - alpha
  expect_gte(audit$ideal_relevance, 0.90)
  expect_lte(audit$ideal_relevance, 1.00)
})

test_that("p-values are valid, monotone in counts, and BH is not looser", {
  items <- .new_items("pvals", rbind(.make_cloud(8, .centers[1, ]),
                                     .make_cloud(8, .centers[2, ]),
                                     .make_cloud(8, .centers[3, ])))
  audit <- sfa_coverage(items, .fake_region(), embed = .fake_embed,
                        model = "fake-encoder", cache = FALSE,
                        sense_gate = FALSE, trim = 0, n_boot = 0)
  expect_true(all(audit$p_values >= 0 & audit$p_values <= 1))
  o <- order(audit$corroboration)
  expect_true(all(diff(audit$p_values[o]) >= -1e-12))
  expect_equal(unname(audit$relevant_items),
               unname(audit$p_values > audit$alpha))
  bh <- sfa_coverage(items, .fake_region(), embed = .fake_embed,
                     model = "fake-encoder", cache = FALSE,
                     sense_gate = FALSE, trim = 0, n_boot = 0,
                     p_adjust = "BH")
  # BH-adjusted p-values are >= raw, so BH can only flag fewer items
  expect_gte(sum(bh$relevant_items), sum(audit$relevant_items))
})

test_that("the critical count rescales with region size (no gaming)", {
  items <- .new_items("scaleinv", rbind(.make_cloud(6, .centers[1, ]),
                                        .make_cloud(6, .centers[2, ]),
                                        .make_cloud(6, .centers[3, ])))
  region_small <- .fake_region()
  keep <- c(1:60, 151:210, 301:360)     # a third of each cluster
  region_small$sentences <- region_small$sentences[keep, ]
  region_small$embeddings <- region_small$embeddings[keep, ]
  expect_warning(
    a_small <- sfa_coverage(items, region_small, embed = .fake_embed,
                            model = "fake-encoder", cache = FALSE,
                            sense_gate = FALSE, trim = 0, n_boot = 0),
    "saturation threshold")
  a_full <- sfa_coverage(items, .fake_region(), embed = .fake_embed,
                         model = "fake-encoder", cache = FALSE,
                         sense_gate = FALSE, trim = 0, n_boot = 0)
  expect_gt(a_full$critical_count, a_small$critical_count)
  # small-region caution: flagged on the object and in the report,
  # worded as a data limitation, never a validity verdict
  expect_true(a_small$small_region)
  expect_false(a_full$small_region)
  expect_output(print(a_small), "CAUTION")
  expect_output(print(a_small), "not a validity verdict")
})

test_that("too little region data refuses to audit with the right wording", {
  items <- .new_items("tiny", rbind(.make_cloud(4, .centers[1, ]),
                                    .make_cloud(4, .centers[2, ])))
  region_tiny <- .fake_region()
  region_tiny$sentences <- region_tiny$sentences[1:20, ]
  region_tiny$embeddings <- region_tiny$embeddings[1:20, ]
  expect_error(
    suppressWarnings(
      sfa_coverage(items, region_tiny, embed = .fake_embed,
                   model = "fake-encoder", cache = FALSE,
                   sense_gate = FALSE, trim = 0, n_boot = 0)),
    "not enough natural-language data")
})

test_that("deprecated delta_q and k_precision warn and map", {
  items <- .new_items("deprec", rbind(.make_cloud(6, .centers[1, ]),
                                      .make_cloud(6, .centers[2, ]),
                                      .make_cloud(6, .centers[3, ])))
  expect_warning(
    a <- sfa_coverage(items, .fake_region(), embed = .fake_embed,
                      model = "fake-encoder", cache = FALSE,
                      sense_gate = FALSE, trim = 0, n_boot = 0,
                      delta_q = 0.90),
    "radius_q")
  expect_equal(a$radius_q, 0.90)
  expect_warning(
    sfa_coverage(items, .fake_region(), embed = .fake_embed,
                 model = "fake-encoder", cache = FALSE,
                 sense_gate = FALSE, trim = 0, n_boot = 0,
                 k_precision = 3L),
    "deprecated")
})

test_that("a missing facet lowers coverage and is quoted in the gaps", {
  items <- .new_items("narrow", rbind(.make_cloud(12, .centers[1, ]),
                                      .make_cloud(12, .centers[2, ])))
  audit <- sfa_coverage(items, .fake_region(), embed = .fake_embed,
                        model = "fake-encoder", cache = FALSE,
                        sense_gate = FALSE, trim = 0, n_boot = 0)
  expect_lt(audit$coverage, 0.80)
  expect_gt(audit$coverage, 0.40)
  expect_gt(length(audit$gaps), 0L)
  gap <- audit$gaps[[1]]
  expect_gte(gap$share, 0.10)
  # the gap should be cluster 3: workplace vocabulary in terms or quotes
  hits <- c(gap$terms, unlist(gap$quotes))
  expect_true(any(grepl("workplace|office|meeting|email|manager",
                        hits)))
  # and the gap table accessor mirrors it
  gt <- sfa_gaps(audit)
  expect_equal(nrow(gt), length(audit$gaps))
})

test_that("item relevance flags construct-irrelevant items", {
  far <- .norm_rows(matrix(stats::rnorm(10 * .dim, mean = -1, sd = 0.05),
                           nrow = 10))
  items <- .new_items("mixed", rbind(.make_cloud(10, .centers[1, ]),
                                     .make_cloud(10, .centers[2, ]), far))
  audit <- sfa_coverage(items, .fake_region(), embed = .fake_embed,
                        model = "fake-encoder", cache = FALSE,
                        sense_gate = FALSE, trim = 0, n_boot = 0)
  expect_lt(audit$item_relevance, 0.80)
  expect_gte(audit$item_relevance, 0.45)
  # the strays are the flagged ones, with tiny p-values
  expect_true(all(!audit$relevant_items[21:30]))
  expect_true(all(audit$p_values[21:30] <= audit$alpha))
})

test_that("the sense gate drops a bimodal low mode and spares unimodal", {
  bimodal <- c(stats::rnorm(100, 0.75, 0.03), stats::rnorm(60, 0.25, 0.03))
  g <- semanticfa:::.cvg_sense_gate(bimodal, min_silhouette = 0.65)
  expect_true(g$applied)
  expect_equal(sum(g$keep), 100L)
  unimodal <- stats::rnorm(160, 0.5, 0.05)
  g2 <- semanticfa:::.cvg_sense_gate(unimodal, min_silhouette = 0.65)
  expect_false(g2$applied)
  expect_true(all(g2$keep))
})

test_that("the item screen drops region sentences that duplicate items", {
  items <- .new_items("clean", .make_cloud(12, .centers[1, ]))
  # plant one item verbatim in the region (same text, same embedding)
  region <- .fake_region()
  region$sentences <- rbind(region$sentences,
                            data.frame(text = items[1], source = "leak"))
  region$embeddings <- rbind(region$embeddings,
                             .fake_embed(items[1]))
  audit <- sfa_coverage(items, region, embed = .fake_embed,
                        model = "fake-encoder", cache = FALSE,
                        sense_gate = FALSE, trim = 0, n_boot = 0)
  expect_gte(audit$item_screen_dropped, 1L)
})

test_that("encoder mismatch warns and print/plot run clean", {
  items <- .new_items("smoke", rbind(.make_cloud(6, .centers[1, ]),
                                     .make_cloud(6, .centers[2, ]),
                                     .make_cloud(6, .centers[3, ])))
  expect_warning(
    sfa_coverage(items, .fake_region(), embed = .fake_embed,
                 model = "other-encoder", cache = FALSE,
                 sense_gate = FALSE, trim = 0, n_boot = 0),
    "one space")
  audit <- sfa_coverage(items, .fake_region(), embed = .fake_embed,
                        model = "fake-encoder", cache = FALSE,
                        sense_gate = FALSE, trim = 0, n_boot = 10)
  expect_output(print(audit), "construct coverage")
  expect_output(print(audit), "item relevance")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_invisible(plot(audit))
  expect_invisible(plot(audit, type = "relevance"))
  expect_invisible(plot(audit, type = "curve"))
})

# --------------------------------------------------------------- batteries

test_that("a multi-factor scale audits per factor by default", {
  texts <- .new_items("battery", rbind(.make_cloud(8, .centers[1, ]),
                                       .make_cloud(8, .centers[2, ])))
  df <- data.frame(item = texts, code = paste0("Q", 1:16),
                   factor = rep(c("Delay", "Guilt"), each = 8),
                   stringsAsFactors = FALSE)
  regions <- list(Delay = .fake_region(), Guilt = .fake_region())
  battery <- sfa_coverage(df, regions, embed = .fake_embed,
                          model = "fake-encoder", cache = FALSE,
                          sense_gate = FALSE, trim = 0, n_boot = 0)
  expect_s3_class(battery, "sfa_coverage_battery")
  expect_named(battery, c("Delay", "Guilt"))
  expect_s3_class(battery$Delay, "sfa_coverage")
  expect_equal(battery$Delay$n_items, 8L)
  expect_output(print(battery), "audit battery")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_invisible(plot(battery, factor = "Guilt", type = "relevance"))

  # a battery element is exactly the manual per-subscale audit
  manual <- sfa_coverage(df[df$factor == "Delay", c("item", "code")],
                         .fake_region(), embed = .fake_embed,
                         model = "fake-encoder", cache = FALSE,
                         sense_gate = FALSE, trim = 0, n_boot = 0)
  expect_equal(battery$Delay$coverage, manual$coverage)
  expect_equal(battery$Delay$item_relevance, manual$item_relevance)
  expect_equal(battery$Delay$corroboration, manual$corroboration)
})

test_that("the factor argument selects a subset or a single factor", {
  texts <- .new_items("facsel", rbind(.make_cloud(6, .centers[1, ]),
                                      .make_cloud(6, .centers[2, ]),
                                      .make_cloud(6, .centers[3, ])))
  df <- data.frame(item = texts,
                   factor = rep(c("A", "B", "C"), each = 6),
                   stringsAsFactors = FALSE)
  one <- sfa_coverage(df, .fake_region(), factor = "B",
                      embed = .fake_embed, model = "fake-encoder",
                      cache = FALSE, sense_gate = FALSE, trim = 0,
                      n_boot = 0)
  expect_s3_class(one, "sfa_coverage")
  expect_equal(one$n_items, 6L)
  expect_message(
    two <- sfa_coverage(df, .fake_region(), factor = c("A", "C"),
                        embed = .fake_embed, model = "fake-encoder",
                        cache = FALSE, sense_gate = FALSE, trim = 0,
                        n_boot = 0),
    "same construct region")
  expect_s3_class(two, "sfa_coverage_battery")
  expect_named(two, c("A", "C"))
  expect_error(
    sfa_coverage(df, .fake_region(), factor = "Nope",
                 embed = .fake_embed, model = "fake-encoder",
                 cache = FALSE, sense_gate = FALSE, trim = 0, n_boot = 0),
    "Unknown factor")
})

test_that("battery region matching and assignment-vector input", {
  texts <- .new_items("facvec", rbind(.make_cloud(6, .centers[1, ]),
                                      .make_cloud(6, .centers[2, ])))
  df <- data.frame(item = texts, factor = rep(c("A", "B"), each = 6),
                   stringsAsFactors = FALSE)
  expect_error(
    sfa_coverage(df, list(A = .fake_region()), embed = .fake_embed,
                 model = "fake-encoder", cache = FALSE,
                 sense_gate = FALSE, trim = 0, n_boot = 0),
    "no entry for factor")
  expect_error(
    sfa_coverage(texts[1:6], list(A = .fake_region()),
                 embed = .fake_embed, model = "fake-encoder",
                 cache = FALSE, sense_gate = FALSE, trim = 0, n_boot = 0),
    "no factor assignments")
  # character items + per-item assignment vector
  battery <- suppressMessages(
    sfa_coverage(texts, .fake_region(),
                 factor = rep(c("A", "B"), each = 6),
                 embed = .fake_embed, model = "fake-encoder",
                 cache = FALSE, sense_gate = FALSE, trim = 0, n_boot = 0))
  expect_s3_class(battery, "sfa_coverage_battery")
  expect_named(battery, c("A", "B"))
})

test_that("cross = TRUE audits every factor against every region", {
  # two 'constructs': cluster-1 region and cluster-2 region
  region1 <- .fake_region()
  keep1 <- 1:150
  region1$sentences <- region1$sentences[keep1, ]
  region1$embeddings <- region1$embeddings[keep1, ]
  region2 <- .fake_region()
  keep2 <- 151:300
  region2$sentences <- region2$sentences[keep2, ]
  region2$embeddings <- region2$embeddings[keep2, ]

  texts <- .new_items("cross", rbind(.make_cloud(8, .centers[1, ]),
                                     .make_cloud(8, .centers[2, ])))
  df <- data.frame(item = texts, factor = rep(c("One", "Two"), each = 8),
                   stringsAsFactors = FALSE)
  x <- suppressWarnings(   # 150-sentence toy regions sit below saturation
    sfa_coverage(df, list(c1 = region1, c2 = region2), cross = TRUE,
                 embed = .fake_embed, model = "fake-encoder",
                 cache = FALSE, sense_gate = FALSE, trim = 0, n_boot = 0))
  expect_s3_class(x, "sfa_coverage_cross")
  m <- sfa_cross_matrix(x)
  expect_equal(dimnames(m), list(c("One", "Two"), c("c1", "c2")))
  # convergent: own-construct cells high; discriminant: crossed cells low
  expect_gt(m["One", "c1"], 0.85)
  expect_gt(m["Two", "c2"], 0.85)
  expect_lt(m["One", "c2"], 0.30)
  expect_lt(m["Two", "c1"], 0.30)
  # a cell is exactly the corresponding single audit
  manual <- suppressWarnings(
    sfa_coverage(df[df$factor == "One", "item"], region2,
                 embed = .fake_embed, model = "fake-encoder",
                 cache = FALSE, sense_gate = FALSE, trim = 0, n_boot = 0))
  expect_equal(x$audits$One$c2$item_relevance, manual$item_relevance)
  expect_output(print(x), "Cross-audit matrix")
  cov_m <- sfa_cross_matrix(x, "coverage")
  expect_true(all(cov_m >= 0 & cov_m <= 1))
  # guard rails
  expect_error(suppressWarnings(
    sfa_coverage(df, region1, cross = TRUE,
                 embed = .fake_embed, model = "fake-encoder",
                 cache = FALSE, sense_gate = FALSE, trim = 0, n_boot = 0)),
    "named list of regions")
  expect_error(sfa_coverage(texts, list(c1 = region1, c2 = region2),
                            cross = TRUE, embed = .fake_embed,
                            model = "fake-encoder", cache = FALSE,
                            sense_gate = FALSE, trim = 0, n_boot = 0),
               "factor assignments")
})

# ------------------------------------------- campaign builders and banks

test_that("sfa_build_regions: one pass, word boundaries, deferred embed", {
  docs <- c(
    paste("People delay and postpone things every day.",
          "To delay a decision is common, and to postpone one is too.",
          "Many report they delay chores; some postpone bills as well."),
    paste("Her career took off after she scared the competition.",
          "A careful careerist never worries about caregiving duties."),
    paste("Guilt and shame follow the delay of important work.",
          "Feeling guilt about postponed work is widely reported."),
    paste("We care about clients and care for their needs.",
          "They care deeply; caring is the whole business model.")
  )
  constructs <- list(
    delay = list(definition = "Delaying things despite expecting costs.",
                 variants = c("delay", "postpone", "postponed")),
    care = list(definition = "Concern for the wellbeing of others.",
                variants = c("care", "caring"))
  )
  dir <- tempfile()
  regions <- suppressWarnings(   # tiny corpus: adequacy warnings expected
    sfa_build_regions(constructs, corpus = docs, target = 50,
                      embeddings = FALSE, instruction = FALSE, dir = dir,
                      progress = FALSE))
  expect_named(regions, c("delay", "care"))
  expect_s3_class(regions$delay, "sfa_region")
  expect_null(regions$delay$embeddings)
  expect_true(file.exists(file.path(dir, "delay.rds")))
  expect_true(file.exists(file.path(dir, "care.rds")))
  # word boundaries: 'care' must not match career/careful/careerist doc 2
  expect_false(any(grepl("career", regions$care$sentences$text)))
  expect_gte(nrow(regions$care$sentences), 1L)
  # an embedding-less region refuses to audit, with clear guidance
  items <- .new_items("nb", .make_cloud(4, .centers[1, ]))
  expect_error(
    sfa_coverage(items, regions$delay, embed = .fake_embed,
                 model = "fake-encoder", cache = FALSE,
                 sense_gate = FALSE, trim = 0, n_boot = 0),
    "sfa_reembed_region")
  # re-embedding fills the space in
  .register(regions$delay$sentences$text,
            .make_cloud(nrow(regions$delay$sentences), .centers[1, ]))
  r2 <- sfa_reembed_region(regions$delay, embed = .fake_embed,
                           model = "fake-encoder", cache = FALSE)
  expect_equal(nrow(r2$embeddings), nrow(r2$sentences))
  expect_false(is.null(r2$reembedded))
})

test_that("embedding banks reproduce direct-embedding audits exactly", {
  items <- .new_items("bank", rbind(.make_cloud(6, .centers[1, ]),
                                    .make_cloud(6, .centers[2, ]),
                                    .make_cloud(6, .centers[3, ])))
  # region instruction is NULL in the fake region, so the bank matches with
  # instruction = FALSE
  bank <- sfa_build_bank(c(items, .seed_text), instruction = FALSE,
                         embed = .fake_embed, model = "fake-encoder",
                         cache = FALSE)
  expect_s3_class(bank, "sfa_bank")
  expect_output(print(bank), "Embedding bank")
  lookup <- sfa_embedding_bank(bank)
  expect_equal(unname(lookup(items[3])), unname(.fake_embed(items[3])))
  expect_error(lookup("a text nobody embedded"), "not in the embedding bank")

  direct <- sfa_coverage(items, .fake_region(), embed = .fake_embed,
                         model = "fake-encoder", cache = FALSE,
                         sense_gate = FALSE, trim = 0, n_boot = 0)
  banked <- sfa_coverage(items, .fake_region(), embed = lookup,
                         model = "fake-encoder", cache = FALSE,
                         sense_gate = FALSE, trim = 0, n_boot = 0)
  expect_equal(banked$coverage, direct$coverage)
  expect_equal(banked$item_relevance, direct$item_relevance)
  expect_equal(banked$corroboration, direct$corroboration)

  f <- tempfile(fileext = ".rds")
  suppressMessages(sfa_build_bank(c(items, .seed_text),
                                  instruction = FALSE, embed = .fake_embed,
                                  model = "fake-encoder", cache = FALSE,
                                  file = f))
  expect_s3_class(readRDS(f), "sfa_bank")
  lookup2 <- sfa_embedding_bank(f)
  expect_equal(unname(lookup2(items[1])), unname(.fake_embed(items[1])))
})

test_that("region banks and combined banks serve region-drawn items", {
  region <- .fake_region()
  rb <- sfa_region_bank(region)
  s3 <- region$sentences$text[3]
  expect_equal(unname(rb(s3)), unname(region$embeddings[3, , drop = FALSE]))
  expect_error(rb("not a region sentence"), "not sentences")

  # combined: region sentences from the region bank, definition from a
  # second bank - the overlap-baseline configuration
  main <- sfa_build_bank(.seed_text, instruction = FALSE,
                         embed = .fake_embed, model = "fake-encoder",
                         cache = FALSE)
  combo <- sfa_combine_banks(rb, sfa_embedding_bank(main))
  got <- combo(c(s3, .seed_text))
  expect_equal(unname(got[1, ]), unname(region$embeddings[3, ]))
  seed_norm <- .fake_embed(.seed_text)
  seed_norm <- seed_norm / sqrt(sum(seed_norm^2))   # banks store unit norm
  expect_equal(unname(got[2, , drop = FALSE]), unname(seed_norm))
  expect_error(combo(c(s3, "missing text")), "none of the combined")

  # end to end: pretend items drawn from the region audit against it
  pretend <- region$sentences$text[c(1, 40, 80, 160, 200, 260, 320, 380)]
  audit <- sfa_coverage(pretend, region, embed = combo,
                        model = "fake-encoder", cache = FALSE,
                        sense_gate = FALSE, trim = 0, screen_items = FALSE,
                        n_boot = 0)
  expect_s3_class(audit, "sfa_coverage")
  expect_gte(audit$item_relevance, 0.85)   # region-drawn items are ideal
})

# ------------------------------------------------------------ region build

test_that("sfa_build_region works on a local corpus and honors options", {
  docs <- c(
    paste("Procrastination is common.",
          "Many people report procrastination at work every week.",
          "Procrastination often brings guilt afterwards."),
    paste("A cooking blog post.",
          "This recipe has nothing to do with the construct."),
    paste("Students describe procrastination before exams.",
          "Procrastination on assignments predicts stress.",
          "Some link procrastination to fear of failure.",
          "Procrastination extra sentence four here today."),
    paste("Procrastination mentioned once more in doc four."),
    paste("Procrastination in the final document five appears.")
  )
  sent_emb <- .make_cloud(40, .centers[1, ])
  # register any sentence the splitter can produce
  all_sents <- unlist(lapply(docs, semanticfa:::.cvg_split_sentences))
  .register(all_sents, .make_cloud(length(all_sents), .centers[1, ]))

  expect_warning(
    region <- sfa_build_region(
      construct = "procrastination",
      definition = "Delaying things despite expecting costs.",
      corpus = docs, target = 100, max_docs = 3, sentences_per_doc = 2,
      min_chars = 10, embed = .fake_embed, instruction = FALSE,
      cache = FALSE, progress = FALSE),
    "saturation threshold")

  expect_s3_class(region, "sfa_region")
  # the insufficient-data note is about data, not construct validity
  expect_output(print(region), "NOTE")
  expect_output(print(region), "limited natural-language")
  expect_equal(region$docs_streamed, 3L)          # max_docs honored
  # doc 2 has no mentions; docs 1 and 3 contribute at most 2 sentences each
  expect_lte(nrow(region$sentences), 4L)
  expect_true(all(grepl("rocrastination", region$sentences$text)))
  expect_equal(nrow(region$embeddings), nrow(region$sentences))
  expect_output(print(region), "Construct region")

  f <- tempfile(fileext = ".rds")
  saveRDS(region, f)
  expect_s3_class(sfa_load_region(f), "sfa_region")

  expect_error(sfa_build_region("zzz-not-here", "def", corpus = docs,
                                embed = .fake_embed, instruction = FALSE,
                                cache = FALSE, progress = FALSE),
               "No sentences")
  expect_error(sfa_build_region("procrastination", definition = "",
                                corpus = docs),
               "definition")
})

test_that("an override definition narrows the region discriminatively", {
  # narrow seed sits on cluster 1; base seed is the overall centroid
  narrow_seed <- "Cluster one flavored narrow definition."
  .register(narrow_seed, matrix(.centers[1, ], nrow = 1))
  items <- .new_items("narrowed", .make_cloud(10, .centers[1, ]))
  # narrowing legitimately shrinks the region below the saturation
  # threshold here; the audit warns and proceeds
  expect_warning(
    audit <- sfa_coverage(items, .fake_region(), definition = narrow_seed,
                          construct = "cluster one only",
                          embed = .fake_embed, model = "fake-encoder",
                          cache = FALSE, sense_gate = FALSE, trim = 0,
                          n_boot = 0),
    "saturation threshold")
  expect_true(audit$narrowed)
  # most of clusters 2-3 should be dropped (300 of 450)
  expect_gte(audit$narrowed_dropped, 200L)
  # and cluster-1 items now nearly cover the narrowed region
  expect_gte(audit$coverage, 0.70)
  expect_output(print(audit), "narrowed to the supplied definition")
})

test_that("the incidental-mention trim drops the low-similarity tail", {
  items <- .new_items("trimtest", rbind(.make_cloud(8, .centers[1, ]),
                                        .make_cloud(8, .centers[2, ]),
                                        .make_cloud(8, .centers[3, ])))
  a0 <- sfa_coverage(items, .fake_region(), embed = .fake_embed,
                     model = "fake-encoder", cache = FALSE,
                     sense_gate = FALSE, trim = 0, n_boot = 0)
  a25 <- sfa_coverage(items, .fake_region(), embed = .fake_embed,
                      model = "fake-encoder", cache = FALSE,
                      sense_gate = FALSE, trim = 0.25, n_boot = 0)
  expect_equal(a0$trim_dropped, 0L)
  expect_gte(a25$trim_dropped, floor(0.24 * a0$region_n))
  expect_equal(a25$region_n + a25$trim_dropped + a25$item_screen_dropped,
               a0$region_n + a0$item_screen_dropped)
})

