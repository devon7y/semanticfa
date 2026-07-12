# Content validity as geometric overlap: audit a scale's items against a
# construct region built by sfa_build_region().
#
# Two headline numbers decompose the audit into Messick's two threats, each
# named by what it scores. Construct coverage: the fraction of construct
# texts with an item within the coverage radius - its complement is
# construct underrepresentation. Item relevance: the fraction of items that
# pass a per-item test against the ideal-item null - its complement is
# construct-irrelevant variance.
#
# One 95% convention governs both calibrations (the Monte Carlo test
# identity: 1 - q is a per-decision Type I error rate). The coverage radius
# is the 95% quantile of a matched-size null - nearest-neighbor distances
# when an *ideal* item set of the same length (points drawn from the region
# itself) poses as the scale - so an ideal scale's construct coverage is
# ~0.95 at any scale length, region size, or embedding dimension. An item
# is flagged when its corroboration count (construct texts within its
# radius) has empirical p <= alpha = .05 against the ideal-item null, so an
# ideal scale's item relevance is also ~0.95. The count threshold rescales
# with region size: sampling more construct text cannot buy relevance.

#' @keywords internal
# Distance from each row of A to its k-th nearest row of B; rows unit-norm,
# Euclidean on the sphere (monotone in cosine).
.cvg_nn_dist <- function(A, B, k = 1L) {
  sims <- tcrossprod(A, B)
  k <- min(k, ncol(sims))
  # k = 1 is the hot path (every bootstrap resample calls it several
  # times): the 1st-nearest similarity is a row maximum, computed at C
  # speed by max.col. Identical values to the general k-th-largest path.
  kth <- if (k == 1L) {
    sims[cbind(seq_len(nrow(sims)),
               max.col(sims, ties.method = "first"))]
  } else {
    apply(sims, 1L, function(s) sort(s, decreasing = TRUE)[k])
  }
  sqrt(pmax(2 - 2 * kth, 0))
}

#' @keywords internal
# Matched-size-null radius calibration. Null model: n_ref points drawn from
# the region itself pose as the scale; pooled NN distances from the rest of
# the region to the draw give the achievable distance scale. The coverage
# radius is the radius_q quantile of the pool.
.cvg_calibrate <- function(C, n_ref, draws = 20L, radius_q = 0.95,
                           quantiles = seq(0.05, 0.95, by = 0.05),
                           seed = 1L) {
  set.seed(seed)
  n <- nrow(C)
  n_ref <- max(1L, min(n_ref, n %/% 2L))
  pool <- unlist(lapply(seq_len(draws), function(i) {
    idx <- sample.int(n, n_ref)
    .cvg_nn_dist(C[-idx, , drop = FALSE], C[idx, , drop = FALSE])
  }))
  list(grid = stats::quantile(pool, quantiles, names = FALSE),
       quantiles = quantiles,
       radius = stats::quantile(pool, radius_q, names = FALSE))
}

#' @keywords internal
# Corroboration counts: construct texts within the coverage radius of each
# item. On unit-norm rows, dist <= radius is cosine >= 1 - radius^2 / 2.
.cvg_corroboration <- function(C, S, radius) {
  colSums(tcrossprod(C, S) >= 1 - radius^2 / 2)
}

#' @keywords internal
# The ideal-item null for corroboration counts: n_ref region points pose as
# items; their counts against the remaining region points, pooled over
# draws. Item p-values and the alpha critical count come from this pool.
.cvg_null_counts <- function(C, n_ref, radius, draws = 200L, seed = 1L) {
  set.seed(seed)
  n <- nrow(C)
  n_ref <- max(1L, min(n_ref, n %/% 2L))
  sort(unlist(lapply(seq_len(draws), function(i) {
    idx <- sample.int(n, n_ref)
    .cvg_corroboration(C[-idx, , drop = FALSE], C[idx, , drop = FALSE],
                       radius)
  })))
}

#' @keywords internal
# Empirical left-tail p-value of counts against a sorted null pool:
# P(ideal item's count <= count).
.cvg_pval <- function(counts, null_sorted) {
  findInterval(counts, null_sorted) / length(null_sorted)
}

#' @keywords internal
# Silhouette for a 1-d two-cluster split (no extra dependency).
.cvg_silhouette_1d <- function(x, cl) {
  s <- vapply(seq_along(x), function(i) {
    own <- abs(x[i] - x[cl == cl[i]])
    a <- if (length(own) > 1L) sum(own) / (length(own) - 1L) else 0
    b <- mean(abs(x[i] - x[cl != cl[i]]))
    if (max(a, b) == 0) 0 else (b - a) / max(a, b)
  }, numeric(1))
  mean(s)
}

#' @keywords internal
# Sense gate: 2-means on the 1-d seed-similarity distribution; the low mode
# (wrong senses, incidental mentions) is dropped only when the split is real.
.cvg_sense_gate <- function(sims, min_silhouette = 0.65, seed = 1L) {
  if (length(unique(sims)) < 3L) {
    return(list(keep = rep(TRUE, length(sims)), applied = FALSE, sil = NA))
  }
  set.seed(seed)
  km <- stats::kmeans(matrix(sims, ncol = 1L), centers = 2L, nstart = 10L)
  sil <- .cvg_silhouette_1d(sims, km$cluster)
  if (sil < min_silhouette) {
    return(list(keep = rep(TRUE, length(sims)), applied = FALSE, sil = sil))
  }
  hi <- which.max(km$centers)
  list(keep = km$cluster == hi, applied = TRUE, sil = sil)
}

#' @keywords internal
.CVG_STOPWORDS <- c(
  "a", "an", "the", "and", "or", "but", "if", "then", "than", "of", "to",
  "in", "on", "at", "by", "for", "with", "about", "into", "through", "is",
  "are", "was", "were", "be", "been", "being", "have", "has", "had", "do",
  "does", "did", "will", "would", "can", "could", "i", "you", "he", "she",
  "it", "we", "they", "my", "your", "his", "her", "its", "our", "their",
  "this", "that", "these", "those", "as", "not", "no", "nor", "so", "very",
  "from", "up", "down", "out", "over", "under", "when", "what", "how",
  "all", "also", "more", "most", "some", "such", "only", "own", "same",
  "s", "t", "don", "just", "now", "may", "one", "there", "which", "who"
)

#' @keywords internal
.cvg_words <- function(text) {
  w <- unlist(strsplit(tolower(gsub("[^a-zA-Z']+", " ", text)), "\\s+"))
  w <- w[nchar(w) > 2L]
  setdiff(unique(w), .CVG_STOPWORDS)
}

#' @keywords internal
# Item screen (circularity rule): drop region sentences that ARE items in
# disguise - near-verbatim leaks of the audited scale into the corpus.
# Two criteria, either sufficient:
#   (a) lexical: at least `overlap_threshold` of the sentence's content
#       words appear in a single item;
#   (b) geometric, self-calibrating: the sentence is MORE similar to an
#       item than to any other region sentence. A leaked item copy always
#       is (it sits essentially on the item); an ordinary on-topic
#       sentence never is (its nearest corpus neighbor is closer than any
#       item). No fixed cosine constant: absolute similarity scales differ
#       wildly across encoders (some spaces are so anisotropic that the
#       MEDIAN sentence-item similarity exceeds 0.9), so any fixed
#       threshold over- or under-screens depending on the model.
.cvg_screen_items <- function(region_text, region_emb, item_text, item_emb,
                              overlap_threshold = 0.6) {
  item_words <- lapply(item_text, .cvg_words)
  max_sim_item <- apply(tcrossprod(region_emb, item_emb), 1L, max)
  rr <- tcrossprod(region_emb)
  diag(rr) <- -Inf
  max_sim_region <- apply(rr, 1L, max)
  keep <- vapply(seq_along(region_text), function(i) {
    if (max_sim_item[i] > max_sim_region[i]) return(FALSE)
    w <- .cvg_words(region_text[i])
    if (length(w) == 0L) return(TRUE)
    !any(vapply(item_words, function(iw) {
      length(intersect(w, iw)) / length(w) >= overlap_threshold
    }, logical(1)))
  }, logical(1))
  keep
}

#' @keywords internal
# Words that distinguish one set of sentences from another (log-odds of
# document frequency, add-one smoothed). Used to label gaps and the covered
# subregion.
.cvg_distinctive_terms <- function(texts, against, top = 8L) {
  tw <- lapply(texts, .cvg_words)
  aw <- lapply(against, .cvg_words)
  vocab <- unique(unlist(tw))
  if (length(vocab) == 0L) return(character(0))
  f1 <- vapply(vocab, function(v)
    sum(vapply(tw, function(w) v %in% w, logical(1))), numeric(1))
  f2 <- vapply(vocab, function(v)
    sum(vapply(aw, function(w) v %in% w, logical(1))), numeric(1))
  score <- log((f1 + 1) / (length(tw) + 2)) - log((f2 + 1) / (length(aw) + 2))
  score <- score * (f1 >= 2)   # require the term to recur within the set
  utils::head(vocab[order(-score)][f1[order(-score)] >= 2], top)
}

#' Audit the Content Validity of a Scale Against a Construct Region
#'
#' Quantifies content validity as geometric overlap between a scale's items
#' and a construct region built with [sfa_build_region()]. Two headline
#' numbers decompose the audit into Messick's two threats, each named by
#' what it scores: **construct coverage** (the fraction of construct texts
#' with an item within the coverage radius; its complement is construct
#' *underrepresentation*) and **item relevance** (the fraction of items
#' passing a per-item test against the ideal-item null; its complement is
#' construct-*irrelevant* variance).
#'
#' One 95% convention calibrates both numbers against an **ideal
#' same-length scale** - items drawn from the construct region itself. The
#' coverage radius is the `radius_q` (default 95%) quantile of the
#' matched-size null's nearest-neighbor distances, so an ideal scale's
#' construct coverage is about 0.95 at any scale length, region size, or
#' embedding dimension. Each item's **corroboration count** (construct
#' texts within its radius) gets an empirical p-value against the
#' ideal-item null; items are flagged at `alpha` (default .05), so an ideal
#' scale's item relevance is also about 0.95. The identity behind the
#' convention: `1 - radius_q` and `alpha` are per-decision Type I error
#' rates of Monte Carlo tests. Because the null's counts grow with region
#' size, the critical count rescales automatically - sampling more
#' construct text cannot inflate relevance.
#'
#' The printed report frames the two remedies for low coverage explicitly:
#' *add items* aimed at the named gaps (each gap is labeled with distinctive
#' terms and quoted example sentences from the region - real corpus content
#' the scale does not reach), or *narrow the construct claim* (the covered
#' subregion's distinctive terms describe what the items actually measure).
#' Narrowing is cheap to test: re-run `sfa_coverage()` on the same region
#' with a narrower `definition` (for example "academic procrastination"),
#' which re-applies the sense gate at audit time - no new extraction.
#'
#' @param items Character vector of item text, or a data frame with an
#'   `item`/`text` column (and optionally `code`), as in [sfa()]. When the
#'   data frame also has a `factor` column (subscale assignments), the
#'   audit runs **per factor by default** - one audit per (item set,
#'   construct claim) pair, which is the unit content validity is defined
#'   on - and returns an `"sfa_coverage_battery"`. Use `factor` to restrict
#'   to a subset.
#' @param region An `"sfa_region"` object from [sfa_build_region()], the
#'   path to one saved with its `file` argument, or - for multi-factor
#'   audits - a list of regions named by factor (each factor is audited
#'   against its own construct region). A single region with a
#'   multi-factor scale audits every factor against that same region, with
#'   a message.
#' @param factor Which factors to audit when the items carry factor
#'   assignments. Default `NULL` audits all of them. A character vector of
#'   factor names restricts the battery to that subset (a single name
#'   returns a plain `"sfa_coverage"` audit). With character-vector
#'   `items` lacking assignments, `factor` may instead be a vector of one
#'   assignment per item.
#' @param cross Audit every factor against every region (requires factor
#'   assignments and a named region list)? Default `FALSE`. The result is
#'   an `"sfa_coverage_cross"` matrix of audits - the content analogue of
#'   a multitrait matrix: items should be relevant to their own
#'   construct's region (diagonal) and irrelevant to their siblings'
#'   (off-diagonal), which is discriminant content validity measured from
#'   item text alone. Off-diagonal relevance is floored by how separable
#'   the constructs are in language, not by zero - the printed output
#'   states this caveat. [sfa_cross_matrix()] extracts the numeric matrix.
#' @param definition Optional definition overriding the region's stored
#'   one - the construct-narrowing workflow. When supplied, the region is
#'   first restricted to the sentences that the narrower definition explains
#'   better than the region's original definition (higher embedding
#'   similarity), then the usual filters apply with the new definition as
#'   seed. Default `NULL` uses `region$definition` with no restriction.
#' @param construct Optional display label for the (possibly narrowed)
#'   construct. Default: the region's construct, or the first words of a
#'   supplied `definition`.
#' @param radius_q Null quantile defining the coverage radius. Default 0.95
#'   (ideal scale covers ~95% of the region; each gap call is a test at
#'   `alpha = 1 - radius_q`).
#' @param alpha Per-item test level against the ideal-item null. Default
#'   0.05. An item is flagged when its corroboration count's empirical
#'   p-value is at most `alpha`.
#' @param p_adjust Multiplicity handling for the item flags: `"none"`
#'   (per-item tests, default) or `"BH"` (Benjamini-Hochberg false
#'   discovery rate across the scale's items: of the flagged items, at most
#'   `alpha` are expected to be false alarms).
#' @param n_draws Draws for the matched-size null behind the radius.
#'   Default 20.
#' @param n_null Draws for the ideal-item count null behind the p-values
#'   (more draws give finer p resolution). Default 200.
#' @param sense_gate Apply the sense gate (2-means on seed similarity;
#'   drops the wrong-sense/incidental-mention mode only when the split is
#'   real)? Default `TRUE`.
#' @param trim Incidental-mention trim: fraction of the region with the
#'   lowest similarity to the definition to drop before auditing (web
#'   corpora carry a tail of sentences that merely mention the term).
#'   Applied after the sense gate and reported in the print output.
#'   Default 0.25; set 0 to disable.
#' @param min_silhouette Minimum 1-d silhouette for the sense gate to fire.
#'   Default 0.65 (a unimodal 1-d Gaussian scores ~0.55 under a forced
#'   2-means split; genuine sense mixtures score higher).
#' @param screen_items Drop region sentences that near-duplicate the audited
#'   items (circularity rule)? Default `TRUE`.
#' @param overlap_threshold Item-screen lexical threshold: a region
#'   sentence is dropped when at least this fraction of its content words
#'   appears in a single item (default 0.6), or - the self-calibrating
#'   geometric criterion, no constant - when it is more similar to an item
#'   than to any other region sentence (a leaked item copy always is; an
#'   on-topic corpus sentence never is; fixed cosine thresholds do not
#'   transfer across encoders' similarity scales).
#' @param n_boot Bootstrap resamples of the region for percentile confidence
#'   intervals (0 to skip). Default 200.
#' @param max_gaps Maximum gap clusters to report. Default 6.
#' @param gap_quotes Example sentences quoted per gap. Default 3.
#' @param embed,model,cache Passed to [sfa_embed()] for the items and seed.
#'   `model` defaults to the region's encoder; overriding it is almost
#'   always a mistake (the audit compares points in one space) and produces
#'   a warning.
#' @param seed Random seed for the null draws, gap clustering, and
#'   bootstrap. Default 1.
#' @param delta_q,k_precision Deprecated (pre-0.3.0 names). `delta_q` is
#'   mapped to `radius_q` with a warning. `k_precision` is ignored with a
#'   warning: the fixed-count rule it set was region-size dependent
#'   (sampling more construct text inflated relevance) and is replaced by
#'   the calibrated per-item test.
#'
#' @returns An object of class `"sfa_coverage"` with the audit numbers,
#'   per-item corroboration counts and p-values, filter accounting, gap
#'   report, and provenance - or, when the items carry factor assignments
#'   and more than one factor is audited, an `"sfa_coverage_battery"`: a
#'   named list of `"sfa_coverage"` audits, one per factor (`x$Depression`
#'   is that factor's full audit). `print()` gives the report (a compact
#'   per-factor table for batteries); `plot()` draws the
#'   proportional-overlap diagram (`type = "coverage"`), the per-item
#'   relevance chart (`type = "relevance"`), or the coverage curve against
#'   the matched-size null (`type = "curve"`); [sfa_gaps()] returns the gap
#'   table.
#'
#' @examples
#' \dontrun{
#' region <- sfa_load_region("procrastination_region.rds")
#' audit  <- sfa_coverage(my_items, region)
#' audit                      # coverage, relevance, gaps, the two remedies
#' plot(audit)                # proportional-overlap (Euler) diagram
#' plot(audit, type = "relevance")   # per-item counts, p-values, flags
#'
#' # a multidimensional battery: items with a 'factor' column audit
#' # per subscale by default, each against its own construct region
#' battery <- sfa_coverage(dass_items,
#'                         region = list(Depression = reg_dep,
#'                                       Anxiety    = reg_anx,
#'                                       Stress     = reg_str))
#' battery                    # one row per factor
#' battery$Depression         # full report for one subscale
#' sfa_coverage(dass_items, reg_dep, factor = "Depression")  # just one
#'
#' # test a narrower claim against the same region - no new extraction
#' sfa_coverage(my_items, region,
#'              definition = "Academic procrastination is the delay of
#'                            study-related tasks despite expecting costs.")
#' }
#' @export
sfa_coverage <- function(items,
                         region,
                         definition = NULL,
                         construct = NULL,
                         factor = NULL,
                         cross = FALSE,
                         radius_q = 0.95,
                         alpha = 0.05,
                         p_adjust = c("none", "BH"),
                         n_draws = 20L,
                         n_null = 200L,
                         sense_gate = TRUE,
                         min_silhouette = 0.65,
                         trim = 0.25,
                         screen_items = TRUE,
                         overlap_threshold = 0.6,
                         n_boot = 200L,
                         max_gaps = 6L,
                         gap_quotes = 3L,
                         embed = "sbert",
                         model = NULL,
                         cache = TRUE,
                         seed = 1L,
                         delta_q = NULL,
                         k_precision = NULL) {
  p_adjust <- match.arg(p_adjust)
  if (!is.null(delta_q)) {
    warning("'delta_q' is deprecated; use 'radius_q'. Using radius_q = ",
            delta_q, ".", call. = FALSE)
    radius_q <- delta_q
  }
  if (!is.null(k_precision)) {
    warning("'k_precision' is deprecated and ignored: the fixed-count rule ",
            "was region-size dependent. Items are now flagged by empirical ",
            "p-value against the ideal-item null at 'alpha'.", call. = FALSE)
  }
  resolved <- .resolve_items(items)
  item_text <- resolved$items
  item_codes <- resolved$codes
  assignments <- resolved$factors

  # 'factor' as one assignment per item (character-vector items path)
  if (!is.null(factor) && is.null(assignments) &&
      length(factor) == length(item_text)) {
    assignments <- as.character(factor)
    factor <- NULL
  }

  # ---- battery dispatch: a multi-factor scale audits per factor by
  # default - content validity is a property of an (item set, construct
  # claim) pair, and a battery makes one claim per subscale.
  if (!is.null(assignments)) {
    lv <- unique(assignments)
    sel <- if (is.null(factor)) lv else {
      bad <- setdiff(factor, lv)
      if (length(bad)) {
        stop("Unknown factor(s): ", paste(bad, collapse = ", "),
             ". The items carry: ", paste(lv, collapse = ", "), ".",
             call. = FALSE)
      }
      unique(factor)
    }
    region_is_list <- is.list(region) && !inherits(region, "sfa_region")
    if (region_is_list && !isTRUE(cross)) {
      miss <- setdiff(sel, names(region))
      if (length(miss)) {
        stop("'region' list has no entry for factor(s): ",
             paste(miss, collapse = ", "), ". Name the list by factor.",
             call. = FALSE)
      }
    } else if (!region_is_list && !isTRUE(cross) && length(sel) > 1L) {
      message("One region supplied for ", length(sel), " factors - every ",
              "factor is audited against the same construct region.")
    }
    audit_pair <- function(f, reg) {
      idx <- assignments == f
      sub <- data.frame(item = item_text[idx], code = item_codes[idx],
                        stringsAsFactors = FALSE)
      sfa_coverage(sub, reg,
                   definition = definition, construct = construct,
                   radius_q = radius_q, alpha = alpha, p_adjust = p_adjust,
                   n_draws = n_draws, n_null = n_null,
                   sense_gate = sense_gate,
                   min_silhouette = min_silhouette, trim = trim,
                   screen_items = screen_items,
                   overlap_threshold = overlap_threshold,
                   n_boot = n_boot,
                   max_gaps = max_gaps, gap_quotes = gap_quotes,
                   embed = embed, model = model, cache = cache, seed = seed)
    }
    # ---- cross-audit matrix: every factor against every region -
    # discriminant content validity (items should be relevant to their own
    # construct's region and irrelevant to their siblings')
    if (isTRUE(cross)) {
      if (!region_is_list || is.null(names(region))) {
        stop("'cross = TRUE' needs a named list of regions (one per ",
             "construct) to cross the factors against.", call. = FALSE)
      }
      audits <- lapply(sel, function(f) {
        stats::setNames(lapply(names(region), function(r)
          audit_pair(f, region[[r]])), names(region))
      })
      return(structure(list(audits = stats::setNames(audits, sel),
                            factors = sel, regions = names(region)),
                       class = "sfa_coverage_cross"))
    }
    audit_one <- function(f)
      audit_pair(f, if (region_is_list) region[[f]] else region)
    if (length(sel) > 1L) {
      return(structure(stats::setNames(lapply(sel, audit_one), sel),
                       class = "sfa_coverage_battery"))
    }
    return(audit_one(sel))
  }
  if (isTRUE(cross)) {
    stop("'cross = TRUE' needs items with factor assignments and a named ",
         "list of regions.", call. = FALSE)
  }
  if (!is.null(factor)) {
    stop("'factor' was supplied but the items carry no factor assignments. ",
         "Add a 'factor' column to the items data frame, or pass 'factor' ",
         "as one assignment per item.", call. = FALSE)
  }
  if (is.list(region) && !inherits(region, "sfa_region")) {
    stop("A list of regions was supplied but the items carry no factor ",
         "assignments to match them to.", call. = FALSE)
  }

  if (is.character(region) && length(region) == 1L) {
    region <- sfa_load_region(region)
  }
  if (!inherits(region, "sfa_region")) {
    stop("'region' must be an sfa_region (see sfa_build_region()) or a ",
         "path to one.", call. = FALSE)
  }
  if (is.null(region$embeddings)) {
    stop("This region has sentences but no embeddings (built with ",
         "embeddings = FALSE). Embed it under an encoder first: ",
         "sfa_reembed_region(region, model = ...).", call. = FALSE)
  }
  if (is.null(model)) {
    model <- region$encoder
  } else if (!identical(model, region$encoder)) {
    warning("Auditing with encoder '", model, "' against a region built ",
            "with '", region$encoder, "'. Coverage compares points in one ",
            "space; mixed encoders make it meaningless.", call. = FALSE)
  }
  narrowing <- !is.null(definition) &&
    !identical(definition, region$definition)
  definition <- definition %||% region$definition
  construct <- construct %||% region$construct

  if (length(item_text) < 2L) {
    stop("Need at least 2 items to audit.", call. = FALSE)
  }

  instr <- region$instruction
  S <- .cvg_normalize(unname(sfa_embed(
    .cvg_wrap_instruction(item_text, instr),
    embed = embed, model = model, cache = cache)))
  seed_vec <- .cvg_normalize(sfa_embed(
    .cvg_wrap_instruction(definition, instr),
    embed = embed, model = model, cache = cache))[1L, ]

  C <- region$embeddings
  region_text <- region$sentences$text

  # ---- narrowing (explicit override definition): keep the sentences the
  # narrower definition explains better than the region's original one.
  n_narrowed <- 0L
  if (narrowing) {
    base_vec <- .cvg_normalize(sfa_embed(
      .cvg_wrap_instruction(region$definition, instr),
      embed = embed, model = model, cache = cache))[1L, ]
    keep <- as.numeric(C %*% seed_vec) > as.numeric(C %*% base_vec)
    n_narrowed <- sum(!keep)
    C <- C[keep, , drop = FALSE]
    region_text <- region_text[keep]
  }

  # ---- sense gate (query-time, against the audit's definition)
  gate <- list(applied = FALSE, sil = NA)
  n_gated <- 0L
  if (isTRUE(sense_gate)) {
    gate <- .cvg_sense_gate(as.numeric(C %*% seed_vec), min_silhouette, seed)
    n_gated <- sum(!gate$keep)
    C <- C[gate$keep, , drop = FALSE]
    region_text <- region_text[gate$keep]
  }

  # ---- incidental-mention trim: web corpora carry a tail of sentences
  # that merely mention the term (listicles, asides, noise); drop the
  # lowest seed-similarity fraction. Disclosed in the printed report.
  n_trimmed <- 0L
  if (trim > 0) {
    sims <- as.numeric(C %*% seed_vec)
    cut <- stats::quantile(sims, trim, names = FALSE)
    keep <- sims > cut
    n_trimmed <- sum(!keep)
    C <- C[keep, , drop = FALSE]
    region_text <- region_text[keep]
  }

  # ---- circularity screen against the audited items
  n_screened <- 0L
  if (isTRUE(screen_items)) {
    keep <- .cvg_screen_items(region_text, C, item_text, S,
                              overlap_threshold)
    n_screened <- sum(!keep)
    C <- C[keep, , drop = FALSE]
    region_text <- region_text[keep]
  }
  if (nrow(C) < 25L) {
    stop("Only ", nrow(C), " region sentences survive the filters - too ",
         "few to audit against. This does not mean the construct is ",
         "invalid: there is not enough natural-language data about it ",
         "(under this name, in this corpus) to estimate content validity ",
         "with this method. Rebuild the region with a higher 'target'/",
         "'max_docs', add term 'variants', or use a domain corpus where ",
         "the construct is discussed.", call. = FALSE)
  }
  small_region <- nrow(C) < 200L
  if (small_region) {
    warning("The audited region has only ", nrow(C), " sentences after ",
            "filtering - below the ~200-sentence saturation threshold. ",
            "Estimates are noisy and coverage is biased favorable at small ",
            "region sizes; interpret with caution. This reflects limited ",
            "corpus data for the construct name, not the construct's ",
            "validity.", call. = FALSE)
  }

  # ---- the audit: construct coverage
  cal <- .cvg_calibrate(C, n_ref = nrow(S), draws = n_draws,
                        radius_q = radius_q, seed = seed)
  d_region <- .cvg_nn_dist(C, S)
  curve <- vapply(cal$grid, function(t) mean(d_region <= t), numeric(1))
  covered <- d_region <= cal$radius
  coverage <- mean(covered)
  auc <- mean(curve)

  # ---- the audit: item relevance (per-item test vs the ideal-item null)
  corrob <- .cvg_corroboration(C, S, cal$radius)
  null_counts <- .cvg_null_counts(C, n_ref = nrow(S), radius = cal$radius,
                                  draws = n_null, seed = seed)
  p_values <- .cvg_pval(corrob, null_counts)
  p_used <- if (p_adjust == "BH") stats::p.adjust(p_values, "BH") else
    p_values
  relevant <- p_used > alpha
  item_relevance <- mean(relevant)
  critical_count <- stats::quantile(null_counts, alpha, names = FALSE)
  ideal_relevance <- mean(.cvg_pval(null_counts, null_counts) > alpha)

  assign_nn <- apply(tcrossprod(C, S), 1L, which.max)
  p_assign <- tabulate(assign_nn, nbins = nrow(S)) / nrow(C)
  p_assign <- p_assign[p_assign > 0]
  evenness <- if (nrow(S) > 1L) {
    -sum(p_assign * log(p_assign)) / log(nrow(S))
  } else 1

  # ---- bootstrap CIs over the region sample (radius and critical count
  # recalibrated inside every resample)
  boot <- NULL
  if (n_boot > 0L) {
    set.seed(seed)
    stats_b <- vapply(seq_len(n_boot), function(b) {
      idx <- sample.int(nrow(C), nrow(C), replace = TRUE)
      Cb <- C[idx, , drop = FALSE]
      cal_b <- .cvg_calibrate(Cb, n_ref = nrow(S), draws = 5L,
                              radius_q = radius_q, seed = seed + b)
      null_b <- .cvg_null_counts(Cb, n_ref = nrow(S), radius = cal_b$radius,
                                 draws = 5L, seed = seed + b)
      p_b <- .cvg_pval(.cvg_corroboration(Cb, S, cal_b$radius), null_b)
      if (p_adjust == "BH") p_b <- stats::p.adjust(p_b, "BH")
      c(mean(.cvg_nn_dist(Cb, S) <= cal_b$radius), mean(p_b > alpha))
    }, numeric(2))
    boot <- list(
      coverage_ci  = stats::quantile(stats_b[1, ], c(0.025, 0.975),
                                     names = FALSE),
      relevance_ci = stats::quantile(stats_b[2, ], c(0.025, 0.975),
                                     names = FALSE))
  }

  # ---- gaps: cluster the uncovered mass, label it, quote it
  gaps <- list()
  unc <- which(!covered)
  if (length(unc) >= 5L) {
    set.seed(seed)
    U <- C[unc, , drop = FALSE]
    k_gap <- if (length(unc) < 20L) 1L else {
      ks <- 2L:min(max_gaps, length(unc) %/% 5L)
      if (length(ks) == 0L) 1L else {
        wss <- vapply(ks, function(k)
          stats::kmeans(U, k, nstart = 5L)$tot.withinss, numeric(1))
        # elbow-lite: smallest k capturing 80% of the drop from k=1
        base <- stats::kmeans(U, 1L)$tot.withinss
        rel <- (base - wss) / base
        ks[which(rel >= 0.8 * max(rel))[1L]]
      }
    }
    cl <- if (k_gap == 1L) rep(1L, length(unc)) else
      stats::kmeans(U, k_gap, nstart = 10L)$cluster
    covered_text <- region_text[covered]
    gaps <- lapply(seq_len(k_gap), function(g) {
      members <- unc[cl == g]
      centroid <- colMeans(C[members, , drop = FALSE])
      ord <- order(-as.numeric(C[members, , drop = FALSE] %*% centroid))
      list(share = length(members) / nrow(C),
           terms = .cvg_distinctive_terms(region_text[members],
                                          covered_text),
           quotes = region_text[members[utils::head(ord, gap_quotes)]])
    })
    gaps <- gaps[order(-vapply(gaps, `[[`, numeric(1), "share"))]
  }
  covered_terms <- if (any(covered) && any(!covered)) {
    .cvg_distinctive_terms(region_text[covered], region_text[!covered])
  } else character(0)

  structure(list(
    construct = construct, definition = definition,
    n_items = nrow(S), item_codes = item_codes, item_text = item_text,
    region_n = nrow(C), region_provenance = list(
      corpus = region$corpus, encoder = region$encoder,
      extracted = region$extracted, semanticfa = region$semanticfa),
    sense_gate = list(applied = gate$applied, silhouette = gate$sil,
                      dropped = n_gated),
    narrowed = narrowing, narrowed_dropped = n_narrowed,
    trim = trim, trim_dropped = n_trimmed,
    item_screen_dropped = n_screened,
    radius = cal$radius, radius_q = radius_q, alpha = alpha,
    p_adjust = p_adjust, small_region = small_region,
    grid = cal$grid, grid_quantiles = cal$quantiles, curve = curve,
    coverage = coverage, auc = auc,
    item_relevance = item_relevance,
    corroboration = stats::setNames(as.integer(corrob), item_codes),
    p_values = stats::setNames(p_values, item_codes),
    relevant_items = stats::setNames(relevant, item_codes),
    critical_count = critical_count, ideal_relevance = ideal_relevance,
    evenness = evenness, boot = boot,
    d_region = d_region, covered = covered,
    gaps = gaps, covered_terms = covered_terms,
    call_params = list(radius_q = radius_q, alpha = alpha,
                       p_adjust = p_adjust, n_draws = n_draws,
                       n_null = n_null, seed = seed)
  ), class = "sfa_coverage")
}

#' Gap Table from a Coverage Audit
#'
#' @param x An `"sfa_coverage"` object from [sfa_coverage()].
#' @returns A data frame with one row per gap: share of region mass, label
#'   terms, and an example quote.
#' @export
sfa_gaps <- function(x) {
  if (!inherits(x, "sfa_coverage")) {
    stop("'x' must be an sfa_coverage object.", call. = FALSE)
  }
  if (length(x$gaps) == 0L) {
    return(data.frame(share = numeric(0), terms = character(0),
                      example = character(0)))
  }
  data.frame(
    share = vapply(x$gaps, `[[`, numeric(1), "share"),
    terms = vapply(x$gaps, function(g)
      paste(g$terms, collapse = ", "), character(1)),
    example = vapply(x$gaps, function(g)
      g$quotes[1] %||% "", character(1)),
    stringsAsFactors = FALSE
  )
}

#' @export
print.sfa_coverage <- function(x, digits = 2, ...) {
  fmt <- function(v) formatC(v, digits = digits, format = "f")
  ci <- function(ci_v) {
    if (is.null(ci_v)) "" else
      paste0(" [", fmt(ci_v[1]), ", ", fmt(ci_v[2]), "]")
  }
  cat("Content-validity audit: ", x$n_items, " items vs \"",
      x$construct, "\"\n", sep = "")
  cat("  region: ", x$region_n, " sentences (",
      x$region_provenance$corpus, ")",
      if (x$narrowed) paste0("; narrowed to the supplied definition (-",
                             x$narrowed_dropped, ")") else "",
      if (x$sense_gate$applied) paste0("; sense gate dropped ",
                                       x$sense_gate$dropped) else "",
      if (x$trim_dropped > 0) paste0("; trim (", x$trim, ") dropped ",
                                     x$trim_dropped) else "",
      if (x$item_screen_dropped > 0) paste0("; item screen dropped ",
                                            x$item_screen_dropped) else "",
      "\n", sep = "")
  cat("  encoder: ", x$region_provenance$encoder, "\n", sep = "")
  if (isTRUE(x$small_region)) {
    cat("  CAUTION: region below the ~200-sentence saturation threshold - ",
        "limited corpus\n  data for this construct name (not a validity ",
        "verdict); estimates are noisy and\n  coverage is biased ",
        "favorable at this size.\n", sep = "")
  }
  cat("\n")

  n_flag <- sum(!x$relevant_items)
  cat("  construct coverage  ", fmt(x$coverage), ci(x$boot$coverage_ci),
      "   (ideal ~ ", fmt(x$radius_q),
      "; shortfall = construct underrepresentation)\n", sep = "")
  cat("  item relevance      ", fmt(x$item_relevance),
      ci(x$boot$relevance_ci), "   (", x$n_items - n_flag, " of ",
      x$n_items, " items pass at alpha = ", x$alpha,
      if (x$p_adjust == "BH") ", BH-adjusted" else "",
      "; ideal ~ ", fmt(x$ideal_relevance), ")\n", sep = "")
  cat("  curve AUC           ", fmt(x$auc), "   (ideal ~ 0.50)\n", sep = "")
  cat("  evenness            ", fmt(x$evenness), "\n\n", sep = "")

  if (n_flag > 0L) {
    cat("  Flagged items (corroboration count; p vs the ideal-item null,\n")
    cat("  critical count = ", round(x$critical_count), "):\n", sep = "")
    flagged <- which(!x$relevant_items)
    flagged <- flagged[order(x$p_values[flagged])]
    for (i in flagged) {
      cat("    ", format(x$item_codes[i], width = 8), " n = ",
          format(x$corroboration[i], width = 4), "  p = ",
          fmt(x$p_values[i]), "  \"",
          substr(x$item_text[i], 1, 60),
          if (nchar(x$item_text[i]) > 60) "..." else "", "\"\n", sep = "")
    }
    cat("\n")
  }

  low_cov <- x$coverage < x$radius_q - 0.10
  if (low_cov && length(x$gaps) > 0L) {
    cat("  Uncovered content (", fmt(1 - x$coverage),
        " of region mass). Two remedies:\n\n", sep = "")
    cat("  1) ADD ITEMS aimed at the gaps:\n")
    for (i in seq_along(x$gaps)) {
      g <- x$gaps[[i]]
      cat("     gap ", i, " (", fmt(g$share), " of region): ",
          paste(g$terms, collapse = ", "), "\n", sep = "")
      for (q in g$quotes) {
        cat("       \"", substr(q, 1, 110),
            if (nchar(q) > 110) "..." else "", "\"\n", sep = "")
      }
    }
    if (length(x$covered_terms) > 0L) {
      cat("\n  2) or NARROW THE CLAIM - the items best cover content ",
          "described by:\n     ",
          paste(x$covered_terms, collapse = ", "), "\n", sep = "")
      cat("     (re-run sfa_coverage() on this region with a narrower",
          "'definition' to test a renamed construct)\n")
    }
  } else if (length(x$gaps) > 0L) {
    cat("  Largest remaining gap (", fmt(x$gaps[[1]]$share),
        " of region): ", paste(x$gaps[[1]]$terms, collapse = ", "),
        "\n", sep = "")
  }
  invisible(x)
}

#' @export
print.sfa_coverage_battery <- function(x, digits = 2, ...) {
  fmt <- function(v) formatC(v, digits = digits, format = "f")
  n_items <- sum(vapply(x, `[[`, numeric(1), "n_items"))
  cat("Content-validity audit battery: ", length(x), " factors, ",
      n_items, " items\n", sep = "")
  cat("  encoder: ", x[[1L]]$region_provenance$encoder, "\n\n", sep = "")
  w <- max(nchar(names(x)), 6L)
  cat(sprintf("  %-*s %6s  %-20s %-20s %-10s %s\n", w, "factor", "items",
              "construct", "coverage", "relevance", "flagged"))
  for (f in names(x)) {
    a <- x[[f]]
    ci <- if (!is.null(a$boot)) {
      paste0(" [", fmt(a$boot$coverage_ci[1]), ",",
             fmt(a$boot$coverage_ci[2]), "]")
    } else ""
    cat(sprintf("  %-*s %6d  %-20s %-20s %-10s %d\n", w, f, a$n_items,
                substr(a$construct, 1, 20), paste0(fmt(a$coverage), ci),
                fmt(a$item_relevance), sum(!a$relevant_items)))
  }
  cat("\n  (ideal same-length scale ~ ", fmt(x[[1L]]$radius_q),
      " on both numbers; print one element, e.g. x$", names(x)[1L],
      ", for its full report)\n", sep = "")
  invisible(x)
}

#' Extract the Numeric Matrix from a Cross-Audit
#'
#' @param x An `"sfa_coverage_cross"` from `sfa_coverage(..., cross =
#'   TRUE)`.
#' @param what `"relevance"` (default; the discriminant-informative
#'   number) or `"coverage"`.
#' @returns A numeric matrix, factors in rows, regions in columns.
#' @export
sfa_cross_matrix <- function(x, what = c("relevance", "coverage")) {
  if (!inherits(x, "sfa_coverage_cross")) {
    stop("'x' must be an sfa_coverage_cross object.", call. = FALSE)
  }
  what <- match.arg(what)
  field <- if (what == "relevance") "item_relevance" else "coverage"
  m <- do.call(rbind, lapply(x$audits, function(row)
    vapply(row, `[[`, numeric(1), field)))
  dimnames(m) <- list(x$factors, x$regions)
  m
}

#' @export
print.sfa_coverage_cross <- function(x, digits = 2, ...) {
  m <- sfa_cross_matrix(x, "relevance")
  cat("Cross-audit matrix: item relevance of each factor's items vs ",
      "each construct region\n", sep = "")
  cat("  (", length(x$factors), " factors x ", length(x$regions),
      " regions; encoder: ",
      x$audits[[1L]][[1L]]$region_provenance$encoder, ")\n\n", sep = "")
  fm <- formatC(m, digits = digits, format = "f")
  # mark the matched (own-construct) cells
  for (f in rownames(m)) {
    if (f %in% colnames(m)) fm[f, f] <- paste0(fm[f, f], "*")
  }
  print(noquote(fm))
  cat("\n  * own construct. Items should be relevant to their own region",
      "and not their siblings'\n")
  cat("  (discriminant content validity). Off-diagonal relevance is",
      "floored by how separable\n")
  cat("  the constructs are in language, not by zero.",
      "x$audits[[factor]][[region]] holds any\n")
  cat("  cell's full audit;",
      "sfa_cross_matrix(x, \"coverage\") gives the coverage matrix.\n")
  invisible(x)
}

#' Plot One Factor of a Content-Validity Audit Battery
#'
#' @param x An `"sfa_coverage_battery"` from [sfa_coverage()] on a
#'   multi-factor scale.
#' @param factor Which factor's audit to plot. Default: the first.
#' @param ... Passed to [plot.sfa_coverage()] (e.g. `type = "relevance"`).
#' @returns The plotted `"sfa_coverage"` audit, invisibly.
#' @export
plot.sfa_coverage_battery <- function(x, factor = names(x)[1L], ...) {
  factor <- match.arg(factor, names(x))
  plot(x[[factor]], ...)
}

# ---------------------------------------------------------------- plotting

#' @keywords internal
# Proportional Euler diagram: two equal disks whose overlap area IS the
# measured construct coverage. Every point is a real text/item placed by
# its full-space verdict; within-region positions are arbitrary (uniform),
# so counts, areas, and the headline number agree by construction. This is
# a constructed diagram, not a projection: no 2-d projection of the
# embedding space can preserve these fractions.
.cvg_plot_coverage <- function(x, seed = 20260711) {
  r <- 1
  cov <- x$coverage
  lens_area <- function(d) {
    2 * r^2 * acos(d / (2 * r)) - 0.5 * d * sqrt(4 * r^2 - d^2)
  }
  lo <- 1e-9
  hi <- 2 * r - 1e-9
  target <- cov * pi * r^2
  for (i in seq_len(200)) {
    mid <- (lo + hi) / 2
    if (lens_area(mid) > target) lo <- mid else hi <- mid
  }
  d <- (lo + hi) / 2

  sample_region <- function(n, cx, inside_other, ocx, min_sep = 0) {
    pts <- matrix(numeric(0), ncol = 2)
    while (nrow(pts) < n) {
      p <- stats::runif(2, -r, r)
      if (sum(p^2) > r^2) next
      p[1] <- p[1] + cx
      io <- (p[1] - ocx)^2 + p[2]^2 <= r^2
      if (io != inside_other) next
      if (min_sep > 0 && nrow(pts) > 0 &&
          any((pts[, 1] - p[1])^2 + (pts[, 2] - p[2])^2 < min_sep^2)) next
      pts <- rbind(pts, p)
    }
    pts
  }

  n_cov <- sum(x$covered)
  n_unc <- sum(!x$covered)
  n_rel <- sum(x$relevant_items)
  n_flag <- sum(!x$relevant_items)
  set.seed(seed)
  P_cov <- sample_region(n_cov, 0, TRUE, d)
  P_unc <- sample_region(n_unc, 0, FALSE, d)
  P_rel <- if (n_rel) sample_region(n_rel, d, TRUE, 0, min_sep = 0.14) else
    matrix(numeric(0), ncol = 2)
  P_flg <- if (n_flag) sample_region(n_flag, d, FALSE, 0, min_sep = 0.14) else
    matrix(numeric(0), ncol = 2)

  c_fill <- "#cfe2f7"; c_edge <- "#2a78d6"; c_pt <- "#1c5cab"
  s_fill <- "#fbe9c0"; s_edge <- "#c28400"; s_pt <- "#a06d00"
  lens_fill <- "#cde5b4"; g_pt <- "#417d1e"

  op <- graphics::par(mar = c(1, 1, 4, 1))
  on.exit(graphics::par(op))
  graphics::plot.new()
  graphics::plot.window(xlim = c(-r - 0.2, d + r + 0.2),
                        ylim = c(-r - 0.15, r + 0.3), asp = 1)
  t <- seq(0, 2 * pi, length.out = 720)
  graphics::polygon(cos(t), sin(t), col = c_fill, border = NA)
  graphics::polygon(d + cos(t), sin(t), col = s_fill, border = NA)
  c1 <- cbind(cos(t), sin(t))
  c2 <- cbind(d + cos(t), sin(t))
  b1 <- c1[(c1[, 1] - d)^2 + c1[, 2]^2 <= r^2, , drop = FALSE]
  b2 <- c2[c2[, 1]^2 + c2[, 2]^2 <= r^2, , drop = FALSE]
  lens <- rbind(b1, b2)
  cen <- colMeans(lens)
  lens <- lens[order(atan2(lens[, 2] - cen[2], lens[, 1] - cen[1])), ]
  graphics::polygon(lens, col = lens_fill, border = NA)
  graphics::polygon(cos(t), sin(t), border = c_edge, lwd = 2)
  graphics::polygon(d + cos(t), sin(t), border = s_edge, lwd = 2)

  graphics::points(P_cov, pch = 16, cex = 0.35,
                   col = grDevices::adjustcolor(c_pt, 0.6))
  graphics::points(P_unc, pch = 16, cex = 0.35,
                   col = grDevices::adjustcolor(c_pt, 0.6))
  if (n_rel) graphics::points(P_rel, pch = 17, cex = 1.15, col = g_pt)
  if (n_flag) graphics::points(P_flg, pch = 17, cex = 1.15, col = s_pt)

  graphics::text(-r * 0.55, r + 0.16,
                 paste0("construct region: \"", x$construct, "\""),
                 col = c_pt, font = 2, cex = 0.95)
  graphics::text(d + r * 0.55, r + 0.16, paste0("scale (", x$n_items,
                                                " items)"),
                 col = s_pt, font = 2, cex = 0.95)
  fmt <- function(v) formatC(v, digits = 2, format = "f")
  graphics::title(main = paste0(
    "construct coverage = ", fmt(x$coverage), " (ideal ~ ", fmt(x$radius_q),
    ")  ·  item relevance = ", fmt(x$item_relevance), " (",
    sum(x$relevant_items), "/", x$n_items, " items)"),
    adj = 0, cex.main = 0.95)
  invisible(x)
}

#' @keywords internal
# Per-item relevance chart: corroboration counts on a log1p axis, the
# calibrated critical count as a dashed line, every bar labeled with its
# count and empirical p-value, flagged items in red.
.cvg_plot_relevance <- function(x) {
  ord <- order(x$corroboration)          # worst first, drawn at the top
  vals <- x$corroboration[ord]
  pv <- x$p_values[ord]
  flg <- !x$relevant_items[ord]
  labs <- vapply(x$item_text[ord], function(t)
    if (nchar(t) > 46) paste0(substr(t, 1, 45), "…") else t,
    character(1))
  tr <- function(v) log10(1 + v)
  n <- length(vals)
  ticks <- c(0, 1, 3, 10, 30, 100, 300, 1000)
  ticks <- ticks[tr(ticks) <= tr(max(vals)) + 0.3]

  op <- graphics::par(mar = c(4, 16, 4, 4), mgp = c(2.4, 0.6, 0))
  on.exit(graphics::par(op))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, tr(max(vals)) * 1.18),
                        ylim = c(n + 0.6, 0.4))
  y <- seq_len(n)
  graphics::rect(0, y - 0.31, pmax(tr(vals), 0.02), y + 0.31,
                 col = ifelse(flg, "#e34948", "#2a78d6"), border = NA)
  graphics::abline(v = tr(x$critical_count), lty = 2, col = "grey30")
  graphics::axis(1, at = tr(ticks), labels = ticks, cex.axis = 0.8)
  graphics::mtext(labs, side = 2, at = y, las = 1, cex = 0.62, line = 0.4,
                  col = ifelse(flg, "#b13231", "grey20"))
  graphics::text(pmax(tr(vals), 0.02) + 0.04, y,
                 paste0(vals, "  ·  p = ",
                        formatC(pv, digits = 2, format = "f")),
                 adj = 0, cex = 0.6, col = "grey25", xpd = NA)
  graphics::title(
    xlab = "construct texts within the coverage radius (log scale)",
    main = paste0("item relevance = ",
                  formatC(x$item_relevance, digits = 2, format = "f"),
                  " (", sum(x$relevant_items), "/", x$n_items,
                  " items pass at alpha = ", x$alpha,
                  ")  ·  ideal ~ ",
                  formatC(x$ideal_relevance, digits = 2, format = "f")),
    adj = 0, cex.main = 0.95)
  invisible(x)
}

#' @keywords internal
# Coverage curve vs the matched-size null (the pre-0.3.0 default plot).
.cvg_plot_curve <- function(x, ...) {
  d <- sort(x$d_region)
  ecdf_y <- seq_along(d) / length(d)
  graphics::plot(d, ecdf_y, type = "l", lwd = 2,
                 xlab = "distance to nearest item (embedding space)",
                 ylab = "fraction of region within distance",
                 main = paste0("Coverage curve: ", x$n_items, " items vs \"",
                               x$construct, "\""), ...)
  graphics::abline(v = x$radius, lty = 2, col = "grey40")
  graphics::abline(h = x$coverage, lty = 3, col = "grey60")
  graphics::points(x$radius, x$coverage, pch = 19)
  graphics::text(x$radius, min(x$coverage + 0.08, 0.97),
                 labels = paste0("construct coverage = ",
                                 formatC(x$coverage, digits = 2,
                                         format = "f")),
                 pos = 2, cex = 0.85)
  graphics::mtext(paste0("coverage radius = ",
                         formatC(x$radius, digits = 2, format = "f"),
                         " (matched-size null, q = ", x$radius_q, ")"),
                  side = 3, cex = 0.75, col = "grey30")
  invisible(x)
}

#' Plot a Content-Validity Audit
#'
#' @param x An `"sfa_coverage"` object from [sfa_coverage()].
#' @param type `"coverage"` (default) draws the proportional-overlap Euler
#'   diagram: two equal disks whose overlap area equals the measured
#'   construct coverage, filled with the real texts (dots) and items
#'   (triangles) placed by their full-space verdicts. `"relevance"` draws
#'   the per-item chart: corroboration counts, empirical p-values, and the
#'   calibrated critical count. `"curve"` draws the coverage curve against
#'   the matched-size null.
#' @param seed Seed for the (arbitrary, uniform) within-region point
#'   placement in the `"coverage"` diagram. Default 20260711.
#' @param ... Passed to the base plotting calls (`"curve"` only).
#' @returns `x`, invisibly.
#' @export
plot.sfa_coverage <- function(x, type = c("coverage", "relevance", "curve"),
                              seed = 20260711, ...) {
  type <- match.arg(type)
  switch(type,
         coverage = .cvg_plot_coverage(x, seed = seed),
         relevance = .cvg_plot_relevance(x),
         curve = .cvg_plot_curve(x, ...))
}
