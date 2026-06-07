# =============================================================================
# Method note. Vetting a candidate item against a scale's constructs by
# comparing its embedding to (a) the construct-name embedding and (b) the
# construct's existing-item centroid extends the construct-anchoring approach
# of:
#
#   Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#     taxonomic incommensurability in psychological measurement. Nature Human
#     Behaviour, 9(1), 1-14. https://doi.org/10.1038/s41562-024-02089-y
# =============================================================================

#' Vet a Candidate Scale Item Before Data Collection
#'
#' Scores draft item text against an existing scale: how well it matches each
#' construct, whether it discriminates (low cross-loading risk), how it compares
#' to the construct's current items, and whether it duplicates one of them ---
#' entirely response-free. Each candidate is scored on two complementary axes
#' per construct:
#' \describe{
#'   \item{\strong{Similarity to name}}{Cosine between the candidate and the
#'     embedding of the construct's name (e.g. "Depression"): does it sound like
#'     the construct?}
#'   \item{\strong{Similarity to other items}}{Cosine between the candidate and
#'     the centroid of the construct's existing items: does it look like the
#'     other items?}
#' }
#' When the two disagree they are informative: high name + low items is a
#' \emph{gap-filler} (on-topic but covering new ground); low name + high items is
#' \emph{drift} (looks like the items but not the construct).
#'
#' @param x An object of class \code{"sfa"} carrying theoretical factor labels
#'   and stored (raw) embeddings.
#' @param item Character vector of one or more candidate items to vet.
#' @param construct Optional name of the construct you intend the item for
#'   (matched to the factor labels by exact, case-insensitive, or unique-prefix
#'   match). When supplied, the verdict is reported relative to that construct as
#'   well as the best-matching one.
#' @param reverse_key Logical; set \code{TRUE} if the candidate is a
#'   reverse-keyed item (its embedding is flipped before comparison). Default
#'   \code{FALSE}.
#' @param redundancy_cutoff Similarity to the nearest existing item at or above
#'   which the candidate is flagged as a near-duplicate. Default 0.90.
#' @param embed,model Embedding backend and model used to embed the candidate(s)
#'   and the construct names. Default to those recorded on \code{x}.
#'
#' @returns An object of class \code{"sfa_item_fit"}: a list with
#'   \code{similarity_to_name} and \code{similarity_to_items} (candidate x
#'   construct matrices), a per-candidate \code{summary} data frame (best
#'   construct, the two similarities, second-best construct and gap, strength
#'   versus the average existing item, nearest item and its similarity, and a
#'   verdict), and the per-construct average existing-item similarity
#'   \code{avg_item_fit}.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. \emph{Nature Human
#' Behaviour}, 9(1), 1--14. \doi{10.1038/s41562-024-02089-y}
#'
#' @seealso \code{\link{sfa_anchor}}, \code{\link{sfa_redundancy}}
#' @examples
#' \dontrun{
#' fit <- sfa(dass_df, nfactors = 3)            # fit with a real embedding model
#' sfa_item_fit(fit, "I am sad all the time", construct = "Depression")
#' sfa_item_fit(fit, c("I feel calm and relaxed",
#'                     "My heart was racing"))   # vet several at once
#' }
#' @export
sfa_item_fit <- function(x, item, construct = NULL, reverse_key = FALSE,
                         redundancy_cutoff = 0.90, embed = NULL, model = NULL) {
  if (!inherits(x, "sfa")) stop("'x' must be an 'sfa' object.", call. = FALSE)
  if (!is.character(item) || length(item) == 0L) {
    stop("'item' must be a non-empty character vector.", call. = FALSE)
  }
  factors <- x$item_data$factor
  if (is.null(factors)) {
    stop("sfa_item_fit() needs theoretical factor labels. Refit sfa() from a ",
         "data.frame with a 'factor' column (or named items).", call. = FALSE)
  }
  raw <- x$input_embeddings
  if (is.null(raw)) {
    stop("'x' has no stored embeddings (fit from a precomputed similarity ",
         "matrix); sfa_item_fit() needs item embeddings.", call. = FALSE)
  }
  raw <- as.matrix(raw)
  storage.mode(raw) <- "double"
  codes  <- x$item_data$code
  if (is.null(codes)) codes <- sprintf("item_%02d", seq_len(nrow(raw)))
  texts  <- x$item_data$item
  scoring <- x$item_data$scoring
  if (is.null(scoring)) scoring <- rep(1, nrow(raw))
  constructs <- unique(factors)

  if (!is.null(construct)) construct <- .match_construct(construct, constructs)

  # Work in raw, unit-norm, forward-aligned space (reverse-keyed items flipped to
  # their construct's pole) so cosine reflects semantic + valence agreement.
  raw_norm <- .row_normalize(raw)         # zero-norm rows stay zero (no NaN)
  aligned  <- raw_norm * scoring

  # construct centroids (unit-norm) from the aligned existing items; a construct
  # whose items cancel to a zero centroid stays zero (cosine 0, not NaN)
  cents <- t(vapply(constructs, function(g) {
    colMeans(aligned[factors == g, , drop = FALSE])
  }, numeric(ncol(raw))))
  cents <- .row_normalize(cents)

  # how well existing items fit their own construct (leave-one-out), averaged
  anchorM <- .anchor_centroid(aligned, factors, constructs)
  own_idx <- match(factors, constructs)
  own_sim <- anchorM[cbind(seq_along(factors), own_idx)]
  avg_item_fit <- vapply(constructs, function(g)
    mean(own_sim[factors == g], na.rm = TRUE), numeric(1))

  # --- embed the candidate(s) and the construct names ---
  embed <- embed %||% (if (!is.null(x$embed_method) &&
                           x$embed_method %in% c("sbert", "openai"))
                       x$embed_method else "sbert")
  # a string backend needs a model; a custom embed function does not
  if (!is.function(embed)) {
    model <- model %||% x$embed_model
    if (is.null(model)) {
      stop("sfa_item_fit() needs an embedding model, but this 'sfa' object used ",
           "precomputed embeddings. Pass a custom embed function, or embed=/model=.",
           call. = FALSE)
    }
  }
  cand <- as.matrix(sfa_embed(item, embed = embed, model = model))
  storage.mode(cand) <- "double"
  cand <- .row_normalize(cand)
  if (isTRUE(reverse_key)) cand <- -cand

  name_emb <- as.matrix(sfa_embed(constructs, embed = embed, model = model))
  storage.mode(name_emb) <- "double"
  name_emb <- .row_normalize(name_emb)

  sim_name  <- cand %*% t(name_emb)
  sim_items <- cand %*% t(cents)
  colnames(sim_name) <- colnames(sim_items) <- constructs
  rownames(sim_name) <- rownames(sim_items) <- item

  red <- cand %*% t(aligned)              # candidate x existing-item cosine
  colnames(red) <- codes

  # --- per-candidate summary ---
  n_c <- length(item)
  summ <- data.frame(
    item = item, best = NA_character_,
    sim_name = NA_real_, sim_items = NA_real_,
    second = NA_character_, gap = NA_real_,
    strength = NA_character_, nearest = NA_character_, nearest_sim = NA_real_,
    verdict = NA_character_, stringsAsFactors = FALSE
  )
  for (i in seq_len(n_c)) {
    si <- sim_items[i, ]
    ord <- order(si, decreasing = TRUE)
    best <- constructs[ord[1]]
    if (length(constructs) >= 2L) {              # cross-loading needs >= 2 constructs
      second <- constructs[ord[2]]
      gap <- si[ord[1]] - si[ord[2]]
    } else {
      second <- NA_character_
      gap <- NA_real_
    }
    avg <- avg_item_fit[[best]]
    strength <- if (!is.finite(avg)) "about typical"   # singleton construct: no baseline
                else if (si[ord[1]] >= avg + 0.05) "stronger than average"
                else if (si[ord[1]] <= avg - 0.05) "weaker than average"
                else "about typical"
    j <- which.max(red[i, ])
    summ$best[i] <- best
    summ$sim_name[i] <- sim_name[i, best]
    summ$sim_items[i] <- si[ord[1]]
    summ$second[i] <- second
    summ$gap[i] <- gap
    summ$strength[i] <- strength
    summ$nearest[i] <- codes[j]
    summ$nearest_sim[i] <- red[i, j]
    summ$verdict[i] <- .item_fit_verdict(gap, strength, red[i, j],
                                         best, second, redundancy_cutoff)
  }

  structure(list(
    items = item, construct = construct, reverse_key = isTRUE(reverse_key),
    constructs = constructs,
    similarity_to_name = sim_name, similarity_to_items = sim_items,
    avg_item_fit = avg_item_fit, nearest_item_text = texts,
    redundancy = red, redundancy_cutoff = redundancy_cutoff,
    summary = summ
  ), class = "sfa_item_fit")
}

#' @keywords internal
.match_construct <- function(value, constructs) {
  value <- as.character(value)[1]
  hit <- which(constructs == value)
  if (length(hit) != 1L) hit <- which(tolower(constructs) == tolower(value))
  if (length(hit) != 1L) hit <- which(startsWith(tolower(constructs), tolower(value)))
  if (length(hit) > 1L) {
    stop("'construct' \"", value, "\" is ambiguous; matches: ",
         paste(constructs[hit], collapse = ", "), ".", call. = FALSE)
  }
  if (length(hit) == 0L) {
    stop("'construct' \"", value, "\" matches no factor. Factors are: ",
         paste(constructs, collapse = ", "), ".", call. = FALSE)
  }
  constructs[hit]
}

#' @keywords internal
# Row-wise L2 normalization; zero-norm rows are left as zero (cosine 0) rather
# than becoming NaN, so a degenerate item/centroid cannot crash downstream logic.
.row_normalize <- function(M) {
  nrm <- sqrt(rowSums(M^2))
  nrm[!is.finite(nrm) | nrm <= 0] <- 1
  M / nrm
}

#' @keywords internal
.item_fit_verdict <- function(gap, strength, nearest_sim, best, second, cutoff) {
  if (nearest_sim >= cutoff) {
    return("redundant (near-duplicate of an existing item)")
  }
  if (strength == "weaker than average") {
    return(paste0("weak match - fits ", best, " worse than its existing items ",
                  "(may not belong to this scale)"))
  }
  if (!is.na(gap) && gap < 0.07) {       # gap is NA for a single-construct scale
    return(paste0("cross-loads (", best, " vs ", second, ") - poor discrimination"))
  }
  paste0("good fit for ", best, " - clean and non-redundant")
}

#' @export
print.sfa_item_fit <- function(x, digits = 2, ...) {
  cat("Candidate item fit\n")
  cat("  Method: Wulff & Mata (2025), extended\n")
  if (isTRUE(x$reverse_key)) cat("  (candidates treated as reverse-keyed)\n")
  cat("\n")
  for (i in seq_along(x$items)) {
    s <- x$summary[i, ]
    cat(sprintf("Candidate %d: \"%s\"\n", i, x$items[i]))
    # per-construct profile
    nm <- round(x$similarity_to_name[i, ], digits)
    it <- round(x$similarity_to_items[i, ], digits)
    ord <- order(it, decreasing = TRUE)
    cat("  Similarity to (name | other items), by construct:\n")
    for (k in ord) {
      cat(sprintf("    %-16s name %5.2f   items %5.2f\n",
                  x$constructs[k], nm[k], it[k]))
    }
    cat(sprintf("  Best construct : %s\n", s$best))
    if (!is.na(s$gap)) {
      disc <- if (s$gap >= 0.15) "discriminates well"
              else if (s$gap >= 0.07) "some cross-loading risk"
              else paste0("ambiguous - cross-loads with ", s$second)
      cat(sprintf("  Cross-loading  : gap to 2nd (%s) = %.2f  ->  %s\n",
                  s$second, s$gap, disc))
    } else {
      cat("  Cross-loading  : n/a (scale has a single construct)\n")
    }
    avgval <- x$avg_item_fit[[s$best]]
    if (is.finite(avgval)) {
      cat(sprintf("  Strength       : %s for %s (items %.2f vs avg %.2f)\n",
                  s$strength, s$best, s$sim_items, avgval))
    } else {
      cat(sprintf("  Strength       : no baseline for %s (single-item construct)\n",
                  s$best))
    }
    red_txt <- if (s$nearest_sim >= x$redundancy_cutoff) "near-duplicate" else "distinct"
    near_text <- x$nearest_item_text[match(s$nearest, names(x$redundancy[i, ]))]
    cat(sprintf("  Redundancy     : closest item %s", s$nearest))
    if (!is.null(near_text) && !is.na(near_text)) {
      cat(sprintf(" \"%s\"", near_text))
    }
    cat(sprintf(" (%.2f) -> %s\n", s$nearest_sim, red_txt))
    if (!is.null(x$construct) && x$construct != s$best) {
      cat(sprintf("  Target (%s) : items %.2f (best is %s) -> may not belong here\n",
                  x$construct, x$similarity_to_items[i, x$construct], s$best))
    }
    cat(sprintf("  Verdict        : %s\n\n", s$verdict))
  }
  invisible(x)
}
