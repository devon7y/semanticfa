# =============================================================================
# Method reference. Embedding construct labels and comparing items/scales to
# their construct anchors implements the approach of:
#
#   Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#     taxonomic incommensurability in psychological measurement. Nature Human
#     Behaviour, 9(1), 1-14. https://doi.org/10.1038/s41562-024-02089-y
# =============================================================================

#' Construct-Label and Centroid Anchoring
#'
#' Produces an item-by-construct similarity matrix --- the embedding analogue of
#' a factor-loading table. Items are sign-aligned by their scoring direction
#' first (embeddings encode topic, not valence, so a reverse-keyed item would
#' otherwise point away from its construct), so each cell is a \emph{belonging
#' strength}: high means the item belongs to that construct (for forward and
#' reverse items alike), low means it does not. Read it like a loadings matrix
#' --- a well-behaved item is high in its own construct's column and low in the
#' others; an item whose largest value lands on a different construct is a
#' semantic cross-loader and a candidate for review.
#'
#' Two anchor types are available:
#' \describe{
#'   \item{\code{"centroid"}}{(default) Each construct's anchor is the mean of
#'     its own (sign-aligned) item embeddings. An item's similarity to its own
#'     construct is computed leave-one-out (the item is excluded from its own
#'     anchor), mirroring a corrected item-total correlation. Self-contained ---
#'     needs no construct text and works for any \code{sfa} object.}
#'   \item{\code{"label"}}{Each construct's anchor is the embedding of the
#'     construct's name (or a richer gloss supplied via \code{labels}). Requires
#'     an embedding backend or precomputed \code{label_embeddings}. Cleanest for
#'     the default \code{"atomic_reversed"} and \code{"atomic"} encodings.}
#' }
#'
#' @param x An object of class \code{"sfa"} carrying theoretical factor labels
#'   (i.e. fit from items with a \code{factor} column).
#' @param anchor One of \code{"centroid"} (default), \code{"label"}, or
#'   \code{"both"}.
#' @param labels Optional construct labels for the label anchor: either a
#'   character vector (one per construct, in the order of
#'   \code{unique(factors)}) or a named vector mapping construct -> label text.
#'   Defaults to the construct names themselves.
#' @param label_embeddings Optional precomputed numeric matrix of label
#'   embeddings (one row per construct; named rows are matched to constructs).
#'   Use when the \code{sfa} object was built from precomputed embeddings.
#' @param embed,model Embedding backend and model for the label anchor. Default
#'   to the backend/model recorded on \code{x}.
#'
#' @returns An object of class \code{"sfa_anchor"}: a list with the requested
#'   \code{centroid} and/or \code{label} item-by-construct similarity matrices,
#'   plus \code{constructs}, \code{factors}, and \code{codes}.
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. \emph{Nature Human
#' Behaviour}, 9(1), 1--14. \doi{10.1038/s41562-024-02089-y}
#'
#' @seealso \code{\link{sfa_simplify}}, \code{\link{sfa}}
#' @examples
#' data(big5)
#' fit <- sfa(
#'   data.frame(code = big5$codes, item = big5$items,
#'              factor = big5$factors, scoring = big5$scoring),
#'   embeddings = big5$embeddings, scoring = big5$scoring, nfactors = 5)
#'
#' # item-by-construct belonging matrix (read like a loadings table)
#' a <- sfa_anchor(fit, anchor = "centroid")
#' head(round(a$centroid, 2))
#' @export
sfa_anchor <- function(x, anchor = c("centroid", "label", "both"),
                       labels = NULL, label_embeddings = NULL,
                       embed = NULL, model = NULL) {
  if (!inherits(x, "sfa")) stop("'x' must be an 'sfa' object.", call. = FALSE)
  anchor <- match.arg(anchor)

  factors <- x$item_data$factor
  if (is.null(factors)) {
    stop("sfa_anchor() needs theoretical factor labels. Refit sfa() from a ",
         "data.frame with a 'factor' column (or named items).", call. = FALSE)
  }
  emb <- x$input_embeddings
  if (is.null(emb)) {
    stop("'x' has no stored embeddings (it was fit from a precomputed ",
         "similarity matrix); sfa_anchor() needs item embeddings.",
         call. = FALSE)
  }
  emb <- as.matrix(emb)
  emb <- emb / sqrt(rowSums(emb^2))         # unit-norm raw embeddings
  codes <- x$item_data$code
  rownames(emb) <- codes
  constructs <- unique(factors)

  # Belonging is topical relatedness to each construct's centroid, measured in
  # the raw (un-flipped) embedding space. Embeddings encode topic, not valence,
  # so a reverse-keyed item is still topically close to its construct; belonging
  # is therefore positive for forward and reverse items alike, and -- because it
  # uses the raw embeddings -- does not depend on the chosen encoding.
  scoring <- x$item_data$scoring
  if (is.null(scoring)) scoring <- rep(1, length(codes))
  aligned <- emb

  out <- list(constructs = constructs, factors = factors, codes = codes,
              items = x$item_data$item, scoring = scoring, anchor = anchor)

  if (anchor %in% c("centroid", "both")) {
    out$centroid <- .anchor_centroid(aligned, factors, constructs)
  }
  if (anchor %in% c("label", "both")) {
    lab_emb <- .resolve_label_embeddings(x, constructs, labels,
                                         label_embeddings, embed, model)
    out$label <- .anchor_label(aligned, lab_emb, constructs)
  }

  structure(out, class = "sfa_anchor")
}

#' @keywords internal
.anchor_centroid <- function(emb, factors, constructs) {
  n <- nrow(emb)
  K <- length(constructs)
  csum <- matrix(0, K, ncol(emb))
  cnt <- integer(K)
  for (k in seq_len(K)) {
    idx <- which(factors == constructs[k])
    csum[k, ] <- colSums(emb[idx, , drop = FALSE])
    cnt[k] <- length(idx)
  }
  own <- match(factors, constructs)
  M <- matrix(NA_real_, n, K, dimnames = list(rownames(emb), constructs))
  for (i in seq_len(n)) {
    for (k in seq_len(K)) {
      if (k == own[i]) {
        if (cnt[k] <= 1) next                       # singleton: own-sim undefined
        cen <- (csum[k, ] - emb[i, ]) / (cnt[k] - 1) # leave-one-out
      } else {
        cen <- csum[k, ] / cnt[k]
      }
      d <- sqrt(sum(cen^2))
      M[i, k] <- if (d > 0) sum(emb[i, ] * cen) / d else NA_real_
    }
  }
  M
}

#' @keywords internal
.anchor_label <- function(emb, lab_emb, constructs) {
  if (ncol(lab_emb) != ncol(emb)) {
    stop("Label embedding dimension (", ncol(lab_emb), ") does not match item ",
         "embedding dimension (", ncol(emb), "). Use the same model.",
         call. = FALSE)
  }
  lab_norm <- lab_emb / sqrt(rowSums(lab_emb^2))
  M <- emb %*% t(lab_norm)            # emb rows are already unit-norm
  dimnames(M) <- list(rownames(emb), constructs)
  M
}

#' @keywords internal
.resolve_label_embeddings <- function(x, constructs, labels, label_embeddings,
                                      embed, model) {
  if (!is.null(label_embeddings)) {
    le <- as.matrix(label_embeddings)
    if (!is.null(rownames(le))) {
      miss <- setdiff(constructs, rownames(le))
      if (length(miss)) {
        stop("label_embeddings missing rows for: ",
             paste(miss, collapse = ", "), call. = FALSE)
      }
      le <- le[constructs, , drop = FALSE]
    } else if (nrow(le) != length(constructs)) {
      stop("label_embeddings must have one row per construct (",
           length(constructs), ") or named rows.", call. = FALSE)
    }
    storage.mode(le) <- "double"
    return(le)
  }

  lab_text <- if (is.null(labels)) {
    constructs
  } else if (!is.null(names(labels))) {
    miss <- setdiff(constructs, names(labels))
    if (length(miss)) {
      stop("labels missing entries for: ", paste(miss, collapse = ", "),
           call. = FALSE)
    }
    unname(labels[constructs])
  } else {
    if (length(labels) != length(constructs)) {
      stop("labels must be length ", length(constructs),
           " or a named vector.", call. = FALSE)
    }
    labels
  }

  embed <- embed %||% (if (!is.null(x$embed_method) &&
                           x$embed_method %in% c("sbert", "openai"))
                       x$embed_method else "sbert")
  # a string backend needs a model; a custom embed function does not
  if (!is.function(embed)) {
    model <- model %||% x$embed_model
    if (is.null(model)) {
      stop("Label anchoring needs an embedding model, but this 'sfa' object used ",
           "precomputed embeddings. Pass label_embeddings=, a custom embed ",
           "function, or embed=/model=.", call. = FALSE)
    }
  }
  le <- sfa_embed(lab_text, embed = embed, model = model)
  storage.mode(le) <- "double"
  le
}

#' @export
print.sfa_anchor <- function(x, digits = 2, n_flag = 8, ...) {
  cat("Semantic anchoring\n")
  cat("  Method: Wulff & Mata (2025)\n")
  cat("  Anchor:", x$anchor, "\n")
  cat("  Constructs:", length(x$constructs),
      "| Items:", length(x$codes), "\n\n")

  for (ty in intersect(c("centroid", "label"), names(x))) {
    M <- x[[ty]]
    cat(toupper(substring(ty, 1, 1)), substring(ty, 2),
        " anchor - item x construct cosine:\n", sep = "")
    print(round(M, digits))

    own <- match(x$factors, colnames(M))
    own_sim <- M[cbind(seq_len(nrow(M)), own)]
    ord <- order(own_sim)
    cat("\n  Weakest own-construct similarity (review/removal candidates):\n")
    for (i in utils::head(ord, n_flag)) {
      others <- M[i, -own[i], drop = TRUE]
      flag <- ""
      if (length(others) && !all(is.na(others))) {
        bo <- which.max(others)
        if (!is.na(own_sim[i]) && others[bo] > own_sim[i]) {
          flag <- sprintf("   <-- higher on %s (%.2f)",
                          names(others)[bo], others[bo])
        }
      }
      cat(sprintf("    %-12s %-18s own=%.2f%s\n",
                  x$codes[i], x$factors[i], own_sim[i], flag))
      if (!is.null(x$items)) {
        cat(sprintf("        \"%s\"\n", x$items[i]))
      }
    }
    cat("\n")
  }
  invisible(x)
}
