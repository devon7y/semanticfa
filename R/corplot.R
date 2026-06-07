#' Heatmap of an Item-by-Item Similarity Matrix
#'
#' Draws a \code{\link[psych]{cor.plot}} heatmap of a semantic similarity matrix
#' with sensible defaults for a many-item scale. By default the items are
#' \strong{grouped by their subscale/factor} (so each construct forms a block on
#' the diagonal), the bulky transformed-embeddings attribute is removed, and all
#' axis labels are shown.
#'
#' Grouping happens only for display --- the underlying similarity matrix from
#' \code{\link{sfa_similarity}} keeps its original item order (rows aligned with
#' the items' scoring, codes, and embeddings), which the rest of the package
#' relies on.
#'
#' @param x An \code{"sfa"} object, or a similarity matrix from
#'   \code{\link{sfa_similarity}}.
#' @param factors Optional per-item subscale labels used to group the items. For
#'   an \code{"sfa"} object, defaults to its theoretical factors; for a matrix,
#'   defaults to a \code{"factors"} attribute if present.
#' @param labels Optional per-item axis labels. By default uses short item codes
#'   (the \code{code} column for an \code{"sfa"} object, or a \code{"codes"}
#'   attribute / short dimnames on a matrix). If only sentence-like labels are
#'   available, compact codes are generated from the factors (e.g. \code{A1},
#'   \code{A2}, \code{D1}, ...) rather than printing full item text.
#' @param group Logical: reorder items so each factor forms a contiguous block
#'   (default \code{TRUE}). Ignored when no \code{factors} are available.
#' @param order Optional character vector giving the order of the factor blocks
#'   (default: alphabetical). Entries are matched to the factor labels by exact,
#'   case-insensitive, or unique-prefix match, so for Depression/Anxiety/Stress
#'   both \code{c("Depression","Anxiety","Stress")} and \code{c("D","A","S")}
#'   work. A non-matching or ambiguous entry is an error; any factors omitted
#'   from \code{order} are appended after the listed ones.
#' @param numbers,upper,gap.axis,cex.axis,xlas Passed to
#'   \code{\link[psych]{cor.plot}}; defaults are tuned for a many-item matrix
#'   (no in-cell numbers, upper triangle, every label shown, small label text).
#' @param ... Further arguments passed to \code{\link[psych]{cor.plot}}.
#'
#' @returns The (grouped, relabelled) matrix that was plotted, invisibly.
#'
#' @seealso \code{\link{sfa_similarity}}, \code{\link{sfa}}
#' @examples
#' data(big5)
#' fit <- sfa(
#'   data.frame(code = big5$codes, item = big5$items,
#'              factor = big5$factors, scoring = big5$scoring),
#'   embeddings = big5$embeddings, scoring = big5$scoring, nfactors = 5)
#'
#' sfa_corplot(fit)                      # heatmap, grouped by the Big Five
#' @export
sfa_corplot <- function(x, factors = NULL, labels = NULL, group = TRUE,
                        order = NULL, numbers = FALSE, upper = TRUE,
                        gap.axis = -1, cex.axis = 0.75, xlas = 2, ...) {
  explicit_labels <- !is.null(labels)
  if (inherits(x, "sfa")) {
    sim <- x$sim_matrix
    if (is.null(factors)) factors <- x$item_data$factor
    if (is.null(labels))  labels  <- x$item_data$code
  } else {
    sim <- x
    if (is.null(factors)) factors <- attr(x, "factors")
    if (is.null(labels))  labels  <- attr(x, "codes")     # recorded codes, if any
    if (is.null(labels))  labels  <- rownames(x)
  }
  if (is.null(sim)) stop("No similarity matrix to plot.", call. = FALSE)

  sim <- as.matrix(sim)
  attr(sim, "transformed_embeddings") <- NULL          # drop bulky attributes
  attr(sim, "factors") <- NULL
  attr(sim, "codes") <- NULL

  # default to short codes, not full item text: if the only labels available are
  # sentence-like, replace them with compact codes -- factor-based when factors
  # are available (A1, A2, D1, ...), otherwise generic (I1, I2, ...)
  if (!explicit_labels && !.looks_like_codes(labels)) {
    labels <- if (!is.null(factors) && length(factors) == nrow(sim)) {
      .gen_item_codes(factors)
    } else {
      paste0("I", seq_len(nrow(sim)))
    }
  }
  if (!is.null(labels)) dimnames(sim) <- list(labels, labels)

  if (isTRUE(group) && !is.null(factors)) {
    if (length(factors) != nrow(sim)) {
      stop("'factors' length (", length(factors), ") does not match the ",
           "number of items (", nrow(sim), ").", call. = FALSE)
    }
    f <- as.character(factors)
    lv <- if (is.null(order)) sort(unique(f)) else .resolve_group_order(order, unique(f))
    ord <- order(match(f, lv))                          # group; stable within block
    sim <- sim[ord, ord]
  }

  psych::cor.plot(sim, numbers = numbers, upper = upper,
                  gap.axis = gap.axis, cex.axis = cex.axis, xlas = xlas, ...)
  invisible(sim)
}

#' @keywords internal
.looks_like_codes <- function(l) {
  !is.null(l) && length(l) > 0 && !any(is.na(l)) &&
    !any(grepl("\\s", l)) && max(nchar(l)) <= 12
}

#' @keywords internal
# Map each entry of a user-supplied block order to a factor level, accepting
# exact, case-insensitive, and unique-prefix/abbreviation matches (so
# order = c("D","A","S") resolves to Depression/Anxiety/Stress). Errors on a
# non-match or an ambiguous prefix; unmentioned levels are appended in order.
.resolve_group_order <- function(order, levels) {
  levels <- unique(as.character(levels))
  resolved <- unname(vapply(as.character(order), function(o) {
    hit <- which(levels == o)                                   # exact
    if (length(hit) != 1L) hit <- which(tolower(levels) == tolower(o))  # case-insensitive
    if (length(hit) != 1L) hit <- which(startsWith(tolower(levels), tolower(o)))  # prefix
    if (length(hit) > 1L) {
      stop("'order' entry \"", o, "\" is ambiguous; matches: ",
           paste(levels[hit], collapse = ", "), ".", call. = FALSE)
    }
    if (length(hit) == 0L) {
      stop("'order' entry \"", o, "\" matches no factor. Factors are: ",
           paste(levels, collapse = ", "), ".", call. = FALSE)
    }
    levels[hit]
  }, character(1)))
  c(resolved, setdiff(levels, resolved))     # append any factors not listed
}

#' @keywords internal
.gen_item_codes <- function(factors) {
  f <- as.character(factors)
  ab <- abbreviate(sort(unique(f)), minlength = 1L)
  idx <- integer(length(f))
  for (g in unique(f)) {
    w <- which(f == g)
    idx[w] <- seq_along(w)
  }
  paste0(ab[f], idx)
}
