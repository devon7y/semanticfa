# =============================================================================
# Method reference. Projecting item/word vectors onto an axis defined by two
# opposing poles implements the approach of:
#
#   Grand, G., Blank, I. A., Pereira, F., & Fedorenko, E. (2022). Semantic
#     projection recovers rich human knowledge of multiple object features from
#     word embeddings. Nature Human Behaviour, 6(7), 975-987.
#     https://doi.org/10.1038/s41562-022-01316-8
# =============================================================================

#' Semantic Projection onto Bipolar Axes
#'
#' Places each item on a continuous scale defined by two opposing text poles
#' (Grand et al. 2022). An axis is built as the direction from a "low" pole to a
#' "high" pole (e.g. \emph{mild} -> \emph{severe}, \emph{passive} -> \emph{active});
#' every item is then projected onto that line. Unlike factor grouping (which
#' says \emph{which} construct an item belongs to), projection says \emph{where
#' along a named dimension} the item falls --- useful for checking that a scale's
#' items span a full range of intensity/severity, ordering items, or locating
#' items on an interpretable axis.
#'
#' This uses the \strong{cosine} of each item against the pole-difference axis (a
#' length-normalized variant of Grand et al.'s raw inner-product projection),
#' so scores are comparable across items of differing embedding norm. As in
#' Grand et al., a \emph{bipolar} (two-pole) axis is what gives a diagnostic
#' direction; a single pole is far less informative.
#'
#' @param x An \code{"sfa"} object, or a numeric item-embedding matrix
#'   (n_items x dim) with item rownames.
#' @param axes A named list of axes. Each element defines the two poles, as
#'   either a named character vector \code{c(low = "...", high = "...")} or a
#'   list \code{list(low = c(...phrases...), high = c(...phrases...))} (multiple
#'   phrases per pole are averaged, which is more robust).
#' @param normalize Logical. If \code{TRUE} (default), rescale each item's
#'   projection so 0 = the low pole and 1 = the high pole (values may fall
#'   outside 0 to 1). If \code{FALSE}, return the raw cosine projection in
#'   the range -1 to 1.
#' @param pole_embeddings Optional named list (one entry per axis) of precomputed
#'   pole embeddings, each a list with \code{low} and \code{high} numeric
#'   matrices/vectors. Use when \code{x} carries no embedding backend.
#' @param embed,model Embedding backend/model for the pole text. Default to the
#'   backend/model recorded on \code{x}.
#'
#' @returns An object of class \code{"sfa_projection"}: a list with the
#'   item-by-axis \code{scores} matrix, the axis definitions, and \code{normalize}.
#'
#' @references
#' Grand, G., Blank, I. A., Pereira, F., & Fedorenko, E. (2022). Semantic
#' projection recovers rich human knowledge of multiple object features from
#' word embeddings. \emph{Nature Human Behaviour}, 6(7), 975--987.
#' \doi{10.1038/s41562-022-01316-8}
#'
#' @seealso \code{\link{sfa_anchor}}
#' @examples
#' data(big5)
#' fit <- sfa(
#'   data.frame(code = big5$codes, item = big5$items,
#'              factor = big5$factors, scoring = big5$scoring),
#'   embeddings = big5$embeddings, scoring = big5$scoring, nfactors = 5)
#'
#' # project items onto a neuroticism -> extraversion axis using precomputed poles
#' poles <- list(NtoE = list(
#'   low  = big5$embeddings[big5$factors == "Neuroticism", ],
#'   high = big5$embeddings[big5$factors == "Extraversion", ]))
#' pr <- sfa_project(fit, axes = list(NtoE = c(low = "neurotic", high = "extraverted")),
#'                   pole_embeddings = poles)
#' head(round(pr$scores, 2))
#'
#' \dontrun{
#' # with a live embedding backend, name the poles in words and they are embedded:
#' sfa_project(fit, axes = list(severity = c(low = "mild", high = "severe")))
#' }
#' @export
sfa_project <- function(x, axes, normalize = TRUE, pole_embeddings = NULL,
                        embed = NULL, model = NULL) {
  emb <- .item_embeddings(x)
  codes <- rownames(emb)
  if (!is.list(axes) || is.null(names(axes)) || any(names(axes) == "")) {
    stop("'axes' must be a named list, one entry per axis.", call. = FALSE)
  }
  axes <- lapply(axes, .normalize_axis)

  # L2-normalize item vectors so projections are comparable across items
  emb_u <- emb / sqrt(rowSums(emb^2))

  scores <- matrix(NA_real_, nrow(emb), length(axes),
                   dimnames = list(codes, names(axes)))
  for (j in seq_along(axes)) {
    poles <- .resolve_pole_embeddings(x, axes[[j]], pole_embeddings[[names(axes)[j]]],
                                      embed, model)
    if (ncol(poles$low) != ncol(emb)) {
      stop("Pole embedding dim (", ncol(poles$low), ") does not match item ",
           "embedding dim (", ncol(emb), ") for axis '", names(axes)[j],
           "'. Use the same model.", call. = FALSE)
    }
    low_vec <- colMeans(poles$low)
    high_vec <- colMeans(poles$high)
    axis_dir <- high_vec - low_vec
    nrm <- sqrt(sum(axis_dir^2))
    if (nrm == 0) stop("Axis '", names(axes)[j], "' has identical poles.",
                       call. = FALSE)
    unit <- axis_dir / nrm
    proj <- as.numeric(emb_u %*% unit)
    if (normalize) {
      lo <- sum((low_vec / sqrt(sum(low_vec^2))) * unit)
      hi <- sum((high_vec / sqrt(sum(high_vec^2))) * unit)
      proj <- if (hi != lo) (proj - lo) / (hi - lo) else proj
    }
    scores[, j] <- proj
  }

  structure(list(scores = scores, axes = axes, normalize = normalize),
            class = "sfa_projection")
}

#' @keywords internal
.item_embeddings <- function(x) {
  if (inherits(x, "sfa")) {
    emb <- x$input_embeddings %||% x$embeddings
    if (is.null(emb)) stop("'x' has no stored embeddings.", call. = FALSE)
    emb <- as.matrix(emb)
    if (is.null(rownames(emb))) rownames(emb) <- x$item_data$code
    return(emb)
  }
  if (is.matrix(x) && is.numeric(x)) {
    if (is.null(rownames(x))) rownames(x) <- sprintf("item_%02d", seq_len(nrow(x)))
    return(x)
  }
  stop("'x' must be an 'sfa' object or a numeric embedding matrix.", call. = FALSE)
}

#' @keywords internal
.normalize_axis <- function(e) {
  if (is.list(e) && all(c("low", "high") %in% names(e))) {
    return(list(low = as.character(e$low), high = as.character(e$high)))
  }
  if (is.character(e) && all(c("low", "high") %in% names(e))) {
    return(list(low = unname(e["low"]), high = unname(e["high"])))
  }
  if (is.character(e) && length(e) == 2L) {
    return(list(low = e[1], high = e[2]))
  }
  stop("Each axis must be c(low=, high=) or list(low=, high=).", call. = FALSE)
}

#' @keywords internal
.resolve_pole_embeddings <- function(x, axis, precomputed, embed, model) {
  if (!is.null(precomputed)) {
    lo <- as.matrix(precomputed$low); hi <- as.matrix(precomputed$high)
    if (is.null(dim(precomputed$low))) lo <- matrix(precomputed$low, nrow = 1)
    if (is.null(dim(precomputed$high))) hi <- matrix(precomputed$high, nrow = 1)
    return(list(low = lo, high = hi))
  }
  embed <- embed %||% (if (inherits(x, "sfa") && !is.null(x$embed_method) &&
                           x$embed_method %in% c("sbert", "openai"))
                       x$embed_method else "sbert")
  # a string backend needs a model; a custom embed function does not
  if (!is.function(embed)) {
    model <- model %||% (if (inherits(x, "sfa")) x$embed_model else NULL)
    if (is.null(model)) {
      stop("Semantic projection needs an embedding model to embed the poles. ",
           "Pass pole_embeddings=, a custom embed function, or embed=/model=.",
           call. = FALSE)
    }
  }
  list(low = sfa_embed(axis$low, embed = embed, model = model),
       high = sfa_embed(axis$high, embed = embed, model = model))
}

#' @export
print.sfa_projection <- function(x, digits = 2, n_show = 5, ...) {
  cat("Semantic projection onto", ncol(x$scores), "axis/axes\n")
  cat("  Method: Grand et al. (2022)\n")
  cat("  Scale:", if (x$normalize) "0 = low pole, 1 = high pole" else
      "raw cosine projection [-1, 1]", "\n\n")
  for (j in seq_len(ncol(x$scores))) {
    ax <- x$axes[[j]]
    s <- x$scores[, j]
    cat(sprintf("Axis '%s'  [%s <-> %s]\n", colnames(x$scores)[j],
                paste(ax$low, collapse = "/"), paste(ax$high, collapse = "/")))
    ord <- order(s)
    cat("  lowest: ")
    cat(paste(sprintf("%s=%.*f", names(s)[utils::head(ord, n_show)], digits,
                      s[utils::head(ord, n_show)]), collapse = "  "), "\n")
    cat("  highest:")
    top <- rev(utils::tail(ord, n_show))
    cat(" ", paste(sprintf("%s=%.*f", names(s)[top], digits, s[top]),
                   collapse = "  "), "\n\n", sep = "")
  }
  invisible(x)
}
