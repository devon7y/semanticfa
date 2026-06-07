#' t-SNE Map of Item Embeddings
#'
#' A 2-D t-SNE scatter of the scale's items, the embedding-space companion to
#' \code{\link{sfa_corplot}}: each point is an item, points are coloured by their
#' theoretical factor and labelled with their short code, so you can see at a
#' glance which items cluster together, which sit between constructs, and which
#' are outliers. Operates on the same (transformed) item embeddings the factor
#' analysis uses, or on a similarity matrix (converted to a distance).
#'
#' @param x An \code{"sfa"} object (uses its item embeddings) or a symmetric
#'   numeric item-by-item similarity matrix.
#' @param factors,labels Optional per-item factor labels and point labels
#'   (codes). Default to those carried on \code{x} (or the matrix's
#'   \code{"factors"}/\code{"codes"} attributes).
#' @param color Logical; colour points by factor (default \code{TRUE}).
#' @param perplexity t-SNE perplexity. If \code{NULL}, a safe value is chosen for
#'   the item count (\code{max(1, min(30, floor((n - 1) / 3)))}).
#' @param seed Random seed for reproducibility (t-SNE is stochastic).
#' @param pch,cex Point symbol and size.
#' @param legend Logical; draw a factor legend (default \code{TRUE}).
#' @param ... Passed to \code{\link[graphics]{plot}}.
#'
#' @returns Invisibly, a list with the 2-D coordinates \code{Y}, the
#'   \code{factors}, and the \code{labels}.
#'
#' @references
#' van der Maaten, L., & Hinton, G. (2008). Visualizing data using t-SNE.
#' \emph{Journal of Machine Learning Research}, 9, 2579--2605.
#'
#' @seealso \code{\link{sfa_corplot}}
#' @examples
#' \dontrun{
#' data(big5)
#' fit <- sfa(
#'   data.frame(code = big5$codes, item = big5$items,
#'              factor = big5$factors, scoring = big5$scoring),
#'   embeddings = big5$embeddings, scoring = big5$scoring, nfactors = 5)
#' sfa_tsneplot(fit)
#' }
#' @export
sfa_tsneplot <- function(x, factors = NULL, labels = NULL, color = TRUE,
                         perplexity = NULL, seed = 42, pch = 19, cex = 0.9,
                         legend = TRUE, ...) {
  if (!requireNamespace("Rtsne", quietly = TRUE)) {
    stop("sfa_tsneplot() needs the 'Rtsne' package. ",
         "Install it with install.packages(\"Rtsne\").", call. = FALSE)
  }
  vecs <- NULL
  sim <- NULL
  if (inherits(x, "sfa")) {
    vecs <- x$embeddings
    if (is.null(vecs)) sim <- x$sim_matrix
    if (is.null(factors)) factors <- x$item_data$factor
    if (is.null(labels))  labels  <- x$item_data$code
    n <- nrow(x$sim_matrix)
  } else {
    sim <- as.matrix(x)
    if (nrow(sim) != ncol(sim)) {
      stop("Need an 'sfa' object or a square similarity matrix.", call. = FALSE)
    }
    if (is.null(factors)) factors <- attr(sim, "factors")
    if (is.null(labels))  labels  <- attr(sim, "codes")
    n <- nrow(sim)
  }
  if (n < 5L) {
    stop("t-SNE needs at least 5 items (got ", n, ").", call. = FALSE)
  }
  if (!is.null(labels) && length(labels) != n) {
    stop("'labels' length (", length(labels), ") does not match the number of ",
         "items (", n, ").", call. = FALSE)
  }
  if (!is.null(factors) && length(factors) != n) {
    stop("'factors' length (", length(factors), ") does not match the number of ",
         "items (", n, ").", call. = FALSE)
  }
  if (is.null(perplexity)) perplexity <- max(1, min(30, floor((n - 1) / 3)))

  withr::with_seed(seed, {
    ts <- if (!is.null(vecs)) {
      Rtsne::Rtsne(as.matrix(vecs), perplexity = perplexity, pca = TRUE,
                   check_duplicates = FALSE)
    } else {
      Rtsne::Rtsne(stats::as.dist(1 - sim), is_distance = TRUE,
                   perplexity = perplexity, check_duplicates = FALSE)
    }
  })
  Y <- ts$Y
  if (!is.null(labels)) rownames(Y) <- labels

  f <- NULL
  cols <- "black"
  pal <- NULL
  if (isTRUE(color) && !is.null(factors)) {
    f <- factor(factors)
    pal <- grDevices::hcl.colors(max(nlevels(f), 2L), "Dark 3")[seq_len(nlevels(f))]
    cols <- pal[as.integer(f)]
  }

  graphics::plot(Y, col = cols, pch = pch, cex = cex,
                 xlab = "t-SNE 1", ylab = "t-SNE 2",
                 main = "t-SNE of item embeddings", ...)
  if (!is.null(labels)) {
    graphics::text(Y, labels = labels, pos = 3, cex = cex * 0.7, col = cols)
  }
  if (isTRUE(legend) && !is.null(f)) {
    graphics::legend("topright", legend = levels(f), col = pal, pch = pch,
                     cex = 0.8, bty = "n")
  }

  invisible(list(Y = Y, factors = factors, labels = labels))
}
