#' 2-D Item Map (t-SNE, UMAP, PCA, or MDS)
#'
#' A 2-D scatter of the scale's items, the embedding-space companion to
#' \code{\link{sfa_corplot}}: each point is an item, points are coloured by their
#' theoretical factor and labelled with their short code, so you can see at a
#' glance which items cluster together, which sit between constructs, and which
#' are outliers. Operates on the same (transformed) item embeddings the factor
#' analysis uses, or on a similarity matrix (converted to a distance).
#'
#' The projection method is selectable via \code{method};
#' \code{method = "tsne"} reproduces the original behaviour. \code{sfa_tsneplot()}
#' is a deprecated alias kept for back-compatibility.
#'
#' @param x An \code{"sfa"} object (uses its item embeddings) or a symmetric
#'   numeric item-by-item similarity matrix.
#' @param method Projection: \code{"tsne"} (default, needs \pkg{Rtsne}),
#'   \code{"umap"} (needs \pkg{uwot}), \code{"pca"} (base R), or \code{"mds"}
#'   (classical multidimensional scaling, base R). PCA and MDS need no extra
#'   package; t-SNE and UMAP are better at showing local clusters but are only
#'   sensible above a handful of items.
#' @param factors,labels Optional per-item factor labels and point labels
#'   (codes). Default to those carried on \code{x} (or the matrix's
#'   \code{"factors"}/\code{"codes"} attributes).
#' @param color Logical; colour points by factor (default \code{TRUE}).
#' @param perplexity t-SNE perplexity (\code{method = "tsne"}). If \code{NULL},
#'   a safe value is chosen for the item count
#'   (\code{max(1, min(30, floor((n - 1) / 3)))}).
#' @param n_neighbors UMAP neighbourhood size (\code{method = "umap"}). If
#'   \code{NULL}, \code{min(15, n - 1)}.
#' @param seed Random seed for reproducibility (t-SNE and UMAP are stochastic).
#' @param pch,cex Point symbol and size.
#' @param legend Logical; draw a factor legend (default \code{TRUE}).
#' @param ... Passed to \code{\link[graphics]{plot}}.
#'
#' @returns Invisibly, a list with the 2-D coordinates \code{Y}, the
#'   \code{factors}, the \code{labels}, and the \code{method} used.
#'
#' @references
#' van der Maaten, L., & Hinton, G. (2008). Visualizing data using t-SNE.
#' \emph{Journal of Machine Learning Research}, 9, 2579--2605.
#'
#' McInnes, L., Healy, J., & Melville, J. (2018). UMAP: Uniform Manifold
#' Approximation and Projection for dimension reduction. arXiv:1802.03426.
#'
#' @seealso \code{\link{sfa_corplot}}
#' @examples
#' \dontrun{
#' data(big5)
#' fit <- sfa(
#'   data.frame(code = big5$codes, item = big5$items,
#'              factor = big5$factors, scoring = big5$scoring),
#'   embeddings = big5$embeddings, scoring = big5$scoring, nfactors = 5)
#' sfa_itemplot(fit)                    # t-SNE (default)
#' sfa_itemplot(fit, method = "umap")   # UMAP
#' sfa_itemplot(fit, method = "pca")    # PCA, no extra package
#' }
#' @export
sfa_itemplot <- function(x, method = c("tsne", "umap", "pca", "mds"),
                         factors = NULL, labels = NULL, color = TRUE,
                         perplexity = NULL, n_neighbors = NULL,
                         seed = 42, pch = 19, cex = 0.9, legend = TRUE, ...) {
  method <- match.arg(method)

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
    if (is.null(labels))  labels  <- rownames(sim)   # fall back like sfa_corplot
    n <- nrow(sim)
  }
  if (n < 3L) {
    stop("Need at least 3 items (got ", n, ").", call. = FALSE)
  }
  if (method %in% c("tsne", "umap") && n < 5L) {
    stop(toupper(method), " needs at least 5 items (got ", n,
         "); try method = \"pca\" or \"mds\".", call. = FALSE)
  }
  if (!is.null(labels) && length(labels) != n) {
    stop("'labels' length (", length(labels), ") does not match the number of ",
         "items (", n, ").", call. = FALSE)
  }
  if (!is.null(factors) && length(factors) != n) {
    stop("'factors' length (", length(factors), ") does not match the number of ",
         "items (", n, ").", call. = FALSE)
  }

  Y <- .project_2d(vecs, sim, method, perplexity, n_neighbors, n, seed)
  if (!is.null(labels)) rownames(Y) <- labels

  lab <- switch(method,
    tsne = c("t-SNE 1", "t-SNE 2", "t-SNE of item embeddings"),
    umap = c("UMAP 1",  "UMAP 2",  "UMAP of item embeddings"),
    pca  = c("PC 1",    "PC 2",    "PCA of item embeddings"),
    mds  = c("MDS 1",   "MDS 2",   "Classical MDS of item embeddings"))

  f <- NULL
  cols <- "black"
  pal <- NULL
  if (isTRUE(color) && !is.null(factors)) {
    f <- factor(factors)
    pal <- grDevices::hcl.colors(max(nlevels(f), 2L), "Dark 3")[seq_len(nlevels(f))]
    cols <- pal[as.integer(f)]
  }

  graphics::plot(Y, col = cols, pch = pch, cex = cex,
                 xlab = lab[1], ylab = lab[2], main = lab[3], ...)
  if (!is.null(labels)) {
    graphics::text(Y, labels = labels, pos = 3, cex = cex * 0.7, col = cols)
  }
  if (isTRUE(legend) && !is.null(f)) {
    graphics::legend("topright", legend = levels(f), col = pal, pch = pch,
                     cex = 0.8, bty = "n")
  }

  invisible(list(Y = Y, factors = factors, labels = labels, method = method))
}

#' @rdname sfa_itemplot
#' @export
sfa_tsneplot <- function(x, method = c("tsne", "umap", "pca", "mds"), ...) {
  .Deprecated("sfa_itemplot")
  sfa_itemplot(x, method = method, ...)
}

#' @keywords internal
# Produce 2-D coordinates from item vectors (preferred) or a similarity matrix,
# by the chosen method. t-SNE/UMAP gate on their Suggested packages; PCA/MDS are
# base R. A similarity matrix is turned into a distance (1 - sim) where needed.
.project_2d <- function(vecs, sim, method, perplexity, n_neighbors, n, seed) {
  withr::with_seed(seed, {
    if (method == "tsne") {
      if (!requireNamespace("Rtsne", quietly = TRUE)) {
        stop("method = \"tsne\" needs the 'Rtsne' package. ",
             "Install it, or use method = \"pca\" / \"mds\".", call. = FALSE)
      }
      if (is.null(perplexity)) perplexity <- max(1, min(30, floor((n - 1) / 3)))
      ts <- if (!is.null(vecs)) {
        Rtsne::Rtsne(as.matrix(vecs), perplexity = perplexity, pca = TRUE,
                     check_duplicates = FALSE)
      } else {
        Rtsne::Rtsne(stats::as.dist(1 - sim), is_distance = TRUE,
                     perplexity = perplexity, check_duplicates = FALSE)
      }
      Y <- ts$Y
    } else if (method == "umap") {
      if (!requireNamespace("uwot", quietly = TRUE)) {
        stop("method = \"umap\" needs the 'uwot' package. ",
             "Install it, or use method = \"pca\" / \"mds\".", call. = FALSE)
      }
      if (is.null(n_neighbors)) n_neighbors <- min(15L, n - 1L)
      input <- if (!is.null(vecs)) as.matrix(vecs) else stats::as.dist(1 - sim)
      Y <- as.matrix(uwot::umap(input, n_neighbors = n_neighbors,
                                n_components = 2, verbose = FALSE))
    } else if (method == "pca") {
      M <- if (!is.null(vecs)) as.matrix(vecs) else as.matrix(sim)
      Y <- stats::prcomp(M, center = TRUE, scale. = FALSE)$x   # full scores; padded below
    } else {  # mds
      d <- if (!is.null(vecs)) stats::dist(as.matrix(vecs)) else stats::as.dist(1 - sim)
      Y <- stats::cmdscale(d, k = 2)
    }
    # degenerate-input safeguard: pad to (at least) 2 columns before slicing,
    # covering 1-D embeddings (prcomp) and 0-eigenvalue cmdscale alike
    Y <- as.matrix(Y)
    while (ncol(Y) < 2L) Y <- cbind(Y, 0)
    Y[, 1:2, drop = FALSE]
  })
}
