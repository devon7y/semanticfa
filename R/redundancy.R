# =============================================================================
# Method reference. Detecting locally dependent (redundant) items via weighted
# topological overlap implements Unique Variable Analysis:
#
#   Christensen, A. P., Garrido, L. E., & Golino, H. (2023). Unique Variable
#     Analysis: A network psychometrics method to detect local dependence.
#     Multivariate Behavioral Research, 58(6), 1165-1182.
#     https://doi.org/10.1080/00273171.2023.2194606
# =============================================================================

#' Detect Redundant (Near-Duplicate) Items
#'
#' Finds pairs of items that are so semantically similar they are effectively
#' duplicates --- they add length without adding information. This is distinct
#' from \code{\link{sfa_simplify}}, which removes \emph{weak} items (far from
#' their construct); redundancy targets \emph{near-twin} items (very close to
#' \emph{each other}).
#'
#' @param x An \code{"sfa"} object (uses its similarity matrix) or a symmetric
#'   numeric item-by-item similarity matrix.
#' @param threshold Redundancy cutoff. Item pairs with overlap at or above this
#'   value are flagged. Defaults to 0.25 for \code{"wto"} (the Unique Variable
#'   Analysis cut-off) and 0.80 for \code{"cosine"}.
#' @param method Overlap measure:
#'   \describe{
#'     \item{\code{"wto"}}{(default) Unique Variable Analysis (Christensen et al.
#'       2023): absolute weighted topological overlap on an \strong{EBICglasso}
#'       network, the paper's estimator. Requires the \pkg{EGAnet} package.
#'       Because an embedding similarity matrix has no response sample, the
#'       network is estimated with a nominal sample size large enough to keep
#'       the EBIC model selection in its stable regime (EBIC over-shrinks to an
#'       empty graph when the sample size equals the item count). Estimating a
#'       sparse network first is what gives wTO its discriminating power;
#'       computing it on the dense matrix compresses every pair into a narrow
#'       band.}
#'     \item{\code{"cosine"}}{Direct pairwise similarity. Dependency-free and
#'       well spread for dense embedding matrices.}
#'   }
#'
#' @returns An object of class \code{"sfa_redundancy"}: a list with the flagged
#'   \code{pairs} (data frame: item_i, item_j, overlap), redundant \code{clusters}
#'   (connected groups of mutually redundant items), and \code{suggest_remove}
#'   (all-but-one item per cluster --- keep one representative).
#'
#' @references
#' Christensen, A. P., Garrido, L. E., & Golino, H. (2023). Unique Variable
#' Analysis: A network psychometrics method to detect local dependence.
#' \emph{Multivariate Behavioral Research}, 58(6), 1165--1182.
#' \doi{10.1080/00273171.2023.2194606}
#'
#' @seealso \code{\link{sfa_simplify}}
#' @examples
#' data(big5)
#' fit <- sfa(
#'   data.frame(code = big5$codes, item = big5$items,
#'              factor = big5$factors, scoring = big5$scoring),
#'   embeddings = big5$embeddings, scoring = big5$scoring, nfactors = 5)
#'
#' # flag near-duplicate item pairs
#' sfa_redundancy(fit, threshold = 0.8, method = "cosine")
#' @export
sfa_redundancy <- function(x, threshold = NULL, method = c("wto", "cosine")) {
  method <- match.arg(method)
  sim <- if (inherits(x, "sfa")) x$sim_matrix else as.matrix(x)
  if (is.null(sim) || nrow(sim) != ncol(sim)) {
    stop("Need an 'sfa' object or a square similarity matrix.", call. = FALSE)
  }
  codes <- rownames(sim)
  if (is.null(codes)) codes <- sprintf("item_%02d", seq_len(nrow(sim)))
  n <- nrow(sim)
  if (is.null(threshold)) threshold <- if (method == "wto") 0.25 else 0.80

  overlap <- if (method == "wto") .uva_wto(sim) else abs(sim)
  dimnames(overlap) <- list(codes, codes)

  # flag pairs at/above threshold
  ut <- which(upper.tri(overlap) & overlap >= threshold, arr.ind = TRUE)
  if (nrow(ut) == 0) {
    pairs <- data.frame(item_i = character(0), item_j = character(0),
                        overlap = numeric(0), stringsAsFactors = FALSE)
  } else {
    pairs <- data.frame(
      item_i = codes[ut[, 1]], item_j = codes[ut[, 2]],
      overlap = round(overlap[ut], 3), stringsAsFactors = FALSE
    )
    pairs <- pairs[order(-pairs$overlap), ]
  }

  # connected components of the redundancy graph -> clusters
  clusters <- .redundant_clusters(overlap >= threshold, codes)

  # keep the most "central" item per cluster (highest mean similarity to the
  # rest of the data), mark the others for removal
  suggest_remove <- character(0)
  if (length(clusters)) {
    centrality <- rowMeans(abs(sim))
    names(centrality) <- codes
    for (cl in clusters) {
      keep <- cl[which.max(centrality[cl])]
      suggest_remove <- c(suggest_remove, setdiff(cl, keep))
    }
  }

  structure(list(
    pairs = pairs, clusters = clusters, suggest_remove = suggest_remove,
    threshold = threshold, method = method
  ), class = "sfa_redundancy")
}

#' @keywords internal
# Faithful Unique Variable Analysis (Christensen et al. 2023): absolute weighted
# topological overlap on an EBICglasso network -- the paper's estimator, and the
# one EGAnet::UVA uses (abs(wto(network))). An embedding similarity matrix has no
# response sample, so the network is estimated with a nominal sample size large
# enough to keep EBIC selection in its stable regime: EBIC over-shrinks the
# glasso to an empty graph when n equals the item count, but the selected model
# is stable for n well above that, so the result is effectively n-independent.
.uva_wto <- function(sim) {
  if (!requireNamespace("EGAnet", quietly = TRUE)) {
    stop("method = \"wto\" follows Unique Variable Analysis (Christensen et ",
         "al., 2023), which estimates a network first; that step needs the ",
         "'EGAnet' package. Install EGAnet, or use method = \"cosine\".",
         call. = FALSE)
  }
  n_nom <- max(1000L, 20L * nrow(sim))    # nominal sample size (stable EBIC regime)
  net <- suppressWarnings(suppressMessages(
    EGAnet::network.estimation(data = sim, n = n_nom, model = "glasso",
                               verbose = FALSE)))
  w <- abs(as.matrix(suppressWarnings(suppressMessages(EGAnet::wto(net)))))
  w <- (w + t(w)) / 2
  diag(w) <- 0
  w
}

#' @keywords internal
.redundant_clusters <- function(adj, codes) {
  diag(adj) <- FALSE
  n <- nrow(adj)
  seen <- logical(n)
  clusters <- list()
  for (i in seq_len(n)) {
    if (seen[i] || !any(adj[i, ])) next
    comp <- i
    frontier <- i
    seen[i] <- TRUE
    while (length(frontier)) {
      nb <- which(apply(adj[frontier, , drop = FALSE], 2, any) & !seen)
      if (!length(nb)) break
      seen[nb] <- TRUE
      comp <- c(comp, nb)
      frontier <- nb
    }
    if (length(comp) > 1) clusters[[length(clusters) + 1]] <- codes[sort(comp)]
  }
  clusters
}

#' @export
print.sfa_redundancy <- function(x, ...) {
  cat("Redundant-item detection\n")
  cat("  Method: Christensen et al. (2023)\n")
  cat(sprintf("  Measure: %s | threshold: %.2f\n", x$method, x$threshold))
  cat(sprintf("  Redundant pairs: %d | redundant clusters: %d\n",
              nrow(x$pairs), length(x$clusters)))
  if (nrow(x$pairs)) {
    cat("\n  Top redundant pairs:\n")
    show <- utils::head(x$pairs, 10)
    for (i in seq_len(nrow(show))) {
      cat(sprintf("    %-10s ~ %-10s  overlap=%.3f\n",
                  show$item_i[i], show$item_j[i], show$overlap[i]))
    }
  }
  if (length(x$suggest_remove)) {
    cat(sprintf("\n  Suggested removals (keep one per cluster): %s\n",
                paste(x$suggest_remove, collapse = ", ")))
  }
  invisible(x)
}
