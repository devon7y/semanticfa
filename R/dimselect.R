# =============================================================================
# Method reference. Selecting embedding-dimension "depth" by traversing
# coordinates and optimizing a composite of NMI and TEFI implements:
#
#   Golino, H. (2026). Optimizing the landscape of LLM embeddings with Dynamic
#     Exploratory Graph Analysis for generative psychometrics: A Monte Carlo
#     study [Manuscript under review, Proceedings of the 90th Annual
#     International Meeting of the Psychometric Society]. arXiv:2601.17010.
#
# TMFG network estimation, Walktrap community detection, and TEFI are provided
# by the EGAnet package (Christensen, Golino, & colleagues).
# =============================================================================

#' Embedding-Dimension Selection via Dynamic EGA (DynEGA)
#'
#' Selects how many leading embedding coordinates ("depth") to use before
#' factor analysis, instead of defaulting to the full vector. Following Golino
#' (2026), the embedding is treated as a searchable landscape: structural
#' information is not uniformly distributed across coordinates, so a sub-range
#' of dimensions can recover the construct structure more cleanly than the whole
#' vector (and denoise the over-factoring seen with some embedding models).
#'
#' The coordinate index is treated as a pseudo-time axis. The function sweeps
#' increasing depths \eqn{d}; at each depth it builds the item-by-item
#' association matrix from the first \eqn{d} coordinates, estimates the network
#' with the Triangulated Maximally Filtered Graph (TMFG) and detects communities
#' with the Walktrap algorithm (both via \pkg{EGAnet}, as in Golino 2026), then
#' scores the resulting partition with:
#' \itemize{
#'   \item the Total Entropy Fit Index (\strong{TEFI}; lower is better), and
#'   \item Normalized Mutual Information (\strong{NMI}) against the theoretical
#'     factor labels, when available (higher is better).
#' }
#' Both metrics are min-max normalized across the swept depths and combined into
#' a composite \eqn{C(d) = w_{NMI}\,NMI_{norm} - w_{TEFI}\,TEFI_{norm}}
#' (default weights 0.70 / 0.30, per Golino 2026). The depth maximizing
#' \eqn{C} is returned. With no theoretical labels the selection falls back to
#' minimizing TEFI alone (less reliable; a single metric can yield structurally
#' incoherent optima).
#'
#' @section Selection engine vs. analysis engine:
#' Depth is scored with the EGA network / Walktrap partition (Golino's engine).
#' When the chosen depth then feeds \code{\link[psych]{fa}}-based extraction
#' (the default in \code{\link{sfa}}), the subspace that is best for EGA
#' recovery is not guaranteed to be best for the EFA solution. For results that
#' match the selection criterion, pair \code{dim_select = "dynega"} with
#' \code{n_factors_method = "EGA"}. Golino (2026) also reports the largest gains
#' for richer item pools (more than ~15 items per dimension); short scales may
#' see little or no benefit.
#'
#' @param embeddings Numeric matrix (n_items x embedding_dim).
#' @param factors Optional character/factor vector of theoretical labels, one
#'   per item, enabling the NMI term. If \code{NULL}, TEFI-only selection.
#' @param scoring Optional numeric +1/-1 vector (keying), passed to the
#'   similarity transform.
#' @param encoding Similarity transform used at each depth (default
#'   \code{"atomic_reversed"}). See \code{\link{sfa_similarity}}.
#' @param min_depth Smallest depth to evaluate (default 3, with a minimum of 3
#'   imposed for TMFG stability).
#' @param max_depth Largest depth to evaluate (default: full embedding
#'   dimension).
#' @param step Depth increment. Default chooses a step giving at most
#'   \code{max_eval} evaluations (Golino used 5).
#' @param max_eval Soft cap on the number of depths evaluated when \code{step}
#'   is left at its default (default 150).
#' @param weights Named numeric vector \code{c(nmi=, tefi=)} for the composite
#'   (default \code{c(nmi = 0.70, tefi = 0.30)}).
#' @param algorithm Community-detection algorithm passed to \pkg{EGAnet}
#'   (default \code{"walktrap"}).
#'
#' @returns An object of class \code{"sfa_dimselect"}: a list with
#'   \code{optimal_depth}, the full \code{trajectory} data frame (depth, n_dim,
#'   nmi, tefi, and normalized/composite columns), the \code{weights} used, and
#'   \code{full_dim}.
#'
#' @references
#' Golino, H. (2026). Optimizing the landscape of LLM embeddings with Dynamic
#' Exploratory Graph Analysis for generative psychometrics: A Monte Carlo study
#' Manuscript under review. \emph{Proceedings of the 90th Annual International
#' Meeting of the Psychometric Society}. arXiv:2601.17010.
#'
#' @seealso \code{\link{sfa}} (use \code{dim_select = "dynega"}),
#'   \code{\link{sfa_similarity}}
#' @examples
#' data(big5)
#' \donttest{
#' if (requireNamespace("EGAnet", quietly = TRUE)) {
#'   # small depth grid for a quick illustration
#'   ds <- sfa_dimselect(big5$embeddings, factors = big5$factors,
#'                       scoring = big5$scoring, max_depth = 80, step = 20)
#'   ds$optimal_depth
#' }
#' }
#' @export
sfa_dimselect <- function(embeddings, factors = NULL, scoring = NULL,
                          encoding = "atomic_reversed",
                          min_depth = 3L, max_depth = NULL, step = NULL,
                          max_eval = 150L,
                          weights = c(nmi = 0.70, tefi = 0.30),
                          algorithm = "walktrap") {
  if (!requireNamespace("EGAnet", quietly = TRUE)) {
    stop("sfa_dimselect() requires the 'EGAnet' package (TMFG + Walktrap + ",
         "TEFI). Install with: install.packages('EGAnet')", call. = FALSE)
  }
  embeddings <- as.matrix(embeddings)
  storage.mode(embeddings) <- "double"
  D <- ncol(embeddings)
  n_items <- nrow(embeddings)

  min_depth <- max(3L, as.integer(min_depth))
  if (is.null(max_depth)) max_depth <- D
  max_depth <- min(as.integer(max_depth), D)
  if (max_depth < min_depth) {
    stop("max_depth (", max_depth, ") is below min_depth (", min_depth, ").",
         call. = FALSE)
  }
  if (is.null(step)) {
    span <- max_depth - min_depth
    step <- max(1L, as.integer(ceiling((span + 1) / max_eval)))
  }
  step <- max(1L, as.integer(step))
  depths <- seq.int(min_depth, max_depth, by = step)
  if (depths[length(depths)] != max_depth) depths <- c(depths, max_depth)

  use_nmi <- !is.null(factors)
  if (use_nmi) factors <- as.character(factors)

  nmi <- rep(NA_real_, length(depths))
  tefi <- rep(NA_real_, length(depths))
  ndim <- rep(NA_integer_, length(depths))

  for (i in seq_along(depths)) {
    d <- depths[i]
    res <- tryCatch(
      .dynega_eval(embeddings[, seq_len(d), drop = FALSE], encoding, scoring,
                   factors, use_nmi, n_items, algorithm),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      nmi[i] <- res$nmi
      tefi[i] <- res$tefi
      ndim[i] <- res$n_dim
    }
  }

  traj <- data.frame(depth = depths, n_dim = ndim, nmi = nmi, tefi = tefi)

  # composite over depths with valid scores
  w_nmi <- unname(weights[["nmi"]])
  w_tefi <- unname(weights[["tefi"]])
  nmi_n <- .minmax(nmi)
  tefi_n <- .minmax(tefi)
  if (use_nmi && any(is.finite(nmi))) {
    composite <- w_nmi * nmi_n - w_tefi * tefi_n
  } else {
    composite <- -tefi_n                       # TEFI-only fallback
    if (!use_nmi) {
      warning("No theoretical 'factors' supplied; selecting depth by TEFI ",
              "alone. A single metric can yield structurally incoherent ",
              "optima (Golino 2026).", call. = FALSE)
    }
  }
  traj$nmi_norm <- nmi_n
  traj$tefi_norm <- tefi_n
  traj$composite <- composite

  if (!any(is.finite(composite))) {
    stop("DynEGA failed at every depth (network estimation did not converge).",
         call. = FALSE)
  }
  opt <- depths[which.max(replace(composite, !is.finite(composite), -Inf))]

  structure(list(
    optimal_depth = as.integer(opt),
    trajectory = traj,
    weights = c(nmi = w_nmi, tefi = w_tefi),
    used_nmi = use_nmi && any(is.finite(nmi)),
    full_dim = D
  ), class = "sfa_dimselect")
}

#' @keywords internal
.dynega_eval <- function(sub, encoding, scoring, factors, use_nmi, n_items,
                         algorithm) {
  sim <- sfa_similarity(sub, encoding = encoding, scoring = scoring)
  attr(sim, "transformed_embeddings") <- NULL
  sim <- matrix(as.numeric(sim), n_items, n_items)
  sim <- .check_psd(sim)

  ega <- suppressWarnings(suppressMessages(
    EGAnet::EGA(data = sim, n = n_items, model = "TMFG",
                algorithm = algorithm, plot.EGA = FALSE, verbose = FALSE)
  ))
  membership <- ega$wc
  n_dim <- ega$n.dim
  if (is.null(membership) || all(is.na(membership))) return(NULL)

  te <- suppressWarnings(suppressMessages(
    EGAnet::tefi(sim, structure = membership)))
  tefi_val <- .extract_tefi(te)

  nmi_val <- NA_real_
  if (use_nmi) {
    keep <- !is.na(membership)
    nmi_val <- .compute_nmi(membership[keep], factors[keep])
  }
  list(nmi = nmi_val, tefi = tefi_val,
       n_dim = if (is.null(n_dim) || is.na(n_dim)) NA_integer_
               else as.integer(n_dim))
}

#' @keywords internal
.extract_tefi <- function(te) {
  if (is.numeric(te) && length(te) == 1L) return(as.numeric(te))
  if (is.data.frame(te) && "VN.Entropy.Fit" %in% names(te)) {
    return(as.numeric(te[["VN.Entropy.Fit"]][1]))
  }
  if (is.list(te) && !is.null(te[["VN.Entropy.Fit"]])) {
    return(as.numeric(te[["VN.Entropy.Fit"]][1]))
  }
  suppressWarnings(as.numeric(unlist(te))[1])
}

#' @keywords internal
.minmax <- function(v) {
  finite <- v[is.finite(v)]
  if (length(finite) == 0) return(rep(NA_real_, length(v)))
  rng <- range(finite)
  if (diff(rng) == 0) {
    out <- rep(0.5, length(v))
    out[!is.finite(v)] <- NA_real_
    return(out)
  }
  (v - rng[1]) / (rng[2] - rng[1])
}

#' @export
print.sfa_dimselect <- function(x, ...) {
  cat("DynEGA embedding-dimension selection\n")
  cat("  Method: Golino (2026)\n")
  cat(sprintf("  Full embedding dim: %d\n", x$full_dim))
  cat(sprintf("  Depths evaluated:   %d (range %d-%d)\n",
              nrow(x$trajectory), min(x$trajectory$depth),
              max(x$trajectory$depth)))
  cat(sprintf("  Criterion:          %s\n",
              if (x$used_nmi) sprintf("%.2f*NMI - %.2f*TEFI (normalized)",
                                      x$weights[["nmi"]], x$weights[["tefi"]])
              else "TEFI only (no theoretical labels)"))
  cat(sprintf("  Optimal depth:      %d of %d coordinates\n",
              x$optimal_depth, x$full_dim))
  opt_row <- x$trajectory[x$trajectory$depth == x$optimal_depth, ]
  if (nrow(opt_row)) {
    cat(sprintf("    at optimum: n_dim=%s  NMI=%.3f  TEFI=%.3f\n",
                opt_row$n_dim[1], opt_row$nmi[1], opt_row$tefi[1]))
  }
  invisible(x)
}
