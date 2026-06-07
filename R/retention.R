#' Embedding-Adapted Parallel Analysis
#'
#' Determines the number of factors to retain from an embedding similarity
#' matrix using random unit vectors as the null distribution, avoiding the need
#' for a participant-level sample size.
#'
#' @param sim_matrix Numeric similarity matrix (n_items x n_items).
#' @param embeddings Numeric embedding matrix (n_items x embedding_dim).
#' @param n_iter Number of random iterations (default 100).
#' @param percentile Percentile of null eigenvalues to use as threshold
#'   (default 95).
#' @param seed Random seed, used via [withr::with_seed()] without touching the
#'   global RNG state.
#'
#' @returns A list with components:
#' \describe{
#'   \item{n_factors}{Integer: suggested number of factors.}
#'   \item{observed}{Numeric vector: observed eigenvalues (descending).}
#'   \item{percentiles}{Numeric vector: threshold eigenvalues from the null.}
#' }
#'
#' @references
#' Horn, J. L. (1965). A rationale and test for the number of factors in factor
#' analysis. \emph{Psychometrika}, 30(2), 179--185.
#'
#' Yanitski, D. & Westbury, C. (2025). Embedding-adapted parallel analysis for
#' semantic factor analysis.
#'
#' @export
sfa_parallel <- function(sim_matrix, embeddings, n_iter = 100L,
                         percentile = 95, seed = 42L) {
  # accept a fitted sfa object: pull its similarity matrix and embeddings
  if (inherits(sim_matrix, "sfa")) {
    fit <- sim_matrix
    if (missing(embeddings) || is.null(embeddings)) embeddings <- fit$embeddings
    sim_matrix <- fit$sim_matrix
    if (is.null(embeddings)) {
      stop("This 'sfa' object has no embeddings (it was fit from a precomputed ",
           "similarity matrix); parallel analysis needs embeddings.",
           call. = FALSE)
    }
  }
  n_items <- nrow(sim_matrix)
  embed_dim <- ncol(embeddings)

  obs_eigs <- eigen(sim_matrix, symmetric = TRUE, only.values = TRUE)$values
  obs_eigs <- sort(obs_eigs, decreasing = TRUE)

  random_eigs <- withr::with_seed(seed, {
    re <- matrix(NA_real_, nrow = n_iter, ncol = n_items)
    for (i in seq_len(n_iter)) {
      rand <- matrix(stats::rnorm(n_items * embed_dim), nrow = n_items)
      rand <- rand / sqrt(rowSums(rand^2))
      rand_sim <- tcrossprod(rand)
      diag(rand_sim) <- 1.0
      eigs <- eigen(rand_sim, symmetric = TRUE, only.values = TRUE)$values
      re[i, ] <- sort(eigs, decreasing = TRUE)
    }
    re
  })

  pctiles <- apply(random_eigs, 2, stats::quantile, probs = percentile / 100)
  n_factors <- sum(obs_eigs > pctiles)
  n_factors <- max(1L, as.integer(n_factors))

  structure(
    list(
      n_factors = n_factors,
      observed = obs_eigs,
      percentiles = pctiles,
      embed_dim = embed_dim,
      n_iter = n_iter,
      percentile = percentile
    ),
    class = "sfa_parallel"
  )
}

#' @keywords internal
.retention_kaiser <- function(eigenvalues) {
  max(1L, as.integer(sum(eigenvalues > 1)))
}

#' @keywords internal
.retention_tefi <- function(sim_matrix, max_factors, rotate, fm) {
  n_items <- nrow(sim_matrix)
  if (is.null(max_factors)) max_factors <- min(n_items - 1L, 15L)
  max_factors <- min(max_factors, n_items - 1L)

  tefi_vals <- rep(NA_real_, max_factors)
  for (k in seq_len(max_factors)) {
    tryCatch({
      fa_k <- suppressWarnings(psych::fa(sim_matrix, nfactors = k,
                        rotate = rotate, fm = fm,
                        n.obs = NA, warnings = FALSE))
      # real TEFI of the observed matrix under the k-factor item partition
      membership <- .assign_items(unclass(fa_k$loadings))
      tefi_vals[k] <- .compute_tefi(sim_matrix, membership)
    }, error = function(e) NULL)
  }

  valid <- which(!is.na(tefi_vals))
  if (length(valid) == 0) return(1L)
  as.integer(valid[which.min(tefi_vals[valid])])
}

#' @keywords internal
.retention_ega <- function(sim_matrix) {
  if (!requireNamespace("EGAnet", quietly = TRUE)) {
    stop(
      "The 'EGAnet' package is required for EGA-based retention. ",
      "Install with: install.packages('EGAnet')",
      call. = FALSE
    )
  }
  # Response-free similarity matrix: use TMFG (a correlation-matrix filtering
  # method that needs no sample size), with a numeric 'n' placeholder (EGAnet
  # requires numeric n; TMFG does not use it to build the graph).
  ega_result <- suppressWarnings(suppressMessages(
    EGAnet::EGA(data = sim_matrix, n = nrow(sim_matrix), model = "TMFG",
                plot.EGA = FALSE, verbose = FALSE)
  ))
  n_factors <- ega_result$n.dim
  if (is.null(n_factors) || is.na(n_factors)) n_factors <- 1L
  as.integer(n_factors)
}

#' Unified Factor Retention Diagnostics
#'
#' Runs multiple factor retention methods on an embedding similarity matrix and
#' tabulates the results, mirroring the workflow of
#' \code{\link[EFAtools]{N_FACTORS}}.
#'
#' @param sim_matrix Numeric similarity matrix (n_items x n_items).
#' @param embeddings Numeric embedding matrix (n_items x embedding_dim).
#'   Required when \code{"parallel"} is in \code{methods}.
#' @param methods Character vector of retention methods to run. Supported:
#'   \code{"parallel"}, \code{"kaiser"}, \code{"TEFI"}, \code{"EGA"}.
#' @param seed Random seed for parallel analysis.
#' @param parallel_iter Iterations for parallel analysis.
#' @param max_factors Maximum factors to test for TEFI (default: auto).
#' @param rotate Rotation for TEFI extraction (default \code{"oblimin"}).
#' @param fm Extraction method for TEFI (default \code{"minres"}).
#' @param ... Additional arguments (currently unused).
#'
#' @returns An object of class \code{"sfa_nfactors"} with:
#' \describe{
#'   \item{methods}{Data frame with one row per method: method name, suggested
#'     \code{n_factors}.}
#'   \item{consensus}{Integer: modal recommendation across methods.}
#'   \item{eigenvalues}{Numeric vector: observed eigenvalues.}
#'   \item{parallel}{Parallel analysis result (if run), or \code{NULL}.}
#' }
#'
#' @export
sfa_nfactors <- function(sim_matrix, embeddings = NULL,
                         methods = c("parallel", "kaiser", "TEFI"),
                         seed = 42L, parallel_iter = 100L,
                         max_factors = NULL,
                         rotate = "oblimin", fm = "minres", ...) {
  # accept a fitted sfa object: pull its similarity matrix and embeddings
  if (inherits(sim_matrix, "sfa")) {
    fit <- sim_matrix
    if (is.null(embeddings)) embeddings <- fit$embeddings
    sim_matrix <- fit$sim_matrix
  }
  methods <- match.arg(methods, c("parallel", "kaiser", "TEFI", "EGA"),
                       several.ok = TRUE)

  eigs <- eigen(sim_matrix, symmetric = TRUE, only.values = TRUE)$values
  eigs <- sort(eigs, decreasing = TRUE)

  results <- data.frame(method = character(0), n_factors = integer(0),
                        stringsAsFactors = FALSE)
  pa_result <- NULL

  for (m in methods) {
    nf <- tryCatch(switch(m,
      parallel = {
        if (is.null(embeddings)) {
          stop("'embeddings' is required for parallel analysis.", call. = FALSE)
        }
        pa_result <- sfa_parallel(sim_matrix, embeddings,
                                  n_iter = parallel_iter, seed = seed)
        pa_result$n_factors
      },
      kaiser = .retention_kaiser(eigs),
      TEFI = .retention_tefi(sim_matrix, max_factors, rotate, fm),
      EGA = .retention_ega(sim_matrix)
    ), error = function(e) {
      warning("Retention method '", m, "' failed: ", conditionMessage(e),
              call. = FALSE)
      NA_integer_
    })
    results <- rbind(results, data.frame(method = m, n_factors = as.integer(nf),
                                         stringsAsFactors = FALSE))
  }

  valid_nf <- results$n_factors[!is.na(results$n_factors)]
  consensus <- if (length(valid_nf) > 0) {
    tab <- table(valid_nf)
    as.integer(names(tab)[which.max(tab)])
  } else {
    NA_integer_
  }

  structure(
    list(
      methods = results,
      consensus = consensus,
      eigenvalues = eigs,
      parallel = pa_result
    ),
    class = "sfa_nfactors"
  )
}

#' @export
print.sfa_nfactors <- function(x, ...) {
  cat("Factor retention analysis (embedding-adapted)\n\n")
  cat(sprintf("  %-12s %s\n", "Method", "n_factors"))
  for (i in seq_len(nrow(x$methods))) {
    nf <- x$methods$n_factors[i]
    nf_str <- if (is.na(nf)) "failed" else as.character(nf)
    cat(sprintf("  %-12s %s\n", x$methods$method[i], nf_str))
  }
  cat("  ", strrep("-", 24), "\n", sep = "")
  cat(sprintf("  %-12s %s\n", "Consensus",
              if (is.na(x$consensus)) "N/A" else as.character(x$consensus)))

  eig_str <- paste(format(head(x$eigenvalues, 10), digits = 2), collapse = "  ")
  if (length(x$eigenvalues) > 10) eig_str <- paste0(eig_str, "  ...")
  cat("\nEigenvalues:", eig_str, "\n")
  invisible(x)
}

#' @export
print.sfa_parallel <- function(x, ...) {
  cat("Embedding-adapted parallel analysis\n")
  cat("  Suggested factors:", x$n_factors, "\n")
  cat("  Embedding dimension:", x$embed_dim, "\n")
  cat("  Iterations:", x$n_iter, "\n")
  cat("  Percentile:", x$percentile, "\n")
  invisible(x)
}
