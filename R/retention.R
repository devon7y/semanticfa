#' Embedding-Adapted Parallel Analysis
#'
#' Determines the number of factors to retain from an embedding similarity
#' matrix using random unit vectors as the null distribution, avoiding the need
#' for a participant-level sample size.
#'
#' The adaptation keeps Horn's (1965) logic --- retain leading eigenvalues that
#' exceed those of structureless data of the same size --- but replaces the
#' respondent-level null with similarity matrices of random Gaussian unit
#' vectors in the item count and embedding dimension of the data. Retention
#' follows Horn's sequential rule: leading eigenvalues are counted until the
#' first falls at or below its null percentile (against the 95th null
#' percentile by default, a common modern choice; Horn compared against the
#' null mean). Two caveats follow from the
#' null. First, random unit vectors in a high-dimensional space are nearly
#' orthogonal, so the null similarity matrix is near-identity and the
#' eigenvalue thresholds concentrate just above one. Second, the null carries
#' none of the general positive similarity component that real item embeddings
#' share, so it is a structureless baseline, not a matched one. Benchmarking
#' on embedding similarity matrices, Garrido et al. (2025) found conventional
#' parallel analysis systematically overextracted; corroborate retention with
#' the other criteria in [sfa_nfactors()].
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
#' Garrido, L. E., Russell-Lasalandra, L. L., & Golino, H. (2025). Estimating
#' dimensional structure in generative psychometrics: Comparing PCA and network
#' methods using large language model item embeddings. PsyArXiv preprint.
#' \doi{10.31234/osf.io/2s7pw_v1}
#'
#' @export
sfa_parallel <- function(sim_matrix, embeddings, n_iter = 100L,
                         percentile = 95, seed = 42L) {
  # accept a fitted sfa object: pull its similarity matrix and embeddings
  if (inherits(sim_matrix, "sfa")) {
    fit <- sim_matrix
    if (missing(embeddings) || is.null(embeddings)) {
      embeddings <- fit$transformed_embeddings
    }
    sim_matrix <- fit$sim_matrix
    if (is.null(embeddings)) {
      stop("This 'sfa' object has no embeddings (it was fit from a precomputed ",
           "similarity matrix); parallel analysis needs embeddings.",
           call. = FALSE)
    }
  }
  n_iter <- .assert_count(n_iter, "n_iter")
  if (!is.numeric(percentile) || length(percentile) != 1L ||
      !is.finite(percentile) || percentile <= 0 || percentile >= 100) {
    stop("'percentile' must be a single number in (0, 100).", call. = FALSE)
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
  # Horn's sequential rule: count leading eigenvalues until the first falls at
  # or below its null percentile (not all eigenvalues anywhere above theirs).
  below <- which(obs_eigs <= pctiles)
  n_factors <- if (length(below) == 0L) length(obs_eigs) else below[1L] - 1L
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

#' Empirical Kaiser Criterion for Embedding Similarity Matrices
#'
#' Applies the empirical Kaiser criterion (EKC; Braeken & van Assen, 2017) to
#' an embedding similarity matrix, with the embedding dimension playing the
#' role of the sample size.
#'
#' The EKC replaces Kaiser's fixed threshold of one with a series of reference
#' eigenvalues. The first reference is the asymptotic maximum sample
#' eigenvalue of a null-model correlation matrix, \eqn{(1 + \sqrt{\gamma})^2}
#' with \eqn{\gamma} the variables-to-sample-size ratio (Marchenko-Pastur
#' upper edge). Each subsequent reference applies Braeken and van Assen's
#' proportional correction for the variance already absorbed by preceding
#' observed eigenvalues, floored at one. Retention follows the same sequential
#' first-crossing rule as [sfa_parallel()]: leading eigenvalues are counted
#' until the first falls at or below its reference. The serial correction
#' addresses the classical parallel-analysis weakness that reference values
#' ignore variance captured by real factors.
#'
#' Adaptation note: with similarity matrices computed across embedding
#' dimensions (see [sfa_similarity()]), the sample size is the embedding
#' dimension, so \eqn{\gamma} is items over dimensions. Embedding dimensions
#' are coordinates rather than sampled respondents, so the Marchenko-Pastur
#' bound is a heuristic reference here, not a sampling-theoretic one; treat
#' the result as one voice among the criteria in [sfa_nfactors()].
#'
#' @param sim_matrix Numeric similarity matrix (n_items x n_items), or a
#'   fitted \code{"sfa"} object.
#' @param embeddings Numeric embedding matrix (n_items x embedding_dim), used
#'   only for its column count. Optional if \code{n} is given.
#' @param n Sample size to use in place of \code{ncol(embeddings)}: the
#'   embedding dimension for similarity matrices, or the respondent count when
#'   applying the criterion to a conventional correlation matrix.
#'
#' @returns A list of class \code{"sfa_ekc"} with components:
#' \describe{
#'   \item{n_factors}{Integer: suggested number of factors.}
#'   \item{observed}{Numeric vector: observed eigenvalues (descending).}
#'   \item{references}{Numeric vector: EKC reference eigenvalues.}
#'   \item{n}{The sample size used.}
#' }
#'
#' @references
#' Braeken, J., & van Assen, M. A. L. M. (2017). An empirical Kaiser
#' criterion. \emph{Psychological Methods}, 22(3), 450--466.
#' \doi{10.1037/met0000074}
#'
#' @examples
#' data(big5)
#' sim <- sfa_similarity(big5$embeddings, "mean_centered_pearson")
#' sfa_ekc(sim, big5$embeddings)
#'
#' @export
sfa_ekc <- function(sim_matrix, embeddings = NULL, n = NULL) {
  if (inherits(sim_matrix, "sfa")) {
    fit <- sim_matrix
    if (is.null(embeddings)) embeddings <- fit$transformed_embeddings
    sim_matrix <- fit$sim_matrix
  }
  if (is.null(n)) {
    if (is.null(embeddings)) {
      stop("Supply 'embeddings' (for its dimension) or 'n'.", call. = FALSE)
    }
    n <- ncol(embeddings)
  }
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 2) {
    stop("'n' must be a single number >= 2.", call. = FALSE)
  }
  J <- nrow(sim_matrix)
  l <- sort(eigen(sim_matrix, symmetric = TRUE, only.values = TRUE)$values,
            decreasing = TRUE)
  up <- (1 + sqrt(J / n))^2
  # Braeken & van Assen (2017): reference_j = max(remaining-average-variance
  # correction times the null upper edge, 1), with l_0 = 0
  prior <- c(0, cumsum(l)[-J])
  refs <- pmax(((J - prior) / (J - seq_len(J) + 1)) * up, 1)
  below <- which(l <= refs)
  n_factors <- if (length(below) == 0L) J else below[1L] - 1L

  structure(
    list(
      n_factors = max(1L, as.integer(n_factors)),
      observed = l,
      references = refs,
      n = n
    ),
    class = "sfa_ekc"
  )
}

#' @export
print.sfa_ekc <- function(x, ...) {
  cat("Empirical Kaiser criterion (embedding-adapted)\n")
  cat("  Suggested factors:", x$n_factors, "\n")
  cat("  Sample size used:", x$n, "\n")
  cat("  First reference:", format(x$references[1], digits = 4), "\n")
  invisible(x)
}

#' Velicer's Minimum Average Partial for Similarity Matrices
#'
#' Applies Velicer's (1976) minimum average partial (MAP) test to an embedding
#' similarity matrix. MAP extracts principal components one at a time and
#' tracks the average squared partial correlation among the residuals; the
#' component count at the minimum is the suggested dimensionality.
#'
#' MAP needs no sample size and no null model, so it transfers to similarity
#' matrices without adaptation. Interpret it with care in this setting: on
#' embedding similarity matrices MAP tends to track all reliably estimated
#' structure, including minor components well beyond the interpretable factor
#' count, which is why it is available in [sfa_nfactors()] but not part of the
#' default method set.
#'
#' @param sim_matrix Numeric similarity matrix (n_items x n_items), or a
#'   fitted \code{"sfa"} object.
#' @param max_factors Largest component count to evaluate (default: number of
#'   items minus two). The scan also stops early if a residual variance
#'   becomes non-positive.
#'
#' @returns A list of class \code{"sfa_map"} with components:
#' \describe{
#'   \item{n_factors}{Integer: component count at the minimum average squared
#'     partial correlation (floored at one).}
#'   \item{map}{Numeric vector: the MAP criterion at each evaluated count.}
#'   \item{map0}{Baseline average squared off-diagonal correlation with no
#'     components removed.}
#' }
#'
#' @references
#' Velicer, W. F. (1976). Determining the number of components from the matrix
#' of partial correlations. \emph{Psychometrika}, 41(3), 321--327.
#' \doi{10.1007/BF02293557}
#'
#' @examples
#' data(big5)
#' sim <- sfa_similarity(big5$embeddings, "mean_centered_pearson")
#' sfa_map(sim)
#'
#' @export
sfa_map <- function(sim_matrix, max_factors = NULL) {
  if (inherits(sim_matrix, "sfa")) sim_matrix <- sim_matrix$sim_matrix
  J <- nrow(sim_matrix)
  if (is.null(max_factors)) max_factors <- J - 2L
  max_factors <- min(.assert_count(max_factors, "max_factors"), J - 2L)

  e <- eigen(sim_matrix, symmetric = TRUE)
  off <- function(M) (sum(M^2) - sum(diag(M)^2)) / (J * (J - 1))
  map0 <- off(sim_matrix / tcrossprod(sqrt(diag(sim_matrix))))

  map_vals <- rep(NA_real_, max_factors)
  for (m in seq_len(max_factors)) {
    lam <- e$vectors[, seq_len(m), drop = FALSE] %*%
      diag(sqrt(e$values[seq_len(m)]), m, m)
    resid <- sim_matrix - tcrossprod(lam)
    d <- diag(resid)
    if (any(d < 1e-10)) break
    map_vals[m] <- off(resid / tcrossprod(sqrt(d)))
  }

  valid <- which(!is.na(map_vals))
  n_factors <- if (length(valid) == 0L) 1L else valid[which.min(map_vals[valid])]

  structure(
    list(
      n_factors = max(1L, as.integer(n_factors)),
      map = map_vals,
      map0 = map0
    ),
    class = "sfa_map"
  )
}

#' @export
print.sfa_map <- function(x, ...) {
  cat("Velicer's minimum average partial\n")
  cat("  Suggested factors:", x$n_factors, "\n")
  cat("  MAP at minimum:", format(min(x$map, na.rm = TRUE), digits = 4), "\n")
  cat("  Baseline (0 components):", format(x$map0, digits = 4), "\n")
  invisible(x)
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
  # requires numeric n; TMFG does not use it to build the graph). Note the
  # embedding benchmark of Garrido et al. (2025) validated EGA with EBICglasso
  # networks; TMFG is this package's sample-size-free substitute, so their
  # accuracy evidence transfers to it only indirectly.
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
#'   Required when \code{"parallel"} or \code{"EKC"} is in \code{methods}.
#' @param methods Character vector of retention methods to run. Supported:
#'   \code{"parallel"} ([sfa_parallel()]), \code{"kaiser"} (latent-root
#'   criterion), \code{"TEFI"}, \code{"EGA"} (requires EGAnet),
#'   \code{"EKC"} ([sfa_ekc()]), and \code{"MAP"} ([sfa_map()]). The
#'   default runs parallel analysis alone, matching the field's conventional
#'   retention default; request the multi-criterion battery explicitly (the
#'   package's own demonstration uses
#'   \code{c("parallel", "kaiser", "TEFI", "EGA", "EKC")}). Notes for
#'   choosing: EGA needs the suggested EGAnet package; the latent-root rule
#'   is retained for reference despite its known liberal bias; TEFI tends to
#'   run low on embedding similarity matrices; and MAP tends to track
#'   reliable minor structure well past the interpretable factor count,
#'   which would pull the modal consensus deep.
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
#'   \item{consensus}{Integer: modal recommendation across methods. When two
#'     or more recommendations tie for the mode, the smallest tied value is
#'     returned (the more parsimonious solution). With a single method this
#'     equals that method's suggestion, and \code{print()} omits the
#'     consensus line.}
#'   \item{eigenvalues}{Numeric vector: observed eigenvalues.}
#'   \item{parallel}{Parallel analysis result (if run), or \code{NULL}.}
#' }
#'
#' @export
sfa_nfactors <- function(sim_matrix, embeddings = NULL,
                         methods = "parallel",
                         seed = 42L, parallel_iter = 100L,
                         max_factors = NULL,
                         rotate = "oblimin", fm = "minres", ...) {
  # accept a fitted sfa object: pull its similarity matrix and embeddings
  if (inherits(sim_matrix, "sfa")) {
    fit <- sim_matrix
    if (is.null(embeddings)) embeddings <- fit$transformed_embeddings
    sim_matrix <- fit$sim_matrix
  }
  methods <- match.arg(methods,
                       c("parallel", "kaiser", "TEFI", "EGA", "EKC", "MAP"),
                       several.ok = TRUE)
  parallel_iter <- .assert_count(parallel_iter, "parallel_iter")

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
      EGA = .retention_ega(sim_matrix),
      EKC = {
        if (is.null(embeddings)) {
          stop("'embeddings' is required for the empirical Kaiser criterion.",
               call. = FALSE)
        }
        sfa_ekc(sim_matrix, embeddings)$n_factors
      },
      MAP = sfa_map(sim_matrix)$n_factors
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
    # modal recommendation; ties resolve to the smallest tied value (table()
    # names sort as character, so compare numerically, not lexically)
    tab <- table(valid_nf)
    min(as.integer(names(tab)[tab == max(tab)]))
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
  if (nrow(x$methods) > 1L) {
    cat("  ", strrep("-", 24), "\n", sep = "")
    cat(sprintf("  %-12s %s\n", "Consensus",
                if (is.na(x$consensus)) "N/A" else as.character(x$consensus)))
  }

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
