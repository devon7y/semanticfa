#' Comparison-Data Misfit Profile
#'
#' Computes a comparison-data misfit profile for an embedding similarity
#' structure, adapting the comparison data method of Ruscio and Roche (2012)
#' to the response-free setting. For each candidate factor count k, the
#' function builds a finite population of comparison data with known k-factor
#' structure that reproduces both the model-implied correlation matrix and the
#' empirical marginal distributions (an iterative rank-remapping refinement
#' after the GenData program of Ruscio & Kaczetow, 2008), draws bootstrap
#' samples of the empirical size, and records how well each sample's
#' eigenvalue profile reproduces the observed one (root-mean-square residual,
#' RMSR).
#'
#' The deliverable is the profile, not a verdict. On conventional response
#' data with a crisp factor boundary, the profile shows a sharp elbow at the
#' true count. On embedding similarity matrices the misfit typically declines
#' smoothly without an elbow, because a k-factor model with diagonal
#' uniqueness cannot reproduce the heavy anisotropic tail of an embedding
#' spectrum, and each added factor keeps improving reproduction. For the same
#' reason Ruscio and Roche's sequential significance rule (each k tested
#' against k - 1 with a one-tailed Mann-Whitney test) saturates at
#' \code{n_factors_max} on embedding matrices at any conventional alpha, and
#' inflates with the case count on response data too. The rule is therefore
#' only run when \code{alpha} is supplied explicitly, and its verdict should
#' be read alongside the profile shape rather than in place of it.
#'
#' With \code{input = "embeddings"}, cases are embedding dimensions: the
#' Pearson correlations of the transposed embedding matrix equal the
#' \code{"mean_centered_pearson"} similarity of [sfa_similarity()], so the
#' profile addresses exactly the matrix that encoding factors. Other
#' encodings are not correlation matrices of any data matrix, so the profile
#' is computed in the correlation metric regardless.
#'
#' @param x Item embeddings (n_items x embedding_dim; also accepts a fitted
#'   \code{"sfa"} object or an \code{"sfa_embeddings"} object from
#'   [sfa_load_npz()]) when \code{input = "embeddings"}, or a raw data matrix
#'   (cases x variables, e.g. survey responses) when \code{input = "data"}.
#' @param input Whether \code{x} holds item embeddings (default) or raw
#'   case-by-variable data.
#' @param n_factors_max Largest factor count to profile (default 10, capped
#'   at one third of the variable count).
#' @param n_samples Bootstrap samples per factor count (default 500).
#' @param n_pop Size of each comparison population (default 10000).
#' @param alpha Optional alpha for Ruscio and Roche's sequential
#'   Mann-Whitney stopping rule. Default \code{NULL} skips the rule; see
#'   Details for why.
#' @param fm Factor extraction method for the comparison models (default
#'   \code{"minres"}).
#' @param gen_iter Refinement iterations for the population generator
#'   (default 6; continuous marginals converge in a few refinements).
#' @param seed Random seed, used via [withr::with_seed()] without touching
#'   the global RNG state.
#'
#' @returns A list of class \code{"sfa_cd"} with components:
#' \describe{
#'   \item{median_rmsr}{Numeric vector: median RMSR at each factor count
#'     (NA where extraction failed).}
#'   \item{profile}{Numeric vector: median RMSR normalized by its one-factor
#'     value.}
#'   \item{improvement}{Numeric vector: relative improvement (proportion)
#'     from each factor count to the next.}
#'   \item{rmsr}{Numeric matrix (n_samples x n_factors_max): the full RMSR
#'     distributions.}
#'   \item{eigenvalues}{Numeric vector: observed eigenvalues (descending).}
#'   \item{n_factors}{Integer: sequential-rule verdict, only when
#'     \code{alpha} was supplied (otherwise \code{NA}).}
#'   \item{alpha, n, n_samples, n_pop}{Settings used.}
#' }
#'
#' @references
#' Ruscio, J., & Roche, B. (2012). Determining the number of factors to
#' retain in an exploratory factor analysis using comparison data of known
#' factorial structure. \emph{Psychological Assessment}, 24(2), 282--292.
#' \doi{10.1037/a0025697}
#'
#' Ruscio, J., & Kaczetow, W. (2008). Simulating multivariate nonnormal data
#' using an iterative algorithm. \emph{Multivariate Behavioral Research},
#' 43(3), 355--381. \doi{10.1080/00273170802285693}
#'
#' Goretzko, D., & Ruscio, J. (2024). The comparison data forest: A new
#' comparison data approach to determine the number of factors in exploratory
#' factor analysis. \emph{Behavior Research Methods}, 56, 1838--1851.
#' \doi{10.3758/s13428-023-02122-4}
#'
#' @examples
#' \dontrun{
#' data(big5)
#' cd <- sfa_cd(big5$embeddings, n_samples = 100)
#' print(cd)
#' plot(cd)
#' }
#'
#' @export
sfa_cd <- function(x, input = c("embeddings", "data"),
                   n_factors_max = 10L, n_samples = 500L, n_pop = 10000L,
                   alpha = NULL, fm = "minres", gen_iter = 6L, seed = 42L) {
  input <- match.arg(input)
  if (inherits(x, "sfa")) x <- x$transformed_embeddings
  if (inherits(x, "sfa_embeddings")) x <- x$embeddings
  x <- as.matrix(x)
  if (!is.numeric(x) || length(dim(x)) != 2L) {
    stop("'x' must be a numeric matrix.", call. = FALSE)
  }
  # cases in rows: transpose embeddings so dimensions play the case role
  D <- if (input == "embeddings") t(x) else x
  n <- nrow(D)
  J <- ncol(D)
  if (n <= J) {
    stop("Need more cases than variables (embeddings: more dimensions than ",
         "items).", call. = FALSE)
  }
  n_factors_max <- min(.assert_count(n_factors_max, "n_factors_max"),
                       floor(J / 3))
  n_samples <- .assert_count(n_samples, "n_samples")
  n_pop <- .assert_count(n_pop, "n_pop")
  gen_iter <- .assert_count(gen_iter, "gen_iter")
  if (!is.null(alpha) &&
      (!is.numeric(alpha) || length(alpha) != 1L || alpha <= 0 || alpha >= 1)) {
    stop("'alpha' must be NULL or a single number in (0, 1).", call. = FALSE)
  }

  R_emp <- stats::cor(D)
  obs_eigs <- sort(eigen(R_emp, symmetric = TRUE, only.values = TRUE)$values,
                   decreasing = TRUE)

  rmsr <- withr::with_seed(seed, {
    out <- matrix(NA_real_, nrow = n_samples, ncol = n_factors_max)
    for (k in seq_len(n_factors_max)) {
      pop <- tryCatch(
        .cd_population(D, R_emp, k, n_pop, gen_iter, fm),
        error = function(e) NULL)
      if (is.null(pop)) next
      for (s in seq_len(n_samples)) {
        samp <- pop[sample.int(n_pop, n, replace = TRUE), , drop = FALSE]
        eigs <- sort(eigen(stats::cor(samp), symmetric = TRUE,
                           only.values = TRUE)$values, decreasing = TRUE)
        out[s, k] <- sqrt(mean((eigs - obs_eigs)^2))
      }
    }
    out
  })

  med <- apply(rmsr, 2, function(z) {
    if (all(is.na(z))) NA_real_ else stats::median(z)
  })
  profile <- med / med[1]
  improvement <- -diff(med) / utils::head(med, -1L)

  n_factors <- NA_integer_
  if (!is.null(alpha)) {
    n_factors <- 1L
    for (k in 2:n_factors_max) {
      if (all(is.na(rmsr[, k]))) break
      p <- suppressWarnings(
        stats::wilcox.test(rmsr[, k], rmsr[, k - 1L],
                           alternative = "less", exact = FALSE)$p.value)
      if (is.na(p) || p >= alpha) break
      n_factors <- k
    }
  }

  structure(
    list(
      median_rmsr = med,
      profile = profile,
      improvement = improvement,
      rmsr = rmsr,
      eigenvalues = obs_eigs,
      n_factors = n_factors,
      alpha = alpha,
      n = n,
      n_samples = n_samples,
      n_pop = n_pop
    ),
    class = "sfa_cd"
  )
}

#' Comparison population with k-factor structure and empirical marginals
#'
#' Iterative rank-remapping generator adapting GenData (Ruscio & Kaczetow,
#' 2008): start from the k-factor model-implied correlation matrix, generate
#' a Gaussian population, remap each variable's values onto a bootstrap of
#' its empirical distribution, then nudge the generating matrix by the
#' resulting reproduction error and keep the best population seen.
#'
#' @keywords internal
#' @noRd
.cd_population <- function(D, R_emp, k, n_pop, gen_iter, fm) {
  J <- ncol(D)
  fa_k <- suppressWarnings(suppressMessages(
    psych::fa(R_emp, nfactors = k, rotate = "none", fm = fm,
              n.obs = NA, warnings = FALSE)))
  lam <- unclass(fa_k$loadings)
  sigma <- tcrossprod(lam)
  diag(sigma) <- 1
  target <- .cd_nearpd(sigma)

  # per-variable sorted bootstrap of the empirical marginals
  marginals <- apply(D, 2, function(v) sort(sample(v, n_pop, replace = TRUE)))

  gen <- target
  best_pop <- NULL
  best_err <- Inf
  for (it in seq_len(gen_iter)) {
    ch <- tryCatch(chol(gen), error = function(e) chol(.cd_nearpd(gen)))
    pop <- matrix(stats::rnorm(n_pop * J), nrow = n_pop) %*% ch
    for (j in seq_len(J)) {
      pop[order(pop[, j]), j] <- marginals[, j]
    }
    err_mat <- target - stats::cor(pop)
    err <- sqrt(mean(err_mat[lower.tri(err_mat)]^2))
    if (err < best_err) {
      best_err <- err
      best_pop <- pop
    }
    gen <- .cd_nearpd(gen + err_mat)
  }
  best_pop
}

#' @keywords internal
#' @noRd
.cd_nearpd <- function(M) {
  M <- (M + t(M)) / 2
  e <- eigen(M, symmetric = TRUE)
  vals <- pmax(e$values, 1e-8)
  out <- e$vectors %*% (vals * t(e$vectors))
  d <- sqrt(diag(out))
  out <- out / tcrossprod(d)
  (out + t(out)) / 2
}

#' @export
print.sfa_cd <- function(x, digits = 3, ...) {
  cat("Comparison-data misfit profile (Ruscio & Roche, adapted)\n")
  cat(sprintf("  Cases: %d, samples per k: %d, population: %d\n\n",
              x$n, x$n_samples, x$n_pop))
  k <- seq_along(x$median_rmsr)
  tab <- data.frame(
    k = k,
    median_RMSR = round(x$median_rmsr, digits),
    normalized = round(x$profile, digits),
    improvement = c(NA, round(x$improvement * 100, 1))
  )
  names(tab)[4] <- "improvement_pct"
  print(tab, row.names = FALSE)
  if (!is.na(x$n_factors)) {
    cat(sprintf("\nSequential rule (alpha = %s): %d factors\n",
                format(x$alpha), x$n_factors))
    cat("  (Read alongside the profile: this rule saturates on embedding\n")
    cat("   similarity matrices; see ?sfa_cd.)\n")
  } else {
    cat("\nNo single-count verdict: inspect the profile for an elbow\n")
    cat("(sharp drop then flattening) versus a smooth graded decline.\n")
  }
  invisible(x)
}

#' @export
plot.sfa_cd <- function(x, ...) {
  op <- graphics::par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.2, 1))
  on.exit(graphics::par(op), add = TRUE)
  k <- seq_along(x$median_rmsr)
  graphics::plot(k, x$profile, type = "b", pch = 19,
                 xlab = "Factors (k)", ylab = "Median RMSR / one-factor RMSR",
                 main = "Misfit profile", ...)
  graphics::plot(k[-1], x$improvement * 100, type = "h", lwd = 3,
                 xlab = "Factors (k)", ylab = "Improvement over k - 1 (%)",
                 main = "Relative improvement", ...)
  invisible(x)
}
