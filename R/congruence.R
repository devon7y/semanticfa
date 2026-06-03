#' Compare Semantic and Empirical Factor Structures
#'
#' Computes agreement metrics between a semantic factor analysis result and a
#' reference factor structure (from empirical data or theory).
#'
#' @param sfa_fit An object of class \code{"sfa"}.
#' @param target A \code{psych::fa} object, a \code{loadings} matrix, a named
#'   factor label vector (one per item), or a correlation/similarity matrix.
#' @param metrics Character vector of metrics to compute. Supported:
#'   \code{"tucker"}, \code{"nmi"}, \code{"ari"}, \code{"frobenius"},
#'   \code{"disattenuated"}.
#'
#' @returns A list of class \code{"sfa_congruence"} with one component per
#'   requested metric.
#'
#' @references
#' Garrido, L. E., et al. (preprint). NMI and ARI for factor partition agreement.
#'
#' Wang, Y. (preprint). Frobenius similarity of inter-factor correlations.
#'
#' Hommel, B. E. & Arslan, R. C. (2025). Disattenuated latent correlation.
#'
#' @export
sfa_congruence <- function(sfa_fit, target,
                           metrics = c("tucker", "nmi", "ari", "frobenius",
                                       "disattenuated")) {
  metrics <- match.arg(metrics,
    c("tucker", "nmi", "ari", "frobenius", "disattenuated"),
    several.ok = TRUE)

  result <- list()

  target_loadings <- NULL
  target_labels <- NULL
  target_phi <- NULL

  if (inherits(target, "fa") || inherits(target, "psych")) {
    target_loadings <- unclass(target$loadings)
    target_labels <- .assign_items(target_loadings)
    target_phi <- target$Phi
    if (is.null(target_phi)) target_phi <- diag(ncol(target_loadings))
  } else if (inherits(target, "loadings") || (is.matrix(target) && ncol(target) > 1 &&
             ncol(target) < nrow(target))) {
    target_loadings <- unclass(target)
    target_labels <- .assign_items(target_loadings)
  } else if (is.character(target) || is.factor(target)) {
    target_labels <- as.character(target)
  } else if (is.matrix(target) && nrow(target) == ncol(target)) {
    # similarity/correlation matrix — for disattenuated only
  }

  sfa_loadings <- unclass(sfa_fit$loadings)
  sfa_labels <- .assign_items(sfa_loadings)
  sfa_phi <- sfa_fit$Phi
  if (is.null(sfa_phi)) sfa_phi <- diag(ncol(sfa_loadings))

  if ("tucker" %in% metrics) {
    if (is.null(target_loadings)) {
      warning("Tucker congruence requires loadings in 'target'; skipping.",
              call. = FALSE)
    } else {
      result$tucker <- psych::factor.congruence(sfa_loadings, target_loadings)
    }
  }

  if ("nmi" %in% metrics) {
    if (is.null(target_labels)) {
      warning("NMI requires factor labels in 'target'; skipping.", call. = FALSE)
    } else {
      result$nmi <- .compute_nmi(sfa_labels, target_labels)
    }
  }

  if ("ari" %in% metrics) {
    if (is.null(target_labels)) {
      warning("ARI requires factor labels in 'target'; skipping.", call. = FALSE)
    } else {
      result$ari <- .compute_ari(sfa_labels, target_labels)
    }
  }

  if ("frobenius" %in% metrics) {
    if (is.null(target_phi)) {
      warning("Frobenius requires a target with factor correlations; skipping.",
              call. = FALSE)
    } else {
      result$frobenius <- .compute_frobenius(sfa_phi, target_phi)
    }
  }

  if ("disattenuated" %in% metrics) {
    if (!is.null(sfa_fit$sim_matrix) && is.matrix(target) &&
        nrow(target) == ncol(target) &&
        nrow(target) == nrow(sfa_fit$sim_matrix)) {
      result$disattenuated <- .compute_disattenuated(
        sfa_fit$sim_matrix, target
      )
    } else if (!is.null(target_loadings)) {
      # reconstruct target correlation from loadings
      target_repr <- target_loadings %*% target_phi %*% t(target_loadings)
      diag(target_repr) <- 1
      result$disattenuated <- .compute_disattenuated(
        sfa_fit$sim_matrix, target_repr
      )
    } else {
      warning("Disattenuated correlation requires matrices of matching size; ",
              "skipping.", call. = FALSE)
    }
  }

  structure(result, class = "sfa_congruence")
}

#' @export
print.sfa_congruence <- function(x, ...) {
  cat("Factor structure congruence\n\n")
  if (!is.null(x$tucker)) {
    cat("Tucker phi (factor-by-factor):\n")
    print(round(x$tucker, 3))
    cat("\n")
  }
  if (!is.null(x$nmi))
    cat(sprintf("  NMI:            %.3f\n", x$nmi))
  if (!is.null(x$ari))
    cat(sprintf("  ARI:            %.3f\n", x$ari))
  if (!is.null(x$frobenius))
    cat(sprintf("  Frobenius:      %.3f\n", x$frobenius))
  if (!is.null(x$disattenuated))
    cat(sprintf("  Disattenuated:  %.3f\n", x$disattenuated))
  invisible(x)
}

# --- internal metric implementations ---

#' @keywords internal
.assign_items <- function(loadings_mat) {
  apply(abs(loadings_mat), 1, function(row) {
    colnames(loadings_mat)[which.max(row)]
  })
}

#' @keywords internal
.compute_nmi <- function(labels_a, labels_b) {
  labels_a <- as.character(labels_a)
  labels_b <- as.character(labels_b)
  n <- length(labels_a)
  ua <- unique(labels_a)
  ub <- unique(labels_b)

  contingency <- matrix(0L, nrow = length(ua), ncol = length(ub))
  for (i in seq_len(n)) {
    r <- match(labels_a[i], ua)
    c <- match(labels_b[i], ub)
    contingency[r, c] <- contingency[r, c] + 1L
  }

  p_ab <- contingency / n
  p_a <- rowSums(p_ab)
  p_b <- colSums(p_ab)

  mi <- 0
  for (i in seq_along(ua)) {
    for (j in seq_along(ub)) {
      if (p_ab[i, j] > 0) {
        mi <- mi + p_ab[i, j] * log(p_ab[i, j] / (p_a[i] * p_b[j]))
      }
    }
  }

  h_a <- -sum(p_a[p_a > 0] * log(p_a[p_a > 0]))
  h_b <- -sum(p_b[p_b > 0] * log(p_b[p_b > 0]))
  denom <- sqrt(h_a * h_b)
  if (denom == 0) 0 else mi / denom
}

#' @keywords internal
.compute_ari <- function(labels_a, labels_b) {
  labels_a <- as.character(labels_a)
  labels_b <- as.character(labels_b)
  n <- length(labels_a)
  ua <- unique(labels_a)
  ub <- unique(labels_b)

  contingency <- matrix(0L, nrow = length(ua), ncol = length(ub))
  for (i in seq_len(n)) {
    r <- match(labels_a[i], ua)
    c <- match(labels_b[i], ub)
    contingency[r, c] <- contingency[r, c] + 1L
  }

  comb2 <- function(x) x * (x - 1L) / 2L

  sum_comb_c <- sum(comb2(contingency))
  sum_comb_a <- sum(comb2(rowSums(contingency)))
  sum_comb_b <- sum(comb2(colSums(contingency)))

  n_total <- sum(contingency)
  if (n_total < 2) return(0)
  n_choose_2 <- comb2(n_total)
  expected <- sum_comb_a * sum_comb_b / n_choose_2
  max_index <- 0.5 * (sum_comb_a + sum_comb_b)
  denom <- max_index - expected
  if (denom == 0) {
    return(if (sum_comb_c == expected) 1 else 0)
  }
  (sum_comb_c - expected) / denom
}

#' @keywords internal
.compute_frobenius <- function(mat_a, mat_b) {
  if (!identical(dim(mat_a), dim(mat_b))) return(NA_real_)
  a <- as.numeric(mat_a)
  b <- as.numeric(mat_b)
  num <- sum(a * b)
  denom <- sqrt(sum(a^2)) * sqrt(sum(b^2))
  if (denom == 0) NA_real_ else num / denom
}

#' @keywords internal
.compute_disattenuated <- function(mat_x, mat_y) {
  lt_x <- mat_x[lower.tri(mat_x)]
  lt_y <- mat_y[lower.tri(mat_y)]

  r_obs <- stats::cor(lt_x, lt_y)

  rel_x <- .split_half_reliability(lt_x)
  rel_y <- .split_half_reliability(lt_y)

  denom <- sqrt(rel_x * rel_y)
  if (denom <= 0) return(r_obs)
  min(r_obs / denom, 1.0)
}

#' @keywords internal
.split_half_reliability <- function(vec) {
  n <- length(vec)
  if (n < 4) return(1)
  odd <- vec[seq(1, n, by = 2)]
  even <- vec[seq(2, n, by = 2)]
  min_len <- min(length(odd), length(even))
  r <- stats::cor(odd[seq_len(min_len)], even[seq_len(min_len)])
  2 * r / (1 + r)
}
