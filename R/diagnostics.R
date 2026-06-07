#' @keywords internal
.compute_kmo <- function(corr_matrix, alpha = 1e-6) {
  corr_reg <- .regularize_corr(corr_matrix, alpha = alpha)
  inv_corr <- solve(corr_reg)
  d <- sqrt(pmax(diag(inv_corr), 1e-12))
  partial_corr <- -inv_corr / outer(d, d)
  diag(partial_corr) <- 0

  corr_sq <- corr_matrix
  diag(corr_sq) <- 0
  corr_sq <- corr_sq^2
  partial_sq <- partial_corr^2

  partial_sum <- colSums(partial_sq)
  corr_sum <- colSums(corr_sq)
  per_item <- corr_sum / (corr_sum + partial_sum)

  total <- sum(corr_sq) / (sum(corr_sq) + sum(partial_sq))

  list(total = total, per_item = per_item)
}

#' @keywords internal
# Entropy of a density matrix, matching EGAnet's matrix_entropy:
# -sum(diag(M %*% log(M))) = -sum_{ij} M_ij log(M_ij) for symmetric M.
.matrix_entropy <- function(density_matrix) {
  -sum(diag(density_matrix %*% log(density_matrix)), na.rm = TRUE)
}

#' @keywords internal
# Total Entropy Fit Index (Golino, Moulder, et al. 2021), partition-based.
# Faithful reimplementation of EGAnet::tefi: uses |R|, normalizes by the number
# of variables, and combines per-dimension vs total entropy with a sqrt(NF)
# complexity penalty. Lower (more negative) is better.
.compute_tefi <- function(corr_matrix, structure) {
  R <- abs(corr_matrix)
  n <- ncol(R)
  if (n == 0L || is.null(structure)) return(NA_real_)
  structure <- as.character(structure)
  H_total <- .matrix_entropy(R / n)
  comms <- unique(structure)
  NF <- length(comms)
  H_within <- vapply(comms, function(g) {
    idx <- structure == g
    Rg <- R[idx, idx, drop = FALSE]
    .matrix_entropy(Rg / ncol(Rg))
  }, numeric(1))
  mean_within <- mean(H_within, na.rm = TRUE)
  sum_within <- mean_within * NF
  (mean_within - H_total) + (H_total - sum_within) * sqrt(NF)
}

#' @keywords internal
.compute_rmsr_caf <- function(observed_corr, fa_obj) {
  loadings <- unclass(fa_obj$loadings)
  uniquenesses <- fa_obj$uniquenesses
  phi <- fa_obj$Phi
  if (is.null(phi)) phi <- diag(ncol(loadings))

  common <- loadings %*% phi %*% t(loadings)
  reproduced <- common + diag(uniquenesses)
  residual <- observed_corr - reproduced            # full residual (diagonal ~ 0)

  off_diag <- residual[upper.tri(residual)]
  rmsr <- sqrt(mean(off_diag^2))

  # CAF (Common part Accounted For; Lorenzo-Seva, Timmerman & Kiers, 2011):
  # 1 - KMO of the residual *correlation* matrix after removing the common part
  # only (uniquenesses retained on the diagonal). Subtracting the uniquenesses
  # too -- as the RMSR residual does -- zeroes the diagonal, so the
  # standardization divides by ~0 and KMO degenerates to 1 (CAF == 0). Keeping
  # Psi on the diagonal gives KMO a valid correlation matrix.
  caf <- tryCatch({
    res_caf <- observed_corr - common               # diagonal = uniquenesses
    d <- sqrt(pmax(diag(res_caf), 1e-12))
    res_caf <- res_caf / outer(d, d)
    diag(res_caf) <- 1
    1 - .compute_kmo(res_caf)$total
  }, error = function(e) NA_real_)

  list(rmsr = rmsr, caf = caf, residual = residual)
}

#' @keywords internal
.compute_omega <- function(loadings_df, factor_names, theoretical_factors = NULL,
                           codes = NULL) {
  loadings_mat <- as.matrix(loadings_df)

  if (!is.null(theoretical_factors) && !is.null(codes)) {
    max_loading_col <- apply(abs(loadings_mat), 1, which.max)
    assignment_map <- stats::setNames(factor_names[max_loading_col], codes)
  } else {
    max_loading_col <- apply(abs(loadings_mat), 1, which.max)
    assignment_map <- stats::setNames(factor_names[max_loading_col],
                                       rownames(loadings_mat))
  }

  results <- data.frame(
    factor = factor_names,
    omega_assigned = NA_real_,
    stringsAsFactors = FALSE
  )

  if (!is.null(theoretical_factors)) {
    results$omega_theoretical <- NA_real_
  }

  for (i in seq_along(factor_names)) {
    fn <- factor_names[i]
    col_idx <- i

    # omega for items assigned by highest loading
    mask_assigned <- assignment_map == fn
    if (sum(mask_assigned) >= 2) {
      l <- loadings_mat[mask_assigned, col_idx]
      u <- 1 - l^2
      denom <- sum(l)^2 + sum(u)
      if (denom > 0) results$omega_assigned[i] <- sum(l)^2 / denom
    }

    # omega for items from theoretical factor
    if (!is.null(theoretical_factors)) {
      theo_labels <- unique(theoretical_factors)
      # match extracted factor to nearest theoretical factor via DAAL
      theo_mean_abs <- vapply(theo_labels, function(tl) {
        mask <- theoretical_factors == tl
        if (sum(mask) < 1) return(0)
        mean(abs(loadings_mat[mask, col_idx]))
      }, numeric(1))
      best_theo <- theo_labels[which.max(theo_mean_abs)]
      mask_theo <- theoretical_factors == best_theo
      if (sum(mask_theo) >= 2) {
        l <- loadings_mat[mask_theo, col_idx]
        u <- 1 - l^2
        denom <- sum(l)^2 + sum(u)
        if (denom > 0) results$omega_theoretical[i] <- sum(l)^2 / denom
      }
      results$matched_theoretical[i] <- best_theo
    }
  }
  results
}

#' @keywords internal
.compute_daal <- function(loadings_mat, theoretical_factors) {
  theo_unique <- sort(unique(theoretical_factors))
  factor_names <- colnames(loadings_mat)
  if (is.null(factor_names)) factor_names <- paste0("Factor", seq_len(ncol(loadings_mat)))

  daal <- matrix(NA_real_, nrow = length(factor_names), ncol = length(theo_unique),
                 dimnames = list(factor_names, theo_unique))

  for (i in seq_along(factor_names)) {
    for (j in seq_along(theo_unique)) {
      mask <- theoretical_factors == theo_unique[j]
      daal[i, j] <- mean(abs(loadings_mat[mask, i]))
    }
  }
  daal
}

#' @keywords internal
.random_item_calibration <- function(n_items, embed_dim, n_factors, rotate,
                                     fm, n_iter = 100, seed = 42) {
  rmsr_null <- numeric(0)
  caf_null <- numeric(0)
  tefi_null <- numeric(0)

  withr::with_seed(seed, {
    for (iter in seq_len(n_iter)) {
      rand <- matrix(stats::rnorm(n_items * embed_dim), nrow = n_items)
      norms <- sqrt(rowSums(rand^2))
      rand <- rand / norms
      sim <- tcrossprod(rand)
      diag(sim) <- 1.0

      tryCatch({
        fa_rand <- psych::fa(sim, nfactors = max(1L, n_factors),
                             rotate = rotate, fm = fm,
                             n.obs = NA, warnings = FALSE)
        rc <- .compute_rmsr_caf(sim, fa_rand)
        rmsr_null <- c(rmsr_null, rc$rmsr)
        caf_null <- c(caf_null, rc$caf)
        tefi_null <- c(tefi_null,
                       .compute_tefi(sim, .assign_items(unclass(fa_rand$loadings))))
      }, error = function(e) NULL)
    }
  })

  list(
    rmsr = rmsr_null,
    caf = caf_null,
    tefi = tefi_null
  )
}
