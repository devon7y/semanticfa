#' Coerce to psych fa Object
#'
#' @param x An object to coerce.
#' @param ... Additional arguments (unused).
#' @returns An object of class \code{c("psych", "fa")}.
#' @export
as_psych <- function(x, ...) {
  UseMethod("as_psych")
}

#' @rdname as_psych
#' @export
as_psych.sfa <- function(x, ...) {
  x$.fa
}

# Rule-of-thumb label for the root-mean-square residual (smaller is better;
# conventional residual-fit bands, cf. SRMR guidelines).
#' @keywords internal
.label_rmsr <- function(x) {
  if (is.na(x)) return("")
  lab <- if (x <= 0.05) "good"
         else if (x <= 0.08) "acceptable"
         else if (x <= 0.10) "mediocre"
         else "poor"
  paste0(" (", lab, " - lower is better)")
}

# Rule-of-thumb label for the common-part-accounted-for index (bounded 0-1,
# higher is better: residuals hold less common variance).
#' @keywords internal
.label_caf <- function(x) {
  if (is.na(x)) return("")
  lab <- if (x >= 0.50) "adequate"
         else if (x >= 0.30) "marginal"
         else "low"
  paste0(" (", lab, " - higher is better)")
}

#' @export
print.sfa <- function(x, cutoff = 0.3, sort = TRUE, ...) {
  cat("Semantic Factor Analysis\n")
  cat("  Encoding:", x$encoding, "\n")
  is_default_model <- identical(x$embed_model, .SFA_DEFAULT_MODEL)
  if (!is.null(x$embed_model)) {
    cat("  Model:", x$embed_model, if (is_default_model) "(default)" else "", "\n")
  }
  if (!is.null(x$embedding_dim)) cat("  Embedding dim:", x$embedding_dim, "\n")
  if (!is.null(x$dim_select)) {
    cat(sprintf("  Dim-select: EGA depth optimization chose %d of %d coordinates\n",
                x$dim_select$optimal_depth, x$dim_select$full_dim))
  }
  cat("  Factors:", x$factors, " (", x$fm, " + ", x$rotation, ")\n", sep = "")
  if (is_default_model) {
    cat("  Note: larger embedding models recover factor structure more",
        "accurately.\n",
        "        For higher fidelity, set",
        "model = \"Qwen/Qwen3-Embedding-4B\" (8 GB RAM)\n",
        "        or model = \"Qwen/Qwen3-Embedding-8B\" (16 GB RAM).\n")
  }
  cat("\n")

  cat("Diagnostics:\n")
  if (!is.null(x$kmo))
    cat(sprintf("  KMO:  %.3f", x$kmo$total))
  kmo_val <- x$kmo$total
  if (!is.null(kmo_val)) {
    label <- if (kmo_val >= 0.9) "marvelous"
             else if (kmo_val >= 0.8) "meritorious"
             else if (kmo_val >= 0.7) "middling"
             else if (kmo_val >= 0.6) "mediocre"
             else "poor"
    cat(paste0(" (", label, " - higher is better)"))
  }
  cat("\n")
  if (!is.null(x$tefi))
    cat(sprintf("  TEFI: %.4f (lower is better)\n", x$tefi))
  if (!is.null(x$rmsr) && !is.na(x$rmsr))
    cat(sprintf("  RMSR: %.4f%s\n", x$rmsr, .label_rmsr(x$rmsr)))
  if (!is.null(x$caf) && !is.na(x$caf))
    cat(sprintf("  CAF:  %.4f%s\n", x$caf, .label_caf(x$caf)))

  if (any(x$heywood)) {
    n_hw <- sum(x$heywood)
    cat(sprintf("  Heywood cases: %d item(s) with communality > 1\n", n_hw))
  }

  cat("\nFactor loadings:\n")
  ld <- x$loadings
  if (sort) {
    tryCatch({
      ld <- psych::fa.sort(ld)
    }, error = function(e) NULL)
  }
  print(ld, cutoff = cutoff, ...)

  if (!is.null(x$Phi)) {
    cat("\nFactor correlations (Phi):\n")
    print(round(x$Phi, 3))
  }

  vacc <- x$Vaccounted
  if (!is.null(vacc)) {
    cat("\nVariance accounted for:\n")
    print(round(vacc, 3))
  }

  invisible(x)
}

#' @export
summary.sfa <- function(object, ...) {
  print.sfa(object, ...)

  cat("\nMcDonald's omega per factor:\n")
  if (!is.null(object$omega)) {
    print(object$omega, row.names = FALSE, digits = 3)
  }

  cat("\nCommunalities:\n")
  comm_df <- data.frame(
    communality = round(object$communality, 3),
    uniqueness = round(object$uniquenesses, 3)
  )
  if (any(object$heywood)) {
    comm_df$heywood <- ifelse(object$heywood, "*", "")
  }
  print(comm_df)

  if (!is.null(object$calibration)) {
    cat("\nMonte Carlo calibration (random-item null):\n")
    for (metric in names(object$calibration)) {
      arr <- object$calibration[[metric]]
      arr <- arr[is.finite(arr)]
      if (length(arr) > 0) {
        cat(sprintf("  %5s null: mean=%.4f  5%%=%.4f  95%%=%.4f\n",
                    toupper(metric), mean(arr),
                    stats::quantile(arr, 0.05),
                    stats::quantile(arr, 0.95)))
      }
    }
  }

  invisible(object)
}

#' @export
plot.sfa <- function(x, type = c("scree", "loadings", "residuals",
                                 "similarity"), ...) {
  type <- match.arg(type)

  switch(type,
    scree = .plot_scree(x, ...),
    loadings = .plot_loadings(x, ...),
    residuals = .plot_residuals(x, ...),
    similarity = sfa_corplot(x, ...)
  )
}

#' @keywords internal
.plot_scree <- function(x, ...) {
  eigs <- x$values
  n <- length(eigs)
  plot(seq_len(n), eigs, type = "b", pch = 19,
       xlab = "Factor number", ylab = "Eigenvalue",
       main = "Scree plot with parallel analysis threshold",
       ...)
  if (!is.null(x$parallel)) {
    graphics::lines(seq_len(n), x$parallel$percentiles[seq_len(n)],
                    col = "red", lty = 2, lwd = 2)
    graphics::legend("topright",
                     legend = c("Observed", paste0(x$parallel$percentile, "th percentile")),
                     col = c("black", "red"), lty = c(1, 2), pch = c(19, NA))
  }
  graphics::abline(h = 1, col = "grey50", lty = 3)
}

#' @keywords internal
.plot_loadings <- function(x, cutoff = 0.3, ...) {
  ld <- unclass(x$loadings)
  tryCatch({
    ld <- unclass(psych::fa.sort(x$loadings))
  }, error = function(e) NULL)

  ld_display <- ld
  ld_display[abs(ld_display) < cutoff] <- NA

  n_items <- nrow(ld)
  n_factors <- ncol(ld)

  graphics::image(
    seq_len(n_factors), seq_len(n_items), t(ld[n_items:1, ]),
    col = grDevices::hcl.colors(50, "RdBu"),
    zlim = c(-1, 1),
    xlab = "Factor", ylab = "",
    axes = FALSE,
    main = "Factor loadings",
    ...
  )
  graphics::axis(1, at = seq_len(n_factors), labels = colnames(ld))
  graphics::axis(2, at = seq_len(n_items), labels = rev(rownames(ld)),
                 las = 2, cex.axis = 0.6)
}

#' @keywords internal
.plot_residuals <- function(x, ...) {
  if (is.null(x$residual)) {
    message("No residual matrix available.")
    return(invisible(NULL))
  }
  off_diag <- x$residual[upper.tri(x$residual)]
  graphics::hist(off_diag, breaks = 50, col = "steelblue",
       main = "Off-diagonal residuals",
       xlab = "Residual correlation",
       ...)
  graphics::abline(v = 0, col = "red", lty = 2)
}
