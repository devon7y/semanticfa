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

#' @export
print.sfa <- function(x, cutoff = 0.3, sort = TRUE, ...) {
  cat("Semantic Factor Analysis\n")
  cat("  Encoding:", x$encoding, "\n")
  if (!is.null(x$embed_model)) cat("  Model:", x$embed_model, "\n")
  if (!is.null(x$embedding_dim)) cat("  Embedding dim:", x$embedding_dim, "\n")
  cat("  Factors:", x$factors, " (", x$fm, " + ", x$rotation, ")\n", sep = "")
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
    cat(paste0(" (", label, ")"))
  }
  cat("\n")
  if (!is.null(x$tefi))
    cat(sprintf("  TEFI: %.4f\n", x$tefi))
  if (!is.null(x$rmsr))
    cat(sprintf("  RMSR: %.4f\n", x$rmsr))
  if (!is.null(x$caf) && !is.na(x$caf))
    cat(sprintf("  CAF:  %.4f\n", x$caf))

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
plot.sfa <- function(x, type = c("scree", "loadings", "residuals"), ...) {
  type <- match.arg(type)

  switch(type,
    scree = .plot_scree(x, ...),
    loadings = .plot_loadings(x, ...),
    residuals = .plot_residuals(x, ...)
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
