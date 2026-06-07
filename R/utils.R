#' @importFrom GPArotation oblimin
#' @importFrom utils head
#' @importFrom stats cor quantile rnorm runif setNames
#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a

#' @keywords internal
.regularize_corr <- function(corr_matrix, alpha = 1e-6) {
  n <- nrow(corr_matrix)
  reg <- corr_matrix + alpha * diag(n)
  d <- sqrt(diag(reg))
  reg <- reg / outer(d, d)
  dimnames(reg) <- dimnames(corr_matrix)
  reg
}

#' @keywords internal
.check_heywood <- function(communalities) {
  hw <- communalities > 1.0
  if (any(hw)) {
    n_hw <- sum(hw)
    names_hw <- names(communalities)[hw]
    if (is.null(names_hw)) names_hw <- which(hw)
    warning(
      n_hw, " item(s) have communality > 1 (Heywood cases): ",
      paste(names_hw, collapse = ", "),
      ". Consider reducing nfactors or using encoding = 'mean_centered_pearson'.",
      call. = FALSE
    )
  }
  hw
}

#' @keywords internal
.check_psd <- function(mat, alpha = 1e-6) {
  eig <- eigen(mat, symmetric = TRUE)
  min_eig <- min(eig$values)
  if (min_eig < -1e-10) {
    message(
      "Similarity matrix was not positive semi-definite (min eigenvalue = ",
      format(min_eig, digits = 3),
      "); projected to a nearby positive semi-definite correlation matrix ",
      "(negative eigenvalues clipped, diagonal rescaled to 1)."
    )
    # Clip negative eigenvalues to a small positive floor and reconstruct
    # (V diag(vals) V'), then rescale the diagonal back to 1. A fixed ridge
    # cannot repair a matrix whose minimum eigenvalue is far below zero.
    vals <- pmax(eig$values, alpha)
    psd <- eig$vectors %*% (vals * t(eig$vectors))
    d <- sqrt(diag(psd))
    psd <- psd / outer(d, d)
    dimnames(psd) <- dimnames(mat)
    mat <- psd
  }
  mat
}

#' @keywords internal
.resolve_items <- function(items, scoring = NULL, embeddings = NULL) {
  if (is.data.frame(items)) {
    item_col <- if ("item" %in% names(items)) "item"
                else if ("text" %in% names(items)) "text"
                else stop("data.frame must have an 'item' or 'text' column.", call. = FALSE)
    item_text <- as.character(items[[item_col]])
    codes <- if ("code" %in% names(items)) as.character(items[["code"]])
             else sprintf("item_%02d", seq_along(item_text))
    factors <- if ("factor" %in% names(items)) as.character(items[["factor"]]) else NULL
    if (is.null(scoring) && "scoring" %in% names(items)) {
      scoring <- as.numeric(items[["scoring"]])
    }
  } else if (is.character(items)) {
    item_text <- items
    codes <- if (!is.null(names(items))) names(items)
             else sprintf("item_%02d", seq_along(items))
    factors <- NULL
  } else {
    stop("'items' must be a character vector or data.frame.", call. = FALSE)
  }

  if (!is.null(embeddings)) {
    if (!is.matrix(embeddings) || !is.numeric(embeddings)) {
      stop("'embeddings' must be a numeric matrix.", call. = FALSE)
    }
    if (nrow(embeddings) != length(item_text)) {
      stop(
        "Number of rows in 'embeddings' (", nrow(embeddings),
        ") must match number of items (", length(item_text), ").",
        call. = FALSE
      )
    }
  }

  list(
    items = item_text,
    codes = codes,
    factors = factors,
    scoring = scoring
  )
}

#' @keywords internal
.resolve_scoring <- function(scoring, n_items, encoding) {

  if (is.null(scoring)) {
    if (encoding == "atomic_reversed") {
      message(
        "No scoring provided; defaulting to all +1 ",
        "(equivalent to 'atomic' encoding)."
      )
    }
    return(rep(1, n_items))
  }
  scoring <- as.numeric(scoring)
  if (length(scoring) != n_items) {
    stop(
      "Length of 'scoring' (", length(scoring),
      ") must match number of items (", n_items, ").",
      call. = FALSE
    )
  }
  if (!all(scoring %in% c(-1, 1))) {
    stop("All 'scoring' values must be +1 or -1.", call. = FALSE)
  }
  scoring
}
