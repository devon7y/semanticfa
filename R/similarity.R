#' Compute Embedding Similarity Matrix
#'
#' Transforms item embeddings into an item-by-item similarity matrix using one
#' of several published methods.
#'
#' @param embeddings Numeric matrix (n_items x embedding_dim).
#' @param encoding Character string specifying the similarity transform:
#'   \code{"atomic_reversed"} (default), \code{"atomic"}, \code{"squid"}, or
#'   \code{"mean_centered_pearson"}. See Details.
#' @param scoring Numeric vector of +1/-1 per item (keying direction). If
#'   \code{NULL}, defaults to all +1 with an informative message when
#'   \code{encoding} is \code{"atomic_reversed"} or \code{"squid"}.
#'
#' @details
#' \describe{
#'   \item{\code{"atomic_reversed"}}{Multiply each embedding by its scoring
#'     direction, L2-normalize, then compute cosine similarity (Guenole et al.).}
#'   \item{\code{"atomic"}}{L2-normalize without sign-flipping, then cosine
#'     similarity. Equivalent to \code{"atomic_reversed"} with all +1 scoring.}
#'   \item{\code{"squid"}}{Subtract the questionnaire-mean embedding (SQuID;
#'     Pellert et al. 2026), apply scoring sign-flip, L2-normalize, then cosine
#'     similarity. Recovers negative between-dimension correlations.}
#'   \item{\code{"mean_centered_pearson"}}{Apply scoring, mean-center each
#'     embedding across its dimensions, L2-normalize. Cosine similarity then
#'     equals Pearson correlation, yielding a true correlation matrix (Pokropek
#'     2026; Kmetty et al. 2021).}
#' }
#'
#' @returns A symmetric numeric matrix (n_items x n_items) with 1s on the
#'   diagonal.
#'
#' @references
#' Guenole, N., D'Urso, E. D., Samo, A., & Sun, T. (2024). Pseudo Factor
#' Analysis of Language Embedding Similarity Matrices.
#'
#' Pellert, M., et al. (2026). SQuID: Semantic Questionnaire Item
#' Decomposition.
#'
#' Pokropek, A. (2026). CFA with word embeddings.
#'
#' Kmetty, Z., et al. (2021). Mean-centered cosine as Pearson correlation.
#'
#' @export
sfa_similarity <- function(embeddings, encoding = "atomic_reversed",
                           scoring = NULL) {
  encoding <- match.arg(encoding,
    c("atomic_reversed", "atomic", "squid", "mean_centered_pearson"))

  n_items <- nrow(embeddings)
  scoring <- .resolve_scoring(scoring, n_items, encoding)

  if (encoding == "atomic") {
    scoring <- rep(1, n_items)
    encoding <- "atomic_reversed"
  }

  transformed <- switch(encoding,
    atomic_reversed = .apply_atomic_reversed(embeddings, scoring),
    squid           = .apply_squid(embeddings, scoring),
    mean_centered_pearson = .apply_mean_centered_pearson(embeddings, scoring)
  )

  sim <- tcrossprod(transformed)
  diag(sim) <- 1.0
  sim <- (sim + t(sim)) / 2

  attr(sim, "transformed_embeddings") <- transformed
  sim
}

# --- internal transforms ported from qwen3_efa_v2.py ---

#' @keywords internal
.apply_atomic_reversed <- function(embeddings, scoring) {
  scoring_vec <- as.numeric(scoring)
  signed <- embeddings * scoring_vec
  norms <- sqrt(rowSums(signed^2))
  zero_norm <- norms == 0
  if (any(zero_norm)) {
    warning(sum(zero_norm), " item(s) have zero norm after sign-flipping; ",
            "using original embeddings for those items.", call. = FALSE)
    signed[zero_norm, ] <- embeddings[zero_norm, ]
    norms[zero_norm] <- sqrt(rowSums(embeddings[zero_norm, , drop = FALSE]^2))
  }
  signed / norms
}

#' @keywords internal
.apply_squid <- function(embeddings, scoring) {
  centroid <- colMeans(embeddings)
  centered <- sweep(embeddings, 2, centroid)
  scoring_vec <- as.numeric(scoring)
  centered <- centered * scoring_vec
  norms <- sqrt(rowSums(centered^2))
  norms[norms == 0] <- 1
  centered / norms
}

#' @keywords internal
.apply_mean_centered_pearson <- function(embeddings, scoring) {
  x <- embeddings * as.numeric(scoring)
  storage.mode(x) <- "double"
  row_means <- rowMeans(x)
  x_centered <- x - row_means
  norms <- sqrt(rowSums(x_centered^2))
  norms[norms == 0] <- 1
  x_centered / norms
}
