#' Compute Embedding Similarity Matrix
#'
#' Transforms item embeddings into an item-by-item similarity matrix using one
#' of several published methods.
#'
#' @param embeddings Numeric matrix (n_items x embedding_dim).
#' @param encoding Character string specifying the similarity transform:
#'   \code{"atomic"} (default), \code{"atomic_reversed"}, \code{"squid"}, or
#'   \code{"mean_centered_pearson"}. See Details.
#' @param scoring Numeric vector of +1/-1 per item (keying direction). Applies
#'   only to the atomic encodings (Guenole et al.); \code{"squid"} and
#'   \code{"mean_centered_pearson"} are keying-free by design, and passing
#'   \code{scoring} with real reverse-keyed (\code{-1}) items to them is ignored
#'   with a warning. If \code{NULL}, defaults to all +1 (with a message for
#'   \code{"atomic_reversed"}).
#' @param factors Optional character/factor vector of per-item subscale labels.
#'   When supplied it is \emph{recorded} on the returned matrix (as a
#'   \code{"factors"} attribute) so that \code{\link{sfa_corplot}} can group the
#'   items; it does \strong{not} reorder the matrix (rows stay aligned with the
#'   input items).
#' @param codes Optional character vector of short item codes (e.g.
#'   \code{"D3"}, \code{"A2"}). Recorded on the returned matrix (as a
#'   \code{"codes"} attribute) and used as axis labels by
#'   \code{\link{sfa_corplot}}.
#'
#' @details
#' \describe{
#'   \item{\code{"atomic"}}{(default) L2-normalize each embedding, then cosine
#'     similarity. Equivalent to \code{"atomic_reversed"} with all +1 scoring.}
#'   \item{\code{"atomic_reversed"}}{Multiply each embedding by its scoring
#'     direction (+1/-1) first, L2-normalize, then cosine similarity (Guenole et
#'     al.). Use this for scales with reverse-keyed items.}
#'   \item{\code{"squid"}}{Subtract the questionnaire-mean embedding (SQuID;
#'     Pellert et al. 2026), L2-normalize, then cosine similarity. The centering
#'     recovers negative between-dimension correlations, so this encoding is
#'     keying-free (no scoring/sign-flip). Note that Pellert et al. flag
#'     reverse-keyed items as a residual challenge that centering mitigates but
#'     does not fully neutralize.}
#'   \item{\code{"mean_centered_pearson"}}{Mean-center each embedding across its
#'     dimensions, L2-normalize. Cosine similarity then equals Pearson
#'     correlation, yielding a true correlation matrix (the centered-cosine =
#'     Pearson identity is noted by Pokropek 2026; Kmetty et al. 2021; and
#'     Casella et al. 2024). Keying-free.}
#' }
#'
#' @returns A symmetric numeric matrix (n_items x n_items) with 1s on the
#'   diagonal.
#'
#' @references
#' Milano, N., Luongo, M., Ponticorvo, M., & Marocco, D. (2025). Semantic
#' analysis of test items through large language model embeddings predicts
#' a-priori factorial structure of personality tests. \emph{Current Research in
#' Behavioral Sciences}, 8, 100168. \doi{10.1016/j.crbeha.2025.100168}
#'
#' Casella, M., Luongo, M., Marocco, D., Milano, N., & Ponticorvo, M. (2024).
#' LLM embeddings on test items predict post hoc loadings in personality tests.
#' \emph{Ital-IA 2024: 4th National Conference on Artificial Intelligence},
#' CEUR Workshop Proceedings.
#'
#' Guenole, N., D'Urso, E. D., Samo, A., Sun, T., & Haslbeck, J. M. B.
#' (Preprint). Enhancing Scale Development: Pseudo Factor Analysis of Language
#' Embedding Similarity Matrices. OSF. \url{https://osf.io/3mpzb/}
#'
#' Pellert, M., Lechner, C. M., Sen, I., & Strohmaier, M. (2026). Neural network
#' embeddings recover value dimensions from psychometric survey items on par with
#' human data (Survey and Questionnaire Item Embeddings Differentials, SQuID).
#' \emph{Findings of the Association for Computational Linguistics: EACL 2026},
#' 5738--5752.
#'
#' Pokropek, A. (2026). From keyword-based text measures to latent variables:
#' Confirmatory factor analysis with word embeddings. \emph{EPJ Data Science}.
#' \doi{10.1140/epjds/s13688-026-00654-1}
#'
#' Kmetty, Z., Koltai, J., & Rudas, T. (2021). The presence of occupational
#' structure in online texts based on word embedding NLP models. \emph{EPJ Data
#' Science}, 10, 55. \doi{10.1140/epjds/s13688-021-00311-9}
#'
#' @export
sfa_similarity <- function(embeddings, encoding = "atomic",
                           scoring = NULL, factors = NULL, codes = NULL) {
  # accept a fitted sfa object: return its already-built similarity matrix, with
  # factor/code attributes attached for grouping/labelling in plots
  if (inherits(embeddings, "sfa")) {
    fit <- embeddings
    sim <- fit$sim_matrix
    if (!is.null(fit$item_data$factor)) {
      attr(sim, "factors") <- as.character(fit$item_data$factor)
    }
    if (!is.null(fit$item_data$code)) {
      attr(sim, "codes") <- as.character(fit$item_data$code)
    }
    return(sim)
  }
  # accept a loaded sfa_embeddings object (from sfa_load_npz); explicit
  # scoring/factors/codes args still take precedence
  if (inherits(embeddings, "sfa_embeddings")) {
    obj <- embeddings
    if (is.null(scoring)) scoring <- obj$scoring
    if (is.null(factors)) factors <- obj$factors
    if (is.null(codes))   codes   <- obj$codes
    embeddings <- obj$embeddings
  }
  encoding <- match.arg(encoding,
    c("atomic_reversed", "atomic", "squid", "mean_centered_pearson"))

  embeddings <- as.matrix(embeddings)
  if (!is.numeric(embeddings) || length(dim(embeddings)) != 2L) {
    stop("'embeddings' must be a numeric matrix (n_items x dim).", call. = FALSE)
  }
  if (nrow(embeddings) < 2L || ncol(embeddings) < 1L) {
    stop("'embeddings' must have at least 2 rows and 1 column (got ",
         nrow(embeddings), " x ", ncol(embeddings), ").", call. = FALSE)
  }
  if (anyNA(embeddings) || any(!is.finite(embeddings))) {
    stop("'embeddings' contains non-finite values (NA/NaN/Inf).", call. = FALSE)
  }

  n_items <- nrow(embeddings)
  if (!is.null(factors) && length(factors) != n_items) {
    stop("'factors' must have one entry per item (", n_items, ").",
         call. = FALSE)
  }
  if (!is.null(codes) && length(codes) != n_items) {
    stop("'codes' must have one entry per item (", n_items, ").",
         call. = FALSE)
  }

  # Item keying (scoring) applies only to the atomic encodings (Guenole et al.).
  # SQuID and mean-centered Pearson are keying-free by design. Only warn when
  # real reverse-keyed items (-1) would be silently ignored.
  if (!is.null(scoring) && any(as.numeric(scoring) < 0) &&
      !encoding %in% c("atomic", "atomic_reversed")) {
    warning("'scoring' has reverse-keyed items but is ignored for encoding = \"",
            encoding, "\": this method is keying-free by design. Use ",
            "\"atomic_reversed\" for keyed sign-flipping.", call. = FALSE)
  }

  transformed <- switch(encoding,
    atomic          = .apply_atomic_reversed(embeddings, rep(1, n_items)),
    atomic_reversed = .apply_atomic_reversed(
                        embeddings,
                        .resolve_scoring(scoring, n_items, "atomic_reversed")),
    squid           = .apply_squid(embeddings),
    mean_centered_pearson = .apply_mean_centered_pearson(embeddings)
  )

  sim <- tcrossprod(transformed)
  diag(sim) <- 1.0
  sim <- (sim + t(sim)) / 2
  if (any(!is.finite(sim))) {
    stop("Similarity matrix contains non-finite values; check for degenerate ",
         "(e.g. all-zero or collinear) item embeddings.", call. = FALSE)
  }

  attr(sim, "transformed_embeddings") <- transformed
  if (!is.null(factors)) attr(sim, "factors") <- as.character(factors)
  if (!is.null(codes)) attr(sim, "codes") <- as.character(codes)
  sim
}

# --- internal transforms ported from qwen3_efa_v2.py ---

#' @keywords internal
.apply_atomic_reversed <- function(embeddings, scoring) {
  scoring_vec <- as.numeric(scoring)
  signed <- embeddings * scoring_vec
  norms <- sqrt(rowSums(signed^2))
  zero_norm <- !is.finite(norms) | norms == 0
  if (any(zero_norm)) {
    warning(sum(zero_norm), " item(s) have a zero-norm embedding; they are ",
            "returned as zero vectors (cosine 0 to all other items).",
            call. = FALSE)
    norms[zero_norm] <- 1   # leave the row as a zero vector -> cosine 0, not NaN
  }
  signed / norms
}

#' @keywords internal
# SQuID (Pellert et al. 2026): subtract the questionnaire-mean embedding from
# each item, then cosine. Keying-free by design -- the centering is what
# recovers negative correlations, so no scoring/sign-flip is applied.
.apply_squid <- function(embeddings) {
  centroid <- colMeans(embeddings)
  centered <- sweep(embeddings, 2, centroid)
  norms <- sqrt(rowSums(centered^2))
  norms[norms == 0] <- 1
  centered / norms
}

#' @keywords internal
# Mean-centered cosine = Pearson correlation between item embedding vectors
# (Pokropek 2026; Kmetty et al. 2021). Keying-free: no scoring/sign-flip.
.apply_mean_centered_pearson <- function(embeddings) {
  storage.mode(embeddings) <- "double"
  row_means <- rowMeans(embeddings)
  x_centered <- embeddings - row_means
  norms <- sqrt(rowSums(x_centered^2))
  norms[norms == 0] <- 1
  x_centered / norms
}
