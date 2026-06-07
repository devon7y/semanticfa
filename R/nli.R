# =============================================================================
# Method reference. Using natural language inference (entailment vs.
# contradiction) to obtain *signed*, valence-aware relations between sentences
# builds on:
#
#   Bowman, S. R., Angeli, G., Potts, C., & Manning, C. D. (2015). A large
#     annotated corpus for learning natural language inference. In Proceedings
#     of the 2015 Conference on Empirical Methods in Natural Language Processing
#     (pp. 632-642). Association for Computational Linguistics.
#     https://doi.org/10.18653/v1/D15-1075
#
# Assigning a NEGATIVE sign to contradictory item pairs to make item similarity
# valence-aware (polarity calibration of NLI relations) follows:
#
#   Hommel, B. E., & Arslan, R. C. (2025). Language Models Accurately Infer
#     Correlations Between Psychological Items and Scales From Text Alone.
#     Advances in Methods and Practices in Psychological Science, 8(4).
#     https://doi.org/10.1177/25152459251377093
# =============================================================================

#' Signed Item Similarity from Natural Language Inference
#'
#' Builds an item-by-item similarity matrix from natural language inference
#' (NLI) rather than cosine similarity. For each ordered item pair the NLI model
#' returns probabilities of \emph{entailment} (E) and \emph{contradiction} (C);
#' the signed relation is \eqn{E - C} (near +1 = same meaning/direction, near
#' -1 = opposite). Unlike plain embeddings --- which place antonyms close
#' because they share a topic --- NLI separates "means the same" from "means the
#' opposite", so reverse-keyed items are handled directly (Bowman et al., 2015;
#' Hommel & Arslan, 2025).
#'
#' The resulting matrix can be passed straight to \code{\link{sfa}} via its
#' \code{similarity} argument.
#'
#' @param items Character vector of item texts.
#' @param model NLI cross-encoder model name (default
#'   \code{"cross-encoder/nli-deberta-v3-base"}), used by the default
#'   \code{classifier}.
#' @param classifier Optional function taking two equal-length character vectors
#'   \code{(premises, hypotheses)} and returning a matrix/data frame with numeric
#'   columns \code{entailment} and \code{contradiction} (one row per pair). These
#'   are typically probabilities, but any finite numeric scores are accepted ---
#'   only the signed difference \code{entailment - contradiction} is used, so the
#'   values need not lie in \code{[0, 1]}. Supply this to use a custom NLI backend
#'   (or for testing); the default uses a \pkg{sentence-transformers}
#'   \code{CrossEncoder} via \pkg{reticulate}.
#' @param symmetric Logical: average the two directions (i,j) and (j,i)
#'   (default \code{TRUE}).
#'
#' @returns A symmetric numeric matrix (n_items x n_items) of signed relations
#'   with 1 on the diagonal and item text as dimnames. With a probability
#'   classifier (the default) the off-diagonal values lie in \eqn{[-1, 1]}
#'   (1 = same direction, -1 = opposite). A custom classifier returning raw
#'   (non-probability) scores may yield values outside \eqn{[-1, 1]}; these are
#'   passed through unchanged, so such a matrix may not be correlation-like and
#'   may need rescaling before \code{sfa(similarity = ...)}.
#'
#' @references
#' Bowman, S. R., Angeli, G., Potts, C., & Manning, C. D. (2015). A large
#' annotated corpus for learning natural language inference. In \emph{Proceedings
#' of the 2015 Conference on Empirical Methods in Natural Language Processing}
#' (pp. 632--642). Association for Computational Linguistics.
#' \doi{10.18653/v1/D15-1075}
#'
#' Hommel, B. E., & Arslan, R. C. (2025). Language models accurately infer
#' correlations between psychological items and scales from text alone.
#' \emph{Advances in Methods and Practices in Psychological Science}, 8(4).
#' \doi{10.1177/25152459251377093}
#'
#' @seealso \code{\link{sfa}}, \code{\link{sfa_similarity}}
#' @examples
#' data(big5)
#' # custom classifier (no Python needed) returning entailment/contradiction probs
#' clf <- function(premise, hypothesis) {
#'   same <- substr(premise, 1, 3) == substr(hypothesis, 1, 3)
#'   data.frame(entailment    = ifelse(same, 0.8, 0.1),
#'              contradiction = ifelse(same, 0.05, 0.5))
#' }
#' M <- sfa_nli_matrix(big5$items[1:6], classifier = clf)
#' round(M, 2)
#'
#' \dontrun{
#' # default backend uses a Python NLI cross-encoder via reticulate:
#' M <- sfa_nli_matrix(big5$items)
#' fit <- sfa(big5$items, similarity = M)
#' }
#' @export
sfa_nli_matrix <- function(items, model = "cross-encoder/nli-deberta-v3-base",
                           classifier = NULL, symmetric = TRUE) {
  items <- as.character(items)
  n <- length(items)
  if (n < 2) stop("Need at least two items.", call. = FALSE)
  if (is.null(classifier)) classifier <- .nli_crossencoder(model)

  # all ordered off-diagonal pairs
  grid <- expand.grid(i = seq_len(n), j = seq_len(n))
  grid <- grid[grid$i != grid$j, , drop = FALSE]
  probs <- classifier(items[grid$i], items[grid$j])
  probs <- as.data.frame(probs)
  if (!all(c("entailment", "contradiction") %in% names(probs))) {
    stop("'classifier' must return columns 'entailment' and 'contradiction'.",
         call. = FALSE)
  }
  if (nrow(probs) != nrow(grid)) {
    stop("'classifier' returned ", nrow(probs), " rows for ", nrow(grid),
         " item pairs; it must return one row per pair.", call. = FALSE)
  }
  if (!is.numeric(probs$entailment) || !is.numeric(probs$contradiction)) {
    stop("'classifier' columns 'entailment'/'contradiction' must be numeric.",
         call. = FALSE)
  }
  signed <- probs$entailment - probs$contradiction
  if (any(!is.finite(signed))) {
    stop("'classifier' produced non-finite entailment/contradiction values.",
         call. = FALSE)
  }

  M <- matrix(0, n, n)
  M[cbind(grid$i, grid$j)] <- signed
  diag(M) <- 1
  if (symmetric) M <- (M + t(M)) / 2
  dimnames(M) <- list(items, items)
  M
}

#' @keywords internal
.nli_crossencoder <- function(model) {
  .sfa_py_require("sentence-transformers")
  st <- tryCatch(reticulate::import("sentence_transformers"),
                 error = function(e) stop(
                   "Python 'sentence-transformers' could not be loaded (",
                   conditionMessage(e), "). Run sfa_install_python(), or pass ",
                   "a 'classifier' function.", call. = FALSE))
  ce <- st$CrossEncoder(model)
  function(premises, hypotheses) {
    pairs <- reticulate::r_to_py(lapply(seq_along(premises),
                                        function(k) list(premises[k], hypotheses[k])))
    logits <- ce$predict(pairs, apply_softmax = TRUE)
    logits <- reticulate::py_to_r(logits)
    logits <- matrix(as.numeric(logits), nrow = length(premises))
    # cross-encoder/nli-* label order: 0 = contradiction, 1 = entailment, 2 = neutral
    data.frame(contradiction = logits[, 1], entailment = logits[, 2])
  }
}
