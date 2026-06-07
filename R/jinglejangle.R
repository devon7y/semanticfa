# =============================================================================
# Method reference. The embedding-based jingle-jangle / taxonomic-
# incommensurability detection implemented here is from:
#
#   Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#     taxonomic incommensurability in psychological measurement. Nature Human
#     Behaviour, 9(5), 944-954. https://doi.org/10.1038/s41562-024-02089-y
#
#   Wulff, D. U., & Mata, R. (2026). Escaping the jingle-jangle jungle:
#     Increasing conceptual clarity in psychology using large language models.
#     Current Directions in Psychological Science. Advance online publication.
#     https://doi.org/10.1177/09637214251382083
# =============================================================================

#' Detect Jingle and Jangle Fallacies Across Scales
#'
#' Compares whole scales by the meaning of their items versus the meaning of
#' their names to surface two classic measurement problems (Wulff & Mata, 2025,
#' 2026): \strong{jingle} (scales with similar \emph{names} but dissimilar
#' \emph{content}) and \strong{jangle} (scales with dissimilar names but similar
#' content).
#'
#' Each scale is represented by a content vector (the mean of its item
#' embeddings) and a label vector (the embedding of its name). For every pair of
#' scales the function compares content similarity with label similarity; large
#' divergences flag the two fallacies.
#'
#' @param scales A named list; each element is a character vector of the scale's
#'   item texts. The names are used as scale labels unless \code{labels} is given.
#' @param labels Optional character vector of scale names (construct labels), one
#'   per scale, overriding the list names.
#' @param embed,model Embedding backend and model (default the package default
#'   sbert model).
#' @param flag Absolute content-minus-label similarity difference at which to
#'   flag a pair (default 0.20).
#' @param item_embeddings,label_embeddings Optional precomputed embeddings: a
#'   named list of per-scale item-embedding matrices, and a matrix of label
#'   embeddings (one row per scale). Use when no embedding backend is available.
#'
#' @returns An object of class \code{"sfa_jinglejangle"}: a list with the
#'   \code{content_sim} and \code{label_sim} scale-by-scale matrices and a
#'   \code{flags} data frame (scale_a, scale_b, content_sim, label_sim,
#'   divergence, type).
#'
#' @references
#' Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
#' taxonomic incommensurability in psychological measurement. \emph{Nature Human
#' Behaviour}, 9(5), 944--954. \doi{10.1038/s41562-024-02089-y}
#'
#' Wulff, D. U., & Mata, R. (2026). Escaping the jingle-jangle jungle:
#' Increasing conceptual clarity in psychology using large language models.
#' \emph{Current Directions in Psychological Science}. Advance online
#' publication. \doi{10.1177/09637214251382083}
#'
#' @seealso \code{\link{sfa_anchor}}
#' @examples
#' data(big5)
#' scales <- list(
#'   Extraversion = big5$items[big5$factors == "Extraversion"],
#'   Sociability  = big5$items[big5$factors == "Extraversion"],  # same content, new name
#'   Neuroticism  = big5$items[big5$factors == "Neuroticism"])
#'
#' # precomputed embeddings so the example needs no backend
#' ie <- lapply(scales, function(items)
#'   big5$embeddings[match(items, big5$items), , drop = FALSE])
#' le <- big5$embeddings[match(c("E1", "C31", "N11"), big5$codes), , drop = FALSE]
#' sfa_jinglejangle(scales, item_embeddings = ie, label_embeddings = le)
#'
#' \dontrun{
#' # with a live backend, pass the scales and their names are embedded directly:
#' sfa_jinglejangle(scales)
#' }
#' @export
sfa_jinglejangle <- function(scales, labels = NULL, embed = "sbert",
                             model = NULL, flag = 0.20,
                             item_embeddings = NULL, label_embeddings = NULL) {
  if (!is.list(scales) || length(scales) < 2) {
    stop("'scales' must be a named list of at least two scales.", call. = FALSE)
  }
  scale_names <- names(scales)
  if (is.null(scale_names) || any(scale_names == "")) {
    stop("'scales' must be a named list.", call. = FALSE)
  }
  if (is.null(labels)) labels <- scale_names
  model <- model %||% .SFA_DEFAULT_MODEL

  # content vectors (mean item embedding per scale)
  content_list <- if (!is.null(item_embeddings)) {
    lapply(scale_names, function(s) colMeans(as.matrix(item_embeddings[[s]])))
  } else {
    lapply(scale_names, function(s)
      colMeans(sfa_embed(scales[[s]], embed = embed, model = model)))
  }
  content <- do.call(rbind, content_list)
  rownames(content) <- scale_names

  # label vectors
  if (!is.null(label_embeddings)) {
    lab <- as.matrix(label_embeddings)
  } else {
    lab <- sfa_embed(labels, embed = embed, model = model)
  }
  rownames(lab) <- scale_names

  content_sim <- .cos_matrix(content)
  label_sim <- .cos_matrix(lab)

  ut <- which(upper.tri(content_sim), arr.ind = TRUE)
  div <- content_sim[ut] - label_sim[ut]
  type <- ifelse(div >= flag, "jangle",
                 ifelse(div <= -flag, "jingle", "-"))
  flags <- data.frame(
    scale_a = scale_names[ut[, 1]], scale_b = scale_names[ut[, 2]],
    content_sim = round(content_sim[ut], 3), label_sim = round(label_sim[ut], 3),
    divergence = round(div, 3), type = type, stringsAsFactors = FALSE
  )
  flags <- flags[order(-abs(flags$divergence)), ]

  structure(list(content_sim = content_sim, label_sim = label_sim,
                 flags = flags, flag_threshold = flag),
            class = "sfa_jinglejangle")
}

#' @keywords internal
.cos_matrix <- function(m) {
  u <- m / sqrt(rowSums(m^2))
  s <- tcrossprod(u)
  diag(s) <- 1
  (s + t(s)) / 2
}

#' @export
print.sfa_jinglejangle <- function(x, ...) {
  cat("Jingle-jangle detection across", nrow(x$content_sim), "scales\n")
  cat("  Method: Wulff & Mata (2025, 2026)\n")
  cat(sprintf("  Flag threshold |content - label| >= %.2f\n", x$flag_threshold))
  flagged <- x$flags[x$flags$type != "-", ]
  if (nrow(flagged) == 0) {
    cat("\n  No jingle/jangle pairs flagged.\n")
  } else {
    cat(sprintf("\n  %-12s %-12s content label  div    type\n", "scale_a", "scale_b"))
    for (i in seq_len(nrow(flagged))) {
      r <- flagged[i, ]
      cat(sprintf("  %-12s %-12s %5.2f  %5.2f  %+5.2f  %s\n",
                  r$scale_a, r$scale_b, r$content_sim, r$label_sim,
                  r$divergence, r$type))
    }
    cat("\n  jangle = same content, different name; ",
        "jingle = same name, different content\n", sep = "")
  }
  invisible(x)
}
