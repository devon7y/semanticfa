# =============================================================================
# Method reference. Response-free, embedding-based short-form item selection is
# in the spirit of (but not a reimplementation of) the response-free
# simplification work of:
#
#   Wang, B., Zhang, Y., Hu, Y., Hou, H., Peng, K., & Ni, S. (2026).
#     Discovering semantic latent structures in psychological scales: A
#     response-free pathway to efficient simplification [Preprint].
#     arXiv:2602.12575.
#
#   Jung, S.-J., & Seo, J.-W. (2025). A transformer-based embedding approach to
#     developing short-form psychological measures. Frontiers in Psychology, 16,
#     Article 1640864. https://doi.org/10.3389/fpsyg.2025.1640864
#
# Note on faithfulness: this function selects items by centroid/medoid proximity
# within a supplied or fitted grouping. It does NOT implement Wang et al.'s
# specific pipeline (LLM embedding -> UMAP -> HDBSCAN density clustering ->
# c-TF-IDF), which discovers the number and composition of factors with no
# predefined count; nor Jung & Seo's K-Means partitioning. The shared idea is
# response-free, embedding-based item reduction. Per Wang et al., outputs are
# CANDIDATE short forms that still require psychometric validation.
# =============================================================================

#' Response-Free Scale Simplification
#'
#' Selects a reduced (short-form) item set per group using only the items'
#' semantic structure --- no human response data --- and reports how well the
#' reduced set preserves the factor structure of the full scale (in the spirit of
#' Wang et al., 2026; Jung & Seo, 2025). It selects items by centroid/medoid
#' proximity within a grouping, rather than reimplementing those papers' specific
#' clustering pipelines. The output is a \strong{candidate} short form that
#' should be validated psychometrically before use.
#'
#' Two selection strategies are offered:
#' \describe{
#'   \item{\code{"anchor"}}{(default) Keep the items most similar to their own
#'     group's centroid (un-flipped, leave-one-out; see \code{\link{sfa_anchor}});
#'     drop the weakest. Simple and interpretable, but can retain near-duplicate
#'     items (see \code{\link{sfa_redundancy}}).}
#'   \item{\code{"medoid"}}{Within each group, greedily select items that are
#'     both representative (close to the group centroid) and non-redundant
#'     (spread apart in embedding space). Trades a little central tendency for
#'     broader coverage.}
#' }
#'
#' After selection the scale is re-fit on the kept items and compared with the
#' full-scale solution: number of factors retained and structure recovery
#' against the theoretical grouping (NMI and ARI).
#'
#' @param x An object of class \code{"sfa"} with stored input embeddings (fit
#'   with this version of \code{sfa()}).
#' @param target_n Integer number of items to keep per group. Groups with
#'   \code{<= target_n} items are kept in full.
#' @param method \code{"anchor"} (default) or \code{"medoid"}.
#' @param groups How items are grouped before trimming: \code{"theoretical"}
#'   (default; the \code{factor} labels supplied to \code{sfa()}) or
#'   \code{"fitted"} (each item assigned to its strongest extracted factor ---
#'   lets the groups emerge from the items, after Jung & Seo 2025, and needs no
#'   theoretical key).
#' @param ... Currently unused.
#'
#' @returns An object of class \code{"sfa_simplify"}: a list with \code{keep}
#'   (kept item codes), \code{drop} (dropped items with reasons), the re-fit
#'   \code{reduced_fit}, and a \code{fidelity} report.
#'
#' @references
#' Wang, B., Zhang, Y., Hu, Y., Hou, H., Peng, K., & Ni, S. (2026). Discovering
#' semantic latent structures in psychological scales: A response-free pathway
#' to efficient simplification. arXiv:2602.12575 (preprint).
#'
#' Jung, S.-J., & Seo, J.-W. (2025). A transformer-based embedding approach to
#' developing short-form psychological measures. \emph{Frontiers in Psychology},
#' 16, Article 1640864. \doi{10.3389/fpsyg.2025.1640864}
#'
#' @seealso \code{\link{sfa_anchor}}, \code{\link{sfa_redundancy}},
#'   \code{\link{sfa_congruence}}
#' @examples
#' data(big5)
#' fit <- sfa(
#'   data.frame(code = big5$codes, item = big5$items,
#'              factor = big5$factors, scoring = big5$scoring),
#'   embeddings = big5$embeddings, scoring = big5$scoring, nfactors = 5)
#'
#' # keep the 5 most representative items per construct
#' short <- sfa_simplify(fit, target_n = 5, method = "anchor")
#' short$keep
#'
#' # group by the fitted factors instead of the supplied key (needs no labels)
#' sfa_simplify(fit, target_n = 5, groups = "fitted")$keep
#' @export
sfa_simplify <- function(x, target_n, method = c("anchor", "medoid"),
                         groups = c("theoretical", "fitted"), ...) {
  if (!inherits(x, "sfa")) stop("'x' must be an 'sfa' object.", call. = FALSE)
  method <- match.arg(method)
  groups <- match.arg(groups)
  target_n <- as.integer(target_n)
  if (is.na(target_n) || target_n < 1L) {
    stop("'target_n' must be a positive integer.", call. = FALSE)
  }

  raw <- x$input_embeddings
  if (is.null(raw)) {
    stop("'x' lacks stored input embeddings; re-fit with this version of sfa().",
         call. = FALSE)
  }
  codes   <- x$item_data$code
  items   <- x$item_data$item
  scoring <- x$item_data$scoring
  theo    <- x$item_data$factor

  # --- grouping: theoretical labels, or the fitted factor solution (Jung & Seo
  #     2025: let the groups emerge from the items instead of the assumed key) ---
  grouping <- switch(groups,
    theoretical = {
      if (is.null(theo)) {
        stop("groups = 'theoretical' needs a 'factor' column on the fit; ",
             "use groups = 'fitted' to group by the extracted factors.",
             call. = FALSE)
      }
      theo
    },
    fitted = .assign_items(unclass(x$loadings))[codes]
  )
  ref_labels <- theo %||% grouping        # structure target for the fidelity report

  # --- selection in the raw (un-flipped) embedding space, matching sfa_anchor;
  #     independent of the encoding the fit happened to use ---
  sc <- if (is.null(scoring)) rep(1, length(codes)) else scoring
  aligned <- as.matrix(raw)
  aligned <- aligned / sqrt(rowSums(aligned^2))
  own_sim <- NULL
  if (method == "anchor") {
    M <- .anchor_centroid(aligned, grouping, unique(grouping))
    own <- match(grouping, colnames(M))
    own_sim <- M[cbind(seq_along(grouping), own)]
    keep_idx <- .select_top_per_group(own_sim, grouping, target_n)
  } else {
    keep_idx <- .select_medoids_per_group(aligned, grouping, target_n)
  }
  keep_idx <- sort(keep_idx)
  drop_idx <- setdiff(seq_along(codes), keep_idx)

  # --- drop reasons ---
  reason <- rep("not selected (redundant/peripheral)", length(drop_idx))
  if (method == "anchor") {
    reason <- sprintf("low own-group similarity (%.2f)", own_sim[drop_idx])
  }
  drop_df <- data.frame(code = codes[drop_idx], group = grouping[drop_idx],
                        reason = reason, stringsAsFactors = FALSE)

  # --- refit on kept items ---
  nfm <- if (!is.null(x$Call$n_factors_method)) {
    as.character(x$Call$n_factors_method)
  } else "parallel"
  seed <- if (!is.null(x$Call$seed)) eval(x$Call$seed) else 42L

  df <- data.frame(code = codes[keep_idx], item = items[keep_idx],
                   factor = ref_labels[keep_idx], scoring = sc[keep_idx],
                   stringsAsFactors = FALSE)
  reduced <- suppressWarnings(suppressMessages(
    sfa(df, embeddings = raw[keep_idx, , drop = FALSE],
        scoring = sc[keep_idx], encoding = x$encoding,
        n_factors_method = nfm, seed = seed)
  ))

  # --- fidelity (vs the theoretical structure when available, else the
  #     grouping that drove selection) ---
  full_cong <- suppressWarnings(
    sfa_congruence(x, target = ref_labels, metrics = c("nmi", "ari")))
  red_cong <- suppressWarnings(
    sfa_congruence(reduced, target = ref_labels[keep_idx],
                   metrics = c("nmi", "ari")))
  per_con <- vapply(unique(grouping),
                    function(g) sum(grouping[keep_idx] == g), integer(1))

  fidelity <- list(
    n_full = length(codes), n_reduced = length(keep_idx),
    nfactors_full = x$factors, nfactors_reduced = reduced$factors,
    nmi_full = full_cong$nmi, nmi_reduced = red_cong$nmi,
    ari_full = full_cong$ari, ari_reduced = red_cong$ari,
    constructs = unique(grouping), per_construct = per_con
  )

  structure(list(
    keep = codes[keep_idx], drop = drop_df,
    method = method, groups = groups, target_n = target_n,
    fidelity = fidelity, reduced_fit = reduced
  ), class = "sfa_simplify")
}

#' @keywords internal
.select_top_per_group <- function(score, groups, target_n) {
  keep <- logical(length(score))
  for (g in unique(groups)) {
    idx <- which(groups == g)
    k <- min(target_n, length(idx))
    ord <- idx[order(score[idx], decreasing = TRUE)]
    keep[ord[seq_len(k)]] <- TRUE
  }
  which(keep)
}

#' @keywords internal
.select_medoids_per_group <- function(emb, groups, target_n) {
  keep <- logical(nrow(emb))
  for (g in unique(groups)) {
    idx <- which(groups == g)
    if (length(idx) <= target_n) { keep[idx] <- TRUE; next }
    sub <- emb[idx, , drop = FALSE]          # rows already unit-norm
    cen <- colMeans(sub)
    cen <- cen / sqrt(sum(cen^2))
    sel <- which.max(as.numeric(sub %*% cen)) # seed: most representative
    while (length(sel) < target_n) {
      remaining <- setdiff(seq_len(nrow(sub)), sel)
      maxcos <- apply(sub[remaining, , drop = FALSE] %*% t(sub[sel, , drop = FALSE]),
                      1, max)
      sel <- c(sel, remaining[which.min(maxcos)]) # most distant from selected
    }
    keep[idx[sel]] <- TRUE
  }
  which(keep)
}

#' @export
print.sfa_simplify <- function(x, ...) {
  f <- x$fidelity
  cat("Scale simplification (response-free)\n")
  cat("  Method: centroid/medoid item selection",
      "(in the spirit of Wang et al. 2026; Jung & Seo 2025)\n")
  cat("  Note: candidate short form -- validate psychometrically before use\n")
  cat(sprintf("  Selection: %s | groups: %s | target_n = %d per group\n",
              x$method, x$groups, x$target_n))
  cat(sprintf("  Items: %d -> %d\n", f$n_full, f$n_reduced))
  cat(sprintf("  Factors retained (parallel analysis): %d -> %d\n",
              f$nfactors_full, f$nfactors_reduced))
  cat("  Structure recovery vs theory (NMI / ARI):\n")
  cat(sprintf("    full:    NMI=%.3f  ARI=%.3f\n",
              f$nmi_full %||% NA, f$ari_full %||% NA))
  cat(sprintf("    reduced: NMI=%.3f  ARI=%.3f\n",
              f$nmi_reduced %||% NA, f$ari_reduced %||% NA))
  cat("  Items kept per construct:\n")
  for (i in seq_along(f$constructs)) {
    cat(sprintf("    %-20s %d\n", f$constructs[i], f$per_construct[i]))
  }
  cat(sprintf("\n  Dropped %d item(s); see $drop. Reduced fit in $reduced_fit.\n",
              nrow(x$drop)))
  invisible(x)
}
