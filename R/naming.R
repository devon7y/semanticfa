# Factor naming: retrieve a verbal label for each extracted factor from a
# large candidate pool, using instruction-conditioned embeddings.
#
# Method (research repo: FACTOR_NAMING_METHOD_CURRENT.md):
#   1. Re-embed the items under a construct-retrieval instruction.
#   2. Per factor: dominant-pole, loading-weighted centroid of its items'
#      instruction embeddings; subtract the questionnaire grand mean
#      (alpha = 1); renormalize -> the naming target.
#   3. Rank the candidate pool by cosine; walk down, skipping words whose
#      word family was already taken and words that are not label-eligible
#      (the shipped word list is pre-filtered, so eligibility is implicit);
#      keep the top n_candidates.
#   4. Label = first candidate that is a dictionary construct-noun
#      (`tier1`), else the top candidate.
#   5. Duplicate labels across factors resolved by geometric keeper.
#   6. Leave-one-out candidate sets = the method's error bar.

# The naming instruction. Exported through sfa_naming_instruction() so users
# can see (and knowingly override) it. Reworded variants were tested: they
# never move a label to a different construct, only between adjacent
# phrasings of the same one.
.SFA_NAMING_INSTRUCTION <- paste(
  "Given a questionnaire item, retrieve the name of the psychological",
  "construct that the item measures"
)

#' The Default Naming Instruction
#'
#' Returns the instruction string used to embed items for factor naming.
#' @returns A character scalar.
#' @export
sfa_naming_instruction <- function() .SFA_NAMING_INSTRUCTION

#' Name the Factors of an sfa Fit
#'
#' Retrieves a verbal label for every factor of an [sfa()] fit by embedding
#' the fit's items under a construct-retrieval instruction, building one
#' naming target per factor, and ranking a large pre-filtered candidate
#' pool. Deterministic: the same fit, model, and pool always produce the
#' same labels.
#'
#' @param fit An object of class \code{sfa}.
#' @param model Embedding model used for naming. Default \code{NULL} reuses
#'   the model the fit was embedded with. Passing a different model (e.g. a
#'   larger one) switches naming to that model while keeping the fitted
#'   factor structure - extraction and naming reward different model
#'   properties, and a larger namer typically improves label abstraction.
#' @param pool An \code{sfa_pool} object, or \code{NULL} to fetch/build the
#'   pool for \code{model} via [sfa_pool()].
#' @param n_candidates Length of the gated candidate list per factor
#'   (default 5).
#' @param instruction Override the naming instruction (see
#'   [sfa_naming_instruction()]); a warning notes that results were
#'   validated under the default.
#' @param collision Resolve duplicate labels across factors via the
#'   geometric keeper (default \code{TRUE}).
#' @param loo_sets Compute leave-one-out candidate sets (default
#'   \code{TRUE}).
#' @param salient Loading threshold defining a factor's items for naming:
#'   items whose primary factor this is. Reserved for future use; the
#'   assignment rule is primary-loading, as in the research method.
#' @param block_size Pool rows per block during retrieval (memory knob).
#' @param ... Passed to the embedding backend.
#' @returns An object of class \code{sfa_labels}: a data.frame with one row
#'   per factor (\code{factor}, \code{label}, \code{rule},
#'   \code{candidates}, \code{n_items}, \code{collision_moved}) plus
#'   attributes \code{gated}, \code{model}, \code{instruction}.
#' @export
sfa_name <- function(fit,
                     model = NULL,
                     pool = NULL,
                     n_candidates = 5L,
                     instruction = NULL,
                     collision = TRUE,
                     loo_sets = TRUE,
                     salient = NULL,
                     block_size = 50000L,
                     ...) {
  stopifnot(inherits(fit, "sfa"))
  items <- fit$item_data$item
  model <- model %||% fit$embed_model %||% .SFA_DEFAULT_MODEL
  if (!is.null(instruction)) {
    warning("Using a custom naming instruction; labels were validated ",
            "under sfa_naming_instruction().", call. = FALSE)
  }
  instruction <- instruction %||% .SFA_NAMING_INSTRUCTION

  # ---- 1. instruction embeddings (cached through sfa_embed) --------------
  queries <- sprintf("Instruct: %s\nQuery: %s", instruction, items)
  q <- sfa_embed(queries, embed = "sbert", model = model, ...)
  q <- .rows_unit(q)

  # ---- 2. per-factor naming targets ---------------------------------------
  tg <- .sfa_name_targets(fit, q)

  # ---- 3. pool retrieval ---------------------------------------------------
  if (is.null(pool)) pool <- sfa_pool(model)
  if (ncol(tg$targets) != .pool_dimension(pool)) {
    stop("Pool dimension (", .pool_dimension(pool), ") does not match the ",
         "naming model's embedding dimension (", ncol(tg$targets), "). ",
         "Was this pool built with a different model?", call. = FALSE)
  }
  top <- .sfa_pool_top(pool, tg$targets, k = 400L, block_size = block_size)

  # ---- 4. gating + label rule ---------------------------------------------
  gated <- lapply(seq_len(nrow(tg$targets)), function(j) {
    .sfa_gate(pool$words, top$idx[[j]], top$score[[j]], n_keep = 20L)
  })
  names(gated) <- tg$factor_names
  picks <- lapply(gated, .sfa_pick, n_candidates = n_candidates)

  # ---- 5. leave-one-out candidate sets ------------------------------------
  loo <- vector("list", length(gated))
  names(loo) <- tg$factor_names
  if (isTRUE(loo_sets)) {
    for (j in seq_along(gated)) {
      loo[[j]] <- .sfa_loo(fit, q, tg, j, pool, gated[[j]])
    }
  }

  # ---- 6. collision resolution --------------------------------------------
  moved <- rep(FALSE, length(picks))
  if (isTRUE(collision)) {
    res <- .sfa_keeper(picks, gated, loo, n_candidates)
    picks <- res$picks
    moved <- res$moved
  }

  out <- data.frame(
    factor = tg$factor_names,
    label = vapply(picks, function(p) p$label %||% NA_character_,
                   character(1)),
    rule = vapply(picks, function(p) p$rule, character(1)),
    n_items = tg$n_items,
    collision_moved = moved,
    stringsAsFactors = FALSE
  )
  out$candidates <- lapply(seq_along(picks), function(j) {
    cand <- loo[[j]] %||% character(0)
    lab <- out$label[j]
    if (!is.na(lab) && !.fam_of(pool$words, lab) %in%
        vapply(cand, function(w) .fam_of(pool$words, w), character(1))) {
      cand <- c(cand, lab)
    }
    cand
  })

  structure(out,
            gated = gated,
            model = model,
            instruction = instruction,
            class = c("sfa_labels", "data.frame"))
}

#' @keywords internal
.rows_unit <- function(m) {
  nrm <- sqrt(rowSums(m^2))
  nrm[nrm < 1e-12] <- 1
  m / nrm
}

#' @keywords internal
.pool_dimension <- function(pool) {
  if (is.matrix(pool$emb)) ncol(pool$emb) else pool$emb$d
}

# Build per-factor naming targets from the fit's loadings and the
# instruction-embedded item matrix q (rows unit-normalized).
#' @keywords internal
.sfa_name_targets <- function(fit, q) {
  L <- unclass(fit$loadings)
  factor_names <- colnames(L) %||% paste0("F", seq_len(ncol(L)))
  primary <- apply(abs(L), 1L, which.max)
  g <- colMeans(q)
  targets <- matrix(0, nrow = ncol(L), ncol = ncol(q))
  n_items <- integer(ncol(L))
  sel_list <- vector("list", ncol(L))
  w_list <- vector("list", ncol(L))
  for (j in seq_len(ncol(L))) {
    idx <- which(primary == j)
    if (length(idx) == 0L) idx <- seq_len(nrow(L))
    dom <- sign(L[idx[which.max(abs(L[idx, j]))], j])
    if (dom == 0) dom <- 1
    sel <- idx[sign(L[idx, j]) == dom]
    if (length(sel) == 0L) sel <- idx
    w <- abs(L[sel, j]); w <- w / sum(w)
    cen <- colSums(q[sel, , drop = FALSE] * w)
    cen <- cen / sqrt(sum(cen^2))
    r <- cen - g
    targets[j, ] <- r / sqrt(sum(r^2))
    n_items[j] <- length(sel)
    sel_list[[j]] <- sel
    w_list[[j]] <- w
  }
  list(targets = targets, factor_names = factor_names, n_items = n_items,
       sel = sel_list, w = w_list, grand = g)
}

# Blocked top-k retrieval: returns, per target, the pool row indices and
# scores of its k best candidates (descending).
#' @keywords internal
.sfa_pool_top <- function(pool, targets, k = 400L, block_size = 50000L) {
  n <- .pool_nrow(pool$emb)
  nf <- nrow(targets)
  best_idx <- vector("list", nf)
  best_score <- vector("list", nf)
  for (j in seq_len(nf)) {
    best_idx[[j]] <- integer(0)
    best_score[[j]] <- numeric(0)
  }
  i0 <- 0L
  while (i0 < n) {
    i1 <- min(i0 + block_size, n)
    block <- .pool_block(pool$emb, i0, i1)
    s <- block %*% t(targets)                       # (block x nf)
    for (j in seq_len(nf)) {
      sc <- c(best_score[[j]], s[, j])
      ix <- c(best_idx[[j]], (i0 + 1L):i1)
      ord <- order(sc, decreasing = TRUE)[seq_len(min(k, length(sc)))]
      best_score[[j]] <- sc[ord]
      best_idx[[j]] <- ix[ord]
    }
    i0 <- i1
  }
  list(idx = best_idx, score = best_score)
}

# Family-deduplicated gate walk over a ranked candidate list. The shipped
# word list is pre-filtered to label-eligible words, so eligibility is
# implicit; only family dedup happens here.
#' @keywords internal
.sfa_gate <- function(words, idx, score, n_keep = 20L) {
  fam <- words$family[idx]
  keep <- !duplicated(fam)
  take <- which(keep)[seq_len(min(n_keep, sum(keep)))]
  data.frame(
    word = words$word[idx[take]],
    family = fam[take],
    tier1 = as.logical(words$tier1[idx[take]]),
    score = score[take],
    row = idx[take],
    stringsAsFactors = FALSE
  )
}

# The label rule: first tier-1 candidate within the visible window, else
# the top candidate.
#' @keywords internal
.sfa_pick <- function(gate, n_candidates = 5L, excluded = character(0)) {
  g <- gate[!(gate$family %in% excluded), , drop = FALSE]
  win <- utils::head(g, n_candidates)
  hit <- which(win$tier1)
  if (length(hit) > 0L) {
    row <- win[hit[1L], ]
    return(list(label = row$word, family = row$family, score = row$score,
                rule = "tier1"))
  }
  if (nrow(win) > 0L) {
    row <- win[1L, ]
    return(list(label = row$word, family = row$family, score = row$score,
                rule = "top1"))
  }
  list(label = NULL, family = NA_character_, score = -Inf, rule = "exhausted")
}

# Leave-one-out candidate set: drop each of the factor's items in turn,
# rebuild the target, and take the top family among the factor's gated
# shortlist; the set is the union of fold winners (full-sample winner is
# appended by the caller if displaced).
#' @keywords internal
.sfa_loo <- function(fit, q, tg, j, pool, gate) {
  sel <- tg$sel[[j]]
  if (length(sel) < 2L || nrow(gate) == 0L) {
    return(gate$word[1L])
  }
  L <- unclass(fit$loadings)
  cvec <- .pool_rows(pool, gate$row)                 # shortlist vectors
  winners <- integer(0)
  for (drop in seq_along(sel)) {
    keep <- sel[-drop]
    w <- abs(L[keep, j]); w <- w / sum(w)
    cen <- colSums(q[keep, , drop = FALSE] * w)
    cen <- cen / sqrt(sum(cen^2))
    r <- cen - tg$grand
    r <- r / sqrt(sum(r^2))
    winners <- c(winners, which.max(cvec %*% r))
  }
  full <- colSums(q[sel, , drop = FALSE] * tg$w[[j]])
  full <- full / sqrt(sum(full^2))
  r <- full - tg$grand
  r <- r / sqrt(sum(r^2))
  winners <- unique(c(winners, which.max(cvec %*% r)))
  gate$word[sort(winners)]
}

#' @keywords internal
.pool_rows <- function(pool, rows) {
  if (is.matrix(pool$emb)) {
    m <- pool$emb[rows, , drop = FALSE]
    return(.rows_unit(m))
  }
  out <- matrix(0, nrow = length(rows), ncol = pool$emb$d)
  for (i in seq_along(rows)) {
    out[i, ] <- .pool_block(pool$emb, rows[i] - 1L, rows[i])
  }
  out
}

#' @keywords internal
.fam_of <- function(words, w) {
  hit <- match(w, words$word)
  if (is.na(hit)) w else words$family[hit]
}

# Geometric keeper: if two factors pick the same family, the factor with
# the higher retrieval score keeps it; the loser re-picks, preferring its
# leave-one-out set members, then its wider gate, always excluding families
# already claimed against it.
#' @keywords internal
.sfa_keeper <- function(picks, gated, loo, n_candidates, max_iter = 10L) {
  moved <- rep(FALSE, length(picks))
  excluded <- vector("list", length(picks))
  for (it in seq_len(max_iter)) {
    fams <- vapply(picks, function(p) p$family %||% NA_character_,
                   character(1))
    dup <- names(which(table(fams[!is.na(fams)]) > 1L))
    if (length(dup) == 0L) return(list(picks = picks, moved = moved))
    for (f in dup) {
      js <- which(fams == f)
      keeper <- js[which.max(vapply(picks[js], `[[`, numeric(1), "score"))]
      for (j in setdiff(js, keeper)) {
        excluded[[j]] <- c(excluded[[j]], f)
        gate <- gated[[j]]
        loo_gate <- gate[gate$word %in% (loo[[j]] %||% character(0)), ,
                         drop = FALSE]
        p <- .sfa_pick(loo_gate, n_candidates = nrow(loo_gate),
                       excluded = excluded[[j]])
        if (is.null(p$label)) {
          p <- .sfa_pick(gate, n_candidates = 2L * n_candidates,
                         excluded = excluded[[j]])
        }
        picks[[j]] <- p
        moved[j] <- TRUE
      }
    }
  }
  stop("Collision resolution did not converge.", call. = FALSE)
}

#' @export
print.sfa_labels <- function(x, ...) {
  cat("Factor labels (", attr(x, "model"), ")\n\n", sep = "")
  for (i in seq_len(nrow(x))) {
    cand <- x$candidates[[i]]
    extra <- if (length(cand) > 1L) {
      paste0("  [", paste(cand, collapse = ", "), "]")
    } else ""
    moved <- if (isTRUE(x$collision_moved[i])) " *" else ""
    cat(sprintf("  %-12s %s%s%s\n", x$factor[i],
                ifelse(is.na(x$label[i]), "<none>", x$label[i]),
                moved, extra))
  }
  if (any(x$collision_moved)) {
    cat("\n  * relabeled to resolve a duplicate-label collision\n")
  }
  cat("\nLabels name the pole toward which the factor's positive loadings",
      "point.\n")
  invisible(x)
}
