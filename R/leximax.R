# Leximax: lexical target rotation. Rotates factor axes to maximize
# nameability -- the mean cosine between each factor's naming target and its
# retrieved construct term -- by alternating the package's deterministic
# naming machinery (sfa_name conventions) with oblique target rotation
# (GPArotation). Fit is invariant throughout: every candidate orientation is
# an admissible rotation of the same fitted solution.
#
# Three entry points:
#   * sfa(..., rotate = "leximax")  -- leximax as a rotation option (sfa.R)
#   * sfa_leximax()                 -- post-hoc rotation of an existing fit
#                                      or loading matrix (any EFA)
#   * sfa_lexmap() + sfa_nameability() -- the reusable pool-by-item map and
#                                      the nameability of any orientation
#
# Retrieval inside the optimizer runs in item-coefficient space: every
# naming target lies in the span of the unit item embeddings, so with
# S = pool %*% t(q_unit) (one pool pass) and the item Gram matrix, all
# retrieval scores are exact inner products up to floating-point
# associativity. The converged solution is re-named through the canonical
# blocked pool path (identical to sfa_name's arithmetic) and the canonical
# labels are authoritative.

# ---- the lexical map ---------------------------------------------------------

#' Build the Lexical Map for Leximax Rotation
#'
#' Precomputes everything leximax needs to score orientations of one
#' instrument: the instruction-conditioned item embeddings, their Gram
#' matrix, and the pool-by-item cosine matrix S. Building the map costs one
#' pass over the candidate pool; every subsequent naming of any orientation
#' of the same items is then nearly instant, which is what makes multi-start
#' optimization and Monte Carlo calibration practical.
#'
#' @param x An [sfa()] fit, or a character vector of item texts.
#' @param model Naming model id. Default \code{NULL} follows [sfa_name()]:
#'   the fit's embedding model (or the package default) is used.
#' @param instruction Naming instruction override (see
#'   [sfa_naming_instruction()]).
#' @param pool An [sfa_pool()] object, or \code{NULL} to fetch it.
#' @param block_size Pool rows per block while building S (memory knob;
#'   results are identical for any value).
#' @param ... Passed to the embedding backend.
#' @returns An object of class \code{sfa_lexmap}.
#' @export
sfa_lexmap <- function(x, model = NULL, instruction = NULL, pool = NULL,
                       block_size = 50000L, ...) {
  items <- if (inherits(x, "sfa")) x$item_data$item else as.character(x)
  if (inherits(x, "sfa")) {
    model <- model %||% x$embed_model %||% .SFA_DEFAULT_MODEL
  } else {
    model <- model %||% .SFA_DEFAULT_MODEL
  }
  instruction <- instruction %||% .SFA_NAMING_INSTRUCTION
  queries <- sprintf("Instruct: %s\nQuery: %s", instruction, items)
  q <- sfa_embed(queries, embed = "sbert", model = model, ...)
  q <- .rows_unit(q)
  if (is.null(pool)) pool <- sfa_pool(model)
  if (ncol(q) != .pool_dimension(pool)) {
    stop("Pool dimension (", .pool_dimension(pool), ") does not match the ",
         "naming model's embedding dimension (", ncol(q), ").", call. = FALSE)
  }
  n <- nrow(q)
  npool <- .pool_nrow(pool$emb)
  S <- matrix(0, nrow = npool, ncol = n)
  i0 <- 0L
  while (i0 < npool) {
    i1 <- min(i0 + block_size, npool)
    S[(i0 + 1L):i1, ] <- .pool_block(pool$emb, i0, i1) %*% t(q)
    i0 <- i1
  }
  structure(
    list(S = S, Q = q %*% t(q), q_unit = q, n_items = n, pool = pool,
         model = model, instruction = instruction, items = items,
         block_size = block_size),
    class = "sfa_lexmap"
  )
}

#' @export
print.sfa_lexmap <- function(x, ...) {
  cat("semanticfa lexical map\n")
  cat("  items:  ", x$n_items, "\n")
  cat("  model:  ", x$model, "\n")
  cat("  pool:   ", format(nrow(x$pool$words), big.mark = ","), "terms\n")
  invisible(x)
}

# ---- naming any orientation through the map ------------------------------------

# Dominant-pole item selection and loading weights, per factor (the exact
# item-selection convention of sfa_name / .sfa_name_targets).
#' @keywords internal
.lexi_pole <- function(L) {
  L <- unclass(L)
  factor_names <- colnames(L) %||% paste0("F", seq_len(ncol(L)))
  primary <- apply(abs(L), 1L, which.max)
  sel <- vector("list", ncol(L))
  w <- vector("list", ncol(L))
  for (j in seq_len(ncol(L))) {
    idx <- which(primary == j)
    if (length(idx) == 0L) idx <- seq_len(nrow(L))
    dom <- sign(L[idx[which.max(abs(L[idx, j]))], j])
    if (dom == 0) dom <- 1
    s <- idx[sign(L[idx, j]) == dom]
    if (length(s) == 0L) s <- idx
    ww <- abs(L[s, j])
    sel[[j]] <- s
    w[[j]] <- ww / sum(ww)
  }
  list(sel = sel, w = w, factor_names = factor_names)
}

# Naming-target coefficient vector: target = c %*% q_unit, unit length
# (weighted centroid, renormalize, grand-mean contrast, renormalize).
#' @keywords internal
.lexi_coef <- function(sel, w, Q, n) {
  cw <- numeric(n)
  cw[sel] <- w
  cen_norm <- sqrt(as.numeric(t(cw) %*% Q %*% cw))
  cc <- cw / cen_norm - rep(1 / n, n)
  cc / sqrt(as.numeric(t(cc) %*% Q %*% cc))
}

#' Nameability of an Orientation
#'
#' Runs the full [sfa_name()] selection rule (family-gated candidate walk,
#' construct-noun preference, leave-one-out sets, geometric collision
#' keeper) on an arbitrary pattern matrix of the lexical map's items, and
#' returns the retrieved labels together with the nameability criterion:
#' per factor, the cosine between the factor's naming target and its
#' retrieved term's embedding, and their mean.
#'
#' @param lexmap An [sfa_lexmap()] object.
#' @param loadings A pattern matrix (items x factors) in any orientation of
#'   the fitted solution.
#' @param n_candidates Visible label window (default 5, as in [sfa_name()]).
#' @param collision Resolve duplicate labels via the geometric keeper.
#' @param loo_sets Compute leave-one-out candidate sets.
#' @returns A list with \code{labels} (data frame: factor, label, rule,
#'   n_items, collision_moved, row), \code{candidates} (list column),
#'   \code{criterion} (mean), and \code{per_factor}.
#' @export
sfa_nameability <- function(lexmap, loadings, n_candidates = 5L,
                            collision = TRUE, loo_sets = TRUE) {
  stopifnot(inherits(lexmap, "sfa_lexmap"))
  L <- unclass(loadings)
  stopifnot(nrow(L) == lexmap$n_items)
  pole <- .lexi_pole(L)
  k <- ncol(L)
  n <- lexmap$n_items
  words <- lexmap$pool$words
  coefs <- lapply(seq_len(k), function(j)
    .lexi_coef(pole$sel[[j]], pole$w[[j]], lexmap$Q, n))

  gated <- vector("list", k)
  picks <- vector("list", k)
  loo <- vector("list", k)
  for (j in seq_len(k)) {
    sc <- as.numeric(lexmap$S %*% coefs[[j]])
    ord <- order(sc, decreasing = TRUE)[seq_len(400L)]
    gated[[j]] <- .sfa_gate(words, ord, sc[ord], n_keep = 20L)
    picks[[j]] <- .sfa_pick(gated[[j]], n_candidates = n_candidates)
    if (isTRUE(loo_sets)) {
      loo[[j]] <- .lexi_loo_S(L, pole, j, lexmap, gated[[j]])
    }
  }
  moved <- rep(FALSE, k)
  if (isTRUE(collision)) {
    res <- .sfa_keeper(picks, gated, loo, n_candidates)
    picks <- res$picks
    moved <- res$moved
  }
  label <- vapply(picks, function(p) p$label %||% NA_character_, character(1))
  row <- vapply(seq_len(k), function(j) {
    g <- gated[[j]]
    hit <- match(label[j], g$word)
    if (is.na(hit)) NA_integer_ else g$row[hit]
  }, integer(1))
  per <- vapply(seq_len(k), function(j) {
    if (is.na(row[j])) return(-Inf)
    as.numeric(lexmap$S[row[j], , drop = FALSE] %*% coefs[[j]])
  }, numeric(1))
  cand <- lapply(seq_len(k), function(j) {
    cc <- loo[[j]] %||% character(0)
    if (!is.na(label[j]) && !.fam_of(words, label[j]) %in%
        vapply(cc, function(wd) .fam_of(words, wd), character(1))) {
      cc <- c(cc, label[j])
    }
    cc
  })
  labels <- data.frame(
    factor = pole$factor_names,
    label = label,
    rule = vapply(picks, function(p) p$rule, character(1)),
    n_items = vapply(pole$sel, length, integer(1)),
    collision_moved = moved,
    row = row,
    stringsAsFactors = FALSE
  )
  list(labels = labels, candidates = cand, criterion = mean(per),
       per_factor = per, coefs = coefs)
}

# Leave-one-out candidate set through S (the .sfa_loo convention).
#' @keywords internal
.lexi_loo_S <- function(L, pole, j, lexmap, gate) {
  sel <- pole$sel[[j]]
  if (length(sel) < 2L || nrow(gate) == 0L) {
    return(gate$word[1L])
  }
  Ssub <- lexmap$S[gate$row, , drop = FALSE]
  n <- lexmap$n_items
  winners <- integer(0)
  for (drop in seq_along(sel)) {
    keep <- sel[-drop]
    ww <- abs(L[keep, j])
    ww <- ww / sum(ww)
    cc <- .lexi_coef(keep, ww, lexmap$Q, n)
    winners <- c(winners, which.max(as.numeric(Ssub %*% cc)))
  }
  cc <- .lexi_coef(sel, pole$w[[j]], lexmap$Q, n)
  winners <- unique(c(winners, which.max(as.numeric(Ssub %*% cc))))
  gate$word[sort(winners)]
}

# Canonical re-naming of one orientation through the blocked pool path
# (sfa_name's exact arithmetic); used to certify the optimizer's winner.
#' @keywords internal
.lexi_canonical_labels <- function(lexmap, loadings, n_candidates = 5L) {
  L <- unclass(loadings)
  pole <- .lexi_pole(L)
  q <- lexmap$q_unit
  g <- colMeans(q)
  k <- ncol(L)
  targets <- matrix(0, nrow = k, ncol = ncol(q))
  for (j in seq_len(k)) {
    sel <- pole$sel[[j]]
    cen <- colSums(q[sel, , drop = FALSE] * pole$w[[j]])
    cen <- cen / sqrt(sum(cen^2))
    r <- cen - g
    targets[j, ] <- r / sqrt(sum(r^2))
  }
  top <- .sfa_pool_top(lexmap$pool, targets, k = 400L,
                       block_size = lexmap$block_size)
  gated <- lapply(seq_len(k), function(j)
    .sfa_gate(lexmap$pool$words, top$idx[[j]], top$score[[j]], n_keep = 20L))
  picks <- lapply(gated, .sfa_pick, n_candidates = n_candidates)
  loo <- lapply(seq_len(k), function(j)
    .lexi_loo_S(L, pole, j, lexmap, gated[[j]]))
  res <- .sfa_keeper(picks, gated, loo, n_candidates)
  vapply(res$picks, function(p) p$label %||% NA_character_, character(1))
}

# ---- the optimizer ---------------------------------------------------------------

# Recover an orthogonal unrotated basis A0 from any oblique solution:
# symmetric eigendecomposition of L Phi L', columns scaled by sqrt
# eigenvalue, each column's sign set so its largest-|entry| element is
# positive. A0 A0' = L Phi L' to machine precision, so every rotation of A0
# reproduces the fitted solution exactly.
#' @keywords internal
.lexi_A0 <- function(L, Phi = NULL) {
  L <- unclass(L)
  k <- ncol(L)
  if (is.null(Phi)) Phi <- diag(k)
  M <- L %*% Phi %*% t(L)
  e <- eigen(M, symmetric = TRUE)
  A0 <- e$vectors[, seq_len(k), drop = FALSE] %*%
    diag(sqrt(pmax(e$values[seq_len(k)], 0)), k)
  for (j in seq_len(k)) {
    i <- which.max(abs(A0[, j]))
    if (A0[i, j] < 0) A0[, j] <- -A0[, j]
  }
  dev <- max(abs(A0 %*% t(A0) - M))
  if (dev > 1e-6) {
    stop("A0 recovery failed (common-part deviation ", format(dev), ").",
         call. = FALSE)
  }
  rownames(A0) <- rownames(L)
  A0
}

#' @keywords internal
.lexi_starts <- function(A0, L = NULL, Phi = NULL, n_random = 10L,
                         seed = 42L, rotation = "oblique") {
  k <- ncol(A0)
  starts <- list()
  if (!is.null(L)) {
    T0 <- solve(crossprod(A0), t(A0) %*% unclass(L))
    Th0 <- solve(t(T0))
    Th0 <- sweep(Th0, 2L, sqrt(colSums(Th0^2)), "/")
    starts$oblimin <- Th0
  } else {
    ob <- GPArotation::GPFoblq(A0, method = "oblimin",
                               methodArgs = list(gam = 0))
    starts$oblimin <- ob$Th
  }
  starts$varimax <- GPArotation::GPForth(A0, method = "varimax")$Th
  starts$geominQ <- GPArotation::GPFoblq(A0, method = "geomin",
                                         methodArgs = list(delta = 0.01))$Th
  for (i in seq_len(n_random)) {
    starts[[paste0("random", seed + i - 1L)]] <-
      withr::with_seed(seed + i - 1L, GPArotation::Random.Start(k))
  }
  if (identical(rotation, "orthogonal")) {
    starts <- lapply(starts, function(Th) {
      sv <- svd(Th)
      sv$u %*% t(sv$v)
    })
  }
  starts
}

#' Leximax: Rotate Factor Axes Toward the Construct Lexicon
#'
#' Chooses, among the orientations that reproduce a fitted factor solution
#' identically, the one whose factors sit closest to real construct terms.
#' The optimizer alternates naming (the full [sfa_name()] selection rule on
#' the current pattern matrix) with oblique target rotation toward the
#' retrieved terms' predicted loading profiles, from multiple deterministic
#' starts, until the retrieved term tuple recurs. The recurrent states are
#' the candidate solutions (a fixed point is the one-state case), and the
#' recurrent state with the highest nameability wins, with ties broken by
#' start order and then iteration. The winner is re-named through the
#' canonical blocked pool path and those labels are authoritative.
#'
#' Also available as \code{rotate = "leximax"} in [sfa()].
#'
#' @param x An [sfa()] fit, a \code{psych::fa} fit, or a pattern matrix.
#' @param Phi Factor correlations when \code{x} is a plain matrix (default
#'   identity).
#' @param lexmap An [sfa_lexmap()] for the instrument's items. Required when
#'   \code{x} is a matrix; built automatically from an sfa fit.
#' @param model,pool,instruction Passed to [sfa_lexmap()] when it must be
#'   built.
#' @param n_random Number of seeded random orthonormal starts appended to
#'   the oblimin, varimax, and geominQ starts (default 10).
#' @param seed Base seed for the random starts (default 42).
#' @param col_scale Target column scaling: \code{"unitmax"} (default) or
#'   \code{"z"}.
#' @param rotation \code{"oblique"} (default) or \code{"orthogonal"}.
#' @param normalize Kaiser row normalization inside the target rotation.
#' @param max_iter Iteration cap per start (default 30).
#' @param block_size Passed to [sfa_lexmap()] when it must be built.
#' @param ... Passed to the embedding backend via [sfa_lexmap()].
#' @returns An object of class \code{sfa_leximax}: \code{loadings},
#'   \code{Phi}, \code{Th}, \code{A0}, \code{labels} (with the canonical
#'   retrieved labels and leave-one-out candidate sets), \code{criterion},
#'   \code{per_factor}, \code{start}, \code{iteration}, \code{history},
#'   \code{converged}.
#' @export
sfa_leximax <- function(x, Phi = NULL, lexmap = NULL,
                        model = NULL, pool = NULL, instruction = NULL,
                        n_random = 10L, seed = 42L,
                        col_scale = c("unitmax", "z"),
                        rotation = c("oblique", "orthogonal"),
                        normalize = FALSE, max_iter = 30L,
                        block_size = 50000L, ...) {
  col_scale <- match.arg(col_scale)
  rotation <- match.arg(rotation)

  if (inherits(x, "sfa")) {
    L <- unclass(x$loadings)
    Phi <- x$Phi %||% diag(ncol(L))
    if (is.null(lexmap)) {
      lexmap <- sfa_lexmap(x, model = model, instruction = instruction,
                           pool = pool, block_size = block_size, ...)
    }
  } else if (inherits(x, c("fa", "psych"))) {
    L <- unclass(x$loadings)
    Phi <- x$Phi %||% diag(ncol(L))
    if (is.null(lexmap)) {
      stop("For a psych fit, supply a lexmap built from the item texts ",
           "(sfa_lexmap(items, ...)).", call. = FALSE)
    }
  } else {
    L <- unclass(x)
    Phi <- Phi %||% diag(ncol(L))
    if (is.null(lexmap)) {
      stop("For a plain loading matrix, supply a lexmap built from the ",
           "item texts (sfa_lexmap(items, ...)).", call. = FALSE)
    }
  }
  if (ncol(L) < 2L) {
    stop("Leximax rotation needs at least 2 factors.", call. = FALSE)
  }
  stopifnot(inherits(lexmap, "sfa_lexmap"), nrow(L) == lexmap$n_items)

  A0 <- .lexi_A0(L, Phi)
  starts <- .lexi_starts(A0, L = L, Phi = Phi, n_random = n_random,
                         seed = seed, rotation = rotation)
  fnames <- colnames(unclass(L)) %||% paste0("F", seq_len(ncol(L)))

  orient <- function(Th) {
    if (identical(rotation, "orthogonal")) A0 %*% Th
    else A0 %*% solve(t(Th))
  }
  rotate_to <- function(Th, Tm) {
    if (identical(rotation, "orthogonal")) {
      GPArotation::GPForth(A0, Tmat = Th, method = "target",
                           methodArgs = list(Target = Tm),
                           normalize = normalize)
    } else {
      GPArotation::GPFoblq(A0, Tmat = Th, method = "target",
                           methodArgs = list(Target = Tm),
                           normalize = normalize)
    }
  }

  best <- NULL
  history <- list()
  for (s_idx in seq_along(starts)) {
    s_name <- names(starts)[s_idx]
    Th <- starts[[s_idx]]
    Lc <- orient(Th)
    colnames(Lc) <- fnames
    seen <- character(0)
    visited <- list()
    recur_iter <- NA_integer_
    for (it in seq_len(max_iter)) {
      nm <- sfa_nameability(lexmap, Lc)
      key <- paste(nm$labels$label, collapse = "\x1f")
      visited[[it]] <- list(terms = nm$labels$label, crit = nm$criterion,
                            per = nm$per_factor, L = Lc, Th = Th,
                            labels = nm$labels, candidates = nm$candidates,
                            iter = it - 1L)
      hit <- match(key, seen)
      if (!is.na(hit)) {
        recur_iter <- hit - 1L
        break
      }
      seen <- c(seen, key)
      Tm <- vapply(seq_len(ncol(Lc)), function(j) {
        prof <- lexmap$S[nm$labels$row[j], ]
        if (identical(col_scale, "unitmax")) prof / max(abs(prof))
        else (prof - mean(prof)) / stats::sd(prof)
      }, numeric(lexmap$n_items))
      rot <- rotate_to(Th, Tm)
      Th <- rot$Th
      Lc <- A0 %*% (if (identical(rotation, "orthogonal")) Th
                    else solve(t(Th)))
      colnames(Lc) <- fnames
    }
    converged <- !is.na(recur_iter)
    sols <- if (converged) {
      Filter(function(v) v$iter > recur_iter, visited)
    } else {
      visited[length(visited)]
    }
    crits <- vapply(sols, `[[`, numeric(1), "crit")
    b <- sols[[which.max(crits)]]
    history[[s_name]] <- list(
      converged = converged,
      visited = lapply(visited, function(v)
        list(terms = v$terms, crit = v$crit, iter = v$iter)))
    cand <- list(crit = b$crit, s_idx = s_idx, iter = b$iter, state = b,
                 start = s_name, converged = converged)
    if (is.null(best) || cand$crit > best$crit ||
        (cand$crit == best$crit && cand$s_idx < best$s_idx)) {
      best <- cand
    }
  }

  st <- best$state
  Phi_out <- if (identical(rotation, "orthogonal")) diag(ncol(st$L))
             else t(st$Th) %*% st$Th
  canonical <- .lexi_canonical_labels(lexmap, st$L)
  agreement <- identical(canonical, st$labels$label)
  labels <- st$labels
  if (!agreement) {
    warning("Coefficient-space and canonical labels differ; ",
            "canonical labels reported.", call. = FALSE)
    labels$label <- canonical
  }
  labels$candidates <- I(st$candidates)

  structure(
    list(loadings = st$L, Phi = Phi_out, Th = st$Th, A0 = A0,
         labels = labels, criterion = st$crit, per_factor = st$per,
         start = best$start, iteration = st$iter,
         converged = best$converged, canonical_agreement = agreement,
         rotation = rotation, col_scale = col_scale,
         normalize = normalize, history = history),
    class = "sfa_leximax"
  )
}

#' @export
print.sfa_leximax <- function(x, ...) {
  cat("Leximax rotation (", x$rotation, ", ", ncol(x$loadings),
      " factors)\n\n", sep = "")
  for (i in seq_len(nrow(x$labels))) {
    cat(sprintf("  %-10s %-28s nameability %.3f\n", x$labels$factor[i],
                x$labels$label[i], x$per_factor[i]))
  }
  cat(sprintf("\n  criterion (mean) %.4f   winning start: %s (iteration %d)\n",
              x$criterion, x$start, x$iteration))
  if (!isTRUE(x$converged)) {
    cat("  WARNING: iteration cap reached on the winning start.\n")
  }
  invisible(x)
}
