#' Semantic Factor Analysis
#'
#' Performs exploratory factor analysis on language model embeddings of scale
#' items. Given item text, \code{sfa} embeds each item, transforms embeddings
#' into a similarity matrix, and runs EFA to recover latent factor structure
#' entirely from the text.
#'
#' @param items Character vector of item text, or a data.frame with an
#'   \code{item} (or \code{text}) column and optional \code{code},
#'   \code{factor}, \code{scoring} columns.
#' @param nfactors Integer number of factors to extract, or \code{NULL} for
#'   automatic determination via \code{n_factors_method}.
#' @param rotate Rotation method passed to \code{\link[psych]{fa}}. Default
#'   \code{"oblimin"} (requires \pkg{GPArotation}, which is in Imports).
#' @param fm Extraction method passed to \code{\link[psych]{fa}}. Default
#'   \code{"minres"}.
#' @param encoding Similarity transform: \code{"atomic"} (default),
#'   \code{"atomic_reversed"}, \code{"squid"}, or \code{"mean_centered_pearson"}.
#'   Use \code{"atomic_reversed"} with a \code{scoring} vector to sign-flip
#'   reverse-keyed items. See
#'   \code{\link{sfa_similarity}}.
#' @param embed Embedding backend: \code{"sbert"}, \code{"openai"}, or a
#'   function. Ignored when \code{embeddings} is provided.
#' @param model Model name for the embedding backend. If \code{NULL} (default),
#'   resolves to a backend-appropriate default:
#'   \code{"Qwen/Qwen3-Embedding-0.6B"} (about 1.2 GB) for \code{"sbert"} and
#'   \code{"text-embedding-3-small"} for \code{"openai"}. The sbert default is
#'   chosen to run on any machine. Larger embedding models recover factor
#'   structure more accurately;
#'   for higher fidelity pass \code{"Qwen/Qwen3-Embedding-4B"} (about 8 GB RAM)
#'   or \code{"Qwen/Qwen3-Embedding-8B"} (about 16 GB RAM). When the default
#'   model is used, \code{print()} reminds you of these options.
#' @param embeddings Optional precomputed numeric matrix (n_items x
#'   embedding_dim). When supplied, skips the embedding step entirely.
#' @param similarity Optional precomputed symmetric item-by-item similarity
#'   matrix (n_items x n_items). When supplied, embedding and the encoding
#'   transform are skipped and this matrix is used directly --- e.g. a signed
#'   NLI matrix from \code{\link{sfa_nli_matrix}}. Parallel analysis is
#'   unavailable in this mode (no embeddings), so retention falls back to
#'   \code{"kaiser"} unless \code{nfactors} is set.
#' @param scoring Numeric vector of +1/-1 per item. If \code{NULL}, defaults
#'   to all +1 with an informative message for encoding methods that use it.
#' @param n_factors_method Retention rule when \code{nfactors = NULL}:
#'   \code{"parallel"} (embedding-adapted, default), \code{"kaiser"},
#'   \code{"EGA"}, or \code{"TEFI"}.
#' @param dim_select Embedding-dimension selection before analysis:
#'   \code{"none"} (default, use the full vector) or \code{"dynega"} (select the
#'   leading-coordinate depth that best recovers structure by EGA-based depth
#'   optimization, adapting Golino 2026; see \code{\link{sfa_dimselect}}).
#'   Requires \pkg{EGAnet}.
#' @param n.obs Sample size passed to \code{\link[psych]{fa}}. \code{NA}
#'   (default) suppresses sample-size-dependent fit indices.
#' @param parallel_iter Iterations for embedding parallel analysis.
#' @param seed Random seed for stochastic operations, used via
#'   \code{\link[withr]{with_seed}} without touching the global RNG state.
#' @param calibrate Logical: run an isotropic random-embedding Monte Carlo null
#'   calibration of the fit diagnostics? (Inspired by Pokropek 2026, but using a
#'   random-Gaussian unit-vector null rather than Pokropek's corpus-word
#'   resampling. The two nulls differ in kind: corpus resampling preserves the
#'   baseline thematic similarity that all words in a topic-specific corpus
#'   share, whereas the Gaussian unit-vector null has zero expected inter-item
#'   similarity and is therefore a stricter, structure-free reference.)
#' @param calibrate_iter Iterations for calibration.
#' @param ... Additional arguments passed to \code{\link[psych]{fa}}.
#'
#' @returns An object of class \code{"sfa"} containing factor loadings,
#'   communalities, eigenvalues, variance accounted for, and embedding-specific
#'   diagnostics (KMO, TEFI, RMSR, CAF, McDonald's omega). The \code{$loadings}
#'   component has class \code{"loadings"} and works with
#'   \code{\link[psych]{factor.congruence}} and \code{\link[psych]{fa.sort}}.
#'   Use \code{\link{as_psych}} to obtain the underlying \code{psych::fa}
#'   object.
#'
#' @examples
#' data(big5)
#' # nfactors = 5 keeps this example fast; omit it to let embedding-adapted
#' # parallel analysis (sfa_parallel) choose the number of factors.
#' fit <- sfa(big5$items, embeddings = big5$embeddings, scoring = big5$scoring,
#'            nfactors = 5)
#' print(fit)
#' plot(fit, type = "scree")
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
#' human data. \emph{Findings of the Association for Computational Linguistics:
#' EACL 2026}, 5738--5752.
#'
#' Pokropek, A. (2026). From keyword-based text measures to latent variables:
#' Confirmatory factor analysis with word embeddings. \emph{EPJ Data Science}.
#' \doi{10.1140/epjds/s13688-026-00654-1}
#'
#' @seealso \code{\link{sfa_similarity}}, \code{\link{sfa_parallel}},
#'   \code{\link{sfa_nfactors}}, \code{\link{sfa_embed}},
#'   \code{\link{sfa_congruence}}, \code{\link{as_psych}}
#'
#' @export
sfa <- function(items,
                nfactors         = NULL,
                rotate           = "oblimin",
                fm               = "minres",
                encoding         = "atomic",
                embed            = "sbert",
                model            = NULL,
                embeddings       = NULL,
                similarity       = NULL,
                scoring          = NULL,
                n_factors_method = "parallel",
                dim_select       = c("none", "dynega"),
                n.obs            = NA,
                parallel_iter    = 100L,
                seed             = 42L,
                calibrate        = FALSE,
                calibrate_iter   = 100L,
                ...) {
  cl <- match.call()

  encoding <- match.arg(encoding,
    c("atomic_reversed", "atomic", "squid", "mean_centered_pearson"))
  n_factors_method <- match.arg(n_factors_method,
    c("parallel", "kaiser", "EGA", "TEFI"))
  dim_select <- match.arg(dim_select)

  # validate numeric controls up front for clear error messages
  if (!is.null(nfactors)) nfactors <- .assert_count(nfactors, "nfactors")
  parallel_iter <- .assert_count(parallel_iter, "parallel_iter")
  if (isTRUE(calibrate)) calibrate_iter <- .assert_count(calibrate_iter, "calibrate_iter")

  # accept a loaded sfa_embeddings object (from sfa_load_npz) as the first
  # argument: unpack its embeddings/scoring/codes/factors/items
  if (inherits(items, "sfa_embeddings")) {
    obj <- items
    if (is.null(embeddings)) embeddings <- obj$embeddings
    if (is.null(scoring) && !is.null(obj$scoring)) scoring <- obj$scoring
    n <- nrow(obj$embeddings)
    df <- data.frame(item = obj$items %||% obj$codes %||%
                            sprintf("item_%02d", seq_len(n)),
                     stringsAsFactors = FALSE)
    if (!is.null(obj$codes))   df$code   <- obj$codes
    if (!is.null(obj$factors)) df$factor <- obj$factors
    if (!is.null(obj$scoring)) df$scoring <- obj$scoring
    items <- df
  }

  # accept a bare numeric embedding matrix as the first argument: treat it as the
  # embeddings, using its rownames (or generated codes) as the item labels
  if (is.matrix(items) && is.numeric(items)) {
    if (is.null(embeddings)) embeddings <- items
    rn <- rownames(items)
    items <- if (!is.null(rn)) rn else sprintf("item_%02d", seq_len(nrow(items)))
  }

  resolved <- .resolve_items(items, scoring = scoring, embeddings = embeddings)
  item_text <- resolved$items
  codes     <- resolved$codes
  factors   <- resolved$factors
  scoring   <- resolved$scoring
  n_items   <- length(item_text)

  dimsel <- NULL
  if (!is.null(similarity)) {
    # --- Precomputed item-by-item similarity (e.g. from sfa_nli_matrix()) ---
    sim_matrix <- as.matrix(similarity)
    if (nrow(sim_matrix) != n_items || ncol(sim_matrix) != n_items) {
      stop("'similarity' must be an ", n_items, " x ", n_items,
           " matrix matching the items.", call. = FALSE)
    }
    if (!is.numeric(sim_matrix) || anyNA(sim_matrix) ||
        any(!is.finite(sim_matrix))) {
      stop("'similarity' must be a finite numeric matrix.", call. = FALSE)
    }
    if (!isSymmetric(unname(sim_matrix), tol = 1e-6)) {
      stop("'similarity' must be symmetric.", call. = FALSE)
    }
    if (any(abs(diag(sim_matrix) - 1) > 1e-6)) {
      message("'similarity' diagonal was not all 1; setting the diagonal to 1 ",
              "(treating it as a correlation-like matrix).")
      diag(sim_matrix) <- 1
    }
    transformed   <- NULL
    embeddings    <- NULL
    embed_method  <- "precomputed_similarity"
    embed_model   <- NULL
    embed_dim     <- NA_integer_
    dimnames(sim_matrix) <- list(codes, codes)
    sim_matrix <- .check_psd(sim_matrix)
    if (is.null(nfactors) && n_factors_method == "parallel") {
      message("Embedding-adapted parallel analysis needs embeddings; with ",
              "'similarity' supplied, using 'kaiser' retention instead.")
      n_factors_method <- "kaiser"
    }
    # scoring is not used for a precomputed matrix; default silently (the matrix
    # already encodes whatever keying convention was applied)
    if (is.null(scoring)) scoring <- rep(1, n_items)
    # calibration generates random embeddings, which a precomputed matrix lacks
    if (calibrate) {
      warning("Monte Carlo calibration needs item embeddings; ignoring ",
              "calibrate = TRUE for a precomputed 'similarity' matrix.",
              call. = FALSE)
      calibrate <- FALSE
    }
  } else {
    scoring <- .resolve_scoring(scoring, n_items, encoding)
    # --- Step 1: Obtain embeddings ---
    if (is.null(embeddings)) {
      embeddings <- sfa_embed(item_text, embed = embed, model = model)
      embed_method <- if (is.function(embed)) "custom" else embed
      embed_model <- if (is.function(embed)) NULL
                     else .resolve_embed_model(embed, model)
    } else {
      embed_method <- "precomputed"
      embed_model <- NULL
    }
    rownames(embeddings) <- codes

    # --- Step 1b: Optional embedding-dimension selection (EGA depth opt.) ---
    if (dim_select == "dynega") {
      dimsel <- sfa_dimselect(embeddings, factors = factors, scoring = scoring,
                              encoding = encoding)
      embeddings <- embeddings[, seq_len(dimsel$optimal_depth), drop = FALSE]
    }
    embed_dim <- ncol(embeddings)

    # --- Step 2: Build similarity matrix ---
    sim_matrix <- sfa_similarity(embeddings, encoding = encoding, scoring = scoring)
    transformed <- attr(sim_matrix, "transformed_embeddings")
    attr(sim_matrix, "transformed_embeddings") <- NULL
    dimnames(sim_matrix) <- list(codes, codes)

    sim_matrix <- .check_psd(sim_matrix)
  }

  # --- Step 3: Determine nfactors ---
  pa_result <- NULL
  if (is.null(nfactors)) {
    nfactors <- switch(n_factors_method,
      parallel = {
        pa_result <- sfa_parallel(sim_matrix, transformed,
                                  n_iter = parallel_iter, seed = seed)
        pa_result$n_factors
      },
      kaiser = .retention_kaiser(
        eigen(sim_matrix, symmetric = TRUE, only.values = TRUE)$values
      ),
      EGA = .retention_ega(sim_matrix),
      TEFI = .retention_tefi(sim_matrix, max_factors = NULL,
                             rotate = rotate, fm = fm)
    )
  }
  nfactors <- max(1L, as.integer(nfactors))
  if (nfactors > n_items - 1L) {
    stop("nfactors (", nfactors, ") must be at most n_items - 1 (",
         n_items - 1L, ").", call. = FALSE)
  }

  # --- Step 4: Factor analysis via psych ---
  fa_obj <- psych::fa(sim_matrix, nfactors = nfactors, rotate = rotate,
                      fm = fm, n.obs = n.obs, warnings = FALSE, ...)

  # --- Step 5: Heywood check ---
  hw <- .check_heywood(fa_obj$communality)

  # --- Step 6: Diagnostics ---
  kmo <- tryCatch(.compute_kmo(sim_matrix), error = function(e) {
    list(total = NA_real_, per_item = rep(NA_real_, n_items))
  })
  tefi <- tryCatch(
    .compute_tefi(sim_matrix, .assign_items(unclass(fa_obj$loadings))),
    error = function(e) NA_real_)
  rmsr_caf <- tryCatch(.compute_rmsr_caf(sim_matrix, fa_obj),
                        error = function(e) list(rmsr = NA_real_, caf = NA_real_,
                                                 residual = NULL))

  factor_names <- colnames(unclass(fa_obj$loadings))
  omega <- NULL
  if (!is.null(factors)) {
    omega <- tryCatch(
      .compute_omega(as.data.frame(unclass(fa_obj$loadings)),
                     factor_names, factors, codes),
      error = function(e) NULL
    )
  } else {
    omega <- tryCatch(
      .compute_omega(as.data.frame(unclass(fa_obj$loadings)),
                     factor_names),
      error = function(e) NULL
    )
  }

  daal <- NULL
  if (!is.null(factors)) {
    daal <- tryCatch(.compute_daal(unclass(fa_obj$loadings), factors),
                     error = function(e) NULL)
  }

  calibration <- NULL
  if (calibrate) {
    calibration <- .random_item_calibration(
      n_items = n_items, embed_dim = embed_dim, n_factors = nfactors,
      rotate = rotate, fm = fm, n_iter = calibrate_iter, seed = seed
    )
  }

  # --- Step 7: Assemble return object ---
  item_data <- data.frame(
    code = codes,
    item = item_text,
    scoring = scoring,
    stringsAsFactors = FALSE
  )
  if (!is.null(factors)) item_data$factor <- factors

  out <- list(
    # psych-compatible
    loadings      = fa_obj$loadings,
    Phi           = fa_obj$Phi,
    communality   = fa_obj$communality,
    communalities = fa_obj$communality,
    uniquenesses  = fa_obj$uniquenesses,
    values        = fa_obj$values,
    e.values      = fa_obj$e.values,
    Vaccounted    = fa_obj$Vaccounted,
    rotation      = rotate,
    fm            = fm,
    factors       = nfactors,
    residual      = rmsr_caf$residual,
    fit           = fa_obj$fit,
    fit.off       = fa_obj$fit.off,
    complexity    = fa_obj$complexity,
    Structure     = fa_obj$Structure,
    rot.mat       = fa_obj$rot.mat,
    weights       = fa_obj$weights,
    scores        = NULL,
    n.obs         = n.obs,
    Call          = cl,

    # embedding-specific
    encoding      = encoding,
    embed_method  = embed_method,
    embed_model   = embed_model,
    embedding_dim = embed_dim,
    sim_matrix    = sim_matrix,
    transformed_embeddings = transformed,
    input_embeddings = embeddings,
    dim_select    = dimsel,
    kmo           = kmo,
    tefi          = tefi,
    rmsr          = rmsr_caf$rmsr,
    caf           = rmsr_caf$caf,
    omega         = omega,
    daal          = daal,
    parallel      = pa_result,
    calibration   = calibration,
    heywood       = hw,
    item_data     = item_data,

    # internal
    .fa           = fa_obj
  )

  class(out) <- "sfa"
  out
}
