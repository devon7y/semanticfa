# sem-k: calibrated semantic factor retention. R-side wrapper around the
# vendored Python pipeline (inst/python/semk/semk.py) and the frozen
# consensus_rule_v1.0 artifact (random-forest hybrid trained on the
# planted-truth corpus, with split-conformal intervals). The artifact is
# downloaded once from a GitHub release and cached, mirroring pool.R.

.SFA_SEMK_BASE <- "https://github.com/devon7y/semanticfa/releases/download/semk-v1"
.SFA_SEMK_FILE <- "consensus_rule_v1.0.joblib"
.SFA_SEMK_SHA256 <- "9e1b6ad02777d7523e1a2cea8084bb6467b682ebd052ae0fa22b41f2d21a84fd"

#' @keywords internal
.semk_dir <- function() {
  d <- file.path(tools::R_user_dir("semanticfa", "cache"), "semk")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

#' @keywords internal
.semk_artifact <- function(download = interactive(), quiet = FALSE) {
  # explicit local override (development, air-gapped use, mirrors)
  override <- getOption("semanticfa.semk_artifact", NULL)
  if (!is.null(override)) {
    if (!file.exists(override)) {
      stop("options(semanticfa.semk_artifact=) points to a missing file: ",
           override, call. = FALSE)
    }
    return(override)
  }
  dest <- file.path(.semk_dir(), .SFA_SEMK_FILE)
  if (!file.exists(dest)) {
    if (!isTRUE(download)) {
      stop("The sem-k model artifact (~17 MB) is not cached yet. Re-run ",
           "with download = TRUE (or in an interactive session) to fetch ",
           "it once from ", .SFA_SEMK_BASE, ".", call. = FALSE)
    }
    base <- getOption("semanticfa.semk_url", .SFA_SEMK_BASE)
    url <- paste0(base, "/", .SFA_SEMK_FILE)
    if (!quiet) message("Downloading sem-k artifact (~17 MB, once): ", url)
    status <- utils::download.file(url, dest, mode = "wb", quiet = quiet)
    if (status != 0L || !file.exists(dest)) {
      stop("Download failed for ", url, call. = FALSE)
    }
  }
  got <- digest::digest(file = dest, algo = "sha256")
  if (!identical(got, .SFA_SEMK_SHA256)) {
    file.remove(dest)
    stop("sem-k artifact checksum mismatch (got ", substr(got, 1, 12),
         ", expected ", substr(.SFA_SEMK_SHA256, 1, 12),
         "); the cached file was removed. Re-run to re-download.",
         call. = FALSE)
  }
  dest
}

# module cache for the vendored python
.semk_env <- new.env(parent = emptyenv())

#' @keywords internal
.semk_py <- function() {
  if (!is.null(.semk_env$mod)) return(.semk_env$mod)
  .sfa_py_require(c("numpy", "scipy", "scikit-learn", "joblib"))
  path <- system.file("python", "semk", package = "semanticfa")
  if (!nzchar(path)) {
    stop("Vendored sem-k python module not found in the installed package.",
         call. = FALSE)
  }
  .semk_env$mod <- reticulate::import_from_path("semk", path = path,
                                                delay_load = FALSE)
  .semk_env$mod
}

#' Calibrated Semantic Factor Retention (sem-k)
#'
#' Estimates the number of semantic factors in an item set from its
#' embeddings using sem-k: a learned retention rule trained on a
#' planted-truth corpus of LLM-written item sets with known structure,
#' embedded in realistic encoder geometry. Unlike null-referenced
#' eigenvalue rules, sem-k is a calibrated estimator: its error rates are
#' measured on held-out planted configurations (65.7\% exact, 73.9\%
#' within 25\% on the v1--v4 corpus under Qwen3-Embedding-8B), and every
#' verdict carries a 90\% split-conformal interval.
#'
#' The estimand is \emph{semantic} dimensionality: the number of
#' distinguishable meaning clusters the items' embedding geometry
#' supports. Across 35 scales with large response archives, semantic
#' verdicts track empirical human-data dimensionality far better than
#' documented textbook counts do; where the two diverge (for example,
#' single-construct symptom inventories carrying real symptom-cluster
#' structure), human response data typically diverges the same way.
#' Treat sem-k as one voice alongside the granularity evidence in
#' [sfa_dimselect()] when the interval is wide.
#'
#' The rule generalizes across encoders: retrained and evaluated on nine
#' encoders from six providers (Qwen 0.6B--8B, e5-mistral, NVIDIA
#' llama-embed-nemotron, Microsoft harrier, OpenAI text-embedding-3
#' small/large, Google gemini-embedding-2), exact accuracy stays within
#' 61.6--69.0\% and real-scale verdicts agree across providers at mean
#' pairwise Spearman .89. The shipped artifact is the Qwen3-Embedding-8B
#' model; for other encoders pass the encoder's register floor via
#' \code{floor} (see the calibration files distributed with the sem-k
#' release).
#'
#' Requires Python with \code{numpy}, \code{scipy}, \code{scikit-learn},
#' and \code{joblib} (declared automatically via
#' \code{reticulate::py_require()} on first use), and a one-time ~17 MB
#' artifact download (cached under
#' \code{tools::R_user_dir("semanticfa", "cache")}).
#'
#' @param sim_matrix A fitted \code{"sfa"} object, or a similarity matrix
#'   (accepted for signature symmetry with the other retention criteria;
#'   sem-k computes its own similarity internally from the embeddings).
#' @param embeddings Numeric embedding matrix (n_items x embedding_dim).
#'   Required unless \code{sim_matrix} is a fitted \code{"sfa"} object
#'   that carries embeddings.
#' @param floor Register-floor calibration for the encoder that produced
#'   the embeddings (mean off-diagonal similarity of construct-dead
#'   survey-register items). \code{NULL} (default) uses the training
#'   encoder's floor (Qwen3-Embedding-8B, 0.478).
#' @param seed Random seed for the feature-extraction bootstrap
#'   (verdicts are seed-invariant on 41 of 42 benchmark scales, max
#'   spread 1).
#' @param download Permission to download the model artifact if not yet
#'   cached. Defaults to \code{interactive()} (CRAN policy: no silent
#'   downloads).
#' @param quiet Suppress download progress messages.
#'
#' @returns A list of class \code{"sfa_semk"} with components:
#' \describe{
#'   \item{n_factors}{Integer: the sem-k point estimate of semantic k.}
#'   \item{lo90, hi90}{Integer bounds of the 90\% split-conformal
#'     interval (calibrated coverage 91--96\% across encoders).}
#'   \item{floor}{The register floor used.}
#'   \item{battery}{Named integer vector: the classical battery votes
#'     (kaiser, pa_iso, ekc, map) consumed as features, for reference.}
#'   \item{artifact}{Artifact identifier and training-corpus tag.}
#' }
#'
#' @references
#' Yanitski, D., & Westbury, C. (in preparation). How many factors does a
#' questionnaire mean? Validated factor retention for language-model
#' embedding similarity matrices.
#'
#' Goretzko, D., & Buhner, M. (2020). One model to rule them all? Using
#' machine learning algorithms to determine the number of factors in
#' exploratory factor analysis. \emph{Psychological Methods}, 25(6),
#' 776--786. \doi{10.1037/met0000262}
#'
#' @examples
#' \dontrun{
#' data(big5)
#' sim <- sfa_similarity(big5$embeddings, "mean_centered_pearson")
#' sfa_semk(sim, big5$embeddings)  # 5 [2, 13]
#' }
#'
#' @export
sfa_semk <- function(sim_matrix = NULL, embeddings = NULL, floor = NULL,
                     seed = 42L, download = interactive(), quiet = FALSE) {
  if (inherits(sim_matrix, "sfa")) {
    fit <- sim_matrix
    if (is.null(embeddings)) embeddings <- fit$transformed_embeddings
    if (!is.null(fit$encoding) &&
        fit$encoding %in% c("squid", "atomic_reversed")) {
      warning("sem-k error rates were validated under the ",
              "'mean_centered_pearson' pipeline; the '", fit$encoding,
              "' encoding changes the geometry, so treat this verdict ",
              "as uncalibrated.", call. = FALSE)
    }
    if (is.null(embeddings)) {
      stop("This 'sfa' object has no embeddings (it was fit from a ",
           "precomputed similarity matrix); sem-k needs embeddings.",
           call. = FALSE)
    }
  }
  if (is.null(embeddings)) {
    stop("'embeddings' is required for sem-k.", call. = FALSE)
  }
  embeddings <- as.matrix(embeddings)
  if (!is.numeric(embeddings) || nrow(embeddings) < 8L) {
    stop("'embeddings' must be a numeric matrix with at least 8 items ",
         "(sem-k's validated regime).", call. = FALSE)
  }
  if (!is.null(floor)) {
    if (!is.numeric(floor) || length(floor) != 1L || !is.finite(floor)) {
      stop("'floor' must be a single finite number.", call. = FALSE)
    }
  }
  seed <- .assert_count(seed, "seed")

  artifact <- .semk_artifact(download = download, quiet = quiet)
  mod <- .semk_py()
  res <- mod$predict(embeddings, artifact,
                     floor = if (is.null(floor)) NULL else as.numeric(floor),
                     seed = as.integer(seed))

  battery <- vapply(res$battery, as.integer, integer(1))
  structure(
    list(
      n_factors = as.integer(res$n_factors),
      lo90 = as.integer(res$lo90),
      hi90 = as.integer(res$hi90),
      floor = as.numeric(res$floor),
      battery = battery,
      artifact = paste0("consensus_rule_v1.0 (", res$encoder_trained,
                        ", planted ", res$train_versions, ")"),
      seed = seed
    ),
    class = "sfa_semk"
  )
}

#' @export
print.sfa_semk <- function(x, ...) {
  cat("sem-k calibrated semantic factor retention\n")
  cat("  Suggested factors:", x$n_factors,
      sprintf("[90%% interval: %d, %d]\n", x$lo90, x$hi90))
  cat("  Register floor:", format(x$floor, digits = 4), "\n")
  cat("  Battery votes: ",
      paste(names(x$battery), x$battery, sep = "=", collapse = "  "), "\n")
  cat("  Artifact:", x$artifact, "\n")
  invisible(x)
}
