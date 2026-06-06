#' Load Pre-generated Embeddings from a NumPy .npz File
#'
#' Reads a NumPy \code{.npz} archive of pre-computed item embeddings (and, if
#' present, the item codes, factor labels, scoring, and item text) into a tidy
#' object that \code{\link{sfa}}, \code{\link{sfa_similarity}}, and
#' \code{\link{sfa_corplot}} accept directly --- so loading saved embeddings is
#' one line instead of hand-rolling \pkg{reticulate}/NumPy calls.
#'
#' The archive is expected to contain a 2-D embeddings array; the other fields
#' are optional and matched by name.
#'
#' @param path Path to a \code{.npz} file.
#' @param embeddings_key Name of the embeddings array in the archive
#'   (default \code{"embeddings"}).
#' @param codes_key,items_key,factors_key,scoring_key Names of the optional
#'   metadata arrays (codes, item text, factor labels, +1/-1 scoring). Missing
#'   keys are silently skipped.
#'
#' @returns An object of class \code{"sfa_embeddings"}: a list with
#'   \code{embeddings} (numeric matrix, n_items x dim, with item codes as
#'   rownames when available) and any of \code{codes}, \code{items},
#'   \code{factors}, \code{scoring} found in the archive.
#'
#' @seealso \code{\link{sfa}}, \code{\link{sfa_similarity}},
#'   \code{\link{sfa_corplot}}
#' @examples
#' \dontrun{
#' emb <- sfa_load_npz("DASS_items_8B.npz")
#' emb                                  # summary of what was loaded
#' sfa_corplot(sfa_similarity(emb))     # grouped heatmap, two lines total
#' fit <- sfa(emb)                      # or run the full analysis
#' }
#' @export
sfa_load_npz <- function(path, embeddings_key = "embeddings",
                         codes_key = "codes", items_key = "items",
                         factors_key = "factors", scoring_key = "scoring") {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  .sfa_py_require("numpy")
  np <- tryCatch(reticulate::import("numpy"),
                 error = function(e) stop(
                   "Reading .npz needs Python 'numpy'. Run sfa_install_python ",
                   "(or reticulate::py_install('numpy')).", call. = FALSE))
  npz <- np$load(path, allow_pickle = TRUE)
  files <- tryCatch(as.character(reticulate::py_to_r(npz$files)),
                    error = function(e) character(0))
  get_arr <- function(key) if (key %in% files) npz[[key]] else NULL

  emb <- get_arr(embeddings_key)
  if (is.null(emb)) {
    stop("No '", embeddings_key, "' array in ", path,
         ". Available: ", paste(files, collapse = ", "), call. = FALSE)
  }
  emb <- as.matrix(emb)
  storage.mode(emb) <- "double"

  chr <- function(key) { v <- get_arr(key); if (is.null(v)) NULL else as.character(v) }
  codes   <- chr(codes_key)
  items   <- chr(items_key)
  factors <- chr(factors_key)
  scoring <- { v <- get_arr(scoring_key); if (is.null(v)) NULL else as.integer(v) }

  if (!is.null(codes) && length(codes) == nrow(emb)) {
    rownames(emb) <- codes
  } else if (!is.null(items) && length(items) == nrow(emb)) {
    rownames(emb) <- items
  }

  structure(list(embeddings = emb, codes = codes, items = items,
                 factors = factors, scoring = scoring),
            class = "sfa_embeddings")
}

#' @export
print.sfa_embeddings <- function(x, ...) {
  cat("Loaded embeddings (sfa_embeddings)\n")
  cat(sprintf("  Items: %d  |  Dimensions: %d\n",
              nrow(x$embeddings), ncol(x$embeddings)))
  have <- c(codes = !is.null(x$codes), items = !is.null(x$items),
            factors = !is.null(x$factors), scoring = !is.null(x$scoring))
  cat("  Metadata:", paste(names(have)[have], collapse = ", "),
      if (!any(have)) "(none)" else "", "\n")
  if (!is.null(x$factors)) {
    cat("  Factors:", paste(unique(x$factors), collapse = ", "), "\n")
  }
  if (!is.null(x$codes)) {
    cat("  Codes:  ", paste(utils::head(x$codes, 8), collapse = " "),
        if (length(x$codes) > 8) "..." else "", "\n")
  }
  invisible(x)
}
