# Default sbert embedding model. Keep in sync with the `model =` defaults in
# sfa() and sfa_embed(); print.sfa() uses this to decide whether to show the
# "larger model" upgrade hint.
.SFA_DEFAULT_MODEL <- "Qwen/Qwen3-Embedding-0.6B"

#' Embed Item Text with a Language Model
#'
#' Computes embeddings for a vector of item text using a sentence-transformer
#' or other embedding backend.
#'
#' @param embed Embedding backend: \code{"sbert"} (default, via
#'   \code{reticulate}), \code{"openai"} (via \code{httr2}), or a function
#'   taking a character vector and returning a numeric matrix.
#' @param model Model name passed to the backend (default
#'   \code{"Qwen/Qwen3-Embedding-0.6B"} for sbert). Larger embedding models
#'   recover factor structure more accurately; see \code{\link{sfa}}.
#' @param cache Logical: cache embeddings in
#'   \code{tools::R_user_dir("semanticfa", "cache")}? Default \code{TRUE}.
#' @param ... Additional arguments passed to the embedding backend function.
#'
#' @returns A numeric matrix (n_items x embedding_dim). Rownames are the item
#'   codes when \code{items} is a data frame with a \code{code} column,
#'   otherwise the item text.
#'
#' @param items Character vector of item text, or a data frame with an
#'   \code{item}/\code{text} column (and optionally a \code{code} column, used
#'   as rownames so short codes flow through to plots such as
#'   \code{\link{sfa_corplot}}).
#' @export
sfa_embed <- function(items, embed = "sbert", model = "Qwen/Qwen3-Embedding-0.6B",
                      cache = TRUE, ...) {
  row_labels <- NULL
  if (is.data.frame(items)) {
    resolved   <- .resolve_items(items)
    row_labels <- resolved$codes        # the 'code' column, when present
    items      <- resolved$items        # the item text
  }
  if (is.null(row_labels)) row_labels <- items

  if (is.function(embed)) {
    emb <- embed(items, ...)
    if (!is.matrix(emb) || !is.numeric(emb)) {
      stop("Custom embed function must return a numeric matrix.", call. = FALSE)
    }
    if (nrow(emb) != length(items)) {
      stop("Custom embed function returned ", nrow(emb), " rows for ",
           length(items), " items.", call. = FALSE)
    }
    rownames(emb) <- row_labels
    return(emb)
  }

  embed <- match.arg(embed, c("sbert", "openai"))

  if (cache) {
    key <- .cache_key(items, model)
    cache_dir <- tools::R_user_dir("semanticfa", "cache")
    cache_file <- file.path(cache_dir, paste0(key, ".rds"))
    if (file.exists(cache_file)) {
      cached <- readRDS(cache_file)
      if (is.matrix(cached) && nrow(cached) == length(items)) {
        rownames(cached) <- row_labels    # honor codes vs text for this call
        return(cached)
      }
    }
  }

  emb <- switch(embed,
    sbert  = .embed_sbert(items, model, ...),
    openai = .embed_openai(items, model, ...)
  )

  rownames(emb) <- row_labels

  if (cache) {
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
    saveRDS(emb, cache_file)
  }

  emb
}

#' Clear Embedding Cache
#'
#' Removes all cached embedding files created by [sfa_embed()].
#'
#' @returns Invisible \code{NULL}.
#' @export
sfa_clear_cache <- function() {
  cache_dir <- tools::R_user_dir("semanticfa", "cache")
  if (dir.exists(cache_dir)) {
    files <- list.files(cache_dir, full.names = TRUE)
    file.remove(files)
  }
  invisible(NULL)
}

#' @keywords internal
.cache_key <- function(items, model) {
  payload <- list(items = items, model = model)
  if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(payload, algo = "sha256")
  } else {
    txt <- paste(c(items, model), collapse = "\x1f")
    sprintf("sfa_%d_%d", nchar(txt), sum(utf8ToInt(txt)) %% 1000000007L)
  }
}

#' Provision the Python Environment for Embedding
#'
#' Declares and installs the Python packages needed by the \code{"sbert"}
#' embedding backend and the default \code{\link{sfa_nli_matrix}} classifier
#' (\code{sentence-transformers}, which pulls in \code{torch} and
#' \code{transformers}). With \pkg{reticulate} (>= 1.41) these requirements are
#' also declared automatically on first use via \code{reticulate::py_require()},
#' so calling this is optional --- it is handy for provisioning ahead of time
#' (e.g. on a machine with internet before running offline) or into a specific
#' environment.
#'
#' @param packages Character vector of Python packages to require/install.
#' @param ... Passed to \code{reticulate::py_install()} (e.g. \code{envname},
#'   \code{method}).
#'
#' @returns Invisible \code{NULL}.
#' @examples
#' \dontrun{
#' # one-time setup of the Python embedding environment
#' sfa_install_python()
#' }
#' @export
sfa_install_python <- function(packages = "sentence-transformers", ...) {
  .sfa_py_require(packages)
  reticulate::py_install(packages, ...)
  invisible(NULL)
}

#' @keywords internal
.sfa_py_require <- function(packages) {
  if ("py_require" %in% getNamespaceExports("reticulate")) {
    try(reticulate::py_require(packages), silent = TRUE)
  }
  invisible(NULL)
}

#' @keywords internal
.embed_sbert <- function(items, model, ...) {
  .sfa_py_require("sentence-transformers")
  st <- tryCatch(
    reticulate::import("sentence_transformers"),
    error = function(e) {
      stop(
        "Python 'sentence-transformers' could not be loaded (",
        conditionMessage(e), ").\n",
        "Provision the Python environment with sfa_install_python(), or pass ",
        "precomputed embeddings: sfa(items, embeddings = your_matrix)",
        call. = FALSE
      )
    }
  )
  torch <- reticulate::import("torch")
  device <- if (torch$cuda$is_available()) {
    "cuda"
  } else if (torch$backends$mps$is_available()) {
    "mps"
  } else {
    "cpu"
  }
  encoder <- st$SentenceTransformer(model, device = device)
  emb <- encoder$encode(items, show_progress_bar = FALSE)
  emb_r <- reticulate::py_to_r(emb)
  if (!is.matrix(emb_r)) emb_r <- as.matrix(emb_r)
  storage.mode(emb_r) <- "double"
  emb_r
}

#' @keywords internal
.embed_openai <- function(items, model, ...) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop(
      "The 'openai' embedding backend requires the 'httr2' package.\n",
      "Install with: install.packages('httr2')",
      call. = FALSE
    )
  }
  api_key <- Sys.getenv("OPENAI_API_KEY", "")
  if (api_key == "") {
    stop("OPENAI_API_KEY environment variable is not set.", call. = FALSE)
  }

  resp <- httr2::request("https://api.openai.com/v1/embeddings") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(list(input = as.list(items), model = model)) |>
    httr2::req_perform()

  body <- httr2::resp_body_json(resp)
  emb_list <- body$data
  emb_list <- emb_list[order(vapply(emb_list, `[[`, integer(1), "index"))]
  emb <- do.call(rbind, lapply(emb_list, function(x) unlist(x$embedding)))
  storage.mode(emb) <- "double"
  emb
}
