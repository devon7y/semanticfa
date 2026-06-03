#' Embed Item Text with a Language Model
#'
#' Computes embeddings for a vector of item text using a sentence-transformer
#' or other embedding backend.
#'
#' @param items Character vector of item text.
#' @param embed Embedding backend: \code{"sbert"} (default, via
#'   \code{reticulate}), \code{"openai"} (via \code{httr2}), or a function
#'   taking a character vector and returning a numeric matrix.
#' @param model Model name passed to the backend (default
#'   \code{"all-MiniLM-L6-v2"} for sbert).
#' @param cache Logical: cache embeddings in
#'   \code{tools::R_user_dir("semanticfa", "cache")}? Default \code{TRUE}.
#' @param ... Additional arguments passed to the embedding backend function.
#'
#' @returns A numeric matrix (n_items x embedding_dim) with item text as
#'   rownames.
#'
#' @export
sfa_embed <- function(items, embed = "sbert", model = "all-MiniLM-L6-v2",
                      cache = TRUE, ...) {
  if (is.function(embed)) {
    emb <- embed(items, ...)
    if (!is.matrix(emb) || !is.numeric(emb)) {
      stop("Custom embed function must return a numeric matrix.", call. = FALSE)
    }
    if (nrow(emb) != length(items)) {
      stop("Custom embed function returned ", nrow(emb), " rows for ",
           length(items), " items.", call. = FALSE)
    }
    rownames(emb) <- items
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
        return(cached)
      }
    }
  }

  emb <- switch(embed,
    sbert  = .embed_sbert(items, model, ...),
    openai = .embed_openai(items, model, ...)
  )

  rownames(emb) <- items

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

#' @keywords internal
.embed_sbert <- function(items, model, ...) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop(
      "The 'sbert' embedding backend requires the 'reticulate' package and\n",
      "Python 'sentence-transformers'. Install with:\n",
      "  install.packages('reticulate')\n",
      "  reticulate::py_install('sentence-transformers')\n",
      "Or pass precomputed embeddings: sfa(items, embeddings = your_matrix)",
      call. = FALSE
    )
  }
  st <- tryCatch(
    reticulate::import("sentence_transformers"),
    error = function(e) {
      stop(
        "Python 'sentence-transformers' not found. Install with:\n",
        "  reticulate::py_install('sentence-transformers')\n",
        "Or pass precomputed embeddings: sfa(items, embeddings = your_matrix)",
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
