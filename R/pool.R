# Candidate-pool handling for factor naming: fetch pre-generated pool
# embeddings (word list + per-word metadata + embedding matrix) or build them
# locally for an arbitrary model. Pools live in the user cache
# (tools::R_user_dir("semanticfa", "cache")/pools), never inside the package.

# Registry of pre-generated pools. `parts` allows multi-file assets (GitHub
# release assets are capped at 2 GB each). Sizes are informational and shown
# before download. The URL base is overridable via
# options(semanticfa.pool_url = ...) for mirrors and testing.
.SFA_POOL_BASE <- "https://github.com/devon7y/semanticfa/releases/download/pools-v1"

.SFA_POOL_REGISTRY <- list(
  "Qwen/Qwen3-Embedding-0.6B"      = list(dim = 1024L),
  "Qwen/Qwen3-Embedding-4B"        = list(dim = 2560L),
  "Qwen/Qwen3-Embedding-8B"        = list(dim = 4096L),
  "microsoft/harrier-oss-v1-27b"   = list(dim = 5376L)
)

#' @keywords internal
.pool_slug <- function(model) {
  gsub("[^A-Za-z0-9.]+", "-", model)
}

#' @keywords internal
.pool_dir <- function(dir = NULL) {
  d <- dir %||% file.path(tools::R_user_dir("semanticfa", "cache"), "pools")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

#' @keywords internal
.pool_url <- function(file) {
  base <- getOption("semanticfa.pool_url", .SFA_POOL_BASE)
  paste0(base, "/", file)
}

#' @keywords internal
.pool_download <- function(file, dest, quiet = FALSE) {
  url <- .pool_url(file)
  status <- utils::download.file(url, dest, mode = "wb", quiet = quiet)
  if (status != 0L || !file.exists(dest)) {
    stop("Download failed for ", url, call. = FALSE)
  }
  invisible(dest)
}

#' Fetch or Build the Candidate Pool for a Naming Model
#'
#' Returns the candidate pool used by [sfa_name()]: the pre-filtered word
#' list (369,703 label-eligible terms with precomputed word-family and
#' dictionary-membership columns) together with its embedding matrix under
#' the given model. Pre-generated pools are downloaded once and cached;
#' for models without a pre-generated pool the word list is embedded
#' locally (slow without a GPU) and cached thereafter.
#'
#' @param model Embedding model id (as used by the \code{sbert} backend).
#' @param precision \code{"int8"} (default) or \code{"fp16"}. int8 pools
#'   are half the download and reproduce the fp16 labels on all but 3 of 75
#'   benchmark factors (each a weak factor relabeled with a near-synonym;
#'   the diff is listed in the package documentation). Use \code{"fp16"}
#'   for exact parity with the published research pipeline. Locally built
#'   pools are always fp16.
#' @param download Permission to download missing artifacts. Defaults to
#'   \code{interactive()}; in non-interactive sessions pass \code{TRUE}
#'   explicitly (CRAN policy: no silent large downloads).
#' @param build Permission to embed the word list locally when no
#'   pre-generated pool exists for \code{model}. Defaults to
#'   \code{interactive()}.
#' @param dir Cache directory override (mainly for tests).
#' @returns An object of class \code{sfa_pool}: a list with \code{words}
#'   (data.frame: word, family, tier1), \code{emb} (matrix-like, one row
#'   per word, memory-mapped when read from disk), \code{dim},
#'   \code{model}, \code{precision}.
#' @export
sfa_pool <- function(model,
                     precision = c("int8", "fp16"),
                     download = interactive(),
                     build = interactive(),
                     dir = NULL) {
  precision <- match.arg(precision)
  d <- .pool_dir(dir)
  slug <- .pool_slug(model)

  # ---- word list (shared across models, versioned with the pools) --------
  wl_path <- file.path(d, "wordlist.rds")
  if (!file.exists(wl_path)) {
    if (!download) {
      stop("The naming word list is not cached yet and download = FALSE.\n",
           "Call sfa_pool(model, download = TRUE) once to fetch it.",
           call. = FALSE)
    }
    message("Downloading naming word list (~3 MB) ...")
    .pool_download("wordlist.rds", wl_path)
  }
  words <- readRDS(wl_path)
  stopifnot(all(c("word", "family", "tier1") %in% names(words)))

  # ---- embedding matrix ---------------------------------------------------
  manifest_path <- file.path(d, paste0(slug, "_", precision, ".manifest.rds"))
  emb_path <- file.path(d, paste0(slug, "_", precision, ".npy"))
  scale_path <- file.path(d, paste0(slug, "_", precision, ".scales.npy"))

  if (!file.exists(emb_path)) {
    hosted <- model %in% names(.SFA_POOL_REGISTRY)
    if (hosted) {
      if (!download) {
        stop("Pool for '", model, "' is not cached and download = FALSE.",
             call. = FALSE)
      }
      manifest_file <- paste0(slug, "_", precision, ".manifest.rds")
      .pool_download(manifest_file, manifest_path, quiet = TRUE)
      man <- readRDS(manifest_path)
      message(sprintf("Downloading %s pool for %s (%.1f GB, one-time) ...",
                      precision, model, man$total_bytes / 1e9))
      if (length(man$parts) == 1L) {
        .pool_download(man$parts[[1L]], emb_path)
      } else {
        part_paths <- character(length(man$parts))
        for (i in seq_along(man$parts)) {
          part_paths[i] <- file.path(d, man$parts[[i]])
          .pool_download(man$parts[[i]], part_paths[i])
        }
        .pool_concat(part_paths, emb_path)
        unlink(part_paths)
      }
      if (!is.null(man$scales_file)) {
        .pool_download(man$scales_file, scale_path)
      }
      if (!is.null(man$sha256)) {
        got <- digest::digest(file = emb_path, algo = "sha256")
        if (!identical(got, man$sha256)) {
          unlink(emb_path)
          stop("Checksum mismatch for downloaded pool; removed. Re-run to retry.",
               call. = FALSE)
        }
      }
    } else {
      if (!build) {
        stop("No pre-generated pool exists for '", model, "' and build = FALSE.\n",
             "Call sfa_pool(model, build = TRUE) to embed the word list ",
             "locally (fast on GPU, hours on CPU).", call. = FALSE)
      }
      .sfa_build_pool(model, words$word, emb_path)
      precision <- "fp16"
    }
  }

  emb <- .pool_open(emb_path,
                    scales = if (file.exists(scale_path)) scale_path else NULL,
                    n = nrow(words))
  structure(
    list(words = words, emb = emb, dim = emb$d,
         model = model, precision = precision, path = emb_path),
    class = "sfa_pool"
  )
}

# Concatenate split release assets back into one file (2 GB/asset limit).
#' @keywords internal
.pool_concat <- function(parts, dest) {
  out <- file(dest, "wb")
  on.exit(close(out))
  for (p in parts) {
    inp <- file(p, "rb")
    while (length(chunk <- readBin(inp, "raw", n = 64L * 1024L^2)) > 0L) {
      writeBin(chunk, out)
    }
    close(inp)
  }
  invisible(dest)
}

# Open a pool embedding matrix. .npy files are memory-mapped through numpy
# (reticulate is already an Import) so a 3 GB pool never fully enters RAM;
# plain R matrices are accepted as-is (used by the offline test fixtures).
#' @keywords internal
.pool_open <- function(path, scales = NULL, n = NULL) {
  np <- reticulate::import("numpy", convert = FALSE)
  arr <- np$load(path, mmap_mode = "r")
  sc <- if (!is.null(scales)) np$load(scales) else NULL
  structure(list(np = np, arr = arr, scales = sc,
                 n = as.integer(n %||% reticulate::py_to_r(arr$shape[0])),
                 d = as.integer(reticulate::py_to_r(arr$shape[1]))),
            class = "sfa_pool_mmap")
}

# Read rows [i0, i1) of a pool as a plain double matrix, dequantized and
# L2-normalized. Works for the mmap wrapper and for plain matrices.
#' @keywords internal
.pool_block <- function(emb, i0, i1) {
  if (is.matrix(emb)) {
    block <- emb[(i0 + 1L):i1, , drop = FALSE]
  } else {
    np <- emb$np
    py <- reticulate::import_builtins(convert = FALSE)
    sl <- emb$arr[py$slice(as.integer(i0), as.integer(i1))]
    block <- reticulate::py_to_r(np$asarray(sl, dtype = "float32"))
    if (!is.null(emb$scales)) {
      s <- as.numeric(reticulate::py_to_r(emb$scales))[(i0 + 1L):i1]
      block <- block * s
    }
  }
  nrm <- sqrt(rowSums(block^2))
  nrm[nrm < 1e-12] <- 1
  block / nrm
}

#' @keywords internal
.pool_nrow <- function(emb) {
  if (is.matrix(emb)) nrow(emb) else emb$n
}

# Embed the shipped word list locally for a model without a hosted pool.
#' @keywords internal
.sfa_build_pool <- function(model, words, dest) {
  message("Embedding ", length(words), " candidate words with '", model,
          "' - this is a one-time build (minutes on GPU, hours on CPU).")
  emb <- .embed_sbert(words, model)
  nrm <- sqrt(rowSums(emb^2)); nrm[nrm < 1e-12] <- 1
  emb <- emb / nrm
  np <- reticulate::import("numpy", convert = FALSE)
  np$save(dest, np$asarray(emb, dtype = "float16"))
  invisible(dest)
}

#' @export
print.sfa_pool <- function(x, ...) {
  cat("semanticfa candidate pool\n")
  cat("  model:    ", x$model, "\n")
  cat("  words:    ", format(nrow(x$words), big.mark = ","), "\n")
  cat("  dim:      ", x$dim, "  precision: ", x$precision, "\n", sep = "")
  invisible(x)
}
