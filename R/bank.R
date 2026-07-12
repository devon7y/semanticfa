# Embedding banks: pre-generated embeddings so every analysis runs without
# an encoder. The reproducibility contract (PAPER_PLAN §6): regions and
# banks are generated once (typically on a GPU machine, by these same
# functions), published, and every audit a reader runs consumes them - the
# encoder never has to load on the reader's machine.

#' Pre-Embed a Set of Texts into a Reusable Bank
#'
#' Embeds texts (typically: every scale item, every construct definition,
#' and every planned narrowing of a study) under one encoder and stores the
#' vectors keyed by the exact strings [sfa_coverage()] will pass to its
#' embedder. Feed the result to audits via [sfa_embedding_bank()].
#'
#' @param texts Character vector of raw texts (items, definitions). Wrapped
#'   with `instruction` exactly as [sfa_coverage()] wraps them, so lookups
#'   match.
#' @param instruction `TRUE` (the construct-retrieval instruction the
#'   regions use, default), `FALSE`, or a custom string. Must match the
#'   regions the bank will be used with.
#' @param embed,model,cache As in [sfa_embed()].
#' @param file Optional path to save the bank (`.rds`).
#' @returns An `"sfa_bank"`: vectors, texts, instruction, encoder,
#'   timestamp.
#' @export
sfa_build_bank <- function(texts, instruction = TRUE, embed = "sbert",
                           model = NULL, cache = TRUE, file = NULL) {
  texts <- unique(as.character(texts))
  if (!length(texts)) stop("'texts' is empty.", call. = FALSE)
  instr <- .cvg_instruction_text(instruction)
  wrapped <- .cvg_wrap_instruction(texts, instr)
  emb <- .cvg_normalize(unname(sfa_embed(wrapped, embed = embed,
                                         model = model, cache = cache)))
  rownames(emb) <- wrapped
  bank <- structure(list(
    vectors = emb,
    texts = texts,
    instruction = instr,
    encoder = if (is.function(embed)) "custom function" else
      .resolve_embed_model(embed, model),
    built = Sys.time(),
    semanticfa = as.character(utils::packageVersion("semanticfa"))
  ), class = "sfa_bank")
  if (!is.null(file)) {
    saveRDS(bank, file)
    message("Bank saved to ", file, " (", length(texts), " texts, ",
            ncol(emb), "-d, ", bank$encoder, ")")
  }
  bank
}

#' Use a Pre-Generated Embedding Bank as an Embedder
#'
#' Turns an `"sfa_bank"` (or a path to one) into a lookup function that
#' [sfa_coverage()] and [sfa_embed()] accept as their `embed` argument, so
#' audits run entirely from published embeddings - no encoder loads.
#'
#' A lookup miss is an error, not a fallback: the bank must contain every
#' item, definition, and planned narrowing the audit touches. The error
#' lists what is missing so the bank can be rebuilt to include it.
#'
#' @param bank An `"sfa_bank"` from [sfa_build_bank()], or a path to one.
#' @returns A function `(texts) -> matrix`, for use as `embed = `.
#' @examples
#' \dontrun{
#' bank <- sfa_embedding_bank("bank_qwen8b.rds")
#' region <- sfa_load_region("regions_qwen8b/procrastination.rds")
#' audit <- sfa_coverage(my_items, region, embed = bank,
#'                       model = region$encoder)
#' }
#' @export
sfa_embedding_bank <- function(bank) {
  if (is.character(bank) && length(bank) == 1L) bank <- readRDS(bank)
  if (!inherits(bank, "sfa_bank")) {
    stop("'bank' must be an sfa_bank (see sfa_build_bank()) or a path to ",
         "one.", call. = FALSE)
  }
  vectors <- bank$vectors
  encoder <- bank$encoder
  function(texts, ...) {
    idx <- match(texts, rownames(vectors))
    if (anyNA(idx)) {
      missing <- texts[is.na(idx)]
      stop(length(missing), " text(s) are not in the embedding bank ",
           "(encoder: ", encoder, "). The bank must contain every item, ",
           "definition, and narrowing the audit embeds - rebuild it with ",
           "these texts included. First missing: \"",
           substr(missing[1], 1, 100), "\"", call. = FALSE)
    }
    vectors[idx, , drop = FALSE]
  }
}

#' Use a Region's Own Stored Embeddings as an Embedder
#'
#' Turns an `"sfa_region"` into a lookup function over its own sentences,
#' for use as `embed =` when the "items" of an audit are drawn from a
#' region itself. The canonical use is the cross-audit region-overlap
#' baseline: sentences sampled from construct region A are audited as
#' pretend items against region B, so their embeddings come from A's own
#' stored matrix and no encoder loads. Requires the two regions to share
#' the same instruction (regions built together always do).
#'
#' @param region An `"sfa_region"` (or path to one) with embeddings.
#' @returns A function `(texts) -> matrix`, for use as `embed = `.
#' @export
sfa_region_bank <- function(region) {
  if (is.character(region) && length(region) == 1L) {
    region <- sfa_load_region(region)
  }
  if (!inherits(region, "sfa_region")) {
    stop("'region' must be an sfa_region or a path to one.", call. = FALSE)
  }
  if (is.null(region$embeddings)) {
    stop("This region has no embeddings (built with embeddings = FALSE); ",
         "run sfa_reembed_region() first.", call. = FALSE)
  }
  vectors <- region$embeddings
  rownames(vectors) <- .cvg_wrap_instruction(region$sentences$text,
                                             region$instruction)
  construct <- region$construct
  function(texts, ...) {
    idx <- match(texts, rownames(vectors))
    if (anyNA(idx)) {
      stop(sum(is.na(idx)), " text(s) are not sentences of the '",
           construct, "' region.", call. = FALSE)
    }
    vectors[idx, , drop = FALSE]
  }
}

#' Combine Embedding Lookups
#'
#' Chains embed-functions (from [sfa_embedding_bank()] or
#' [sfa_region_bank()]) into one: each text is served by the first lookup
#' that contains it. Needed when one audit embeds texts from two sources,
#' e.g. region-drawn pretend items (a region bank) plus the target
#' region's definition (the main bank).
#'
#' @param ... Embed functions, tried in order.
#' @returns A function `(texts) -> matrix`, for use as `embed = `.
#' @export
sfa_combine_banks <- function(...) {
  banks <- list(...)
  stopifnot(length(banks) >= 1L, all(vapply(banks, is.function, logical(1))))
  function(texts, ...) {
    n <- length(texts)
    out <- NULL
    filled <- rep(FALSE, n)
    for (b in banks) {
      remaining <- which(!filled)
      if (!length(remaining)) break
      got <- tryCatch(b(texts[remaining]), error = function(e) NULL)
      if (is.null(got)) {
        # this bank lacks some of the texts; resolve one-by-one
        for (i in remaining) {
          v <- tryCatch(b(texts[i]), error = function(e) NULL)
          if (!is.null(v)) {
            if (is.null(out)) out <- matrix(NA_real_, n, ncol(v))
            out[i, ] <- v
            filled[i] <- TRUE
          }
        }
      } else {
        if (is.null(out)) out <- matrix(NA_real_, n, ncol(got))
        out[remaining, ] <- got
        filled[remaining] <- TRUE
      }
    }
    if (!all(filled)) {
      stop(sum(!filled), " text(s) found in none of the combined banks. ",
           "First missing: \"",
           substr(texts[which(!filled)[1]], 1, 80), "\"", call. = FALSE)
    }
    out
  }
}

#' @export
print.sfa_bank <- function(x, ...) {
  cat("Embedding bank: ", length(x$texts), " texts, ",
      ncol(x$vectors), "-d\n", sep = "")
  cat("  encoder: ", x$encoder,
      if (!is.null(x$instruction)) " (instruction-conditioned)" else "",
      "\n", sep = "")
  cat("  built: ", format(x$built, "%Y-%m-%d"), " | semanticfa ",
      x$semanticfa, "\n", sep = "")
  invisible(x)
}
