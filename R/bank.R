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
