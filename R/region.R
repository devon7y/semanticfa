# Construct-region estimation for content-validity auditing (sfa_coverage).
#
# A construct is represented as a region in embedding space: the cloud of
# real corpus sentences that mention the construct term. The region file a
# user builds here is a self-contained, versioned research artifact meant to
# be archived alongside the analysis that used it.

# Instruction used to condition both region sentences and scale items into
# the construct-retrieval subspace of the encoder (Qwen3 query format).
.CVG_INSTRUCTION <-
  "Given a text, retrieve the psychological construct the text is about"

#' @keywords internal
.cvg_instruction_text <- function(instruction) {
  if (isTRUE(instruction)) return(.CVG_INSTRUCTION)
  if (isFALSE(instruction) || is.null(instruction)) return(NULL)
  if (is.character(instruction) && length(instruction) == 1L) return(instruction)
  stop("'instruction' must be TRUE, FALSE, or a single string.", call. = FALSE)
}

#' @keywords internal
.cvg_wrap_instruction <- function(texts, instruction) {
  if (is.null(instruction)) return(texts)
  paste0("Instruct: ", instruction, "\nQuery: ", texts)
}

#' @keywords internal
.cvg_normalize <- function(m) {
  m / pmax(sqrt(rowSums(m^2)), 1e-12)
}

#' @keywords internal
# Simple morphological variants of the construct term, used for matching
# corpus sentences. Users can override via the `variants` argument.
.cvg_variants <- function(term) {
  t <- tolower(trimws(term))
  v <- c(t, paste0(t, "s"), paste0(t, "es"))
  if (grepl("y$", t)) v <- c(v, sub("y$", "ies", t))
  unique(v)
}

#' @keywords internal
.cvg_split_sentences <- function(text) {
  out <- unlist(strsplit(text, "(?<=[.!?])\\s+(?=[A-Z\"'(])", perl = TRUE))
  trimws(out)
}

#' @keywords internal
.cvg_mentions <- function(sentences, variants) {
  low <- tolower(sentences)
  hit <- rep(FALSE, length(low))
  for (v in variants) hit <- hit | grepl(v, low, fixed = TRUE)
  hit
}

#' Build a Construct Region from a Text Corpus
#'
#' Assembles the corpus half of a content-validity audit: a *construct
#' region*, the cloud of real sentences that mention a construct term,
#' embedded in the same space that [sfa_coverage()] will embed the scale's
#' items into. The result is a self-contained object recording its own
#' provenance (corpus, extraction parameters, encoder, date); save it with
#' `file =` and archive it with your analysis so the audit is reproducible.
#'
#' Two kinds of corpus are supported. `corpus = "fineweb-10bt"` streams the
#' `sample-10BT` configuration of the FineWeb corpus (a documented random
#' sample of a modern LLM-training corpus) from the Hugging Face Hub via the
#' Python `datasets` package, stopping as soon as `target` sentences are
#' found or `max_docs` documents have been scanned; common construct terms
#' hit their quota within minutes, rare terms scan the full sample (use an
#' HPC batch job, or lower `max_docs` and check saturation). Alternatively,
#' pass your own corpus: a character vector of documents, a data frame with
#' a `text` column, or paths to plain-text files or directories. A
#' domain-specific corpus (for example, workplace communications for a
#' workplace construct) is a fully disclosed design choice recorded in the
#' region's provenance.
#'
#' Sentences are *not* sense-filtered here: the region stores every mention,
#' and [sfa_coverage()] applies its sense gate at audit time against the
#' definition supplied there. This is what makes construct narrowing cheap:
#' one region file for "procrastination" can be re-audited as "academic
#' procrastination" by re-gating with a narrower definition, with no new
#' extraction.
#'
#' @param construct Construct term to search for, e.g. `"procrastination"`.
#' @param definition One- or two-sentence definition of the intended sense.
#'   Stored with the region and used as the default sense-gate seed in
#'   [sfa_coverage()].
#' @param corpus `"fineweb-10bt"` (default; streams from the Hugging Face
#'   Hub), a character vector of documents, a data frame with a
#'   `text`/`item` column, or a character vector of file or directory paths.
#' @param target Stop once this many matching sentences are collected.
#'   Default 1500.
#' @param max_docs Maximum number of corpus documents to scan. Default
#'   `2e7` (covers the full FineWeb 10BT sample). Lower it to cap runtime on
#'   a laptop; the saturation diagnostics in [sfa_coverage()] show whether
#'   the smaller region was enough.
#' @param sentences_per_doc Maximum sentences kept per document (guards
#'   against one document flooding the region). Default 3.
#' @param min_chars,max_chars Sentence length bounds. Defaults 30 and 500.
#' @param variants Character vector of term spellings to match. Default
#'   `NULL` generates simple morphological variants of `construct`.
#' @param embed,model,cache Passed to [sfa_embed()]; `model` defaults to the
#'   package's default encoder. The audit must use the same encoder, which
#'   [sfa_coverage()] enforces from the region's metadata.
#' @param instruction `TRUE` (default) embeds sentences under the
#'   construct-retrieval instruction (recommended: this register alignment
#'   outperformed alternatives in validation), `FALSE` embeds raw text, or a
#'   custom instruction string.
#' @param file Optional path; when given, the region is saved there with
#'   [saveRDS()] and can be reloaded with [sfa_load_region()].
#' @param progress Print progress while streaming. Default `TRUE`.
#'
#' @returns An object of class `"sfa_region"`: a list with the sentences and
#'   their sources, the embedding matrix, and full provenance metadata.
#'
#' @seealso [sfa_coverage()] to audit a scale against the region,
#'   [sfa_load_region()] to reload a saved region.
#'
#' @examples
#' \dontrun{
#' region <- sfa_build_region(
#'   construct  = "procrastination",
#'   definition = paste("Procrastination is the voluntary delay of an",
#'                      "intended action despite expecting to be worse off."),
#'   file       = "procrastination_region.rds"
#' )
#'
#' # a laptop-friendly build capped at 2M documents
#' region <- sfa_build_region("procrastination", definition = "...",
#'                            max_docs = 2e6)
#'
#' # your own corpus
#' region <- sfa_build_region("procrastination", definition = "...",
#'                            corpus = "~/corpora/workplace_emails/")
#' }
#' @export
sfa_build_region <- function(construct,
                             definition,
                             corpus = "fineweb-10bt",
                             target = 1500,
                             max_docs = 2e7,
                             sentences_per_doc = 3,
                             min_chars = 30,
                             max_chars = 500,
                             variants = NULL,
                             embed = "sbert",
                             model = NULL,
                             instruction = TRUE,
                             cache = TRUE,
                             file = NULL,
                             progress = TRUE) {
  if (!is.character(construct) || length(construct) != 1L || !nzchar(construct)) {
    stop("'construct' must be a single non-empty string.", call. = FALSE)
  }
  if (!is.character(definition) || length(definition) != 1L || !nzchar(definition)) {
    stop("'definition' must be a single non-empty string; it is the ",
         "sense-gate seed for the audit.", call. = FALSE)
  }
  variants <- variants %||% .cvg_variants(construct)
  instr <- .cvg_instruction_text(instruction)

  if (is.character(corpus) && length(corpus) == 1L &&
      identical(tolower(corpus), "fineweb-10bt")) {
    got <- .cvg_stream_fineweb(variants, target, max_docs, sentences_per_doc,
                               min_chars, max_chars, progress)
    corpus_id <- "HuggingFaceFW/fineweb:sample-10BT"
  } else {
    got <- .cvg_local_corpus(corpus, variants, target, max_docs,
                             sentences_per_doc, min_chars, max_chars)
    corpus_id <- got$corpus_id
  }
  if (length(got$text) == 0L) {
    stop("No sentences mentioning '", construct, "' were found. Check the ",
         "term (or supply 'variants'), or raise 'max_docs'.", call. = FALSE)
  }

  # dedupe on a whitespace/case-normalized key
  key <- tolower(gsub("[^a-z0-9]+", " ", tolower(got$text)))
  keep <- !duplicated(key)
  text <- got$text[keep]
  source <- got$source[keep]

  if (isTRUE(progress)) {
    message(length(text), " unique sentences; embedding with ",
            .resolve_embed_model(embed, model), " ...")
  }
  emb <- sfa_embed(.cvg_wrap_instruction(text, instr),
                   embed = embed, model = model, cache = cache)
  emb <- .cvg_normalize(unname(emb))

  region <- structure(list(
    construct      = construct,
    definition     = definition,
    sentences      = data.frame(text = text, source = source,
                                stringsAsFactors = FALSE),
    embeddings     = emb,
    corpus         = corpus_id,
    docs_streamed  = got$docs_streamed,
    target         = target,
    max_docs       = max_docs,
    variants       = variants,
    encoder        = .resolve_embed_model(embed, model),
    instruction    = instr,
    extracted      = Sys.time(),
    semanticfa     = as.character(utils::packageVersion("semanticfa"))
  ), class = "sfa_region")

  if (!is.null(file)) {
    saveRDS(region, file)
    message("Region saved to ", file,
            " - archive this file with your analysis.")
  }
  region
}

#' @keywords internal
# Streams FineWeb sample-10BT through the Python 'datasets' package. The
# whole scan loop runs inside Python (crossing the R/Python boundary per
# document would be orders of magnitude slower); only the matched sentences
# come back to R.
.cvg_stream_fineweb <- function(variants, target, max_docs, per_doc,
                                min_chars, max_chars, progress) {
  .sfa_py_require(c("datasets"))
  ok <- tryCatch({reticulate::import("datasets"); TRUE},
                 error = function(e) FALSE)
  if (!ok) {
    stop("Streaming FineWeb needs the Python 'datasets' package in the same ",
         "environment as the encoder. Install it with ",
         "reticulate::py_install(\"datasets\"), or pass your own corpus via ",
         "the 'corpus' argument.", call. = FALSE)
  }
  reticulate::py_run_string("
import re as _sfa_re

_SFA_SENT = _sfa_re.compile(r\"(?<=[.!?])\\s+(?=[A-Z\\\"'(])\")

def sfa_stream_corpus(variants, target, max_docs, per_doc,
                      min_chars, max_chars, progress):
    from datasets import load_dataset
    ds = load_dataset('HuggingFaceFW/fineweb', name='sample-10BT',
                      split='train', streaming=True)
    texts, sources = [], []
    n_docs = 0
    for doc in ds:
        n_docs += 1
        if n_docs > max_docs or len(texts) >= target:
            break
        body = doc.get('text', '')
        low = body.lower()
        if not any(v in low for v in variants):
            continue
        kept = 0
        for s in _SFA_SENT.split(body):
            s = s.strip()
            if not (min_chars <= len(s) <= max_chars):
                continue
            if any(v in s.lower() for v in variants):
                texts.append(s)
                sources.append(doc.get('url') or '')
                kept += 1
                if kept >= per_doc or len(texts) >= target:
                    break
        if progress and n_docs % 200000 == 0:
            print(f'  scanned {n_docs/1e6:.1f}M documents, '
                  f'{len(texts)} sentences', flush=True)
    return {'text': texts, 'source': sources, 'docs': n_docs}
")
  res <- reticulate::py$sfa_stream_corpus(
    as.list(variants), as.integer(target), as.integer(max_docs),
    as.integer(per_doc), as.integer(min_chars), as.integer(max_chars),
    isTRUE(progress))
  list(text = as.character(unlist(res$text)),
       source = as.character(unlist(res$source)),
       docs_streamed = as.integer(res$docs))
}

#' @keywords internal
# Local corpora: a data frame with a text column, paths to files or
# directories of plain-text files, or a character vector of documents.
.cvg_local_corpus <- function(corpus, variants, target, max_docs, per_doc,
                              min_chars, max_chars) {
  if (is.data.frame(corpus)) {
    col <- intersect(c("text", "item"), names(corpus))[1]
    if (is.na(col)) {
      stop("A data-frame corpus must have a 'text' column.", call. = FALSE)
    }
    docs <- as.character(corpus[[col]])
    corpus_id <- sprintf("user data.frame (%d documents)", length(docs))
  } else if (is.character(corpus) && length(corpus) > 0 &&
             all(file.exists(corpus))) {
    paths <- unlist(lapply(corpus, function(p) {
      if (dir.exists(p)) {
        list.files(p, full.names = TRUE, recursive = TRUE)
      } else p
    }))
    docs <- vapply(paths, function(p) {
      paste(tryCatch(readLines(p, warn = FALSE), error = function(e) ""),
            collapse = " ")
    }, character(1))
    corpus_id <- sprintf("user files (%d documents from %s)", length(docs),
                         paste(utils::head(corpus, 3), collapse = ", "))
  } else if (is.character(corpus)) {
    docs <- corpus
    corpus_id <- sprintf("user character vector (%d documents)", length(docs))
  } else {
    stop("'corpus' must be \"fineweb-10bt\", a character vector, file/",
         "directory paths, or a data frame with a 'text' column.",
         call. = FALSE)
  }

  docs <- utils::head(docs, max_docs)
  texts <- character(0)
  sources <- character(0)
  for (i in seq_along(docs)) {
    if (length(texts) >= target) break
    low <- tolower(docs[[i]])
    if (!any(vapply(variants, grepl, logical(1), x = low, fixed = TRUE))) next
    sents <- .cvg_split_sentences(docs[[i]])
    n <- nchar(sents)
    sents <- sents[n >= min_chars & n <= max_chars]
    sents <- sents[.cvg_mentions(sents, variants)]
    sents <- utils::head(sents, per_doc)
    texts <- c(texts, sents)
    sources <- c(sources, rep(sprintf("doc_%d", i), length(sents)))
  }
  list(text = utils::head(texts, target),
       source = utils::head(sources, target),
       docs_streamed = length(docs), corpus_id = corpus_id)
}

#' @export
print.sfa_region <- function(x, ...) {
  cat("Construct region: \"", x$construct, "\"\n", sep = "")
  cat("  ", nrow(x$sentences), " sentences from ", x$corpus,
      " (", format(x$docs_streamed, big.mark = ","), " documents scanned)\n",
      sep = "")
  cat("  encoder: ", x$encoder,
      if (!is.null(x$instruction)) " (instruction-conditioned)" else "",
      "\n", sep = "")
  cat("  extracted: ", format(x$extracted, "%Y-%m-%d"),
      " | semanticfa ", x$semanticfa, "\n", sep = "")
  cat("  definition: ", x$definition, "\n", sep = "")
  invisible(x)
}

#' Load a Saved Construct Region
#'
#' Reads a region saved by [sfa_build_region()] (via its `file` argument or
#' [saveRDS()]) and validates its class.
#'
#' @param file Path to the saved `.rds` region file.
#' @returns The `"sfa_region"` object.
#' @export
sfa_load_region <- function(file) {
  region <- readRDS(file)
  if (!inherits(region, "sfa_region")) {
    stop("'", file, "' is not a saved sfa_region.", call. = FALSE)
  }
  region
}
