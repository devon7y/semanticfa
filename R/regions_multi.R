# Campaign-scale region building: many constructs in one corpus pass, with
# deferred embedding for encoder ladders. sfa_build_region() (singular)
# remains the one-construct interface; these functions exist so an entire
# multi-construct, multi-encoder study is a sequence of semanticfa calls.

#' @keywords internal
.cvg_slug <- function(x) {
  gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", tolower(x)))
}

#' @keywords internal
# Validate the constructs registry: a named list, each element a list with
# $definition and optional $variants.
.cvg_check_constructs <- function(constructs) {
  if (!is.list(constructs) || is.null(names(constructs)) ||
      any(!nzchar(names(constructs)))) {
    stop("'constructs' must be a named list: name -> list(definition = , ",
         "variants = NULL).", call. = FALSE)
  }
  lapply(names(constructs), function(nm) {
    spec <- constructs[[nm]]
    if (is.character(spec)) spec <- list(definition = spec)
    if (!is.list(spec) || !nzchar(spec$definition %||% "")) {
      stop("Construct '", nm, "' needs a one-sentence 'definition'.",
           call. = FALSE)
    }
    spec$variants <- spec$variants %||% {
      t <- tolower(nm)
      v <- unique(c(t, paste0(t, "s"), paste0(t, "es"),
                    if (endsWith(t, "y")) paste0(substr(t, 1, nchar(t) - 1),
                                                 "ies")))
      v
    }
    spec
  }) |> stats::setNames(names(constructs))
}

#' @keywords internal
# One streaming pass over FineWeb for every construct at once. Variants are
# matched on word boundaries (several campaign components are short common
# words - care, harm, respect - for which substring matching would hit
# career, scared, harmless). The scan loop runs inside Python; only matched
# sentences come back to R.
.cvg_stream_fineweb_multi <- function(registry, target, max_docs, per_doc,
                                      min_chars, max_chars, progress) {
  import_err <- NULL
  ok <- tryCatch({reticulate::import("datasets"); TRUE},
                 error = function(e) {
                   import_err <<- conditionMessage(e)
                   FALSE
                 })
  if (!ok) {
    stop("Streaming FineWeb needs the Python 'datasets' package in the ",
         "reticulate environment. Install it with ",
         "reticulate::py_install(\"datasets\"), or pass your own corpus via ",
         "the 'corpus' argument.\nUnderlying import error: ", import_err,
         call. = FALSE)
  }
  reticulate::py_run_string("
import re as _sfa_re

_SFA_SENT_M = _sfa_re.compile(r\"(?<=[.!?])\\s+(?=[A-Z\\\"'(])\")

def sfa_stream_corpus_multi(registry, target, max_docs, per_doc,
                            min_chars, max_chars, progress):
    # long streams hit transient hub 503s; be patient rather than die
    import datasets.config as _sfa_dcfg
    if hasattr(_sfa_dcfg, 'STREAMING_READ_MAX_RETRIES'):
        _sfa_dcfg.STREAMING_READ_MAX_RETRIES = 50
    if hasattr(_sfa_dcfg, 'STREAMING_READ_RETRY_INTERVAL'):
        _sfa_dcfg.STREAMING_READ_RETRY_INTERVAL = 15
    from datasets import load_dataset
    ds = load_dataset('HuggingFaceFW/fineweb', name='sample-10BT',
                      split='train', streaming=True)
    terms = list(registry)
    matchers = {t: _sfa_re.compile(
                    '|'.join(r'\\b' + _sfa_re.escape(v.lower()) + r'\\b'
                             for v in registry[t]))
                for t in terms}
    pre = {t: [v.lower() for v in registry[t]] for t in terms}
    texts = {t: [] for t in terms}
    sources = {t: [] for t in terms}
    n_docs = 0
    for doc in ds:
        n_docs += 1
        if n_docs > max_docs:
            break
        body = doc.get('text', '')
        low = body.lower()
        hits = [t for t in terms
                if len(texts[t]) < target
                and any(v in low for v in pre[t])
                and matchers[t].search(low)]
        if hits:
            sents = _SFA_SENT_M.split(body)
            for t in hits:
                kept = 0
                for s in sents:
                    s = s.strip()
                    if not (min_chars <= len(s) <= max_chars):
                        continue
                    if matchers[t].search(s.lower()):
                        texts[t].append(s)
                        sources[t].append(doc.get('url') or '')
                        kept += 1
                        if kept >= per_doc or len(texts[t]) >= target:
                            break
        if progress and n_docs % 200000 == 0:
            done = sum(len(v) for v in texts.values())
            print(f'  scanned {n_docs/1e6:.1f}M documents, '
                  f'{done} sentences', flush=True)
        if all(len(texts[t]) >= target for t in terms):
            break
    return {'texts': texts, 'sources': sources, 'docs': n_docs}
")
  res <- reticulate::py$sfa_stream_corpus_multi(
    lapply(registry, as.list), as.integer(target), as.integer(max_docs),
    as.integer(per_doc), as.integer(min_chars), as.integer(max_chars),
    isTRUE(progress))
  list(texts = lapply(res$texts, function(x) as.character(unlist(x))),
       sources = lapply(res$sources, function(x) as.character(unlist(x))),
       docs_streamed = as.integer(res$docs))
}

#' @keywords internal
# Word-boundary multi-construct scan of a local corpus (documents already in
# memory or on disk); mirrors the FineWeb streamer's matching rule.
.cvg_local_corpus_multi <- function(docs, registry, target, per_doc,
                                    min_chars, max_chars) {
  matchers <- lapply(registry, function(v)
    paste0("\\b(", paste(vapply(tolower(v),
                                function(x) gsub("([][{}()+*^$|\\\\?.])",
                                                 "\\\\\\1", x),
                                character(1)), collapse = "|"), ")\\b"))
  texts <- sources <- stats::setNames(vector("list", length(registry)),
                                      names(registry))
  for (t in names(registry)) {
    texts[[t]] <- character(0)
    sources[[t]] <- character(0)
  }
  for (d in seq_along(docs)) {
    low <- tolower(docs[d])
    for (t in names(registry)) {
      if (length(texts[[t]]) >= target) next
      if (!grepl(matchers[[t]], low, perl = TRUE)) next
      sents <- trimws(strsplit(docs[d],
                               "(?<=[.!?])\\s+(?=[A-Z\"'(])",
                               perl = TRUE)[[1]])
      kept <- 0L
      for (s in sents) {
        if (nchar(s) < min_chars || nchar(s) > max_chars) next
        if (!grepl(matchers[[t]], tolower(s), perl = TRUE)) next
        texts[[t]] <- c(texts[[t]], s)
        sources[[t]] <- c(sources[[t]], sprintf("doc_%d", d))
        kept <- kept + 1L
        if (kept >= per_doc || length(texts[[t]]) >= target) break
      }
    }
  }
  list(texts = texts, sources = sources, docs_streamed = length(docs))
}

#' Build Many Construct Regions in One Corpus Pass
#'
#' The campaign-scale companion to [sfa_build_region()]: streams the corpus
#' once and extracts sentences for every construct simultaneously, then
#' builds one `"sfa_region"` per construct. Variants are matched on **word
#' boundaries** (a component like "care" must not match "career"), which
#' also makes compositional names safe: a construct like Honesty-Humility
#' is gathered through `variants = c("honesty", "humility")`.
#'
#' For encoder-ladder studies, build once with `embeddings = FALSE` (a pure
#' extraction; no encoder touched) and embed the same sentence sets under
#' each encoder with [sfa_reembed_region()] - the regions differ only in
#' the embedding space, never in their text.
#'
#' @param constructs Named list: construct name -> `list(definition = ,
#'   variants = NULL)` (a bare definition string also works). Default
#'   variants are simple inflections of the name.
#' @param corpus `"fineweb-10bt"` (streamed once for all constructs), or a
#'   local corpus as in [sfa_build_region()].
#' @param target,max_docs,sentences_per_doc,min_chars,max_chars As in
#'   [sfa_build_region()], applied per construct.
#' @param embeddings Embed each region now? `FALSE` builds sentence-only
#'   regions (no encoder needed; [sfa_coverage()] refuses them until
#'   [sfa_reembed_region()] fills the embeddings in).
#' @param embed,model,cache,instruction As in [sfa_build_region()].
#' @param dir Optional directory: each region is saved as
#'   `{dir}/{slug}.rds`.
#' @param progress Print streaming progress? Default `TRUE`.
#' @returns A named list of `"sfa_region"` objects.
#' @export
sfa_build_regions <- function(constructs,
                              corpus = "fineweb-10bt",
                              target = 1500,
                              max_docs = 20e6,
                              sentences_per_doc = 3,
                              min_chars = 30,
                              max_chars = 500,
                              embeddings = TRUE,
                              embed = "sbert",
                              model = NULL,
                              cache = TRUE,
                              instruction = TRUE,
                              dir = NULL,
                              progress = TRUE) {
  specs <- .cvg_check_constructs(constructs)
  registry <- lapply(specs, `[[`, "variants")
  instr <- .cvg_instruction_text(instruction)
  if (!is.null(dir)) dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  if (is.character(corpus) && length(corpus) == 1L &&
      identical(tolower(corpus), "fineweb-10bt")) {
    got <- .cvg_stream_fineweb_multi(registry, target, max_docs,
                                     sentences_per_doc, min_chars,
                                     max_chars, progress)
    corpus_id <- "HuggingFaceFW/fineweb:sample-10BT"
  } else {
    docs <- if (is.data.frame(corpus)) as.character(corpus$text) else
      as.character(corpus)
    got <- .cvg_local_corpus_multi(docs, registry, target,
                                   sentences_per_doc, min_chars, max_chars)
    corpus_id <- sprintf("user corpus (%d documents)", length(docs))
  }

  regions <- stats::setNames(vector("list", length(specs)), names(specs))
  for (nm in names(specs)) {
    text <- got$texts[[nm]]
    source <- got$sources[[nm]]
    key <- tolower(gsub("[^a-z0-9]+", " ", tolower(text)))
    keep <- !duplicated(key)
    text <- text[keep]
    source <- source[keep]

    if (length(text) < 200L) {
      warning("Only ", length(text), " unique sentences mention '", nm,
              "' - below the ~200-sentence saturation threshold",
              if (length(text) < 25L)
                " and below the 25-sentence audit minimum" else "",
              ". This does not mean the construct is invalid; there is ",
              "limited natural-language data about it under this name in ",
              "this corpus, so content-validity estimates will be ",
              if (length(text) < 25L) "unavailable" else
                "noisy (and biased favorable)", ".", call. = FALSE)
    }

    emb <- NULL
    encoder <- NA_character_
    if (isTRUE(embeddings) && length(text) > 0L) {
      emb <- .cvg_normalize(unname(sfa_embed(
        .cvg_wrap_instruction(text, instr),
        embed = embed, model = model, cache = cache)))
      encoder <- if (is.function(embed)) "custom function" else
        .resolve_embed_model(embed, model)
    }

    regions[[nm]] <- structure(list(
      construct      = nm,
      definition     = specs[[nm]]$definition,
      sentences      = data.frame(text = text, source = source,
                                  stringsAsFactors = FALSE),
      embeddings     = emb,
      corpus         = corpus_id,
      docs_streamed  = got$docs_streamed,
      target         = target,
      max_docs       = max_docs,
      variants       = specs[[nm]]$variants,
      matching       = "word-boundary",
      encoder        = encoder,
      instruction    = instr,
      extracted      = Sys.time(),
      semanticfa     = as.character(utils::packageVersion("semanticfa"))
    ), class = "sfa_region")

    if (!is.null(dir)) {
      saveRDS(regions[[nm]], file.path(dir, paste0(.cvg_slug(nm), ".rds")))
    }
  }
  if (!is.null(dir)) {
    message(length(regions), " regions saved to ", dir)
  }
  regions
}

#' Re-Embed a Construct Region Under Another Encoder
#'
#' Takes a region's sentences (from [sfa_build_region()] or
#' [sfa_build_regions()], including sentence-only regions built with
#' `embeddings = FALSE`) and embeds them under the given encoder, returning
#' a region in that encoder's space. This is how an encoder-ladder study
#' reuses one extraction: the regions differ only in embedding space, never
#' in text.
#'
#' @param region An `"sfa_region"` (or path to one).
#' @param embed,model,cache As in [sfa_build_region()].
#' @param file Optional path to save the re-embedded region.
#' @returns The `"sfa_region"` with new `embeddings` and `encoder`.
#' @export
sfa_reembed_region <- function(region, embed = "sbert", model = NULL,
                               cache = TRUE, file = NULL) {
  if (is.character(region) && length(region) == 1L) {
    region <- sfa_load_region(region)
  }
  if (!inherits(region, "sfa_region")) {
    stop("'region' must be an sfa_region or a path to one.", call. = FALSE)
  }
  region$embeddings <- .cvg_normalize(unname(sfa_embed(
    .cvg_wrap_instruction(region$sentences$text, region$instruction),
    embed = embed, model = model, cache = cache)))
  region$encoder <- if (is.function(embed)) "custom function" else
    .resolve_embed_model(embed, model)
  region$reembedded <- Sys.time()
  region$semanticfa <- as.character(utils::packageVersion("semanticfa"))
  if (!is.null(file)) saveRDS(region, file)
  region
}
