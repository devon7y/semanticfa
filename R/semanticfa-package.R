#' semanticfa: Semantic Factor Analysis of Language Model Embeddings
#'
#' Recovers the latent factor structure of a psychological scale from the
#' \emph{meaning of its item wording} --- no human response data required. It
#' embeds item text with a language model, turns the embeddings into an
#' item-by-item similarity matrix, and runs exploratory factor analysis, with a
#' suite of tools for inspecting and refining the scale.
#'
#' @section Main entry point:
#' \itemize{
#'   \item \code{\link{sfa}} --- run the full pipeline (embed -> similarity ->
#'     retention -> extraction -> diagnostics) and return an \code{"sfa"} object
#'     with \code{print}, \code{summary}, \code{plot}, and \code{\link{as_psych}}
#'     methods.
#' }
#'
#' @section Building blocks:
#' \itemize{
#'   \item \code{\link{sfa_embed}}, \code{\link{sfa_install_python}} --- turn item
#'     text into embeddings.
#'   \item \code{\link{sfa_similarity}} --- similarity transforms / encodings
#'     (atomic, atomic-reversed, SQuID, mean-centered Pearson).
#'   \item \code{\link{sfa_parallel}}, \code{\link{sfa_nfactors}},
#'     \code{\link{sfa_dimselect}} --- choose the number of factors and which
#'     embedding dimensions to use.
#' }
#'
#' @section Item- and scale-level tools:
#' \itemize{
#'   \item \code{\link{sfa_anchor}} --- item-by-construct belonging (a semantic
#'     loading table).
#'   \item \code{\link{sfa_redundancy}} --- detect near-duplicate items.
#'   \item \code{\link{sfa_simplify}} --- build response-free short forms.
#'   \item \code{\link{sfa_project}} --- place items on a named bipolar axis
#'     (e.g. mild -> severe).
#'   \item \code{\link{sfa_jinglejangle}} --- compare whole scales for
#'     jingle/jangle fallacies.
#'   \item \code{\link{sfa_nli_matrix}} --- valence-aware (entailment vs.
#'     contradiction) similarity.
#'   \item \code{\link{sfa_congruence}} --- compare the recovered structure to
#'     theory or empirical data.
#' }
#'
#' @section Example data:
#' \code{\link{big5}} --- IPIP Big-Five 50-item markers with precomputed
#' embeddings, used throughout the examples.
#'
#' @author
#' Authors:
#' \itemize{
#'   \item \strong{Devon Yanitski} (author, maintainer)
#'     \email{dyanitsk@ualberta.ca}
#'     (\href{https://orcid.org/0009-0006-1568-3387}{ORCID})
#'   \item Chris Westbury (author)
#' }
#'
"_PACKAGE"
