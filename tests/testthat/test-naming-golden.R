# Golden parity test: the R selection layer (gate walk, label rule,
# collision keeper) must reproduce the research pipeline's official labels
# for all benchmark factors, given the same ranked candidates. The fixture
# was exported from the Python reference implementation (e2plus_v7).
#
# This tests the selection logic only (deterministic table walks); the
# geometry (targets, retrieval) is covered by unit tests and, end-to-end,
# by the online integration test below it.

test_that("R selection layer reproduces the reference v7 labels", {
  fx_path <- test_path("fixtures", "golden_v7.json")
  skip_if_not(file.exists(fx_path), "golden fixture not present")
  fx <- jsonlite::read_json(fx_path, simplifyVector = FALSE)

  # group factors by scale for collision resolution
  scales <- vapply(fx, `[[`, character(1), "scale")
  mismatches <- character(0)

  for (sc in unique(scales)) {
    keys <- names(fx)[scales == sc]
    gates <- list(); picks <- list(); loos <- list()
    for (k in keys) {
      cand <- fx[[k]]$candidates
      gate <- data.frame(
        word = vapply(cand, `[[`, character(1), "word"),
        family = vapply(cand, `[[`, character(1), "family"),
        tier1 = vapply(cand, `[[`, logical(1), "tier1"),
        score = vapply(cand, `[[`, numeric(1), "score"),
        row = seq_along(cand),
        stringsAsFactors = FALSE
      )
      # research gate deduplicates families in ranking order
      gate <- gate[!duplicated(gate$family), , drop = FALSE]
      gates[[k]] <- gate
      picks[[k]] <- semanticfa:::.sfa_pick(gate, n_candidates = 5L)
      loos[[k]] <- unlist(fx[[k]]$loo_set)
    }
    res <- semanticfa:::.sfa_keeper(unname(picks), unname(gates),
                                    unname(loos), n_candidates = 5L)
    for (i in seq_along(keys)) {
      got <- res$picks[[i]]$label %||% NA_character_
      want <- fx[[keys[i]]]$expected_label
      if (!identical(got, want)) {
        mismatches <- c(mismatches,
                        sprintf("%s: got '%s', want '%s'", keys[i], got, want))
      }
    }
  }
  expect_length(mismatches, 0)
  if (length(mismatches)) print(mismatches)
})

`%||%` <- function(a, b) if (is.null(a)) b else a
