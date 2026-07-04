# Manual test sheet — 0.2.0 factor naming (Big5)

Run in an R session from the package directory. Steps 3 and 6 download
once (~4 MB word list + 380 MB / 1.5 GB pools); everything after is local.

```r
devtools::load_all()
```

## 1. Fit (instant — uses the bundled 8B embeddings)

```r
fit <- sfa(big5$items, nfactors = 5, embeddings = big5$embeddings)
```

## 2. Name with all defaults

Default naming model (Qwen 0.6B) + int8 pool. Expect a download prompt,
then five labels.

```r
labels <- sfa_name(fit)
labels
```

## 3. Inspect what you got

```r
labels$label            # the automatic labels
labels$candidates       # leave-one-out sets (the error bar)
labels$rule             # "tier1" = dictionary construct-noun; "top1" = fallback
labels$collision_moved  # TRUE if a duplicate label was resolved
cbind(labels$label, table(big5$factors)) # eyeball against the true constructs
```

## 4. Inline flag

```r
fit2 <- sfa(big5$items, nfactors = 5, embeddings = big5$embeddings,
            label_factors = TRUE)
fit2$labels
```

## 5. fp16 vs int8 (should agree on Big5)

```r
p16 <- sfa_pool("Qwen/Qwen3-Embedding-0.6B", precision = "fp16")
sfa_name(fit, pool = p16)$label
labels$label            # compare
```

## 6. Bigger naming model (two-encoder mode, same fit)

The 8B namer usually lifts label abstraction. ~1.5 GB pool download.

```r
labels8 <- sfa_name(fit, model = "Qwen/Qwen3-Embedding-8B")
cbind(small = labels$label, large = labels8$label)
```

(If you have GPU + patience: `model = "microsoft/harrier-oss-v1-27b"` —
the model itself is ~54 GB.)

## 7. Guardrails

```r
sfa_naming_instruction()                       # the fixed instruction
sfa_name(fit, instruction = "Name this factor") # expect a warning
sfa_pool("some/unknown-model", build = FALSE)   # expect a clear error
```

## 8. Determinism

```r
identical(sfa_name(fit)$label, labels$label)   # TRUE, and instant (all cached)
```

Cache lives in `tools::R_user_dir("semanticfa", "cache")/pools`;
`sfa_clear_cache()` wipes it if you want to re-test downloads.
