# semanticfa 0.1.2

* New retention method `sfa_ekc()`: the empirical Kaiser criterion (Braeken &
  van Assen, 2017) with the embedding dimension in the sample-size role. Its
  serial reference-eigenvalue correction addresses the classical
  parallel-analysis weakness that reference values ignore variance already
  captured by real factors. Added to the `sfa_nfactors()` **default** method
  set (now `parallel`, `kaiser`, `TEFI`, `EKC`).
* New retention method `sfa_map()`: Velicer's (1976) minimum average partial.
  Available in `sfa_nfactors()` via `methods = "MAP"` but deliberately not a
  default vote: on embedding similarity matrices MAP tracks reliable minor
  structure well past the interpretable factor count.
* New diagnostic `sfa_cd()`: a comparison-data misfit profile adapting Ruscio
  & Roche (2012), with print and plot methods. It reports how well k-factor
  comparison populations reproduce the observed eigenvalue spectrum as k
  grows, rather than a single retention verdict: response data with a crisp
  factor boundary show a sharp elbow, while embedding similarity matrices
  decline smoothly. Ruscio & Roche's sequential significance rule is opt-in
  (`alpha =`) because it saturates at `n_factors_max` on embedding matrices
  (the k-factor comparison model cannot reproduce the anisotropic spectral
  tail, so every added factor keeps helping).

# semanticfa 0.1.1

* Bundled data upgrade: `data(big5)` now ships 50 x 4096 `Qwen3-Embedding-8B`
  item embeddings (rounded to 4 decimal places), replacing the 50 x 384
  `all-MiniLM-L6-v2` sentence-BERT embeddings of 0.1.0. Analyses run on the
  bundled data will differ from 0.1.0.
* `sfa_item_fit()` now compares candidates against unflipped (topical)
  construct centroids, matching `sfa_anchor()` and `sfa_simplify()`. The
  previous behavior sign-flipped reverse-keyed reference items into anti-topic
  vectors, which depressed the item-similarity profile of constructs with many
  reverse-keyed items and could misassign candidates. `reverse_key = TRUE`
  still flips the candidate itself.
* `sfa_congruence()`'s disattenuated metric now returns `NA` with a warning
  when either similarity matrix's split-half reliability is not positive
  (e.g., the checkerboard sign pattern of `atomic_reversed`), instead of
  failing with an unhelpful error.
* `sfa_nli_matrix()` reads the entailment/contradiction label order from the
  cross-encoder's model config instead of assuming the
  `cross-encoder/nli-*` order, so non-default NLI models score correctly (a
  warning falls back to the default order when the config is unavailable).
* `sfa_parallel()` now applies Horn's sequential retention rule (count leading
  eigenvalues until the first falls below its null percentile) instead of
  counting all eigenvalues above their pointwise percentiles, and its
  documentation cites the embedding-benchmark precedent (Garrido et al.).
* `sfa_dimselect()` default encoding is now `"atomic"`, matching `sfa()`.
* Fitted objects store the encoded item vectors as `$transformed_embeddings`
  (previously `$embeddings`, which was easy to confuse with
  `$input_embeddings`).
* `digest` moved from Suggests to Imports: embedding cache keys are now always
  SHA-256 (the previous fallback hash could collide).
* Documentation fixes: `sfa_anchor()` help no longer claims items are
  sign-aligned before anchoring (they never were in this release line, by
  design), the `big5` item-code range reads E1--O50, the README encoding table
  marks `atomic` as keying-free, and README/vignette references to the bundled
  data name the Qwen3 embeddings.

# semanticfa 0.1.0

* Initial release.
* Core `sfa()` function for semantic factor analysis.
* Encoding methods: `atomic_reversed`, `atomic`, `squid`, `mean_centered_pearson`.
* Embedding-adapted parallel analysis (`sfa_parallel()`).
* Unified retention diagnostics (`sfa_nfactors()`).
* Fit diagnostics: KMO, TEFI, RMSR, CAF, McDonald's omega, DAAL.
* Comparison metrics: Tucker phi, NMI, ARI, Frobenius, disattenuated (`sfa_congruence()`).
* Embedding backends: sentence-BERT, OpenAI, custom functions, precomputed.
* Bundled IPIP Big Five 50-item dataset with sentence-BERT embeddings.
