# semanticfa 0.3.0

## Content-validity audit: one 95% convention, per-item p-values

* `sfa_coverage()` now calibrates both headline numbers against an ideal
  same-length scale under a single 95% convention. The coverage radius is
  the 95% quantile of the matched-size null (`radius_q = 0.95`; an ideal
  scale's construct coverage is ~0.95), and each item's corroboration
  count (construct texts within its radius) gets an empirical p-value
  against the ideal-item null, flagged at `alpha = 0.05` (an ideal
  scale's item relevance is also ~0.95). The identity behind the
  convention: `1 - radius_q` and `alpha` are per-decision Type I error
  rates of Monte Carlo tests.
* The fixed-count relevance rule (`k_precision`) is retired: corroboration
  counts grow linearly with region size, so a fixed threshold rewarded
  sampling more construct text. The calibrated critical count rescales
  with the region, making item relevance region-size invariant.
  `k_precision` is ignored with a warning; `delta_q` maps to `radius_q`
  with a warning.
* Adopted vocabulary throughout output, docs, and returned fields:
  *construct coverage* (was "coverage@delta*"), *item relevance* (was
  "precision"), *coverage radius* (was "delta*"), *corroboration count*.
  Renamed fields: `radius`, `radius_q`, `item_relevance`,
  `corroboration`, `p_values`, `relevant_items`, `critical_count`,
  `ideal_relevance`.
* `p_adjust = "BH"` flags items by Benjamini-Hochberg false discovery
  rate across the scale instead of per-item alpha.
* Two new plots. `plot(audit)` draws a proportional Euler diagram: two
  equal disks whose overlap area equals the measured construct coverage,
  filled with the real texts (dots) and items (triangles) placed by their
  full-space verdicts - a constructed diagram, not a projection (no 2-d
  projection of the embedding space preserves these fractions).
  `plot(audit, type = "relevance")` draws the per-item chart:
  corroboration counts on a log axis, empirical p-values on every bar,
  the calibrated critical count as a reference line, flagged items in
  red. The pre-0.3.0 curve plot remains as `type = "curve"`.
* Multi-factor scales audit **per factor by default**: when the items data
  frame carries a `factor` column, `sfa_coverage()` runs one audit per
  subscale - content validity is a property of an (item set, construct
  claim) pair, and a battery makes one claim per subscale - returning an
  `"sfa_coverage_battery"` (a named list of audits with a compact
  per-factor print table). `region` accepts a list named by factor so each
  subscale is audited against its own construct region; the `factor`
  argument restricts the battery to a subset or a single factor.
* `cross = TRUE` audits every factor against every region and returns an
  `"sfa_coverage_cross"` matrix of audits - the content analogue of a
  multitrait matrix, with no data collection: items should be relevant to
  their own construct's region (convergent) and irrelevant to their
  siblings' (discriminant). `sfa_cross_matrix()` extracts the numeric
  relevance or coverage matrix; the print method marks own-construct
  cells and states the caveat that off-diagonal relevance is floored by
  how separable the constructs are in language, not by zero.
* Insufficient-data signaling: when a construct name returns too little
  corpus text, the method says so in the right epistemic register - the
  construct is not "invalid"; there is not enough natural-language data
  about it (under this name, in this corpus) to estimate content validity
  with this method. `sfa_build_region()` warns below the ~200-sentence
  saturation threshold (estimates noisy, coverage biased favorable) and
  errors below the 25-sentence audit minimum; `print()` on regions and
  audits carries a NOTE/CAUTION line; audits store `small_region`.
* The printed report lists flagged items with their counts and p-values,
  and states the ideal benchmark next to both headline numbers.
* Bootstrap CIs now recalibrate both the radius and the critical count
  inside every resample and report `relevance_ci` (was `precision_ci`).
* Verified against the Python reference implementation on identical
  embeddings (431PTQ vs. the procrastination region, Qwen3-Embedding-8B).

# semanticfa 0.2.0

## New features

* `sfa_name()` labels the factors of an `sfa` fit with psychological
  construct names retrieved from a 368k-term pre-filtered candidate pool
  using instruction-conditioned embeddings. Deterministic; returns the
  label, its provenance rule, and a leave-one-out candidate set per factor
  (the method's error bar). Labels name the pole toward which the factor's
  positive loadings point.
* Two-encoder support: `sfa_name(fit, model = ...)` names with a different
  (typically larger) embedding model than the one used for extraction.
* `sfa_pool()` fetches pre-generated pool embeddings for the supported
  models (downloaded once into the user cache) or builds a pool locally for
  any sentence-transformers model. Default precision is int8 (half-size
  downloads); relative to fp16 it changes 3 of 75 benchmark labels, all on
  weak factors, all to near-synonyms ("stress resilience" -> "resilience";
  two analogous changes under the large naming model). Pass
  `precision = "fp16"` for exact parity with the research pipeline.
* `sfa(..., label_factors = TRUE)` runs naming inline and stores the result
  as `fit$labels`.
* `sfa_naming_instruction()` exposes the naming instruction; overriding it
  is supported but warned (label robustness was validated under the
  default).
* The sentence-transformers backend now keeps the most recently used
  encoder resident instead of re-loading it on every `sfa_embed()` call,
  and loading a different model releases the previous one (so an
  extraction model and a large naming model never co-occupy GPU memory).
* New option `semanticfa.torch_dtype` ("float16", "bfloat16", or
  "float32") controls the weight dtype of sentence-transformers models.
  Large naming encoders do not fit common GPUs at float32.
* When sentence-transformers cannot load a model (some text-only
  checkpoints of multimodal families are misrouted through a processor
  that demands an image component), the backend now falls back to a plain
  transformers pipeline reproducing the model's own modules.json:
  attention-mask-based last-token pooling plus L2 normalization.

# semanticfa 0.1.2

* New retention method `sfa_ekc()`: the empirical Kaiser criterion (Braeken &
  van Assen, 2017) with the embedding dimension in the sample-size role. Its
  serial reference-eigenvalue correction addresses the classical
  parallel-analysis weakness that reference values ignore variance already
  captured by real factors. Available in `sfa_nfactors()` via
  `methods = "EKC"`.
* `sfa_nfactors()` default `methods` changed from
  `c("parallel", "kaiser", "TEFI")` to `"parallel"` alone. Retention defaults
  should match the field's conventional expectation (parallel analysis), and
  the old default's silent votes carried known biases (the latent-root rule
  is liberal by construction; TEFI runs low on embedding similarity
  matrices, so the old bare-call consensus could tie-break down to its
  value). The full battery is opt-in, as in the package demonstration
  (`c("parallel", "kaiser", "TEFI", "EGA", "EKC")`), and `print()` now shows
  the consensus line only when two or more methods ran.
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
