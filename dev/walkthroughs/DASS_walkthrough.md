# A guided tour of **semanticfa**, using the DASS

This walkthrough takes you from zero to every function in the package, with the **Depression Anxiety Stress Scales (DASS-42)** as the running example. No prior experience with the package is assumed.

The DASS-42 has **42 items** split across **3 subscales** — Depression, Anxiety, Stress (14 items each) — all **positively keyed** (a higher rating always means *more* symptoms). We'll keep coming back to what that means as we go.

> **The big idea.** Normally you discover a questionnaire's factor structure by giving it to hundreds of people and factor-analyzing their answers. `semanticfa` instead reads the *meaning of the item wording* with a language model and recovers the structure from that alone — **no human responses needed**. It's a tool for inspecting and refining a scale before (or without) collecting data.

> **What's new to test.** This round added new functions *and* changed some existing behavior. Both are folded into the sections below and flagged **🆕 New** / **🔧 Changed**. Please test them and tell me what's confusing, wrong, or missing:
>
> - **New functions:** `sfa_load_npz()` (§3), `sfa_corplot()` + `sfa_itemplot()` (§5 — item map via t-SNE / UMAP / PCA / MDS), `sfa_item_fit()` (§10); plus the `calibrate=` knob (§15).
> - **🔧 Fixed diagnostics:** `CAF` now reports a real value, and `TEFI` is now the genuine partition-based index (it is **negative** — lower is better) (§7).
> - **🔧 Faithful redundancy:** `sfa_redundancy()` now matches Unique Variable Analysis (it sparsifies the network first); the default `threshold` is now `0.25`, and there's a `method = "cosine"` alternative (§11).
> - **🔧 Keying-free encodings:** `squid` and `mean_centered_pearson` no longer apply a scoring sign-flip — they recover negatives from the centering itself (§4).
> - **🔧 `order` accepts abbreviations** in `sfa_corplot()`, e.g. `order = c("D","A","S")` (§5).

------------------------------------------------------------------------

## 0. Install

``` r
# install.packages("remotes")
remotes::install_github("devon7y/semanticfa")
library(semanticfa)
```

The core of the package is pure R. One feature — turning item *text* into embeddings on your machine — needs Python. You only need this if you want the package to embed text for you (you can also bring your own embeddings; see §3).

``` r
# one-time: provision the Python embedding environment
sfa_install_python()
```

This installs `sentence-transformers` into an environment `reticulate` manages for you. (On first real use the package also auto-declares this requirement, so in many setups it "just works" without calling the line above.)

The item map (§5) works out of the box — all four methods (t-SNE, UMAP, PCA, MDS) are ready to use (`Rtsne` and `uwot` are bundled with the package; PCA/MDS are base R). One optional package, **`EGAnet`**, powers EGA-based factor retention / dimension selection and the faithful UVA redundancy method (§6, §11, §15) — install it only if you use those parts.

------------------------------------------------------------------------

## 1. Get your DASS data into shape

`semanticfa` wants a small data frame with up to four columns:

| column | meaning | required? |
|----|----|----|
| `item` | the item text | **yes** |
| `code` | a short label (e.g. `D3`, `A2`, `S1`) | optional |
| `factor` | the theoretical subscale | optional, but unlocks a lot |
| `scoring` | `+1` / `-1` keying direction | optional (defaults to all `+1`) |

``` r
dass <- read.csv("DASS_items.csv", stringsAsFactors = FALSE)
head(dass)
#>   code                                                  item     factor scoring
#> 1   S1        I found myself getting upset by quite trivial...     Stress      1
#> 2   A2                    I was aware of dryness of my mouth.    Anxiety      1
#> 3   D3   I couldn't seem to experience any positive feeling...  Depression     1
```

**DASS note.** Every `scoring` value is `+1` — the DASS has no reverse-worded items. So the default `"atomic"` encoding (plain cosine) is all you need here; the keying-aware `"atomic_reversed"` would give the *same* result. Scales *with* reverse items (e.g. Big Five) need `encoding = "atomic_reversed"` plus a `scoring` vector so the reverse items get sign-flipped.

------------------------------------------------------------------------

## 2. The one-liner

Everything below is optional detail. The headline call is:

``` r
fit <- sfa(dass)
fit
```

That single call: 1. embeds all 42 item texts with the default model (`Qwen/Qwen3-Embedding-0.6B`), 2. builds a 42×42 item-by-item **semantic similarity matrix**, 3. decides **how many factors** to keep (embedding-adapted parallel analysis), 4. extracts the factor solution (via `psych::fa`), and 5. computes a batch of fit diagnostics.

The printout starts like this (numbers will vary by model):

```         
Semantic Factor Analysis
  Encoding: atomic
  Model: Qwen/Qwen3-Embedding-0.6B (default)
  Note: larger embedding models recover factor structure more accurately.
        For higher fidelity, set model = "Qwen/Qwen3-Embedding-4B" (8 GB RAM)
        or model = "Qwen/Qwen3-Embedding-8B" (16 GB RAM).
  Embedding dim: 1024
  Factors: 4  (minres + oblimin)

Diagnostics:
  KMO:  0.97 (marvelous - higher is better)
  TEFI: -44.1 (lower is better)
  RMSR: 0.03 (good - lower is better)
  CAF:  0.34 (marginal - higher is better)
Factor loadings:
  ...
```

**DASS note — don't be alarmed if it isn't 3.** Theory says Depression / Anxiety / Stress = 3 factors, but the DASS subscales are *highly correlated* and share a lot of vocabulary (Stress and Anxiety items both describe arousal/tension). Semantically, the package often lands on **4-ish** factors and a strong shared "general distress" dimension. That's not a bug — it's the well-known overlap of the DASS subscales showing up in the language itself. We'll measure exactly how well it matches the 3-factor theory in §8.

> **Tip for repeat runs.** Embedding is the slow part. If you already have embeddings (a 42×d numeric matrix in item order), skip Python entirely: `sfa(dass, embeddings = my_matrix)` — or load them from a `.npz` file in one line (§3).

------------------------------------------------------------------------

## 3. How embedding works — `sfa_embed()`

If you want the embeddings themselves (to cache, inspect, or reuse):

``` r
emb <- sfa_embed(dass$item)              # 42 x 1024 matrix, one row per item
dim(emb)

# pick a bigger model for higher fidelity:
emb8 <- sfa_embed(dass$item, model = "Qwen/Qwen3-Embedding-8B")

# or bring any embedding you like via a function (no Python needed):
emb_custom <- sfa_embed(dass$item, embed = function(txt) my_encoder(txt))
```

Embeddings are cached, so re-embedding the same items is instant. `sfa_clear_cache()` wipes the cache.

> **Backends.** The default is on-device `sbert` (Qwen). To use OpenAI instead, `sfa_embed(dass$item, embed = "openai")` — with `model = NULL` it now defaults to `text-embedding-3-small` (not the Qwen name) and reads your `OPENAI_API_KEY`; pass `model =` for a different OpenAI model.

**Why model size matters for the DASS.** Smaller models tend to *over-split* the DASS (every cluster of physical-symptom items becomes its own factor); larger Qwen models give cleaner, more theory-like structure. If your machine can afford it, `model = "Qwen/Qwen3-Embedding-4B"` is a good step up.

### 🆕 New — load pre-made embeddings: `sfa_load_npz()`

The high-fidelity 4B/8B models are heavy to run inside R. The practical pattern is to embed **once** elsewhere (e.g. a Python/GPU job) and save a NumPy `.npz`, then load it instantly in every R session:

``` r
emb <- sfa_load_npz("DASS_items_8B.npz")
emb            # prints what was found: embeddings + any codes/items/factors/scoring
```

The archive's `embeddings` array is required; `codes`, `items`, `factor` labels, and `scoring` are picked up automatically **if present** (override the key names with the `*_key` arguments). The result is an `sfa_embeddings` object the rest of the pipeline accepts **directly** — no separate data frame needed:

``` r
fit <- sfa(emb)                       # full analysis straight from the file
sfa_corplot(sfa_similarity(emb))      # or just look at the structure (§5)
```

**DASS payoff.** This is how you get the accuracy of `Qwen3-Embedding-8B` without paying its RAM cost in R: embed the 42 DASS items once into `DASS_items_8B.npz` (bundling the `code`/`factor`/`scoring` arrays so they ride along), and every analysis below loads in one line.

> Reading `.npz` needs Python `numpy` (it only reads the file — it does not embed text). **Test feedback wanted:** note if `sfa_load_npz()` ever tries to install/download Python when you only want to read an existing file.

------------------------------------------------------------------------

## 4. The similarity matrix — `sfa_similarity()`

This is the heart of the method: how item meanings get turned into "correlations."

``` r
sim <- sfa_similarity(emb)                 # default 'atomic' encoding
sim[1:4, 1:4]
```

There are four **encodings** (ways to turn embeddings into a similarity matrix):

| encoding | what it does | when to use for DASS |
|----|----|----|
| `atomic` (default) | plain cosine | fine for the DASS (no reverse items) |
| `atomic_reversed` | cosine after sign-flipping reverse items via `scoring` | use for scales with reverse-keyed items |
| `squid` | subtract the questionnaire's mean item first | **useful**: removes the "everything-is-distress" baseline so the *differences* between Depression/Anxiety/Stress stand out |
| `mean_centered_pearson` | makes cosine equal a true Pearson correlation | use if you want a genuine correlation matrix to hand to other SEM tools |

**DASS-specific reason to try `squid`.** Because every DASS item is about negative affect, *all* items are somewhat similar to *all* others — a strong general factor that can swamp the three subscales. `squid` centers that shared "distress" component out, which can make Depression vs Anxiety vs Stress separate more cleanly:

``` r
fit_squid <- sfa(dass, encoding = "squid")
```

> **🔧 Changed — keying-free encodings.** `squid` and `mean_centered_pearson` recover negative correlations from the centering itself, so they now **ignore `scoring`** entirely (passing a `scoring` with reverse-keyed `-1` items to them warns and drops it). Only the `atomic` encodings use the keying direction. For the all-positive DASS nothing changes; this matters for scales *with* reverse items. **Test feedback wanted:** confirm `sfa_similarity(emb, encoding="squid")` and `...scoring=dass$scoring` give the *same* matrix.

------------------------------------------------------------------------

## 5. 🆕 New — see the structure: `sfa_corplot()` & `sfa_itemplot()`

Two pictures of the *input* side — the meanings themselves — **before** any factor extraction. They make the abstract similarity matrix concrete, and for the DASS they show its overlap problem at a glance.

### Similarity heatmap — `sfa_corplot()`

Draws the item-by-item similarity matrix as a heatmap, with items **grouped into subscale blocks down the diagonal**:

``` r
sfa_corplot(fit)                               # from a fitted object
sfa_corplot(sim)                               # or straight from sfa_similarity()
sfa_corplot(fit, order = c("D", "A", "S"))     # control block order (prefix match)
```

It accepts an `sfa` object or a bare similarity matrix. Grouping is **display only** — the underlying matrix keeps its original item order (the rest of the package relies on that). By default it shows short item codes, no in-cell numbers, and the upper triangle; pass `group = FALSE` to keep the native order.

**DASS payoff.** You can literally *see* the DASS's structure: a warm haze over the **whole** matrix (the general-distress factor — every item is somewhat similar to every other), a crisp **Depression** block on the diagonal, and **Anxiety/Stress blocks that bleed into each other**. The `order = c("D","A","S")` argument lets you arrange the blocks the way the manual presents them.

### Item map — `sfa_itemplot()`

A 2-D scatter — one point per item, **colored by subscale** and labeled with its code. The projection method is selectable via `method`:

``` r
sfa_itemplot(fit)                              # t-SNE (default; needs 'Rtsne')
sfa_itemplot(fit, method = "umap")             # UMAP (needs 'uwot', no Python)
sfa_itemplot(fit, method = "pca")              # PCA  — base R, no extra package
sfa_itemplot(fit, method = "mds")              # classical MDS — base R
sfa_itemplot(fit, perplexity = 8, seed = 1)    # tune t-SNE layout / fix randomness
```

| `method` | needs | character |
|----|----|----|
| `"tsne"` (default) | `Rtsne` (bundled) | local clusters; stochastic (set `seed`) |
| `"umap"` | `uwot` (bundled) | clusters + more global structure; stochastic |
| `"pca"` | — (base R) | linear, deterministic; works out of the box |
| `"mds"` | — (base R) | distance-preserving; deterministic |

**DASS payoff.** The geometric companion to the heatmap: Depression items land in their own cloud, while several **Stress** points sit out among the **Anxiety** cloud — the subscale boundary blur, as a map. t-SNE/UMAP are stochastic (set `seed`) and need ≥ 5 items; **PCA/MDS are deterministic and the lightest** (base R), so they're the quickest first look. For \~42 items the four methods look broadly similar — UMAP's edge shows up mainly at larger scales. **Test feedback wanted:** try a couple of methods and tell me which is clearest.

> The old name `sfa_tsneplot()` still works as a **deprecated alias** for `sfa_itemplot()` (it warns and forwards), so existing code won't break.

------------------------------------------------------------------------

## 6. How many factors? — `sfa_parallel()` and `sfa_nfactors()`

`sfa()` decides this for you, but you can inspect it directly.

``` r
# embedding-adapted parallel analysis (random unit vectors as the null)
pa <- sfa_parallel(sim, emb)
pa

# compare several retention rules side by side
sfa_nfactors(sim, embeddings = emb,
             methods = c("parallel", "kaiser", "TEFI", "EGA"))
```

`sfa_nfactors()` prints a small table — one row per method — plus a consensus.

**DASS note.** Expect the rules to disagree (parallel analysis often says 4-5; EGA may say fewer). The disagreement *is* the finding: the DASS sits between "one distress factor" and "three subscales," and different rules weight that differently. Theory's answer (3) is a reasonable target to hold these against.

------------------------------------------------------------------------

## 7. Reading the solution — `print()`, `summary()`, `plot()`, `as_psych()`

``` r
summary(fit)        # loadings + omega reliability + communalities + (any) Heywood cases
plot(fit, "scree")      # scree plot with the parallel-analysis threshold
plot(fit, "loadings")   # heatmap of the loading matrix
plot(fit, "residuals")  # residual distribution

# hand the result to the psych ecosystem:
psych_obj <- as_psych(fit)
psych::fa.sort(psych_obj$loadings)
```

In the loadings, you're hoping to see three columns that line up with Depression, Anxiety, and Stress items. For the DASS you'll typically see Depression separate cleanly, while **Anxiety and Stress bleed into each other** — again, the real overlap of those constructs.

> `plot(fit, "loadings")` is the heatmap of the **solution** (items × factors); `sfa_corplot()` in §5 is the heatmap of the **input** (item × item). Looking at both is the fastest way to see where the recovered factors came from.

**Diagnostics to read (all printed by `summary`, each with a plain-language grade in parentheses):** - **KMO** — is there enough shared structure to factor at all? (DASS: usually "marvelous," \~0.95+.) - **🔧 TEFI** — the Total Entropy Fit Index (Golino), now computed properly against the factor partition. It is **negative**, and **more negative = better** — use it to *compare* solutions, not as an absolute. (Previously this printed a positive number that wasn't really TEFI; if you saw `+1.9` before and `−44` now, that's the fix, not a regression.) - **RMSR** — average residual; smaller is better (good ≤ 0.05). - **🔧 CAF** — Common part Accounted For; **higher = better**. For the DASS expect a *marginal* value (\~0.3): the strong general-distress factor leaves shared variance in the residuals, which is exactly what CAF detects. (This used to be stuck at `0.0000` for every scale — that bug is fixed.) - **McDonald's ω** — reliability of each recovered factor.

> **Test feedback wanted.** CAF and TEFI were both recently corrected. Please sanity-check that CAF is non-zero and TEFI is negative on your DASS run, and flag anything that looks off.

------------------------------------------------------------------------

## 8. Does the semantic structure match DASS theory? — `sfa_congruence()`

This is where having the `factor` column pays off. Compare the *recovered* structure to the *theoretical* Depression/Anxiety/Stress grouping:

``` r
sfa_congruence(fit, target = dass$factor,
               metrics = c("nmi", "ari"))
```

```         
Factor structure congruence
  NMI:  0.5x        # how much the recovered grouping agrees with theory (0-1)
  ARI:  0.4x        # chance-corrected agreement (0-1)
```

- **NMI / ARI** near 1 = the language model recovered your subscales.
- Middling values for the DASS reflect the Anxiety/Stress overlap, not a failure.

You can also compare against an empirical loading matrix or correlation matrix (if you have one) to get **Tucker's φ**, **Frobenius** similarity of factor correlations, and a **disattenuated** correlation between the semantic and empirical structures:

``` r
sfa_congruence(fit, target = my_empirical_fa)     # a psych::fa object
```

------------------------------------------------------------------------

## 9. Which items belong where? — `sfa_anchor()`

Think of this as a **semantic loading table**: each cell is how strongly an item "belongs" to each subscale.

``` r
a <- sfa_anchor(fit, anchor = "centroid")
round(a$centroid, 2)        # 42 items x 3 subscales
a                            # printout flags the weakest / cross-loading items
```

Read it like a loadings matrix: an item should be high in its own subscale's column and low in the others. The printout surfaces **review candidates** — items that sit closer to a *different* subscale than their own.

**DASS payoff.** This is a fast content-validity check. Expect a few **Stress** items (e.g. "I found it hard to relax", "I felt I was rather touchy") to land nearer **Anxiety**, exposing the classic DASS boundary blur — purely from wording, before you collect a single response.

You can also anchor against the **subscale names themselves** (embed the words "Depression", "Anxiety", "Stress"):

``` r
sfa_anchor(fit, anchor = "label",
           labels = c("Depression", "Anxiety", "Stress"))
```

------------------------------------------------------------------------

## 10. 🆕 New — vet a *candidate* item: `sfa_item_fit()`

§9 scores the items you **already have**. `sfa_item_fit()` does the same for items you're **thinking of adding** — a response-free quality check on draft wording, *before* it ever goes into a questionnaire. Each candidate is scored on two complementary axes per construct:

- **Similarity to name** ("does it *sound* like Depression?" — cosine to the construct-name embedding), and
- **Similarity to other items** ("does it *look* like the other Depression items?" — cosine to the construct's existing-item centroid).

``` r
sfa_item_fit(fit, "I felt there was nothing to look forward to",
             construct = "Depression")

# vet several at once:
sfa_item_fit(fit, c("I feel calm and relaxed",
                    "My heart was pounding for no reason"))
```

The printout gives, per candidate: the best-matching construct, the **cross-loading gap** to the runner-up, whether it's stronger/weaker than the construct's average item, its **nearest existing item** (with a redundancy flag), and a one-line **verdict** — `good fit`, `weak match`, `cross-loads`, or `redundant`.

**DASS payoff.** Before fielding a revised DASS, draft a candidate Depression item and confirm it (a) reads as Depression rather than Stress, (b) isn't a near-duplicate of "I felt that life was meaningless," and (c) fits at least as well as the items already there — all with no respondents. When the two axes **disagree** they're informative: high name + low items = a *gap-filler* (on-topic but covering new ground); low name + high items = *drift* (looks like the items but off-construct). Set `reverse_key = TRUE` to vet a reverse-worded candidate, and `redundancy_cutoff` to tune the near-duplicate threshold (default 0.90).

> Needs the `factor` column **and** stored embeddings — i.e. a fit made from item text or from `sfa(..., embeddings = ...)`, not one built from a precomputed similarity matrix.

------------------------------------------------------------------------

## 11. 🔧 Changed — are any items redundant? — `sfa_redundancy()`

Finds **near-duplicate** items — pairs so similar they add length without information (different from "weak" items). There are two methods:

| method | what it does | default `threshold` | needs |
|----|----|----|----|
| `"wto"` (default) | **Unique Variable Analysis** (Christensen et al. 2023): builds a *sparsified* network first, then weighted topological overlap | `0.25` (the UVA cut-off) | `EGAnet` |
| `"cosine"` | direct pairwise similarity | `0.80` | nothing extra |

``` r
sfa_redundancy(fit)                                   # UVA (wto), threshold 0.25
sfa_redundancy(fit, method = "cosine", threshold = 0.85)
```

> **🔧 What changed.** Weighted topological overlap is only meaningful on a *sparse* network, so `wto` now sparsifies first (via `EGAnet`), matching the real UVA method. Previously it ran on the dense matrix, which compressed every pair into a narrow band and made the cutoff knife-edged (a 0.01 change flipped dozens of pairs). If you ran `sfa_redundancy(fit, threshold = 0.8)` before and got nothing, that's why — `wto` values now live on a different scale, hence the new `0.25` default.

**DASS payoff — and a caveat to test.** The Anxiety subscale has several physical-symptom items (trembling, racing heart, sweating, dry mouth) that are semantically close — exactly what a short form prunes. But the DASS's strong general-distress factor means **`wto` flags a *lot*** (its sparsified network still links most items into one big cluster — that's a property of the scale, not a bug). For a cleaner read of *specific* duplicate pairs on such a homogeneous scale, prefer **`method = "cosine"`** with a high threshold (\~0.85). **Please test both** and tell me which is more useful in practice. (`sfa_item_fit()` in §10 runs the same redundancy check the *other* direction: is a brand-new item a duplicate of something already in the scale?)

------------------------------------------------------------------------

## 12. Build a short form — `sfa_simplify()`

The DASS-21 is a famous half-length version of the DASS-42. You can construct a response-free short form the same way, and **see what it costs**:

``` r
short <- sfa_simplify(fit, target_n = 7, method = "anchor")   # 7 items x 3 = 21
short
short$keep      # the retained item codes
short$drop      # what was dropped, and why
```

```         
Scale simplification (response-free)
  Method: Wang et al. (2026); Jung & Seo (2025)
  Selection: anchor | groups: theoretical | target_n = 7 per group
  Items: 42 -> 21
  Factors retained (parallel analysis): 4 -> 3
  Structure recovery vs theory (NMI / ARI):
    full:    NMI=0.5x  ARI=0.4x
    reduced: NMI=0.6x  ARI=0.5x        # often *improves* after pruning noise
```

Two strategies: - `method = "anchor"` — keep the items most central to each subscale. - `method = "medoid"` — keep items that are central **and** spread out (more content coverage, less redundancy).

And two ways to define the groups: - `groups = "theoretical"` — trim within the official D/A/S subscales. - `groups = "fitted"` — let the groups **emerge from the items** (no key needed).

``` r
sfa_simplify(fit, target_n = 7, method = "medoid", groups = "fitted")
```

**DASS payoff.** You get a principled, data-free DASS-21 candidate, *plus* a fidelity report telling you whether the structure survived the cut.

------------------------------------------------------------------------

## 13. Put items on a clinical-severity scale — `sfa_project()`

`sfa_anchor` tells you *which* subscale an item is in; **projection** tells you *where along a named dimension* it sits. For a clinical scale, the obvious axis is **symptom severity**.

``` r
sev <- sfa_project(fit, axes = list(
  severity = c(low = "mild, minor, slight distress",
               high = "severe, extreme, life-threatening distress")))
sev
```

Each item gets a 0-to-1 severity score from its wording alone. Sort them:

``` r
sort(sev$scores[, "severity"])
```

**DASS payoff.** A good clinical scale should have items spanning **mild → severe** so it can distinguish someone slightly low from someone in crisis. Projection lets you *see the coverage*: you'll find the Depression items "I felt that life was meaningless" / "...wasn't worthwhile" at the severe end, and "I couldn't seem to get going" at the mild end — and you can spot gaps (e.g. too few mild Anxiety items). This is something grouping/factor analysis simply can't show you.

------------------------------------------------------------------------

## 14. Compare the DASS to *other* instruments — `sfa_jinglejangle()`

The DASS Anxiety subscale and the **Beck Anxiety Inventory (BAI)** both claim to measure "anxiety"; DASS Depression and the **Beck Depression Inventory (BDI)** both claim "depression." Do the names match the content?

``` r
scales <- list(
  DASS_Anxiety    = dass$item[dass$factor == "Anxiety"],
  DASS_Depression = dass$item[dass$factor == "Depression"],
  BAI             = bai_items,     # your BAI item texts
  BDI             = bdi_items)     # your BDI item texts

sfa_jinglejangle(scales)
```

```         
Jingle-jangle detection across 4 scales
  Method: Wulff & Mata (2025, 2026)
  ...
  jangle = same content, different name;  jingle = same name, different content
```

**DASS payoff.** This surfaces **jingle** (DASS-Anxiety vs BAI: same label, but the BAI is heavily somatic while DASS-Anxiety mixes in worry) and **jangle** (DASS-Stress vs DASS-Anxiety: different labels, very similar content). It's a one-call map of how the DASS relates to the wider anxiety/depression measurement landscape.

------------------------------------------------------------------------

## 15. Advanced knobs

**Pick the best slice of the embedding — `sfa_dimselect()` / `dim_select`.** Instead of using the whole 1024-dim vector, search for the sub-range of dimensions that recovers the cleanest structure (needs `EGAnet`):

``` r
sfa(dass, dim_select = "dynega", n_factors_method = "EGA")
```

**🆕 New — null-model calibration — `calibrate`.** By default the diagnostics read off the raw semantic solution. `calibrate = TRUE` runs a Monte-Carlo null (Pokropek 2026): it refits under many random-embedding nulls of the same shape and calibrates the fit indices against what *chance* structure would produce.

``` r
sfa(dass, calibrate = TRUE, calibrate_iter = 200)
```

It's slower (one refit per iteration), but it tells you how much of the recovered structure actually beats chance — worth it for a borderline, high-overlap scale like the DASS.

**Contradiction-aware similarity — `sfa_nli_matrix()`.** Plain embeddings call opposites "similar" because they share a topic. An NLI model separates *agree* from *contradict*:

``` r
M   <- sfa_nli_matrix(dass$item)     # signed item-by-item matrix (needs Python)
fit_nli <- sfa(dass, similarity = M) # feed any custom matrix straight in
```

For the DASS (all same-direction items) this matters less than for scales with reverse items — but it's there if you want valence-aware structure, and the `similarity =` argument means you can plug in **any** item-by-item matrix you build yourself.

------------------------------------------------------------------------

## 16. Cheat-sheet

| You want to…                        | Function                            |
|-------------------------------------|-------------------------------------|
| Run the whole thing                 | `sfa()`                             |
| Get embeddings                      | `sfa_embed()`                       |
| Load saved embeddings (`.npz`) 🆕   | `sfa_load_npz()`                    |
| Set up Python                       | `sfa_install_python()`              |
| Clear the embedding cache           | `sfa_clear_cache()`                 |
| Build the similarity matrix         | `sfa_similarity()`                  |
| Heatmap of the similarity matrix 🆕 | `sfa_corplot()`                     |
| Item map (t-SNE/UMAP/PCA/MDS) 🆕    | `sfa_itemplot()`                    |
| Decide \# of factors                | `sfa_parallel()`, `sfa_nfactors()`  |
| Pick embedding dimensions           | `sfa_dimselect()`                   |
| Check items vs their subscale       | `sfa_anchor()`                      |
| Vet a candidate / new item 🆕       | `sfa_item_fit()`                    |
| Find duplicate items                | `sfa_redundancy()`                  |
| Make a short form                   | `sfa_simplify()`                    |
| Rate items on a named axis          | `sfa_project()`                     |
| Compare whole scales                | `sfa_jinglejangle()`                |
| Valence-aware similarity            | `sfa_nli_matrix()`                  |
| Match against theory/empirics       | `sfa_congruence()`                  |
| Use psych/plots                     | `as_psych()`, `plot()`, `summary()` |

------------------------------------------------------------------------

## 17. One honest caveat

Semantic structure is **the structure implied by the item wording**, not the structure of how real people respond. The two usually agree closely (that's why this works), but they can diverge — especially for a scale like the DASS whose subscales are conceptually distinct yet empirically and linguistically entangled. Treat `semanticfa` as a fast, response-free **first look and design aid**: it tells you what your items *say*, which is exactly what you can act on before you ever field the questionnaire.
