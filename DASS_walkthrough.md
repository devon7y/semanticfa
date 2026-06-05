# A guided tour of **semanticfa**, using the DASS

This walkthrough takes you from zero to every function in the package, with the
**Depression Anxiety Stress Scales (DASS-42)** as the running example. No prior
experience with the package is assumed.

The DASS-42 has **42 items** split across **3 subscales** — Depression, Anxiety,
Stress (14 items each) — all **positively keyed** (a higher rating always means
*more* symptoms). We'll keep coming back to what that means as we go.

> **The big idea.** Normally you discover a questionnaire's factor structure by
> giving it to hundreds of people and factor-analyzing their answers.
> `semanticfa` instead reads the *meaning of the item wording* with a language
> model and recovers the structure from that alone — **no human responses
> needed**. It's a tool for inspecting and refining a scale before (or without)
> collecting data.

---

## 0. Install

```r
# install.packages("remotes")
remotes::install_github("devon7y/semanticfa")
library(semanticfa)
```

The core of the package is pure R. One feature — turning item *text* into
embeddings on your machine — needs Python. You only need this if you want the
package to embed text for you (you can also bring your own embeddings; see §3).

```r
# one-time: provision the Python embedding environment
sfa_install_python()
```

This installs `sentence-transformers` into an environment `reticulate` manages
for you. (On first real use the package also auto-declares this requirement, so
in many setups it "just works" without calling the line above.)

---

## 1. Get your DASS data into shape

`semanticfa` wants a small data frame with up to four columns:

| column | meaning | required? |
|---|---|---|
| `item` | the item text | **yes** |
| `code` | a short label (e.g. `D3`, `A2`, `S1`) | optional |
| `factor` | the theoretical subscale | optional, but unlocks a lot |
| `scoring` | `+1` / `-1` keying direction | optional (defaults to all `+1`) |

```r
dass <- read.csv("DASS_items.csv", stringsAsFactors = FALSE)
head(dass)
#>   code                                                  item     factor scoring
#> 1   S1        I found myself getting upset by quite trivial...     Stress      1
#> 2   A2                    I was aware of dryness of my mouth.    Anxiety      1
#> 3   D3   I couldn't seem to experience any positive feeling...  Depression     1
```

**DASS note.** Every `scoring` value is `+1` — the DASS has no reverse-worded
items. That matters later: the default `"atomic_reversed"` encoding (which
flips reverse items) behaves exactly like plain `"atomic"` here, and the
sign-handling machinery elsewhere is effectively a no-op. Scales *with* reverse
items (e.g. Big Five) exercise more of the package.

---

## 2. The one-liner

Everything below is optional detail. The headline call is:

```r
fit <- sfa(dass)
fit
```

That single call:
1. embeds all 42 item texts with the default model (`Qwen/Qwen3-Embedding-0.6B`),
2. builds a 42×42 item-by-item **semantic similarity matrix**,
3. decides **how many factors** to keep (embedding-adapted parallel analysis),
4. extracts the factor solution (via `psych::fa`), and
5. computes a batch of fit diagnostics.

The printout starts like this (numbers will vary by model):

```
Semantic Factor Analysis
  Encoding: atomic_reversed
  Model: Qwen/Qwen3-Embedding-0.6B (default)
  Note: larger embedding models recover factor structure more accurately.
        For higher fidelity, set model = "Qwen/Qwen3-Embedding-4B" (8 GB RAM)
        or model = "Qwen/Qwen3-Embedding-8B" (16 GB RAM).
  Embedding dim: 1024
  Factors: 4  (minres + oblimin)

Diagnostics:
  KMO:  0.84 (meritorious)
  TEFI: ...
  ...
Factor loadings:
  ...
```

**DASS note — don't be alarmed if it isn't 3.** Theory says Depression /
Anxiety / Stress = 3 factors, but the DASS subscales are *highly correlated*
and share a lot of vocabulary (Stress and Anxiety items both describe
arousal/tension). Semantically, the package often lands on **4-ish** factors and
a strong shared "general distress" dimension. That's not a bug — it's the
well-known overlap of the DASS subscales showing up in the language itself. We'll
measure exactly how well it matches the 3-factor theory in §7.

> **Tip for repeat runs.** Embedding is the slow part. If you already have
> embeddings (a 42×d numeric matrix in item order), skip Python entirely:
> `sfa(dass, embeddings = my_matrix)`.

---

## 3. How embedding works — `sfa_embed()`

If you want the embeddings themselves (to cache, inspect, or reuse):

```r
emb <- sfa_embed(dass$item)              # 42 x 1024 matrix, one row per item
dim(emb)

# pick a bigger model for higher fidelity:
emb8 <- sfa_embed(dass$item, model = "Qwen/Qwen3-Embedding-8B")

# or bring any embedding you like via a function (no Python needed):
emb_custom <- sfa_embed(dass$item, embed = function(txt) my_encoder(txt))
```

Embeddings are cached, so re-embedding the same items is instant. `sfa_clear_cache()`
wipes the cache.

**Why model size matters for the DASS.** Smaller models tend to *over-split*
the DASS (every cluster of physical-symptom items becomes its own factor);
larger Qwen models give cleaner, more theory-like structure. If your machine can
afford it, `model = "Qwen/Qwen3-Embedding-4B"` is a good step up.

---

## 4. The similarity matrix — `sfa_similarity()`

This is the heart of the method: how item meanings get turned into "correlations."

```r
sim <- sfa_similarity(emb, encoding = "atomic_reversed", scoring = dass$scoring)
sim[1:4, 1:4]
```

There are four **encodings** (ways to turn embeddings into a similarity matrix):

| encoding | what it does | when to use for DASS |
|---|---|---|
| `atomic_reversed` (default) | cosine after flipping reverse items | fine — DASS has no reverse items, so same as `atomic` |
| `atomic` | plain cosine | equivalent here |
| `squid` | subtract the questionnaire's mean item first | **useful**: removes the "everything-is-distress" baseline so the *differences* between Depression/Anxiety/Stress stand out |
| `mean_centered_pearson` | makes cosine equal a true Pearson correlation | use if you want a genuine correlation matrix to hand to other SEM tools |

**DASS-specific reason to try `squid`.** Because every DASS item is about
negative affect, *all* items are somewhat similar to *all* others — a strong
general factor that can swamp the three subscales. `squid` centers that shared
"distress" component out, which can make Depression vs Anxiety vs Stress
separate more cleanly:

```r
fit_squid <- sfa(dass, encoding = "squid")
```

---

## 5. How many factors? — `sfa_parallel()` and `sfa_nfactors()`

`sfa()` decides this for you, but you can inspect it directly.

```r
# embedding-adapted parallel analysis (random unit vectors as the null)
pa <- sfa_parallel(sim, emb)
pa

# compare several retention rules side by side
sfa_nfactors(sim, embeddings = emb,
             methods = c("parallel", "kaiser", "TEFI", "EGA"))
```

`sfa_nfactors()` prints a small table — one row per method — plus a consensus.

**DASS note.** Expect the rules to disagree (parallel analysis often says 4-5;
EGA may say fewer). The disagreement *is* the finding: the DASS sits between "one
distress factor" and "three subscales," and different rules weight that
differently. Theory's answer (3) is a reasonable target to hold these against.

---

## 6. Reading the solution — `print()`, `summary()`, `plot()`, `as_psych()`

```r
summary(fit)        # loadings + omega reliability + communalities + (any) Heywood cases
plot(fit, "scree")      # scree plot with the parallel-analysis threshold
plot(fit, "loadings")   # heatmap of the loading matrix
plot(fit, "residuals")  # residual distribution

# hand the result to the psych ecosystem:
psych_obj <- as_psych(fit)
psych::fa.sort(psych_obj$loadings)
```

In the loadings, you're hoping to see three columns that line up with Depression,
Anxiety, and Stress items. For the DASS you'll typically see Depression separate
cleanly, while **Anxiety and Stress bleed into each other** — again, the real
overlap of those constructs.

**Diagnostics to read (all printed by `summary`):**
- **KMO** — is there enough shared structure to factor at all? (DASS: usually
  "meritorious," ~0.8+.)
- **TEFI** — entropy-based fit; lower is better.
- **RMSR** — average residual; smaller is better.
- **McDonald's ω** — reliability of each recovered factor.

---

## 7. Does the semantic structure match DASS theory? — `sfa_congruence()`

This is where having the `factor` column pays off. Compare the *recovered*
structure to the *theoretical* Depression/Anxiety/Stress grouping:

```r
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

You can also compare against an empirical loading matrix or correlation matrix
(if you have one) to get **Tucker's φ**, **Frobenius** similarity of factor
correlations, and a **disattenuated** correlation between the semantic and
empirical structures:

```r
sfa_congruence(fit, target = my_empirical_fa)     # a psych::fa object
```

---

## 8. Which items belong where? — `sfa_anchor()`

Think of this as a **semantic loading table**: each cell is how strongly an item
"belongs" to each subscale.

```r
a <- sfa_anchor(fit, anchor = "centroid")
round(a$centroid, 2)        # 42 items x 3 subscales
a                            # printout flags the weakest / cross-loading items
```

Read it like a loadings matrix: an item should be high in its own subscale's
column and low in the others. The printout surfaces **review candidates** —
items that sit closer to a *different* subscale than their own.

**DASS payoff.** This is a fast content-validity check. Expect a few **Stress**
items (e.g. "I found it hard to relax", "I felt I was rather touchy") to land
nearer **Anxiety**, exposing the classic DASS boundary blur — purely from
wording, before you collect a single response.

You can also anchor against the **subscale names themselves** (embed the words
"Depression", "Anxiety", "Stress"):

```r
sfa_anchor(fit, anchor = "label",
           labels = c("Depression", "Anxiety", "Stress"))
```

---

## 9. Are any items redundant? — `sfa_redundancy()`

Finds **near-duplicate** items — pairs so similar they add length without
information (different from "weak" items).

```r
sfa_redundancy(fit, threshold = 0.8)
```

```
Redundant-item detection
  Method: Christensen et al. (2023)
  Redundant pairs: ...
  Top redundant pairs:
    A2 ~ A19   overlap=0.8x      # "dryness of my mouth" ~ "perspired noticeably"
    ...
  Suggested removals (keep one per cluster): ...
```

**DASS payoff.** The Anxiety subscale has several physical-symptom items
(trembling, racing heart, sweating, dry mouth) that are semantically close. This
flags which ones are doing duplicate work — exactly the items a short form would
prune.

---

## 10. Build a short form — `sfa_simplify()`

The DASS-21 is a famous half-length version of the DASS-42. You can construct a
response-free short form the same way, and **see what it costs**:

```r
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

Two strategies:
- `method = "anchor"` — keep the items most central to each subscale.
- `method = "medoid"` — keep items that are central **and** spread out (more
  content coverage, less redundancy).

And two ways to define the groups:
- `groups = "theoretical"` — trim within the official D/A/S subscales.
- `groups = "fitted"` — let the groups **emerge from the items** (no key needed).

```r
sfa_simplify(fit, target_n = 7, method = "medoid", groups = "fitted")
```

**DASS payoff.** You get a principled, data-free DASS-21 candidate, *plus* a
fidelity report telling you whether the structure survived the cut.

---

## 11. Put items on a clinical-severity scale — `sfa_project()`

`sfa_anchor` tells you *which* subscale an item is in; **projection** tells you
*where along a named dimension* it sits. For a clinical scale, the obvious axis
is **symptom severity**.

```r
sev <- sfa_project(fit, axes = list(
  severity = c(low = "mild, minor, slight distress",
               high = "severe, extreme, life-threatening distress")))
sev
```

Each item gets a 0-to-1 severity score from its wording alone. Sort them:

```r
sort(sev$scores[, "severity"])
```

**DASS payoff.** A good clinical scale should have items spanning **mild →
severe** so it can distinguish someone slightly low from someone in crisis.
Projection lets you *see the coverage*: you'll find the Depression items
"I felt that life was meaningless" / "...wasn't worthwhile" at the severe end,
and "I couldn't seem to get going" at the mild end — and you can spot gaps
(e.g. too few mild Anxiety items). This is something grouping/factor analysis
simply can't show you.

---

## 12. Compare the DASS to *other* instruments — `sfa_jinglejangle()`

The DASS Anxiety subscale and the **Beck Anxiety Inventory (BAI)** both claim to
measure "anxiety"; DASS Depression and the **Beck Depression Inventory (BDI)**
both claim "depression." Do the names match the content?

```r
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

**DASS payoff.** This surfaces **jingle** (DASS-Anxiety vs BAI: same label, but
the BAI is heavily somatic while DASS-Anxiety mixes in worry) and **jangle**
(DASS-Stress vs DASS-Anxiety: different labels, very similar content). It's a
one-call map of how the DASS relates to the wider anxiety/depression measurement
landscape.

---

## 13. Advanced knobs

**Pick the best slice of the embedding — `sfa_dimselect()` / `dim_select`.**
Instead of using the whole 1024-dim vector, search for the sub-range of
dimensions that recovers the cleanest structure (needs `EGAnet`):

```r
sfa(dass, dim_select = "dynega", n_factors_method = "EGA")
```

**Contradiction-aware similarity — `sfa_nli_matrix()`.**
Plain embeddings call opposites "similar" because they share a topic. An NLI
model separates *agree* from *contradict*:

```r
M   <- sfa_nli_matrix(dass$item)     # signed item-by-item matrix (needs Python)
fit_nli <- sfa(dass, similarity = M) # feed any custom matrix straight in
```

For the DASS (all same-direction items) this matters less than for scales with
reverse items — but it's there if you want valence-aware structure, and the
`similarity =` argument means you can plug in **any** item-by-item matrix you
build yourself.

---

## 14. Cheat-sheet

| You want to… | Function |
|---|---|
| Run the whole thing | `sfa()` |
| Get embeddings | `sfa_embed()` |
| Set up Python | `sfa_install_python()` |
| Build the similarity matrix | `sfa_similarity()` |
| Decide # of factors | `sfa_parallel()`, `sfa_nfactors()` |
| Pick embedding dimensions | `sfa_dimselect()` |
| Check items vs their subscale | `sfa_anchor()` |
| Find duplicate items | `sfa_redundancy()` |
| Make a short form | `sfa_simplify()` |
| Rate items on a named axis | `sfa_project()` |
| Compare whole scales | `sfa_jinglejangle()` |
| Valence-aware similarity | `sfa_nli_matrix()` |
| Match against theory/empirics | `sfa_congruence()` |
| Use psych/plots | `as_psych()`, `plot()`, `summary()` |

---

## 15. One honest caveat

Semantic structure is **the structure implied by the item wording**, not the
structure of how real people respond. The two usually agree closely (that's why
this works), but they can diverge — especially for a scale like the DASS whose
subscales are conceptually distinct yet empirically and linguistically
entangled. Treat `semanticfa` as a fast, response-free **first look and design
aid**: it tells you what your items *say*, which is exactly what you can act on
before you ever field the questionnaire.
