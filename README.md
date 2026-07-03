# semanticfa

**Response-free semantic analysis of psychometric scales.**

`semanticfa` reads the *meaning of a scale's item wording* with a language model
and recovers, interprets, and refines the scale's latent structure — entirely
from the items, with **no human response data**. Factor analysis on the item
embeddings is the centerpiece, but the package is a full toolkit for working
with a scale before (or without) collecting data: building semantic similarity
matrices, deciding how many factors to keep, reading a semantic "loadings"
table, comparing the recovered structure to theory, flagging redundant items,
building short forms, vetting brand-new candidate items, detecting
jingle/jangle fallacies across scales, and visualizing the item space.

## Installation

```r
# from CRAN (once available):
install.packages("semanticfa")

# development version:
# install.packages("remotes")
remotes::install_github("devon7y/semanticfa")
```

The core of the package is pure R. Turning item *text* into embeddings on your
machine uses Python via `reticulate` — needed only if you want the package to
embed text for you (you can always bring your own embeddings):

```r
sfa_install_python()   # one-time: provisions sentence-transformers
```

Two-dimensional item maps work out of the box (`Rtsne` and `uwot` are bundled).
One optional package, **`EGAnet`**, powers EGA-based factor retention /
dimension selection and the faithful UVA redundancy method — install it only if
you use those parts.

## Quick start

```r
library(semanticfa)
data(big5)   # 50 IPIP Big-Five items + precomputed Qwen3-Embedding-8B embeddings

# one call: embed -> similarity -> retain -> extract -> diagnose
fit <- sfa(
  data.frame(code = big5$codes, item = big5$items,
             factor = big5$factors, scoring = big5$scoring),
  embeddings = big5$embeddings, nfactors = 5)
fit

# interpret and refine, all from the same fit
plot(fit, type = "scree")          # scree with parallel-analysis overlay
sfa_corplot(fit)                   # item-by-item similarity heatmap, grouped by factor
sfa_anchor(fit)                    # item-by-construct "belonging" (a semantic loadings table)
sfa_congruence(fit, target = big5$factors,    # agreement with theory (partition metrics)
               metrics = c("nmi", "ari"))
sfa_redundancy(fit)                # near-duplicate items
```

No respondents are involved at any step.

## What's in the box

### 1. Embed text and build a similarity matrix

| Function | Purpose |
|---|---|
| `sfa_embed()` | Embed item text — on-device sentence-transformers (Qwen3 models, default), the OpenAI API, or any custom function. Results are cached. |
| `sfa_load_npz()` | Load pre-generated embeddings (e.g. a GPU job) from a NumPy `.npz`, no Python needed. |
| `sfa_similarity()` | Item-by-item similarity matrix with a choice of four encodings (below). |
| `sfa_nli_matrix()` | **Signed**, valence-aware similarity from natural-language inference (entailment − contradiction), so reverse-keyed items are handled directly. |
| `sfa_install_python()`, `sfa_clear_cache()` | Provision the embedding environment / clear the cache. |

### 2. Recover the factor structure

| Function | Purpose |
|---|---|
| `sfa()` | The end-to-end pipeline: embed → similarity → retain → extract → diagnose. Accepts raw text, precomputed embeddings, an `sfa_embeddings` object, or a precomputed similarity matrix. |
| `sfa_nfactors()` | How many factors to keep — **parallel analysis, Kaiser, TEFI, and EGA** in one call. |
| `sfa_parallel()` | Embedding-adapted parallel analysis (random-unit-vector null; no sample size needed). |
| `sfa_dimselect()` | Select the informative leading embedding coordinates ("depth") by EGA depth optimization. |
| `as_psych()` | Hand the solution to `psych` (`factor.congruence()`, `fa.sort()`, …) as a standard `fa` object. |

### 3. Interpret the structure

| Function | Purpose |
|---|---|
| `sfa_anchor()` | An item-by-construct **belonging** matrix — a semantic loadings table — built from construct centroids and/or construct-name embeddings. |
| `sfa_project()` | Place items on interpretable **bipolar axes** (e.g. *mild* ↔ *severe*, *passive* ↔ *active*). |
| `sfa_congruence()` | Compare the recovered structure to an empirical or theoretical one: Tucker φ, NMI, ARI, Frobenius, and disattenuated correlation. |
| `sfa_jinglejangle()` | Flag **jingle** (same name, different content) and **jangle** (different name, same content) fallacies across multiple scales. |

### 4. Refine the scale — before collecting data

| Function | Purpose |
|---|---|
| `sfa_redundancy()` | Detect near-duplicate items via faithful **Unique Variable Analysis** (absolute wTO on an EBICglasso network) or a direct cosine criterion. |
| `sfa_simplify()` | Build response-free **short forms** by selecting the most representative items per factor. |
| `sfa_item_fit()` | Vet a **brand-new candidate item**: how well does it match the construct name and the other items, and is it redundant with any of them? |

### 5. Visualize

| Function | Purpose |
|---|---|
| `sfa_corplot()` | Heatmap of the item-by-item similarity matrix, grouped/ordered by factor (`order` accepts factor-name abbreviations, e.g. `c("D","A","S")`). |
| `sfa_itemplot()` | 2-D item map via **t-SNE, UMAP, PCA, or MDS** (`sfa_tsneplot()` is a deprecated alias). |
| `plot(fit, "scree")` | Scree plot with the parallel-analysis overlay. |

### Fit diagnostics

Every `sfa()` fit reports **KMO**, a real partition-based **TEFI** (negative;
lower is better), **RMSR**, **CAF**, McDonald's **ω**, and — when theoretical
factors are supplied — a factor-to-theory alignment matrix (DAAL).
`summary(fit)` adds the full breakdown, and `calibrate = TRUE` adds a Monte
Carlo null reference for the diagnostics.

## Encoding methods (`sfa_similarity(..., encoding=)`)

| Method | Description | Keying |
|---|---|---|
| `"atomic"` (default) | L2-normalize, cosine similarity | keying-free (`scoring` ignored) |
| `"atomic_reversed"` | Sign-flip reverse-keyed items, L2-normalize, cosine | uses `scoring` sign-flip |
| `"squid"` | Subtract the questionnaire-mean embedding, then cosine | keying-free |
| `"mean_centered_pearson"` | Mean-center → cosine = Pearson correlation | keying-free |

## Bundled data

`data(big5)` — the 50-item IPIP Big-Five markers (public domain) with precomputed
`Qwen3-Embedding-8B` embeddings (rounded to 4 decimal places), so every example runs
without Python or network access.

## Learn more

A getting-started tour, worked end-to-end on the bundled Big Five inventory, is
in the package vignette (`vignette("introduction", package = "semanticfa")`).

## References

- Milano, N., Luongo, M., Ponticorvo, M., & Marocco, D. (2025). Semantic
  analysis of test items through large language model embeddings predicts
  a-priori factorial structure of personality tests. *Current Research in
  Behavioral Sciences*, 8, 100168. doi:10.1016/j.crbeha.2025.100168
- Casella, M., Luongo, M., Marocco, D., Milano, N., & Ponticorvo, M. (2024).
  LLM embeddings on test items predict post hoc loadings in personality tests.
  *Ital-IA 2024*, CEUR Workshop Proceedings.
- Guenole, N., D'Urso, E. D., Samo, A., Sun, T., & Haslbeck, J. M. B. (preprint).
  Enhancing Scale Development: Pseudo Factor Analysis of Language Embedding
  Similarity Matrices. OSF: https://osf.io/3mpzb/
- Pellert, M., Lechner, C. M., Sen, I., & Strohmaier, M. (2026). Neural network
  embeddings recover value dimensions from psychometric survey items on par with
  human data (SQuID). *Findings of the ACL: EACL 2026*, 5738–5752.
- Pokropek, A. (2026). From keyword-based text measures to latent variables:
  Confirmatory factor analysis with word embeddings. *EPJ Data Science*.
  doi:10.1140/epjds/s13688-026-00654-1
- Kmetty, Z., Koltai, J., & Rudas, T. (2021). The presence of occupational
  structure in online texts based on word embedding NLP models. *EPJ Data
  Science*, 10, 55. doi:10.1140/epjds/s13688-021-00311-9
- Christensen, A. P., Garrido, L. E., & Golino, H. (2023). Unique Variable
  Analysis: A network psychometrics method to detect local dependence.
  *Multivariate Behavioral Research*, 58(6), 1165–1182.
  doi:10.1080/00273171.2023.2194606
- Golino, H. (2026). Optimizing the landscape of LLM embeddings with Dynamic
  Exploratory Graph Analysis for generative psychometrics. arXiv:2601.17010.
- Grand, G., Blank, I. A., Pereira, F., & Fedorenko, E. (2022). Semantic
  projection recovers rich human knowledge of multiple object features from word
  embeddings. *Nature Human Behaviour*, 6(7), 975–987.
  doi:10.1038/s41562-022-01316-8
- Wulff, D. U., & Mata, R. (2025). Semantic embeddings reveal and address
  taxonomic incommensurability in psychological measurement. *Nature Human
  Behaviour*, 9(5), 944–954. doi:10.1038/s41562-024-02089-y
- Wulff, D. U., & Mata, R. (2026). Escaping the jingle-jangle jungle: Increasing
  conceptual clarity in psychology using large language models. *Current
  Directions in Psychological Science*, 35(2), 59–65.
  doi:10.1177/09637214251382083
- Hommel, B. E., & Arslan, R. C. (2025). Language models accurately infer
  correlations between psychological items and scales from text alone. *Advances
  in Methods and Practices in Psychological Science*, 8(4).
  doi:10.1177/25152459251377093
- Jung, S.-J., & Seo, J.-W. (2025). A transformer-based embedding approach to
  developing short-form psychological measures. *Frontiers in Psychology*, 16,
  1640864. doi:10.3389/fpsyg.2025.1640864
- Wang, B., Zhang, Y., Hu, Y., Hou, H., Peng, K., & Ni, S. (2026). Discovering
  semantic latent structures in psychological scales: A response-free pathway to
  efficient simplification. arXiv:2602.12575.

## License

GPL (>= 3)
