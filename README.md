# semanticfa

Semantic Factor Analysis of Language Model Embeddings.

`semanticfa` performs exploratory factor analysis on language model embeddings
of psychological scale items, recovering latent factor structure entirely from
item text — no human response data required.

## Installation

```r
# install.packages("devtools")
devtools::install_github("devon7y/semanticfa")
```

## Quick start

```r
library(semanticfa)
data(big5)

fit <- sfa(
  big5$items,
  nfactors   = 5,
  embeddings = big5$embeddings,
  scoring    = big5$scoring
)
print(fit)
plot(fit, type = "scree")
```

## Features

- **Multiple encoding methods**: atomic reversed (Guenole et al.), SQuID
  centering (Pellert et al. 2026), mean-centered Pearson (Pokropek 2026)
- **Embedding-adapted parallel analysis**: random unit vector null distribution
  (no sample size needed)
- **Unified retention diagnostics**: `sfa_nfactors()` runs parallel analysis,
  Kaiser, TEFI, and EGA in one call
- **psych-compatible output**: `$loadings` works with `psych::factor.congruence()`,
  `psych::fa.sort()`, and all standard tools
- **Pluggable embedding backends**: sentence-BERT (default), OpenAI API,
  custom functions, or precomputed matrices
- **Fit diagnostics**: KMO, TEFI, RMSR, CAF, McDonald's omega, DAAL
- **Structure comparison**: Tucker phi, NMI, ARI, Frobenius, disattenuated
  correlation via `sfa_congruence()`

## Encoding methods

| Method | Description |
|---|---|
| `"atomic_reversed"` | Sign-flip by keying, L2-normalize, cosine similarity |
| `"atomic"` | L2-normalize, cosine similarity (no sign-flip) |
| `"squid"` | Subtract questionnaire-mean embedding, then cosine |
| `"mean_centered_pearson"` | Mean-center → cosine = Pearson correlation |

## References

- Guenole, N., et al. (2024). Pseudo Factor Analysis of Language Embedding
  Similarity Matrices.
- Pellert, M., et al. (2026). SQuID: Semantic Questionnaire Item Decomposition.
- Pokropek, A. (2026). CFA with word embeddings.
- Kmetty, Z., et al. (2021). Mean-centered cosine as Pearson correlation.
- Golino, H. (preprint). TEFI and EGA on LLM embeddings.

## License

GPL (>= 3)
