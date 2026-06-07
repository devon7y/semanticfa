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

## License

GPL (>= 3)
