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
