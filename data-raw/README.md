# data-raw — artifact provenance

The naming artifacts (word list + pool embeddings) are built offline from
the research repository (LLM_Factor_Analysis) and hosted as GitHub release
assets; nothing here runs at package build time.

- `build_wordlist.py` — builds the canonical shipped word list: the
  label-eligible subset (dictionary construct-noun rules over WordNet) of
  the 1M-term census (WordNet + filtered Wikipedia titles + open-ontology
  completeness additions), with precomputed `family` (per-token WordNet
  noun lemmatization) and `tier1` (single-noun dictionary membership)
  columns. Output: `wordlist.rds` (369,703 rows).
- `export_golden_fixture.py` — exports the selection-parity fixture
  (tests/testthat/fixtures/golden_v7.json) from the research pipeline's
  official outputs.
- `make_manifests.R` — computes SHA-256 checksums, splits any asset over
  GitHub's 2 GB release limit into parts, and writes the per-pool
  `.manifest.rds` files consumed by `sfa_pool()`.

Pool embedding matrices are produced on HPC with the research repo's
`embed_wordlist.py` (Qwen models) / `embed_terms.py` (other encoders),
filtered to the eligible rows in canonical word-list order.
