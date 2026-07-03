# Validation Report — semanticfa: An R Package for Response-Free Semantic Factor Analysis of Psychometric Scales

Manuscript state: working tree, 2026-07-03 (post peer-review revision + STSR literature + transcript regenerated under semanticfa 0.1.1; 67 pp compiled)
Overall status: YELLOW (mechanical pre-flight green; reference validation not yet converged; judgment passes not run; one author to-do open)

## Transcript regeneration — 2026-07-03

`reproduce.Rout` regenerated under the released semanticfa 0.1.1 on a Rorqual (Alliance Canada) login node, R 4.5.0, seed 42, BATCH_EXIT:0. Every headline value is byte-identical to the June run (encoding table, loadings, omegas, Tucker matrix, congruence) with three classes of change, all synced into the manuscript: (1) psych's arbitrary MR factor-name assignment rotated (N=MR3 unchanged; C=MR2, A=MR5, E=MR4, O=MR1) — prose now uses emitter-derived `\valMr*` macros so labels can never go stale again; (2) the sequential parallel-analysis rule corrects 0.6B retention from 4 to 3 (4B stays 4, 8B stays 5; modal consensuses unchanged at six/six/five); (3) timings reflect the CPU-only cluster (NLI 4103.2 s, embed 128.2 s, dimselect 1.9 s, calibrate 59.5 s) and the live-re-embed minimum cosine drifted .9999 → .9998. Verbatim-excerpt integrity verified programmatically: every excerpt line matches the new Rout byte-for-byte except the manuscript's long-standing reflow of overlong lines (29 lines, all pre-dating this revision and content-identical) and deliberate elision markers.

Pass status:
- Mechanical pre-flight : PASS (2026-07-02, this revision) — see notes below
- Reference validation  : round 1 applied (2026-06, 66 CORRECT / 9 MINOR / 0 SERIOUS); rounds 2–3 NOT RUN (awaiting author go-ahead) → STALE against this revision
- Missing-citation pass : not run (one known gap is already scheduled: the semantic-theory-of-survey-response / Semantic Scale Network literature, PDFs pending author download)
- Statistics pass       : not run
- Claim pass            : not run

## Mechanical pre-flight — 2026-07-02

- Compile + cross-refs: PASS (latexmk clean; 0 undefined references/citations; 0 "??" in PDF)
- BibTeX log: PASS (no warnings/errors beyond the cosmetic others/6th-ed note)
- Left-over flags: PASS (0 MISSINGCITE/INCOMPLETE/TODO) — the deliberate OSF placeholder in `methods_open_practices.tex` remains and must be filled before submission
- Figures: PASS — all 9 figures staged by `reproduce/make_paper_assets.R` from `reproduce/figures/` (md5-verified; this fixed the stale mislabeled `loadings.pdf` whose MR1/MR4 column labels were swapped)
- Numbers discipline: PASS — 102 macros in `generated/values.tex`, every used `\val…` defined; 4 defined-but-unused spares (`\valConsensusK`, `\valEgaKWord`, `\valMcpRKeyed`, `\valTimeNli`) kept intentionally. Tables tab:tucker, tab:encodings, S1, S2 are generated from `reproduce/output/*.csv`. Remaining literals in prose are design constants (seed 42, thresholds .85/.25/0.20, item counts, model dims) and citation-derived values from sourced papers.
- Statistical sanity: N/A in the classical sense (no t/F/p triples); all values traced to `reproduce/output/` by construction of the macro pipeline
- Prohibited punctuation: NOT ENFORCED — the manuscript's established prose style uses em dashes and semicolons throughout (authorial choice predating this pass); newly added sentences avoid them. Flag for the author: a full purge would be a manuscript-wide stylistic rewrite.
- Abbreviations / initials: unchanged from the previously validated draft; no new abbreviations introduced by this revision

## Refresh loop

`reproduce/reproduce.R` (analysis) → `reproduce/make_paper_assets.R` (macros + tables + figures into `latex/generated/` and `latex/figures/`) → `latexmk -pdf -jobname=<slug> manuscript.tex`. The manuscript is a view over `reproduce/output/`; no data-derived number is hand-typed.

## Reference validation

| Round | CORRECT | MINOR | SERIOUS |
|------:|--------:|------:|--------:|
| 1 | 66 | 9 (fixes applied) | 0 |

Rounds 2–3 pending author go-ahead (`dev/scripts/validate-round2.workflow.js`). This revision changed citation-adjacent text in: methods_pipeline (Kaiser 1960 now cited for the latent-root rule; TMFG/EBICglasso wording), introduction_pipelines (ARI scoped to Wang et al.; t-SNE cite removed there, still cited in the same subsection's final paragraph), discussion_limitations (new cites of openpsychometrics-2018, qwen-2025, pokropek-2026, garrido-etal-2025, guenole-etal-2025 in the added limitation paragraphs) — include these in the next round.

SEVEN NEW REFERENCES added 2026-07-02 (semantic theory of survey response literature; PDFs in papers/, txt in papers_txt/, bib entries built from the PDFs): arnulf-etal-2014, arnulf-etal-2018, arnulf-etal-2021, gefen-larsen-2017, larsen-bong-2016, rosenbusch-etal-2020, nimon-etal-2016. Cited in introduction_prediction (new opening paragraph), introduction_taxonomy, discussion_encodings, discussion_limitations. All seven are FIRST-ROUND candidates for the next reference-validation pass (never validated). Drafting reads: 2014/2018/2021/Rosenbusch read in full or near-full; Gefen-Larsen, Larsen-Bong, Nimon read at abstract + results + discussion/conclusion depth — the per-reference Opus validators must do the complete full-text pass.

## Package-method faithfulness audit — 2026-07-02 (separate from manuscript reference validation)

12 Opus auditors, one per method–source pairing (papers_txt ↔ R/): 0 misimplementations. TEFI numerically identical to EGAnet::tefi (5+ dp); ARI matches Hubert & Arabie Eq. 5 term-for-term; UVA wTO n-independence verified empirically. All attribution-precision fixes applied to the package docs (see NEWS 0.1.1).

## Open items

- OSF link placeholder in `methods_open_practices.tex` (author)
- Semantic-theory-of-survey-response literature paragraph: PDFs to be downloaded by author, then cite in `introduction_prediction.tex`/`introduction_pipelines.tex` and soften the toolkit-gap claim accordingly
- Reference-validation rounds 2–3, then missing-citation / statistics / claim passes
- Em-dash/semicolon policy decision (see pre-flight note)
