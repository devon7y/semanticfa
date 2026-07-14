export const meta = {
  name: 'condense-semanticfa',
  description: 'Sync the semanticfa manuscript to the regenerated transcript (bundled-data switch + MR label swap), then condense prose from 94 to under 70 pages: one Fable agent per section file, preserving every citation, number, verbatim block, table, and content point; then recompile and verify.',
  phases: [
    { title: 'Sync',       detail: 'one agent rebuilds every transcript-quoted verbatim block and MR-label reference from the fresh reproduce.Rout' },
    { title: 'SyncVerify', detail: 'adversarial check: every verbatim block byte-compared against the transcript; MR-domain pairings verified; compile' },
    { title: 'Condense',   detail: 'one agent per prose section file, parallel, in-place rewrite' },
    { title: 'Verify',     detail: 'citation-inventory diff, recompile, page count' },
  ],
}

// args = { files: { "introduction_x.tex": targetPercent, ... } }
const A = (typeof args === 'string') ? JSON.parse(args) : (args || {})
const FILES = A.files || {}
const ROOT = '/Users/devon7y/VS_Code/semanticfa'
const SECTIONS = ROOT + '/latex/sections'

const RESULT = {
  type: 'object', required: ['file', 'chars_before', 'chars_after', 'summary'],
  properties: {
    file: { type: 'string' }, chars_before: { type: 'number' },
    chars_after: { type: 'number' }, summary: { type: 'string' },
  },
}

const REPORT = {
  type: 'object', required: ['ok', 'pages', 'citation_diff', 'undefined_citations', 'summary'],
  properties: {
    ok: { type: 'boolean' }, pages: { type: 'number' },
    citation_diff: { type: 'string', description: 'diff of per-key citation counts before vs after, or "identical"' },
    undefined_citations: { type: 'number' },
    summary: { type: 'string' },
  },
}

const SYNC_REPORT = {
  type: 'object', required: ['files_edited', 'blocks_synced', 'summary'],
  properties: {
    files_edited: { type: 'array', items: { type: 'string' } },
    blocks_synced: { type: 'number' },
    summary: { type: 'string' },
  },
}

phase('Sync')

const sync = await agent(
`You are synchronizing an APA-7 LaTeX manuscript against a freshly regenerated analysis transcript. The manuscript (${ROOT}/latex/sections/*.tex) quotes real R console output in verbatim blocks; the analysis was re-run after two changes (the package now bundles the 8B item embeddings in data(big5), and the analysis script was extended), so several quoted blocks are stale. The fresh, authoritative transcript is ${ROOT}/reproduce/reproduce.Rout (read it IN FULL), with machine-readable tables in ${ROOT}/reproduce/output/*.csv and the analysis script at ${ROOT}/reproduce/reproduce.R.

WHAT CHANGED between the old and new transcript (from a line diff):
- Section 1 now loads the bundled data directly: data(big5) + str(big5) showing 50 x 4096 embeddings; there is NO sfa_load_npz() call for item embeddings any more.
- Section 4 now loads an offline 0.6B REFERENCE archive: ref06 <- sfa_load_npz("data/Big5_items_0.6B.npz") (printing it shows Dimensions: 1024 and bundled-style factor names), then sfa_clear_cache(), then the timed sfa_embed() call, then summary of cos_live_ref (the variable was renamed from cos_live_bundled).
- The psych factor labels MR4 and MR1 SWAPPED throughout the main fit's outputs (a tiny perturbation flipped psych's arbitrary extraction-order naming): the Conscientiousness-anchored factor is now MR1 (was MR4) and the Openness-anchored factor is now MR4 (was MR1). MR3/MR2/MR5 are unchanged. All loading VALUES are identical to within +/-0.001 (third-decimal wobbles, e.g. N20 .575 -> .574; TEFI -27.7066 -> -27.7059).
- All elapsed timings changed (NLI matrix, sfa_embed, sfa_dimselect, calibrate).
- fit06 in Section 8 is now built with embeddings = ref06$embeddings (not big5$embeddings).
- A new Section 9b (model-size comparison) exists in the transcript; the corresponding manuscript subsection sections/results_modelsize.tex is already written and should be checked against the transcript numbers (0.6B/4B: parallel 4, kaiser 6, TEFI 2, EGA 6, consensus 6; 8B: parallel 5, kaiser 6, TEFI 2, EGA 5, consensus 5) but NOT rewritten.

YOUR TASK — for every section file that quotes transcript content, bring it into EXACT agreement with the new reproduce.Rout:
1. ${ROOT}/latex/sections/results_encodings.tex: (a) replace the first verbatim block (currently shows sfa_load_npz of an ITEMS_NPZ) with the new Section-1 content quoted exactly from the Rout ("> data(big5)" with its comment, the str(big5) output lines, and the E8 assignment line; keep the {footnotesize verbatim} wrappers); (b) update the NLI timing line; (c) replace the live-embedding verbatim block near the end with the new Section-4 content (the ref06 <- sfa_load_npz(...) line, the printed ref06 summary, sfa_clear_cache(), the timed sfa_embed line with its new timing, and the cos_live_ref summary), and update the surrounding prose: the demonstration now checks the live backend against REFERENCE vectors generated offline with the same default model (loaded with sfa_load_npz), not against "the bundled" vectors — the closing sentence should say the live re-embedding matched the offline reference at cosine >= .9999, confirming the on-device backend reproduces the model's archived output.
2. ${ROOT}/latex/sections/results_fit.tex: re-extract EVERY verbatim block from the new Rout (the print(fit) block including the new TEFI value, the omega/DAAL block, the psych::fa.sort block, and the calibrate block with its new elapsed time), then swap MR4<->MR1 in the PROSE so every factor-name-to-domain pairing matches the new outputs (Conscientiousness = MR1, Openness = MR4 now). Verify every number quoted in prose against the new blocks (most are unchanged; third decimals may wobble).
3. ${ROOT}/latex/sections/results_retention.tex: update the sfa_dimselect elapsed time; verify the dimselect verbatim values (optimal depth, NMI, TEFI) against the new Rout and sync if changed.
4. ${ROOT}/latex/sections/results_visualization.tex: it has one MR-label mention; swap if it involves MR4/MR1 and verify against the new loadings.
5. ${ROOT}/latex/sections/results_human_comparison.tex: re-extract the Tucker-phi factor-by-factor block/table from the new Rout (the SEMANTIC factor rows are relabelled by the MR4<->MR1 swap; the human EFA labels are unchanged) and swap MR labels in the surrounding prose accordingly. The numeric phi values are unchanged.
6. ${ROOT}/latex/sections/results_refinement.tex: re-extract the fit06/sfa_item_fit verbatim block (the call now reads embeddings = ref06$embeddings; output values unchanged — verify).
7. ${ROOT}/latex/sections/supplementary_loadings.tex: regenerate the SEMANTIC loadings table's column headers and values from ${ROOT}/reproduce/output/semantic_loadings.csv (column order/labels changed by the MR swap; values wobble in the third decimal). The HUMAN loadings table is unchanged (verify a few rows against human_loadings.csv).
8. Grep ALL section files for any remaining stale timing strings, "cos_live_bundled", "ITEMS_NPZ", or MR-label/domain pairings inconsistent with the new transcript, and fix them.

RULES: verbatim block content must match the Rout byte-for-byte for the lines quoted (the manuscript may elide lines — preserve existing elision style); do not change any prose claims beyond the label swap and the reference-vector reframing described above; do not touch citations; keep all {footnotesize verbatim} wrappers.

Then compile from ${ROOT}/latex (pdflatex/bibtex/pdflatex/pdflatex, nonstopmode) and fix any errors you introduced. Return files_edited, blocks_synced (count of verbatim blocks you rebuilt), summary.`,
  { label: 'sync-transcript', phase: 'Sync', model: 'fable', schema: SYNC_REPORT })

phase('SyncVerify')

const syncCheck = await agent(
`Adversarially verify that the manuscript at ${ROOT}/latex/sections matches the analysis transcript at ${ROOT}/reproduce/reproduce.Rout. A sync agent just rebuilt the transcript-quoted blocks after a re-run; your job is to catch anything it missed or got wrong.

1. Enumerate EVERY verbatim environment in ${ROOT}/latex/sections/*.tex that quotes R console content. For each, locate the corresponding lines in ${ROOT}/reproduce/reproduce.Rout and compare exactly (the manuscript may elide lines; the lines it does quote must match byte-for-byte, including numbers and prompts). Report every mismatch with file:line.
2. Verify every factor-label-to-domain pairing in prose (MR3 = Neuroticism, MR1 = Conscientiousness, MR2/MR5 = Agreeableness-Extraversion blends, MR4 = Openness under the NEW labelling) against the new DAAL/loadings blocks in the Rout. Report any stale pairing.
3. Verify the supplementary semantic-loadings table against ${ROOT}/reproduce/output/semantic_loadings.csv (headers and a 10-row spot check), and the encoding table in results_human_comparison.tex against ${ROOT}/reproduce/output/encoding_table.csv at displayed precision.
4. Verify the model-size subsection (results_modelsize.tex) numbers against the Rout's Section 9b and that figures/corplot_06b.pdf, corplot_4b.pdf, corplot_8b.pdf exist in ${ROOT}/latex/figures.
5. FIX any discrepancies you find (minimal edits, byte-faithful to the Rout), then recompile (pdflatex/bibtex/pdflatex/pdflatex) and report the page count and zero-error status.

Return files_edited (any you had to fix), blocks_synced (count of blocks you checked), summary (mismatches found and fixed; final page count).`,
  { label: 'sync-verify', phase: 'SyncVerify', model: 'fable', schema: SYNC_REPORT })

phase('Condense')

const names = Object.keys(FILES)
log(`Condensing ${names.length} section files`)

const results = await parallel(names.map((f) => () => agent(
`You are condensing ONE section file of an APA-7 LaTeX manuscript (a software/methods paper introducing the R package semanticfa) to reduce page count. The manuscript is double-spaced apa7 man mode, currently 94 pages; the target is under 70, so prose must tighten substantially while losing NOTHING of substance.

YOUR FILE: ${SECTIONS}/${f}
TARGET: cut the file's PROSE character count by about ${FILES[f]}% (verbatim blocks, tables, and headings do not count toward the cut and must not change).

First read ${SECTIONS}/${f} in full. Also skim the neighboring section files in ${SECTIONS}/ as needed to avoid breaking cross-references or duplicating transitions. Then REWRITE ${SECTIONS}/${f} in place.

HARD CONSTRAINTS — violating any of these is failure:
1. Every citation command must survive: the exact same set of \\citep/\\citet/\\citealp commands with the exact same bibkeys must appear after the rewrite (you may move a citation within its paragraph or merge two adjacent cites of the same key into one, but you may NOT drop a bibkey from the file; if you merge two \\citep{X} sites into one, say so in your summary).
2. Every \\label and every \\ref must survive unchanged.
3. Every verbatim/Verbatim environment must survive BYTE-FOR-BYTE (these are real console transcripts; do not touch a single character inside them, do not change their \\footnotesize wrappers).
4. Every table (tabular/booktabs), figure environment, caption, and equation must survive unchanged (captions may be lightly tightened only if purely wordy).
5. Every NUMBER in prose (statistics, Ns, percentages, page counts, thresholds) must survive exactly.
6. Every distinct content point — every claim, caveat, design decision, demonstrated function, interpretation — must survive. You are compressing the WORDING, not the content. Merge overlapping sentences, cut redundant restatements, replace wordy constructions ("it is important to note that" etc.), collapse throat-clearing transitions, convert three-sentence explanations into one precise sentence.
7. Keep scholarly APA prose: complete sentences, no telegraphic fragments, no bullet lists where prose exists now. Keep \\subsection headings as they are.
8. Keep the same opening-sentence function (a reader skimming first sentences should still follow the argument).

Style of cut, in priority order: (a) redundancy WITHIN the file (the same idea stated twice); (b) meta-commentary about what the section will do; (c) expansive paraphrase around citations (state the finding once, tightly); (d) adjectival padding. Do NOT cut by deleting whole topics.

Return: file (the filename), chars_before, chars_after (measure with wc -c), summary (2-4 sentences: what you compressed, any same-key cite merges).`,
  { label: 'condense:' + f.replace('.tex',''), phase: 'Condense', model: 'fable', schema: RESULT })))

const done = results.filter(Boolean)
const before = done.reduce((s, r) => s + (r.chars_before || 0), 0)
const after = done.reduce((s, r) => s + (r.chars_after || 0), 0)
log(`Condensed ${done.length}/${names.length} files: ${before} -> ${after} chars (${Math.round(100 - 100 * after / Math.max(before, 1))}% cut)`)

phase('Verify')

const report = await agent(
`Verify and recompile the condensed semanticfa manuscript.

1. CITATION INVENTORY: run exactly this from ${ROOT}/latex:
   grep -roh '\\\\cite[a-z]*\\(\\[[^]]*\\]\\)\\{0,2\\}{[^}]*}' sections/ | sed 's/.*{//; s/}//' | tr ',' '\\n' | sed 's/^ *//; s/ *$//' | sort | uniq -c | sort -rn
   and diff the result against the pre-condensation inventory at ${ROOT}/dev/scripts/citation_inventory.txt. Bibkeys must all still be present; per-key counts may have DECREASED only where a condenser merged two adjacent same-key cites (small decreases are acceptable; a key vanishing entirely is NOT — report it as a failure). Summarize the diff.
2. Check that no verbatim environment was altered: the manuscript's verbatim blocks are real console output; spot-check several against ${ROOT}/reproduce/reproduce.Rout.
3. COMPILE from ${ROOT}/latex: pdflatex -> bibtex -> pdflatex -> pdflatex (nonstopmode). Fix any LaTeX errors a condenser introduced (unbalanced braces, broken environments) by minimal repair. Report final page count (pdfinfo manuscript.pdf), undefined citations, and any remaining MISSINGCITE flags (one is expected: the intentional Kaiser 1960 flag).
4. Sanity-read the Introduction's first page and the Discussion's first page in the compiled text for coherence (no mid-sentence truncations).

Return: ok, pages, citation_diff, undefined_citations, summary.`,
  { label: 'verify+compile', phase: 'Verify', model: 'fable', schema: REPORT })

return { sync, syncCheck, condensed: done, chars: { before, after }, report }
