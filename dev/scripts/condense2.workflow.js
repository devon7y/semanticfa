export const meta = {
  name: 'condense2-semanticfa',
  description: 'Second condensation pass for the semanticfa manuscript: presentation-level condensing (figure sizes, merged heatmap pair, scriptsize verbatims) plus a deeper prose squeeze on the largest files, iterating until the PDF is under 70 pages.',
  phases: [
    { title: 'Format',   detail: 'one agent: shrink figures, merge the two similarity heatmaps into one two-panel figure, verbatim -> scriptsize' },
    { title: 'Condense', detail: 'parallel second-pass prose condensers on the largest files' },
    { title: 'Verify',   detail: 'citation inventory, compile, page count; loop while over 70' },
  ],
}

const A = (typeof args === 'string') ? JSON.parse(args) : (args || {})
const ROUND1 = A.round1 || []   // files for the first condense round
const ROUND2 = A.round2 || []   // fallback files if still over target
const ROOT = '/Users/devon7y/VS_Code/semanticfa'
const SECTIONS = ROOT + '/latex/sections'

const RESULT = {
  type: 'object', required: ['file', 'chars_before', 'chars_after', 'summary'],
  properties: { file: { type: 'string' }, chars_before: { type: 'number' },
    chars_after: { type: 'number' }, summary: { type: 'string' } },
}
const REPORT = {
  type: 'object', required: ['ok', 'pages', 'citation_diff', 'undefined_citations', 'summary'],
  properties: { ok: { type: 'boolean' }, pages: { type: 'number' },
    citation_diff: { type: 'string' }, undefined_citations: { type: 'number' },
    summary: { type: 'string' } },
}

phase('Format')

const fmt = await agent(
`You are condensing the PRESENTATION of an APA-7 manuscript (apa7 man mode) at ${ROOT}/latex without touching content. Current length 85 pages; every saved page matters. Make exactly these changes:

1. MERGE the two full-width similarity heatmaps into ONE two-panel figure: in ${SECTIONS}/results_encodings.tex, the figure with figures/corplot.pdf and the separate figure with figures/corplot_nli.pdf become a single figure environment with the two images side by side (width=0.48\\textwidth each, \\hfill between), ONE merged caption that preserves ALL information from both original captions (label the panels "Left:" and "Right:"), keeping the label fig:corplot (or whatever the first figure's label is). Then grep ALL section files for \\ref of the second figure's label and reword those references to point at the merged figure's right panel (e.g. "Figure~\\ref{fig:corplot}, right panel"). Move the merged figure to the position of whichever original figure came first, and make sure the surrounding prose still reads correctly (the two figures were introduced in different paragraphs — keep both introductions but have them reference the same figure's left/right panels).
2. SHRINK the remaining single full-width figures: scree.pdf, loadings.pdf, and semantic_vs_human.pdf from their current widths to width=0.6\\textwidth; itemmap.pdf from 0.95 to 0.8\\textwidth. Do not touch the three-panel model-size figure (already 0.32 each).
3. VERBATIM SIZE: in every section file, change the {\\footnotesize ... \\begin{verbatim}} wrappers around console-transcript blocks to {\\scriptsize ... } (keep the verbatim content itself byte-identical).
4. Compile from ${ROOT}/latex (pdflatex/bibtex/pdflatex/pdflatex, nonstopmode), fix any error you introduced, and report the new page count in your summary.

HARD RULES: no caption information may be dropped; no verbatim content may change; no prose may be deleted (only the reference rewordings in step 1); all \\labels must still resolve.

Return: file ("(formatting)"), chars_before 0, chars_after 0, summary (what you changed + new page count).`,
  { label: 'format', phase: 'Format', model: 'fable', schema: RESULT })

// ---------------------------------------------------------------------------

const CONSTRAINTS = `HARD CONSTRAINTS — violating any of these is failure:
1. Every citation command survives with the same bibkeys (merging two adjacent same-key cites is allowed; dropping a bibkey from the file is not).
2. Every \\label and \\ref survives unchanged.
3. Every verbatim environment survives BYTE-FOR-BYTE including its size wrapper.
4. Every table, figure environment, and caption survives (captions: only purely wordy phrases may be tightened).
5. Every NUMBER in prose survives exactly.
6. Every distinct content point — claim, caveat, design decision, demonstrated function, interpretation — survives. Compress wording, not content.
7. Scholarly APA prose: complete sentences, no telegraphic fragments.`

async function condenseRound(files, pct, roundName) {
  phase('Condense')
  log(`${roundName}: ${files.length} files at ~${pct}% prose cut`)
  return parallel(files.map((f) => () => agent(
`Second-pass condensation of ONE section file of an APA-7 LaTeX manuscript: ${SECTIONS}/${f}. A previous pass already removed obvious padding; the manuscript is still ~15 pages over its page budget, so this pass must find the harder cuts.

TARGET: a further ~${pct}% cut of the file's PROSE characters (verbatim blocks, tables, captions, headings excluded).

Where second-pass cuts come from:
- CITATION FRAMING: a finding currently introduced with setup + finding + implication often compresses to one sentence of claim + cite; keep every number.
- SENTENCE FUSION: adjacent sentences sharing a subject or argumentative role fuse into one with a semicolon or participle.
- CROSS-REFERENCE ECONOMY: where this file restates something another section already establishes (check neighbors in ${SECTIONS}/), replace the restatement with a clause + cross-reference, keeping any number stated here.
- RESIDUAL SIGNPOSTING: "as noted above", "it is worth emphasizing", "in other words" constructions.

${CONSTRAINTS}

Read the file in full, rewrite it in place, then verify your own work: grep the citation commands before/after, confirm verbatim blocks unchanged (diff against a copy), and confirm every number survived. Return: file, chars_before, chars_after (wc -c), summary.`,
    { label: 'condense2:' + f.replace('.tex',''), phase: 'Condense', model: 'fable', schema: RESULT })))
}

async function verify(roundName) {
  phase('Verify')
  return agent(
`Verify and recompile the condensed semanticfa manuscript (${roundName}).
1. Citation inventory from ${ROOT}/latex: grep -roh '\\\\cite[a-z]*\\(\\[[^]]*\\]\\)\\{0,2\\}{[^}]*}' sections/ | sed 's/.*{//; s/}//' | tr ',' '\\n' | sed 's/^ *//; s/ *$//' | sort -u — compare against the 73 keys in ${ROOT}/dev/scripts/citation_inventory.txt (every key must still be present somewhere in the manuscript; report any missing key as failure).
2. Spot-check 5 verbatim blocks against ${ROOT}/reproduce/reproduce.Rout for byte-fidelity.
3. Compile: pdflatex -> bibtex -> pdflatex -> pdflatex (nonstopmode); fix any introduced LaTeX error minimally; report pages (pdfinfo), undefined citations, and MISSINGCITE count (exactly 1 expected — the intentional Kaiser 1960 flag).
Return: ok, pages, citation_diff, undefined_citations, summary.`,
    { label: 'verify:' + roundName, phase: 'Verify', model: 'fable', schema: REPORT })
}

await condenseRound(ROUND1, 25, 'round 1')
let rep = await verify('round 1')
log(`After round 1: ${rep ? rep.pages : '?'} pages`)

if (rep && rep.pages > 70 && ROUND2.length) {
  await condenseRound(ROUND2, 20, 'round 2')
  rep = await verify('round 2')
  log(`After round 2: ${rep ? rep.pages : '?'} pages`)
}

return { format: fmt, finalReport: rep }
