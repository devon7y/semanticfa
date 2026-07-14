export const meta = {
  name: 'punctuation-purge-semanticfa',
  description: 'Purge every em dash (---) and semicolon (;) from the manuscript prose, one Fable-5 agent per section file, under strict fidelity constraints (verbatim/macros/numbers/citations/exploratory left untouched).',
  phases: [
    { title: 'Purge', detail: 'one Fable-5 agent per section file recasts em dashes and semicolons' },
  ],
}

const ROOT = '/Users/devon7y/VS_Code/semanticfa'
const A = (typeof args === 'string') ? JSON.parse(args) : (args || {})
const FILES = A.files || []

const RESULT = {
  type: 'object',
  required: ['file', 'em_before', 'semi_before', 'em_after', 'semi_after', 'changed', 'notes'],
  properties: {
    file: { type: 'string' },
    em_before: { type: 'integer' },
    semi_before: { type: 'integer' },
    em_after: { type: 'integer', description: 'em dashes remaining in PROSE (must be 0)' },
    semi_after: { type: 'integer', description: 'semicolons remaining in PROSE (must be 0)' },
    changed: { type: 'boolean' },
    notes: { type: 'string', description: 'anything ambiguous or left in place deliberately (e.g. inside verbatim)' },
  },
}

phase('Purge')
log(`Purging em dashes and semicolons from ${FILES.length} section files`)

const results = await parallel(FILES.map((fname) => () => agent(
`You are copy-editing ONE file of an APA-7 LaTeX manuscript to remove two forbidden punctuation marks from its PROSE: the em dash (LaTeX \`---\`) and the semicolon (\`;\`). This is a mechanical fidelity task, not a rewrite. Follow the rules exactly.

FILE (edit it in place with the Edit tool): ${ROOT}/latex/sections/${fname}

WHAT TO CHANGE:
- Every em dash \`---\` in prose: recast the sentence WITHOUT it, using a comma, a colon, parentheses, or by splitting into two sentences. Pick whichever preserves the exact meaning and reads most naturally. Example: "X worked---the baseline did not" becomes "X worked. The baseline did not." or "X worked, but the baseline did not."
- Every semicolon \`;\` in prose: recast WITHOUT it, using two sentences, a comma plus a conjunction (and/but/so/or), or a comma-separated list. Example: "A holds; B does not" becomes "A holds, but B does not." For semicolon-separated lists ("x; y; z"), use commas, or "x, y, and z" if it is the final list.

ABSOLUTE CONSTRAINTS (violating any of these is a failure):
1. DO NOT touch anything inside a \`\\begin{verbatim} ... \\end{verbatim}\` block. That is R code and console output checked byte-for-byte against a transcript. Leave every character inside verbatim exactly as-is, including any \`;\` or \`---\` there. (Report those in "notes" but DO NOT edit them.)
2. DO NOT change any number, any \`\\val...\` macro, any citation key (\`\\citep\`/\`\\citet\`/\`\\citealp{...}\`), any \`\\ref\`/\`\\label\`, any \`\\url\`, any \`\\texttt\`/\`\\emph\` argument's identifier text, or any equation/math.
3. DO NOT change the word "exploratory" anywhere. Leave every occurrence exactly as written.
4. PRESERVE en dashes \`--\` (two hyphens): they are numeric ranges (e.g. \`1--5\`, \`2--6\\%\`) and relationship compounds (e.g. \`semantic--behavioral\`, \`jingle--jangle\`, \`Kaiser--Meyer--Olkin\`). ONLY remove EM dashes \`---\` (three hyphens). Do not merge or split hyphens.
5. Preserve the math thin-space \`\\;\` (backslash-semicolon) if present. That is not a prose semicolon.
6. Preserve meaning EXACTLY. The only words you may add or remove are minimal connectives (and/but/so/or/that) and articles needed when splitting a sentence. Do not reword for style, do not "improve" anything, do not change terminology, do not touch anything that is not adjacent to an em dash or semicolon.
7. Introduce NO new em dash or semicolon. Do not use a colon where a comma reads better; use colons sparingly and only where they fit.

METHOD: Read the whole file first. Locate every \`---\` and every \`;\` that is in prose (not in verbatim, not the math \`\\;\`). Apply one Edit per occurrence (or a combined Edit for a sentence with several). Then re-read and confirm zero prose em dashes and zero prose semicolons remain.

Return: em_before / semi_before (counts you found in prose), em_after / semi_after (must both be 0), changed (did you edit the file), and notes (any semicolons/dashes left inside verbatim, or any spot where the recast was non-obvious). Count ONLY prose occurrences, never verbatim ones.`,
  { label: 'purge:' + fname, phase: 'Purge', model: 'fable', agentType: 'general-purpose', schema: RESULT })
    .then(r => r || { file: fname, em_before: -1, semi_before: -1, em_after: -1, semi_after: -1, changed: false, notes: 'agent returned null' })))

const R = results.filter(Boolean)
const dirty = R.filter(r => r.em_after !== 0 || r.semi_after !== 0)
log(`Done. ${R.length} files processed, ${dirty.length} still reporting residue`)

return {
  processed: R.length,
  clean: R.filter(r => r.em_after === 0 && r.semi_after === 0).length,
  residue: dirty.map(r => ({ file: r.file, em_after: r.em_after, semi_after: r.semi_after, notes: r.notes })),
  files: R,
}
