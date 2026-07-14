export const meta = {
  name: 'validate-intro-semanticfa',
  description: 'Reference-validation sweep over the new semanticfa Introduction: one Opus agent per cited reference, reading the full source PDF + the full Introduction, running the write-paper skill A-J checks; returns a synthesis of CORRECT/MINOR/SERIOUS verdicts.',
  phases: [
    { title: 'Validate', detail: 'one Opus agent per Introduction-cited reference (A-J checks; full PDF + full Introduction)' },
  ],
}

const A = (typeof args === 'string') ? JSON.parse(args) : (args || {})
const KEYS = A.keys || []
const P = '/Users/devon7y/VS_Code/semanticfa'
const SKILL = '~/.claude/skills/write-paper/SKILL.md'

const VERDICT = {
  type: 'object',
  required: ['bibkey', 'overall', 'read_full_pdf', 'read_full_intro', 'citation_sites', 'faithfulness', 'serious_issues', 'minor_issues', 'bib_metadata_issues', 'best_source_flags', 'recommended_fixes', 'summary'],
  properties: {
    bibkey: { type: 'string' },
    overall: { type: 'string', enum: ['CORRECT', 'MINOR', 'SERIOUS'] },
    read_full_pdf: { type: 'boolean' },
    read_full_intro: { type: 'boolean' },
    citation_sites: { type: 'array', items: { type: 'string' } },
    faithfulness: { type: 'object', required: ['direction', 'magnitude', 'operationalization', 'population', 'attribution_layer'],
      properties: { direction: { type: 'string' }, magnitude: { type: 'string' }, operationalization: { type: 'string' }, population: { type: 'string' }, attribution_layer: { type: 'string' }, notes: { type: 'string' } } },
    serious_issues: { type: 'array', items: { type: 'string' } },
    minor_issues: { type: 'array', items: { type: 'string' } },
    bib_metadata_issues: { type: 'array', items: { type: 'string' } },
    best_source_flags: { type: 'array', items: { type: 'string' } },
    recommended_fixes: { type: 'array', items: { type: 'string' } },
    summary: { type: 'string' },
  },
}

phase('Validate')
log(`Validating ${KEYS.length} Introduction-cited references`)

const verdicts = await parallel(KEYS.map((key) => () => agent(
`Verify citation accuracy for ONE reference in the newly written INTRODUCTION of an academic manuscript, following the write-paper skill's "Validating the References" section (read ${SKILL} for the full A-J check definitions).

Manuscript Introduction: the eight files ${P}/latex/sections/introduction_overview.tex, introduction_meaning.tex, introduction_constructs.tex, introduction_two_structures.tex, introduction_prediction.tex, introduction_taxonomy.tex, introduction_pipelines.tex, introduction_package.tex (these are the only files with the new prose; read ALL EIGHT IN FULL).
Bib file: ${P}/latex/references.bib
Reference to verify: bibkey ${key}.
  - Find its source: a PDF in ${P}/papers/ and the extracted text in ${P}/papers_txt/. Match by filename even if it differs from the bibkey (e.g. garrido-etal-2025 -> Garrido_Etal_Preprint, golino-2026 -> Golino_Preprint, guenole-etal-2025 -> Guenole_Etal_Preprint, hussain-etal-2024 -> Hussian_Etal_2024 [filename typo], wang-etal-2026 -> Wang_Preprint, suarez-alvarez-etal-2026 -> Suárez-Álvarez_Etal_2026, harris-1954 -> Harris_1954 [the file is the 1981 anthology reprint; the cited essay is "Distributional Structure", and a focused extract is at ${P}/dev/intro_sources/Harris_1954_essay.txt], wittgenstein-1953 -> Wittgenstein_1953 [a book; verify the cited sections \\S43 and \\S\\S66--67 exist and say what is claimed]).
  - If NO PDF exists and the entry is an @manual/@misc software, model, or dataset citation (rcoreteam-2025, revelle-2026, golino-christensen-2026, yanitski-westbury-2026, qwen-2025, openpsychometrics-2018), verify ONLY the bib metadata plausibility and the in-text usage (set read_full_pdf=false and say so).

READ EVERYTHING IN FULL -- non-negotiable: read the ENTIRE reference PDF (or its papers_txt extraction) end to end, and the ENTIRE Introduction (all eight files). Then run checks A-J on EVERY Introduction citation site of ${key}:
(A) locate every citation site of ${key} in the introduction_*.tex files (report file + surrounding sentence);
(B) faithfulness by sub-dimension -- direction (must match the paper's OWN headline conclusion, not one sub-result), magnitude (verify EVERY number the manuscript attributes to it -- r, %, N, counts, congruence), operationalization (did it measure what the sentence implies), population, attribution layer (the paper's own contribution vs. something it reports about others);
(C) citation-group coherence (in a \\citep{a,b,c} group, does the specific claim hold for ${key} individually? disjunctions need member-to-disjunct mapping);
(D) quotation/numeric audit (verify any quoted phrase or number verbatim);
(E) precedence (if credited with introducing/originating X, does its own reference list credit an earlier source?);
(F) inference-gap / quantifier mismatch (is the claim a faithful restatement or an over-broad inference; do plural quantifiers exceed the single source?);
(G) omitted internal caveats the cited paper itself attaches;
(H) best-source (review-substitution, recency, author-overlap, repetition-avoidance, construct-substitution, lineage-stretch);
(I) bib metadata vs the PDF (authors COUNT + names, year, venue, volume, number, pages, DOI, entry type; initials-only). For joos-1950 and harris-1954 (newly added), scrutinize metadata especially: confirm joos-1950 = Joos, M. (1950), Description of Language Design, J. Acoust. Soc. Am. 22(6), 701-707; and harris-1954 = Harris, Z. S. (1954), Distributional Structure, Word, 10(2-3), 146-162 (file is a later reprint -- the cite should be the 1954 Word original).

Be objective: flag only when the source clearly contradicts the manuscript or a clearly better source exists. overall = SERIOUS if direction/attribution/operationalization is wrong or a number is materially wrong; MINOR for loose grouping / wording / best-source / metadata; CORRECT otherwise. Report under 400 words, structured by A-J.`,
  { label: 'verify:' + key, phase: 'Validate', model: 'opus', schema: VERDICT })
    .then(v => v || { bibkey: key, overall: 'SERIOUS', read_full_pdf: false, read_full_intro: false, citation_sites: [], faithfulness: {}, serious_issues: ['agent returned null (died/limit) -- rerun'], minor_issues: [], bib_metadata_issues: [], best_source_flags: [], recommended_fixes: [], summary: 'skipped' })))

const V = verdicts.filter(Boolean)
const counts = { CORRECT: 0, MINOR: 0, SERIOUS: 0 }
for (const v of V) { if (counts[v.overall] !== undefined) counts[v.overall]++ }
const nulls = V.filter(v => (v.serious_issues || []).some(s => /returned null/.test(s))).map(v => v.bibkey)

log(`Validation: ${counts.CORRECT} CORRECT, ${counts.MINOR} MINOR, ${counts.SERIOUS} SERIOUS across ${V.length}; ${nulls.length} died (rerun: ${nulls.join(', ')})`)

return {
  counts,
  died: nulls,
  serious: V.filter(v => v.overall === 'SERIOUS' && !nulls.includes(v.bibkey)).map(v => ({ bibkey: v.bibkey, issues: v.serious_issues, fixes: v.recommended_fixes })),
  minor: V.filter(v => v.overall === 'MINOR').map(v => ({ bibkey: v.bibkey, issues: v.minor_issues })),
  bib_metadata: V.filter(v => (v.bib_metadata_issues || []).length).map(v => ({ bibkey: v.bibkey, issues: v.bib_metadata_issues })),
  best_source: V.filter(v => (v.best_source_flags || []).length).map(v => ({ bibkey: v.bibkey, flags: v.best_source_flags })),
  verdicts: V,
}
