export const meta = {
  name: 'validate-semanticfa-round3',
  description: 'Round-3 (confirming) reference validation for the semanticfa manuscript: one Opus agent per cited reference, full A-J checks from scratch, reconciled against the round-2 verdicts.',
  phases: [
    { title: 'Validate', detail: 'one Opus agent per cited reference, prior verdict included for reconciliation (check J)' },
  ],
}

// args = { keys: ["bibkey", ...] } — prior verdicts live in PRIORS_FILE,
// keyed by bibkey; each validator reads its own entry from that file.
const A = (typeof args === 'string') ? JSON.parse(args) : (args || {})
const KEYS = A.keys || []
const ROOT = '/Users/devon7y/VS_Code/semanticfa'
const SKILL = '~/.claude/skills/write-paper/SKILL.md'
const PRIORS_FILE = ROOT + '/dev/scripts/validation_round2_priors.json'

const VERDICT = {
  type: 'object',
  required: ['bibkey', 'overall', 'read_full_pdf', 'read_full_manuscript', 'citation_sites', 'faithfulness', 'serious_issues', 'minor_issues', 'bib_metadata_issues', 'best_source_flags', 'recommended_fixes', 'reconciliation', 'summary'],
  properties: {
    bibkey: { type: 'string' },
    overall: { type: 'string', enum: ['CORRECT', 'MINOR', 'SERIOUS'] },
    read_full_pdf: { type: 'boolean' },
    read_full_manuscript: { type: 'boolean' },
    citation_sites: { type: 'array', items: { type: 'string' } },
    faithfulness: { type: 'object', required: ['direction', 'magnitude', 'operationalization', 'population', 'attribution_layer'],
      properties: { direction: { type: 'string' }, magnitude: { type: 'string' }, operationalization: { type: 'string' }, population: { type: 'string' }, attribution_layer: { type: 'string' }, notes: { type: 'string' } } },
    serious_issues: { type: 'array', items: { type: 'string' } },
    minor_issues: { type: 'array', items: { type: 'string' } },
    bib_metadata_issues: { type: 'array', items: { type: 'string' } },
    best_source_flags: { type: 'array', items: { type: 'string' } },
    recommended_fixes: { type: 'array', items: { type: 'string' } },
    reconciliation: { type: 'string', description: 'agreement/disagreement with the prior-round verdict and why' },
    summary: { type: 'string' },
  },
}

phase('Validate')

log(`Round-2 validation of ${KEYS.length} references`)

const verdicts = await parallel(KEYS.map((key) => () => agent(
`Verify citation accuracy for ONE reference in an academic manuscript — ROUND 2 of validation, following the write-paper skill's "Validating the References" section (read ${SKILL} for the full A-J check definitions).

Manuscript: ${ROOT}/latex/manuscript.tex inputs all section files from ${ROOT}/latex/sections/.
Bib file:   ${ROOT}/latex/references.bib
Reference to verify: bibkey ${key}. Its PDF is in ${ROOT}/papers/ (match by filename) and its text in ${ROOT}/papers_txt/. If NO matching PDF exists and the entry is an @manual/@misc software, model, or dataset citation, verify the bib metadata's plausibility and the in-text usage only (set read_full_pdf=false and say so) — these entries are permitted without PDFs for metadata-only claims.

Prior-round verdict on this reference: Read the JSON file ${PRIORS_FILE} and use the entry under the key "${key}" as the round-1 verdict for reconciliation in check (J).

The manuscript has been REVISED since round 1 (the round-1 SERIOUS and most MINOR issues were addressed by edits to the sections and the bib). Round 2 specifically: do the full A-I analysis FROM SCRATCH as if no prior verdict existed — do NOT collapse A-I into one-line "PASS" entries that just echo round 1; that is checklist drift and defeats the purpose of a second round. Reconcile only in (J), after you have formed your own independent verdict. Disagreement with round 1 is welcome — if you think round 1 was wrong, say so and explain why.

READ EVERYTHING IN FULL — non-negotiable: read the ENTIRE manuscript (every section .tex) and the ENTIRE reference PDF (or its papers_txt extraction) end to end. Then run checks A-J: (A) locate every citation site; (B) faithfulness by sub-dimension — direction (must match the paper's own headline conclusion), magnitude (every number), operationalization, population, attribution layer; (C) citation-group coherence (disjunctive claims need member-to-disjunct mapping); (D) quotation/numeric audit; (E) precedence; (F) inference-gap / quantifier mismatch; (G) omitted internal caveats; (H) best-source (review-substitution, recency, author-overlap, repetition-avoidance, construct-substitution, lineage-stretch); (I) bib metadata vs the PDF (authors COUNT + names, year, venue, volume, pages, DOI, entry type; initials-only). Special notes: wittgenstein-1953 must be an @book entry cited with a section number (e.g. [§43]); the manuscript now ends with Declarations and Open Practices Statement sections before the References (journal-required end matter) — citations there are in scope; if the JSON priors file has NO entry under "${key}", this is a first-round check for a newly added reference (say so in J). (J) RECONCILIATION — state whether your verdict agrees with the prior-round verdict above; if not, explain (manuscript edited, closer reading, prior error).

Be objective: flag only when the source clearly contradicts the manuscript or a clearly better source exists. overall = SERIOUS if direction/attribution/operationalization is wrong or a number is materially wrong; MINOR for loose grouping / wording / best-source / metadata; CORRECT otherwise. Report under 400 words, structured.`,
  { label: 'verify2:' + key, phase: 'Validate', model: 'opus', schema: VERDICT })
    .then(v => v || { bibkey: key, overall: 'SERIOUS', read_full_pdf: false, read_full_manuscript: false, citation_sites: [], faithfulness: {}, serious_issues: ['agent returned null'], minor_issues: [], bib_metadata_issues: [], best_source_flags: [], recommended_fixes: [], reconciliation: 'n/a', summary: 'skipped' })))

const V = verdicts.filter(Boolean)
const counts = { CORRECT: 0, MINOR: 0, SERIOUS: 0 }
for (const v of V) { if (counts[v.overall] !== undefined) counts[v.overall]++ }

log(`Round 2: ${counts.CORRECT} CORRECT, ${counts.MINOR} MINOR, ${counts.SERIOUS} SERIOUS across ${V.length} references`)

return {
  counts,
  serious: V.filter(v => v.overall === 'SERIOUS').map(v => ({ bibkey: v.bibkey, issues: v.serious_issues, fixes: v.recommended_fixes, reconciliation: v.reconciliation })),
  minor: V.filter(v => v.overall === 'MINOR').map(v => ({ bibkey: v.bibkey, issues: v.minor_issues, reconciliation: v.reconciliation })),
  overturned: V.filter(v => v.reconciliation && /disagree|overturn|wrong|differ/i.test(v.reconciliation)).map(v => ({ bibkey: v.bibkey, overall: v.overall, reconciliation: v.reconciliation })),
  verdicts: V,
}
