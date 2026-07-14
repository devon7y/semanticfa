export const meta = {
  name: 'claim-pass-semanticfa',
  description: 'Claim judgment pass (write-paper skill): claim-evidence alignment for every Abstract/Discussion/Conclusion claim against the Results, plus internal-consistency checks on every quantity or claim stated in two or more places. One Opus verifier per unit.',
  phases: [
    { title: 'Enumerate', detail: 'one Fable agent lists the claim units and consistency units' },
    { title: 'Verify', detail: 'one Opus agent per unit, Results as ground truth' },
  ],
}

const ROOT = '/Users/devon7y/VS_Code/semanticfa'
const SKILL = '~/.claude/skills/write-paper/SKILL.md'

const UNITS_SCHEMA = {
  type: 'object',
  required: ['units'],
  properties: {
    units: {
      type: 'array',
      items: {
        type: 'object',
        required: ['id', 'kind', 'text', 'locations'],
        properties: {
          id: { type: 'string' },
          kind: { type: 'string', enum: ['alignment', 'consistency'] },
          text: { type: 'string', description: 'the claim verbatim (alignment) or the quantity/claim and its restatements (consistency)' },
          locations: { type: 'array', items: { type: 'string' }, description: 'section file(s) and rough position' },
        },
      },
    },
  },
}

const VERDICT = {
  type: 'object',
  required: ['id', 'overall', 'finding', 'recommended_fix'],
  properties: {
    id: { type: 'string' },
    overall: { type: 'string', enum: ['CORRECT', 'MINOR', 'SERIOUS'] },
    finding: { type: 'string', description: 'what was checked and what was found, with the exact Results evidence' },
    recommended_fix: { type: 'string', description: 'empty string if CORRECT' },
  },
}

phase('Enumerate')

const enumeration = await agent(
`Enumerate the units for a claim judgment pass on an academic manuscript, following the write-paper skill's "Claim pass" definition (read ${SKILL}, section "Judgment Passes").

Manuscript: ${ROOT}/latex/manuscript.tex inputs all section files from ${ROOT}/latex/sections/. Read every section file in full. Data-derived numbers appear as \\val... macros defined in ${ROOT}/latex/generated/values.tex (each with a provenance comment) — read that file too so you can name quantities precisely.

Produce two kinds of units:
1. kind="alignment": every distinct empirical or evaluative claim in the ABSTRACT (sections/abstract.tex), the DISCUSSION (sections/discussion_*.tex), and the CONCLUSION (sections/conclusion_*.tex) that asserts something the Results must support (direction, magnitude, scope, or certainty). One unit per claim, text quoted verbatim. Merge trivial restatements of the same claim within one section into one unit, but keep Abstract vs Discussion vs Conclusion instances of the same claim as ONE unit listing all locations (the verifier checks each location's calibration).
2. kind="consistency": every quantity or substantive claim that appears in TWO OR MORE places across the manuscript (Abstract vs Results vs Discussion vs tables/captions vs Supplementary), where the unit is the quantity/claim itself and locations list every place it appears. Include the title/running-head pair and sample sizes.

Do not invent units for purely methodological descriptions or for citations of other papers' findings (the reference pass covers those). Target completeness over brevity, but each unit must be genuinely checkable against the Results. Use short stable ids (a1, a2, ... for alignment; c1, c2, ... for consistency).`,
  { label: 'enumerate-units', phase: 'Enumerate', schema: UNITS_SCHEMA })

const units = (enumeration && enumeration.units) ? enumeration.units : []
log(`Enumerated ${units.length} units (${units.filter(u => u.kind === 'alignment').length} alignment, ${units.filter(u => u.kind === 'consistency').length} consistency)`)
if (units.length === 0) return { error: 'enumeration returned no units' }

phase('Verify')

const verdicts = await parallel(units.map((u) => () => agent(
`Verify ONE unit of a claim judgment pass on an academic manuscript, following the write-paper skill's "Claim pass" definition (read ${SKILL}, section "Judgment Passes" -> "Claim pass").

Manuscript: ${ROOT}/latex/manuscript.tex inputs all section files from ${ROOT}/latex/sections/. READ THE ENTIRE MANUSCRIPT end to end (every section .tex). The RESULTS SECTION IS GROUND TRUTH for every downstream claim. Numbers are \\val... macros defined in ${ROOT}/latex/generated/values.tex (with provenance comments); the analysis outputs are in ${ROOT}/reproduce/output/ (results.rds and CSVs — read the CSVs if a table value needs checking).

Unit ${u.id} (${u.kind}):
${u.text}
Locations: ${u.locations.join('; ')}

If kind=alignment: locate the supporting result(s) in the Results section and verify the claim does not OVERSTATE them on direction, magnitude, scope, or certainty. Overstatement examples: "indistinguishable" with no equivalence test, causal or predictive "will" from correlational evidence, generalization beyond the demonstrated case (one inventory, one embedding family), a hedge present in Results but dropped downstream. Also flag UNDERSTATED or stale claims (Results changed, downstream text did not).

If kind=consistency: verify the quantity or claim is stated identically at every location listed (and search the manuscript for any location the enumerator missed). Numbers must agree exactly (same macro or same value at the stated precision); verbal restatements must not drift in direction or scope; terminology must be stable.

Verdict: SERIOUS if a claim contradicts or materially overstates the Results, or two statements of the same quantity disagree; MINOR for a soft overstatement, a dropped hedge, scope drift, or terminology drift; CORRECT otherwise. In "finding", quote the exact Results evidence you checked against. Keep it under 250 words. Do not invent issues: the manuscript deliberately hedges many claims, and a hedged claim matching a hedged result is CORRECT.`,
  { label: 'claim:' + u.id, phase: 'Verify', model: 'opus', schema: VERDICT })
    .then(v => v || { id: u.id, overall: 'SERIOUS', finding: 'agent returned null', recommended_fix: 'rerun' })))

const V = verdicts.filter(Boolean)
const counts = { CORRECT: 0, MINOR: 0, SERIOUS: 0 }
for (const v of V) { if (counts[v.overall] !== undefined) counts[v.overall]++ }
log(`Claim pass: ${counts.CORRECT} CORRECT, ${counts.MINOR} MINOR, ${counts.SERIOUS} SERIOUS across ${V.length} units`)

const unitById = Object.fromEntries(units.map(u => [u.id, u]))
const detail = (v) => ({ id: v.id, kind: (unitById[v.id] || {}).kind, text: (unitById[v.id] || {}).text, locations: (unitById[v.id] || {}).locations, finding: v.finding, fix: v.recommended_fix })

return {
  counts,
  serious: V.filter(v => v.overall === 'SERIOUS').map(detail),
  minor: V.filter(v => v.overall === 'MINOR').map(detail),
  n_units: units.length,
}
