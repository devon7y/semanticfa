export const meta = {
  name: 'write-paper-semanticfa',
  description: 'Write the semanticfa software/methods paper (APA-7, apa7.cls): three-pillar structure, Fable 5 drafting split across token-budgeted literature writers, Opus per-reference citation validation.',
  phases: [
    { title: 'Plan',        detail: 'init latex tree, verify papers_txt, token-count papers, build pillar-aligned writer groups, pre-stage figures' },
    { title: 'Draft',       detail: 'parallel Fable 5 writers: N Introduction-literature agents + Methods + Results' },
    { title: 'Assemble-Intro', detail: 'one Fable 5 agent writes the coherent Introduction from all literature agents’ contributions' },
    { title: 'Discussion',  detail: 'one Fable 5 agent synthesizes the Discussion from every prior agent’s output' },
    { title: 'Abstract',    detail: 'one Fable 5 agent writes the Abstract and Conclusion and does a global coherence pass' },
    { title: 'Assemble',    detail: 'merge bib, compute \\shortcites, finalize preamble, compile' },
    { title: 'Validate',    detail: 'one Opus agent per cited reference: full PDF + full manuscript, A–J checks' },
  ],
}

// ---------------------------------------------------------------------------
// Inputs: args = { title, shortTitle, workingDir, authors }
// ---------------------------------------------------------------------------
const A = (typeof args === 'string') ? JSON.parse(args) : (args || {})
const TITLE = A.title || '(set the title in args)'
const SHORT = A.shortTitle || ''
const ROOT  = A.workingDir || '.'
const SKILL = '~/.claude/skills/write-paper/SKILL.md'
const WRITER = 'fable'
const VALIDATOR = 'opus'
const TOKEN_BUDGET = 800000

// ---------------------------------------------------------------------------
// Paper-specific brief — appended to every drafting agent's preamble.
// ---------------------------------------------------------------------------
const BRIEF = `
PAPER BRIEF — binding for every agent on this paper.

WHAT THIS PAPER IS. A software/methods paper (Behavior Research Methods style)
introducing semanticfa (version 0.1.0, on CRAN): an R package for Semantic
Factor Analysis (SFA) — exploratory factor analysis of language-model
embeddings of psychometric scale items, requiring no human response data.
Authors: Devon Yanitski and Chris Westbury, Department of Psychology,
University of Alberta, Edmonton, Alberta, Canada. Correspondence: Devon
Yanitski, dyanitsk@ualberta.ca. Write the package name as \\texttt{semanticfa}
(lower-case, even at sentence starts — recast the sentence if needed).

THREE PILLARS, ROUGHLY BALANCED; the package demonstration is the centerpiece.

Pillar 1 — THEORY (Introduction, first half). What it means to analyze the
language of a scale: constructs live in a nomological network (Cronbach and
Meehl) — validity is a web of relations, not a hidden essence; a scale's items
are an operationalized semantic proposal for the construct (Wittgenstein's
meaning-as-use; reflective vs. formative indicators); two relational
structures over the same items — behavioral (person-by-item covariance) vs.
semantic (the geometry of item meaning) — are structurally parallel analyses
with different data sources: classical FA recovers behavioral clustering, SFA
recovers linguistic-usage clustering. NEW CONTRIBUTION to foreground:
semantic-behavioral coherence as a relation in the nomological network —
convergence strengthens a construct; divergence is itself informative
(compression, ambiguity, jingle-jangle, theoretical underspecification). SFA
needs no respondents, so scales can be evaluated before data collection. A
prior theory draft exists at
/Users/devon7y/VS_Code/LLM_Factor_Analysis/latex-claude/sections/ (the
introduction_*.tex files): CONDENSE its ideas, never copy verbatim, and keep
only citations whose PDFs are in this project's papers/ folder.

Pillar 2 — REVIEW (Introduction, second half). Synthesize the literature
corpus organized by what each line of work DOES: (a) embeddings predict item
correlations / a-priori factor structure (Milano, Casella, Hommel and Arslan,
Schoenegger, Ravenda, Feraco, Guenole); (b) taxonomy, jingle-jangle,
incommensurability (Wulff and Mata 2025/2026, Stanghellini); (c) clinical /
psychopathology structure from embeddings (Kambeitz, Kojima); (d) methods and
pipelines: SQuID centering (Pellert), CFA with embeddings (Pokropek), Unique
Variable Analysis (Christensen), EGA on embeddings (Golino, Garrido), short
forms (Jung and Seo, Wang), tutorials/overviews (Hussain, Low); (e) critiques
and limits (Uher: statistics is not measurement). Position semanticfa as the
first general-purpose toolkit unifying these threads in one reproducible R
workflow. End the Introduction with a short subsection introducing the package
and previewing the demonstration.

Pillar 3 — DEMONSTRATION (Method + Results; the centerpiece). One reproducible
worked study on the 50 IPIP Big-Five Factor Markers that exercises every
exported function. THE ANALYSES HAVE ALREADY BEEN RUN — never invent or
estimate a number. The ground truth is:
  - ${ROOT}/reproduce/reproduce.R     (the master analysis script)
  - ${ROOT}/reproduce/reproduce.Rout  (the captured console transcript: every
    command WITH its real output — quote code blocks and outputs from here)
  - ${ROOT}/reproduce/output/*.csv    (key tables: encoding comparison, Tucker
    matrix, semantic + human loadings, anchor matrix)
  - figures pre-staged in ${ROOT}/latex/figures/ (see the planner's list)
Facts to state exactly in the Method: main results use precomputed
Qwen/Qwen3-Embedding-8B item embeddings (50 x 4096; last-token pooling, no
instruction prefix) loaded with sfa_load_npz(); the package's bundled
data(big5) carries Qwen/Qwen3-Embedding-0.6B embeddings (50 x 1024) and is
used for the live-embedding demonstrations (sfa_embed, sfa_item_fit); the NLI
matrix uses cross-encoder/nli-deberta-v3-base; construct-label and
projection-pole vectors come from the same Qwen3-Embedding-8B model. The
human benchmark is the Open-Source Psychometrics Project Big Five dataset
(openpsychometrics.org/_rawdata/) — used ONLY as the validation target for the
semantic structure (a conventional EFA on responses, compared with
sfa_congruence); no response data enters the SFA itself. Report the cleaned N
from the transcript. The Results walk the package workflow in stages: embed
and build similarity (4 encodings + NLI) -> retention (parallel / Kaiser /
TEFI / EGA, dimselect) -> the sfa() fit with diagnostics (KMO, TEFI, RMSR,
CAF, omega, DAAL, Monte-Carlo calibration) -> interpretation (anchor,
projection, congruence vs. theory, jingle-jangle) -> refinement (redundancy,
short form, candidate-item vetting) -> visualization -> the semantic-vs-human
comparison (congruence metrics, Tucker matrix, item-pair r), which
operationalizes the semantic-behavioral coherence idea from Pillar 1. Show
REAL R code blocks with REAL (often abridged) output, verbatim from
reproduce.Rout, in a plain verbatim-based environment (apa7-safe; small type,
e.g. wrap verbatim in \\small — do not use minted or shell-escape).

RESULTS NARRATIVE POINTERS (verify every number against reproduce.Rout).
The main fit uses the keying-free mean_centered_pearson encoding (a genuine
correlation matrix); retention consensus is 5 factors (parallel 5, Kaiser 6,
TEFI 2, EGA 5) and the 5-factor solution recovers the Big Five domains. The
encoding-comparison table is a centerpiece result with an instructive
asymmetry: sign-flipping reverse-keyed item embeddings (atomic_reversed)
produces ANTI-TOPIC vectors, so it aligns worst with the keyed human
correlations — flipping in embedding space does not emulate reverse-scoring
in response space. The NLI encoding best matches the theoretical partition
and the raw-response correlations but has the weakest loading-shape (Tucker)
match; the cosine encodings show the reverse profile. The disattenuated
congruence metric is undefined (reported NA) where a similarity matrix's
split-half reliability is negative (the keying flip imposes a checkerboard
pattern) — explain this in one sentence rather than hiding the NA. Human
parallel analysis on N≈874K suggests many minor factors (10); the human EFA
is fixed at 5 on theoretical grounds (Goldberg). The candidate-item demo
deliberately fits the bundled-data model WITHOUT the scoring column (the
script's comments explain why: flipped items cancel topic centroids); mention
this choice. The short-form result (structure recovery improves after
pruning) and the redundancy pairs are strong applied selling points.

DISCUSSION. Synthesize the three pillars: what the demonstrated convergence
(and any divergence) means for semantic-behavioral coherence; use cases
(pre-data scale design, item vetting, short forms, cross-scale screening);
limits. DO NOT OVERCLAIM: SFA complements, never replaces, response-based FA —
cite the convergence-with-divergence evidence (e.g., Kojima, Feraco). Honour
Uher's critique seriously rather than dismissively.

CITATION PROVENANCE (method papers — cite each as the primary source for the
method it underpins): Horn (1965) for parallel analysis / sfa_parallel; van
der Maaten and Hinton (2008) for the t-SNE option in sfa_itemplot; Hubert and
Arabie (1985) for the adjusted Rand index in sfa_congruence; Bowman et al.
(2015, SNLI) for the NLI basis of sfa_nli_matrix; Christensen et al. (2023)
for Unique Variable Analysis in sfa_redundancy; Grand et al. (2022) for
semantic projection in sfa_project; Wulff and Mata (2025, 2026) for
jingle-jangle in sfa_jinglejangle; Pellert et al. (2026) for SQuID centering;
Golino (preprint) and Garrido et al. (preprint) for TEFI/EGA-based retention;
Pokropek (2026) for Monte-Carlo calibration of fit indices; Jung and Seo
(2025) and Wang (preprint) for embedding-based short forms; Hommel and Arslan
(2025) for polarity-calibrated NLI similarity; Kaiser (1958) for varimax /
eigenvalue rules as historically appropriate; Goldberg (1990) for the IPIP
Big-Five Factor Markers. Wittgenstein (1953), Philosophical Investigations, is
in papers_txt: it is a BOOK — use an @book entry and cite the specific
section, \\citep[§43]{wittgenstein-1953}, never the whole volume.

SOFTWARE AND DATA CITATIONS. R, the psych and EGAnet packages, the semanticfa
package itself, the Qwen3-Embedding model, and the Open-Source Psychometrics
dataset may be cited as @manual/@misc entries WITHOUT PDFs — metadata only
(name, version, URL); hang no substantive empirical claim on them. Every
substantive claim still needs a papers/ PDF or a \\MISSINGCITE flag.

REPRODUCIBILITY STATEMENT. The Method (or a short Open Practices paragraph)
must state that all analysis code, precomputed embeddings, and outputs are in
a self-contained reproduction archive (the reproduce/ folder) to be hosted on
OSF; write the URL as \\url{https://osf.io/XXXXX} with a clearly marked
"[OSF link to be inserted]" note — do not invent a real-looking OSF id.

STRUCTURE NOTES. Single References list. Use the standard APA skeleton:
Introduction (no \\section heading; subsections for Pillar 1 themes, Pillar 2
themes, and "The semanticfa Package" / "The Present Demonstration"), Method,
Results, Discussion, Conclusion. Keep the three pillars roughly balanced in
length. Tables use booktabs; figures live in latex/figures and are referenced
with \\label/\\ref, never hard-coded numbers.`

const SKILL_PREAMBLE =
`You are operating as part of the /write-paper workflow. FIRST read the full skill at ${SKILL} (read it in its entirety) and follow every rule in it — folder layout, the apa7 document class, the PDFs-only sourcing rule, the no-first-names bib rule, APA citation handling, figure conventions, and style. The manuscript LaTeX tree is at ${ROOT}/latex. Sources are pre-converted to text in ${ROOT}/papers_txt (one .txt per PDF in ${ROOT}/papers).

NON-NEGOTIABLE READING RULE: when you Read an assigned paper .txt, load the ENTIRE file in a single Read call (pass a large limit, e.g. limit: 1000000, and no offset). The files were token-budgeted to fit your context in full. Do NOT skim abstracts or read section-by-section — read every assigned paper end to end before writing anything.

SOURCES: cite ONLY the provided papers. Never invent a citation or pull facts from the web/RAG/memory. If you genuinely need a source that was not provided, do NOT cite it: use \\MISSINGCITE{...} in-text and note in your summary that the user must approve and add it (with a link and 1-3 sentences why), per the skill.

BIB RULE: store author given names as INITIALS only (\`Last, F. M.\`), and spell the same author identically across entries (this prevents apacite from injecting first initials into in-text citations).
${BRIEF}`

// ===========================================================================
// SCHEMAS
// ===========================================================================
const BIB_ITEMS = {
  type: 'array',
  items: {
    type: 'object', required: ['bibkey', 'bibtex', 'status'],
    properties: {
      bibkey: { type: 'string' },
      bibtex: { type: 'string', description: 'complete bibtex entry (initials only), or "" if status=existing' },
      status: { type: 'string', enum: ['existing', 'new'] },
    },
  },
}

const PLAN_SCHEMA = {
  type: 'object',
  required: ['introGroups', 'methodsInputs', 'resultsInputs', 'tokenReport', 'summary'],
  properties: {
    introGroups: {
      type: 'array',
      description: 'Each element is one Introduction-writer assignment: the list of absolute .txt (or .tex) paths that writer must read in full. Every group must total < the token budget.',
      items: { type: 'object', required: ['files', 'theme', 'tokens'],
        properties: { files: { type: 'array', items: { type: 'string' } }, theme: { type: 'string' }, tokens: { type: 'number' } } },
    },
    methodsInputs: { type: 'array', items: { type: 'string' } },
    resultsInputs: { type: 'array', items: { type: 'string' } },
    tokenReport: { type: 'array', items: { type: 'object', required: ['file', 'tokens'], properties: { file: { type: 'string' }, tokens: { type: 'number' } } } },
    summary: { type: 'string' },
  },
}

const CONTRIB_SCHEMA = {
  type: 'object',
  required: ['bib_entries', 'contributions', 'discussion_seeds', 'summary'],
  properties: {
    bib_entries: BIB_ITEMS,
    contributions: {
      type: 'array',
      description: 'discrete, citation-grounded, drop-in LaTeX prose blocks for the Introduction',
      items: { type: 'object', required: ['topic', 'section_target', 'prose', 'citations'],
        properties: { topic: { type: 'string' }, section_target: { type: 'string' }, prose: { type: 'string' }, citations: { type: 'array', items: { type: 'string' } } } },
    },
    discussion_seeds: { type: 'array', items: { type: 'string' } },
    summary: { type: 'string' },
  },
}

const SECTION_SCHEMA = {
  type: 'object',
  required: ['bib_entries', 'files_written', 'discussion_seeds', 'summary'],
  properties: {
    bib_entries: BIB_ITEMS,
    files_written: { type: 'array', items: { type: 'string' } },
    figures_used: { type: 'array', items: { type: 'string' } },
    discussion_seeds: { type: 'array', items: { type: 'string' } },
    summary: { type: 'string' },
  },
}

const REPORT_SCHEMA = {
  type: 'object',
  required: ['ok', 'pages', 'undefined_citations', 'open_flags', 'summary'],
  properties: {
    ok: { type: 'boolean' },
    pages: { type: 'number' },
    undefined_citations: { type: 'number' },
    open_flags: { type: 'number' },
    cited_keys: { type: 'array', items: { type: 'string' } },
    summary: { type: 'string' },
  },
}

const VERDICT = {
  type: 'object',
  required: ['bibkey', 'overall', 'read_full_pdf', 'read_full_manuscript', 'citation_sites', 'faithfulness', 'serious_issues', 'minor_issues', 'bib_metadata_issues', 'best_source_flags', 'recommended_fixes', 'summary'],
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
    summary: { type: 'string' },
  },
}

// ===========================================================================
// PHASE 0 — PLAN
// ===========================================================================
phase('Plan')

const plan = await agent(
`${SKILL_PREAMBLE}

TASK — initialize the manuscript and produce the drafting PLAN.

Paper title: ${TITLE}
Short title: ${SHORT || '(derive a <=50-char running head)'}
Working dir: ${ROOT}
Authors: ${A.authors || ''}

Do the following, then return the structured PLAN:

1. INITIALIZE the apa7 LaTeX tree exactly as the skill's "Initializing the LaTeX Tree" section prescribes: create ${ROOT}/latex/sections and ${ROOT}/latex/figures; COPY apa7.cls from the skill's base directory (cp ~/.claude/skills/write-paper/apa7.cls ${ROOT}/latex/apa7.cls); write ${ROOT}/latex/manuscript.tex with the full apa7 preamble (title, shorttitle, authorsnames/affiliations, authornote, the \\shortcites{} placeholder, \\input{sections/abstract}), \\begin{document}\\maketitle, the section \\input calls, \\bibliography{references}, \\end{document}; write a stub sections/abstract.tex (\\abstract{}+\\keywords{}); write an empty references.bib; write coordinator stubs (methods/results/discussion/conclusion carry \\section{...}; introduction.tex carries NO \\section — just commented \\input placeholders). SKIP the skill's git-init step (this latex tree lives inside the package repo). Both authors share affiliation 1: Department of Psychology, University of Alberta, Edmonton, Alberta, Canada.

2. VERIFY PDF->TEXT correspondence: ${ROOT}/papers_txt already exists and should be complete — verify every PDF in ${ROOT}/papers has a .txt and report any gaps (do not reconvert existing files).

3. TOKEN-COUNT every paper in ${ROOT}/papers_txt using the ANTHROPIC TOKENIZER (the API key is in the environment as ANTHROPIC_API_KEY). For each .txt, POST its text to the count_tokens endpoint and record input_tokens — write and run a small Python helper, e.g.:
   import os, glob, json, urllib.request
   def count(text):
       req = urllib.request.Request('https://api.anthropic.com/v1/messages/count_tokens',
           data=json.dumps({'model':'claude-fable-5','messages':[{'role':'user','content':text}]}).encode(),
           headers={'x-api-key':os.environ['ANTHROPIC_API_KEY'],'anthropic-version':'2023-06-01','content-type':'application/json'})
       return json.load(urllib.request.urlopen(req))['input_tokens']
   If the API call fails for any reason, fall back to an estimate of ceil(chars/3.5) tokens and say so in your summary. Also token-count the prior theory-draft files /Users/devon7y/VS_Code/LLM_Factor_Analysis/latex-claude/sections/introduction_*.tex (they are inputs to the Pillar-1 writer).

4. BUILD PILLAR-ALIGNED INTRO GROUPS (each group total < ${TOKEN_BUDGET} tokens; prefer more, smaller groups):
   - Exactly one group is the PILLAR-1 / THEORY group: the construct-validity and philosophy papers (Cronbach_Meehl_1955, Wittgenstein_1953, Borsboom_2008, Borsboom_Etal_2004, Bollen_Lennox_1991, Messick_1995, Clark_Watson_2019, Uher_2025, Stanghellini_Etal_2024, plus the factor-analysis history papers Galton_1884, Spearman_1904, Goldberg_1990, Joreskog_1969 if budget allows) AND the theory-draft introduction_*.tex files listed above. If this exceeds the budget, split into two theory groups.
   - The remaining groups cover PILLAR 2 (language models and psychometrics), grouped by line of work per the PAPER BRIEF (prediction-of-structure; taxonomy/jingle-jangle; clinical; methods-and-pipelines; NLP foundations such as Mikolov, Pennington, Devlin, Vaswani, Reimers, plus tutorials and critiques). Every papers_txt file must be assigned to exactly one intro group UNLESS it is plainly methods-only provenance (see step 5) — in that case it may live only in methodsInputs; note any such decision in the summary.
   - Give each group a theme string that names its pillar, e.g. "P1-theory: ..." or "P2-review: ...".

5. methodsInputs (the Methods writer reads ALL of these in full):
   ${ROOT}/reproduce/reproduce.R, ${ROOT}/reproduce/reproduce.Rout, ${ROOT}/reproduce/README.md, ${ROOT}/DESCRIPTION, ${ROOT}/README.md, ${ROOT}/vignettes/introduction.Rmd, and the method-provenance paper .txts: Horn_1965, Kaiser_1958, vanderMaaten_Hinton_2008, Hubert_Arabie_1985, Bowman_Etal_2015, Christensen_Etal_2023, Golino_Preprint, Garrido_Etal_Preprint, Pokropek_2026, Pellert_Etal_2026, Guenole_Etal_Preprint, Grand_Etal_2022, Wulff_Mata_2025, Wulff_Mata_2026, Wang_Preprint, Jung_Seo_2025, Hommel_Arslan_2025, Goldberg_1990 (all under ${ROOT}/papers_txt/). Verify the total is under the token budget; if not, trim the provenance list and say so.

6. resultsInputs: ${ROOT}/reproduce/reproduce.Rout, ${ROOT}/reproduce/reproduce.R, and every CSV in ${ROOT}/reproduce/output/.

7. PRE-STAGE FIGURES: copy the PDFs from ${ROOT}/reproduce/figures/ into ${ROOT}/latex/figures/ under short stable names (corplot.pdf, corplot_nli.pdf, itemmap.pdf, scree.pdf, loadings.pdf, semantic_vs_human.pdf), and list them with one line each on what they show so the Results writer can reference them (corplot = cosine similarity heatmap grouped by factor; corplot_nli = the SIGNED NLI matrix, which shows the negative relations of reverse-keyed items vividly; itemmap = 2x2 t-SNE/UMAP/PCA/MDS item map; scree = scree with parallel-analysis overlay; loadings = item-by-factor heatmap; semantic_vs_human = item-pair scatter of semantic similarity vs human correlation, r = .46, N = 874,434).

Return the PLAN: introGroups (each with files[], theme, tokens), methodsInputs, resultsInputs, the full tokenReport, and a summary (figures staged, budget decisions, any unassigned papers).`,
  { label: 'plan', phase: 'Plan', model: WRITER, schema: PLAN_SCHEMA })

// ===========================================================================
// PHASE 1 — DRAFT
// ===========================================================================
phase('Draft')

const introGroups = (plan && plan.introGroups) ? plan.introGroups : []
const methodsInputs = (plan && plan.methodsInputs) ? plan.methodsInputs : []
const resultsInputs = (plan && plan.resultsInputs) ? plan.resultsInputs : []

const introThunks = introGroups.map((g, i) => () => agent(
`${SKILL_PREAMBLE}

You are Introduction literature writer ${i + 1} of ${introGroups.length} (theme: ${g.theme}). Read EVERY assigned file IN FULL, then produce citation-grounded material to integrate your papers into the Introduction, serving the pillar your theme names (see the PAPER BRIEF). Do NOT write files — return structured contributions; a later agent assembles the coherent Introduction from all writers.

Your assigned files (read each completely, one Read call each):
${(g.files || []).map(f => '  - ' + f).join('\n')}

Also read the full current manuscript under ${ROOT}/latex/sections so you preserve its framing and reuse existing bib keys. If your group includes the prior theory-draft .tex files, treat them as raw material to condense in fresh words — never copy sentences verbatim, and only keep citations whose PDFs exist in ${ROOT}/papers.

Return: bib_entries (every paper you cite — reuse an existing key if already in references.bib with bibtex "", else a full new entry built from the paper text, INITIALS ONLY); contributions (drop-in LaTeX prose blocks tagged with target subsection + the bibkeys used, respecting method-matching — cite a paper for a claim that uses its method, not mere topic overlap); discussion_seeds; summary (note any provided paper that does not fit the Introduction, and any needed-but-missing source as a \\MISSINGCITE proposal).`,
  { label: 'intro:' + (g.theme || ('g' + i)), phase: 'Draft', model: WRITER, schema: CONTRIB_SCHEMA }))

const methodsThunk = () => agent(
`${SKILL_PREAMBLE}

You are the METHODS writer. Write the Method section of the semanticfa paper at academic depth (enough to reproduce, not exhaustive). Read the assigned inputs IN FULL — the reproduce.R script and reproduce.Rout transcript define exactly what was done; the provenance papers ground each method citation. Then WRITE the methods subsection files under ${ROOT}/latex/sections (methods_*.tex) and ensure ${ROOT}/latex/sections/methods.tex coordinator \\inputs them. Suggested subsections (adapt as needed): Materials (the 50 IPIP items, the bundled data, the human benchmark), Embeddings (models, provenance, loading), The semanticfa Pipeline (similarity encodings, retention, extraction, diagnostics), Analysis Plan (the demonstration stages incl. the semantic-vs-human comparison), Open Practices. State embedding models exactly as the PAPER BRIEF specifies. Software/data citations (@manual/@misc) per the brief. Any overflow detail goes to a supplementary_*.tex subsection with a concise pointer.

Assigned inputs (read each completely):
${methodsInputs.map(f => '  - ' + f).join('\n')}

Return: bib_entries (method/toolbox/materials citations, INITIALS ONLY); files_written; discussion_seeds (methodological points for the Discussion); summary.`,
  { label: 'methods', phase: 'Draft', model: WRITER, schema: SECTION_SCHEMA })

const resultsThunk = () => agent(
`${SKILL_PREAMBLE}

You are the RESULTS writer. Read ALL results inputs IN FULL and write the Results section faithful to the numbers (verify every value against reproduce.Rout / the CSVs; never round away signs; never invent a number). Structure the Results by the package workflow stages named in the PAPER BRIEF, showing REAL R code blocks with their REAL (abridged where long) output taken verbatim from reproduce.Rout, in an apa7-safe verbatim environment. WRITE the results subsection files under ${ROOT}/latex/sections (results_*.tex), include the pre-staged figures from ${ROOT}/latex/figures with \\includegraphics + \\label/\\ref, APA-style stats, and booktabs tables (the encoding-comparison table and the Tucker congruence matrix are required tables), and ensure results.tex coordinator \\inputs them. Long material (e.g. the full semantic loadings table) goes to supplementary_*.tex.

Assigned inputs (read each completely):
${resultsInputs.map(f => '  - ' + f).join('\n')}
Pre-staged figures and notes from the planner:
${plan ? (plan.summary || '') : ''}

Return: bib_entries; files_written; figures_used; discussion_seeds (empirical findings for the Discussion); summary.`,
  { label: 'results', phase: 'Draft', model: WRITER, schema: SECTION_SCHEMA })

const drafts = await parallel([...introThunks, methodsThunk, resultsThunk])
const introContribs = drafts.slice(0, introGroups.length).filter(Boolean)
const methodsOut = drafts[introGroups.length] || null
const resultsOut = drafts[introGroups.length + 1] || null

function collectBib(objs) {
  const out = []
  for (const o of objs) { if (o && Array.isArray(o.bib_entries)) out.push(...o.bib_entries) }
  return out
}
function collectSeeds(objs) {
  const out = []
  for (const o of objs) { if (o && Array.isArray(o.discussion_seeds)) out.push(...o.discussion_seeds) }
  return out
}

// ===========================================================================
// PHASE 1b — ASSEMBLE INTRODUCTION
// ===========================================================================
phase('Assemble-Intro')

const introMaterial = introContribs.map((c, i) =>
  `### Intro writer ${i + 1} (${introGroups[i] ? introGroups[i].theme : ''})\nSUMMARY: ${c.summary}\nCONTRIBUTIONS:\n` +
  (c.contributions || []).map(x => `- [${x.section_target}] (${(x.citations || []).join(', ')})\n${x.prose}`).join('\n\n')
).join('\n\n========\n\n')

const introAssembled = await agent(
`${SKILL_PREAMBLE}

You are the INTRODUCTION ASSEMBLER. Weave the literature writers' contributions below into ONE coherent Introduction with a clear narrative arc: Pillar 1 (theory of analyzing scale language; semantic-behavioral coherence as the new nomological relation) flowing into Pillar 2 (the language-models-and-psychometrics literature, organized by line of work), closing with a subsection introducing the semanticfa package and the present demonstration. Keep the two pillars roughly balanced. WRITE the introduction subsection files under ${ROOT}/latex/sections (introduction_*.tex) and the introduction.tex coordinator (which carries NO \\section heading — apa7 repeats the title). Use \\subsection{...} for each subsection. Reuse the bibkeys the writers established; do not introduce new citations beyond their material (use \\MISSINGCITE{...} if a transition genuinely needs an unprovided source).

Contributions from the ${introContribs.length} literature writers:
${introMaterial}

Read the current ${ROOT}/latex/sections (methods/results) so the Introduction's framing matches the rest. Return: files_written; bib_entries ([]); discussion_seeds ([]); summary (the through-line you built).`,
  { label: 'assemble-intro', phase: 'Assemble-Intro', model: WRITER, schema: SECTION_SCHEMA })

// ===========================================================================
// PHASE 2 — DISCUSSION
// ===========================================================================
phase('Discussion')

const seeds = collectSeeds([...introContribs, methodsOut, resultsOut])
const discussion = await agent(
`${SKILL_PREAMBLE}

You are the DISCUSSION writer — synthesize the Discussion from every prior agent's output and the actual results. Read the FULL current manuscript under ${ROOT}/latex/sections (intro, methods, results) and ${ROOT}/reproduce/reproduce.Rout, then WRITE the discussion subsection files under ${ROOT}/latex/sections (discussion_*.tex) + the discussion.tex coordinator. Summarize the key findings; interpret the semantic-vs-human convergence AND divergence through the semantic-behavioral coherence lens from Pillar 1; lay out use cases (pre-data scale evaluation, item vetting, short forms, cross-scale screening); take limitations seriously (semantic structure is the structure of wording, not of people; model dependence; English-language items; the Uher critique); future directions. DO NOT OVERCLAIM — SFA complements response-based FA. Do NOT write the Conclusion (a later agent does that).

Discussion seeds gathered from the writers (use, merge, extend):
${seeds.map((s, i) => (i + 1) + '. ' + s).join('\n')}

Reuse existing bibkeys; add bib_entries only for a genuinely new provided source you cite. Return: files_written; bib_entries; discussion_seeds ([]); summary.`,
  { label: 'discussion', phase: 'Discussion', model: WRITER, schema: SECTION_SCHEMA })

// ===========================================================================
// PHASE 3 — ABSTRACT + CONCLUSION + coherence pass
// ===========================================================================
phase('Abstract')

const abstractOut = await agent(
`${SKILL_PREAMBLE}

You are the ABSTRACT + CONCLUSION writer and final coherence pass. Read the FULL assembled manuscript under ${ROOT}/latex/sections. Then:
1. Write ${ROOT}/latex/sections/abstract.tex as apa7 \\abstract{150-250 words: motivation, the package, the demonstration design, key findings incl. the semantic-human convergence, conclusion} + \\keywords{...} (preamble file).
2. Write ${ROOT}/latex/sections/conclusion.tex (\\section{Conclusion} + a tight synthesis).
3. Do a global coherence pass: terminology (Semantic Factor Analysis / SFA used consistently; \\texttt{semanticfa} for the package; function names in \\texttt{}), framing, and narrative consistent across all sections; the three pillars in balance; note fixes in your summary. Do not introduce unprovided citations.

Return: files_written; bib_entries ([]); discussion_seeds ([]); summary.`,
  { label: 'abstract+conclusion', phase: 'Abstract', model: WRITER, schema: SECTION_SCHEMA })

// ===========================================================================
// PHASE 4 — ASSEMBLE: merge bib, \shortcites, finalize preamble, compile
// ===========================================================================
phase('Assemble')

const allBib = collectBib([...introContribs, methodsOut, resultsOut, discussion])
const newBib = allBib.filter(b => b && b.status === 'new' && b.bibtex && b.bibtex.trim())
const bibBlock = newBib.map(b => b.bibtex.trim()).join('\n\n')

const assemble = await agent(
`${SKILL_PREAMBLE}

You are the ASSEMBLER. Finalize and compile the manuscript.

1. MERGE BIB: append the new bibtex entries below into ${ROOT}/latex/references.bib, DEDUPING by bibkey (keep one entry per key; if two writers produced different entries for the same paper, keep the more complete one). Enforce the no-first-names rule: convert any given name to initials, and unify any author spelled differently across entries.
NEW BIBTEX ENTRIES:
${bibBlock || '(none returned as new — verify all cited keys resolve)'}

2. FILL the preamble in ${ROOT}/latex/manuscript.tex: \\title{${TITLE}}, \\shorttitle{${SHORT || '...'}}; authors: ${A.authors || ''} (both affiliation 1: Department of Psychology, University of Alberta, Edmonton, Alberta, Canada; correspondence per the PAPER BRIEF).

3. COMPUTE \\shortcites: enumerate every CITED bibkey (grep \\cite* in sections/), and for each whose bib entry has 3+ authors, add it to the \\shortcites{...} list in the preamble. Count authors by splitting the author field on " and " AFTER stripping inner braces.

4. SUPPLEMENTARY: if any supplementary_*.tex files exist, create sections/supplementary.tex (per the skill: \\section{Supplementary Materials} + S-prefix float renumbering + \\inputs) and add \\input{sections/supplementary} AFTER \\bibliography{references} in manuscript.tex. Otherwise leave it out.

5. COMPILE from ${ROOT}/latex: pdflatex -> bibtex -> pdflatex -> pdflatex (interaction=nonstopmode). Do NOT add \\bibliographystyle (apa7 sets it). Fix any errors (verbatim environments inside floats, missing figures, undefined keys). Confirm apa7.cls is present.

6. CHECK: report undefined-citation count, page count, and open \\MISSINGCITE/INCOMPLETE flag count. Render the PDF text and grep for stray in-text initials (a sign of an author-name mismatch) and report any.

Return the structured report (ok, pages, undefined_citations, open_flags, cited_keys, summary).`,
  { label: 'assemble+compile', phase: 'Assemble', model: WRITER, schema: REPORT_SCHEMA })

// ===========================================================================
// PHASE 5 — VALIDATE: one Opus agent per cited reference
// ===========================================================================
phase('Validate')

const citedKeys = (assemble && Array.isArray(assemble.cited_keys)) ? assemble.cited_keys : []

const verdicts = await parallel(citedKeys.map((key) => () => agent(
`Verify citation accuracy for ONE reference in an academic manuscript, following the write-paper skill's "Validating the References" section (read ${SKILL} for the full A-J check definitions).

Manuscript: ${ROOT}/latex/manuscript.tex inputs all section files from ${ROOT}/latex/sections/.
Bib file:   ${ROOT}/latex/references.bib
Reference to verify: bibkey ${key}. Its PDF is in ${ROOT}/papers/ (match by filename) and its text in ${ROOT}/papers_txt/. If NO matching PDF exists and the entry is an @manual/@misc software, model, or dataset citation, verify the bib metadata's plausibility and the in-text usage only (set read_full_pdf=false and say so) — these entries are permitted without PDFs for metadata-only claims.

READ EVERYTHING IN FULL — non-negotiable: read the ENTIRE manuscript (every section .tex) and the ENTIRE reference PDF (or its papers_txt extraction) end to end. Then run checks A-J: (A) locate every citation site; (B) faithfulness by sub-dimension — direction (must match the paper's own headline conclusion), magnitude (every number), operationalization, population, attribution layer; (C) citation-group coherence; (D) quotation/numeric audit; (E) precedence; (F) inference-gap / quantifier mismatch; (G) omitted internal caveats; (H) best-source (review-substitution, recency, author-overlap, repetition-avoidance, construct-substitution, lineage-stretch); (I) bib metadata vs the PDF (authors COUNT + names, year, venue, volume, pages, DOI, entry type; initials-only). Special note: wittgenstein-1953 must be an @book entry cited with a section number (e.g. [§43]); Horn (1965), van der Maaten and Hinton (2008), Hubert and Arabie (1985), and Bowman et al. (2015) are the required primary sources for parallel analysis, t-SNE, ARI, and NLI respectively — flag substitution of secondary sources for these. (J) — n/a, first round.

Be objective: flag only when the source clearly contradicts the manuscript or a clearly better source exists. overall = SERIOUS if direction/attribution/operationalization is wrong or a number is materially wrong; MINOR for loose grouping / wording / best-source / metadata; CORRECT otherwise. Report under 400 words, structured.`,
  { label: 'verify:' + key, phase: 'Validate', model: VALIDATOR, schema: VERDICT })
    .then(v => v || { bibkey: key, overall: 'SERIOUS', read_full_pdf: false, read_full_manuscript: false, citation_sites: [], faithfulness: {}, serious_issues: ['agent returned null'], minor_issues: [], bib_metadata_issues: [], best_source_flags: [], recommended_fixes: [], summary: 'skipped' })))

const V = verdicts.filter(Boolean)
const counts = { CORRECT: 0, MINOR: 0, SERIOUS: 0 }
for (const v of V) { if (counts[v.overall] !== undefined) counts[v.overall]++ }
const serious = V.filter(v => v.overall === 'SERIOUS')

log(`Validation: ${counts.CORRECT} CORRECT, ${counts.MINOR} MINOR, ${counts.SERIOUS} SERIOUS across ${V.length} references`)

return {
  title: TITLE,
  compile: assemble,
  validation: {
    counts,
    serious: serious.map(v => ({ bibkey: v.bibkey, issues: v.serious_issues, fixes: v.recommended_fixes })),
    minor: V.filter(v => v.overall === 'MINOR').map(v => ({ bibkey: v.bibkey, issues: v.minor_issues })),
    verdicts: V,
  },
  note: 'Present the validation synthesis to the user before applying fixes; SERIOUS issues should be fixed immediately, then re-validated.',
}
