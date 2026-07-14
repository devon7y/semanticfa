export const meta = {
  name: 'write-intro-semanticfa',
  description: 'Write the semanticfa Introduction (3-pillar guided review, <=10 pp) with Opus: 21 parallel Opus readers each load their assigned papers in full and return citation-grounded contributions, one Opus assembler reads the entire existing manuscript and writes the Introduction subsection files citing every paper, then compile/coverage/trim verification.',
  phases: [
    { title: 'Read',     detail: '21 parallel Opus readers, each loads its assigned papers IN FULL and returns citation-grounded, subsection-tagged contributions + bib entries' },
    { title: 'Assemble', detail: 'one Opus agent reads the entire existing manuscript + all contributions and writes the Introduction subsection files (<=10 pp), citing every paper' },
    { title: 'Verify',   detail: 'Opus: compile, measure intro page span, check every papers_txt paper is cited, check stray initials; report' },
    { title: 'Trim',     detail: 'conditional Opus pass: condense to <=10 intro pages and add any uncited papers, recompile' },
  ],
}

const P = '/Users/devon7y/VS_Code/semanticfa'
const SEC = `${P}/latex/sections`
const TXT = `${P}/papers_txt`
const MODEL = 'opus'

// ---------------------------------------------------------------------------
// The full set of bibkeys already in references.bib (readers REUSE these;
// only joos-1950 and harris-1954 are new and must be built from the text).
// ---------------------------------------------------------------------------
const BIBKEYS = `bollen-lennox-1991, borsboom-2008, borsboom-etal-2004, bowman-etal-2015, buchanan-etal-2001, bunt-etal-2025, casella-etal-2024, christensen-etal-2023, clark-watson-2019, cronbach-meehl-1955, devlin-etal-2019, eberhardt-etal-2025, feraco-toffalini-2025, galton-1884, garrido-etal-2025, goldberg-1990, golino-2026, golino-christensen-2026, grand-etal-2022, grobelny-etal-2025, guenole-etal-2025, gunther-etal-2019, han-etal-2025, hommel-arslan-2025, horn-1965, huang-etal-2025, hubert-arabie-1985, hussain-etal-2024, joreskog-1969, jung-seo-2025, kaiser-1958, kaiser-1960, kambeitz-etal-2025, kmetty-etal-2021, kojima-etal-2026, lara-etal-1992, lim-etal-2025, liu-etal-2025, low-etal-preprint, maharjan-etal-2025, mandera-etal-2017, messick-1995, mikolov-etal-2013, milano-etal-2025a, milano-etal-2025b, muller-2026, openpsychometrics-2018, ormerod-2026, pellert-etal-2026, pennington-etal-2014, peters-etal-2025, pokropek-2026, qwen-2025, ravenda-etal-2025, rcoreteam-2025, reimers-gurevych-2019, revelle-2026, russell-lasalandra-etal-2026, schoenegger-etal-2025, spearman-1904, stanghellini-etal-2024, suarez-alvarez-etal-2026, uher-2025, vandermaaten-hinton-2008, wang-etal-2026, westbury-preprint, westwood-2025, wittgenstein-1953, wulff-mata-2025, wulff-mata-2026, yanitski-westbury-2026, zhang-qiu-2026`

// ---------------------------------------------------------------------------
// Shared framing brief — every agent receives this.
// ---------------------------------------------------------------------------
const FRAMING = `
=== THE PAPER (shared context for every agent) ===
This is a software/methods paper (Behavior Research Methods style) introducing
\\texttt{semanticfa} (v0.1.0, on CRAN), an R package by Devon Yanitski and Chris
Westbury (University of Alberta) for SEMANTIC FACTOR ANALYSIS (SFA): exploratory
factor analysis of the similarity structure of language-model embeddings of
psychometric scale items, requiring NO human response data. Methods, Results,
Discussion, and Conclusion are already written; we are writing the INTRODUCTION
LAST so it is a guided review of the literature leading directly into our paper.

Write the package name as \\texttt{semanticfa} (lower case). Use \\citep/\\citet
(apacite). Author given names in the bib are INITIALS ONLY.

=== THE CENTRAL THESIS (established by the rest of the manuscript; the intro must set it up) ===
A scale's items are a "semantic proposal," written in ordinary language, about a
construct's structure; language-model embeddings make that proposal measurable
before any respondent is recruited. Classical factor analysis recovers the
BEHAVIORAL structure (person x item response covariance); SFA recovers the
SEMANTIC structure (the geometry of item meaning). These are structurally
parallel analyses on different data sources. The agreement between them ---
SEMANTIC-BEHAVIORAL COHERENCE --- is framed as an estimable relation in a
construct's nomological network: convergence is intranetwork validity evidence;
divergence is itself informative (semantic compression of distinct behavioral
dimensions, connotational drift, jingle-jangle, theoretical underspecification,
behavioral covariance whose causes wording does not carry, population effects).
SFA COMPLEMENTS, never replaces, response-based FA (cite the
convergence-with-divergence evidence, e.g. kojima-etal-2026, feraco-toffalini-2025,
zhang-qiu-2026). Honor the realist caveat (borsboom-etal-2004, borsboom-2008):
coherence strengthens evidential webs without certifying the attribute exists.

=== THE INTRODUCTION'S THREE PILLARS AND SUBSECTION FILES ===
The assembler will create these \\subsection files under latex/sections/ (the
introduction.tex coordinator carries NO \\section heading; apa7 repeats the title):

PILLAR 1 -- Theory: what it means to analyze the language of a scale.
  introduction_overview.tex   -- the core question + the thesis in brief.
  introduction_meaning.tex    -- Meaning as use (wittgenstein-1953, cite \\citep[\\S43]{wittgenstein-1953}
        and family resemblance \\citep[\\S\\S66--67]{wittgenstein-1953}) -> meaning-as-use gives no
        quantitative method -> DISTRIBUTIONAL SEMANTICS does (joos-1950, harris-1954: meaning reflected
        in co-occurrence; difference of meaning correlates with difference of distribution) -> modern
        embeddings compress co-occurrence into vector spaces (mikolov-etal-2013, pennington-etal-2014,
        mandera-etal-2017, gunther-etal-2019, westbury-preprint; transformers/contextual: vaswani-etal-2017,
        devlin-etal-2019, reimers-gurevych-2019).
  introduction_constructs.tex -- Constructs are defined by lawful relations, not hidden essences
        (cronbach-meehl-1955 nomological network; messick-1995 content/validity; realism borsboom-etal-2004,
        borsboom-2008). Scale items are an operationalized semantic proposal (reflective vs formative
        bollen-lennox-1991; wording matters clark-watson-2019). THE PARALLEL: distributional theory defines
        word meaning relationally (co-occurrence); nomological theory defines construct meaning relationally
        (relations to behavior/observation/theory/other constructs) -> both relational, not essentialist ->
        scales sit at the intersection because constructs are communicated in language.
  introduction_two_structures.tex -- Two relational structures over the same items: behavioral (person x item
        covariance; statistical lineage galton-1884, spearman-1904, joreskog-1969, goldberg-1990 lexical Big Five;
        retention kaiser-1958, kaiser-1960, horn-1965) vs semantic (embedding similarity). SEMANTIC-BEHAVIORAL
        COHERENCE as a nomological relation; convergence vs informative divergence; why SFA matters (response-free:
        scale development/item screening/construct clarification/taxonomy/jingle-jangle/prediction before data
        collection); early vision of computable semantic factor interpretation (lara-etal-1992). Transition to Pillar 2.

PILLAR 2 -- Review: language models and psychometrics (a guided review organized by what each line of work DOES).
  introduction_prediction.tex -- Embeddings predict item correlations / a-priori factor structure
        (milano-etal-2025a, milano-etal-2025b, casella-etal-2024, hommel-arslan-2025, schoenegger-etal-2025,
        ravenda-etal-2025, feraco-toffalini-2025, guenole-etal-2025). Convergence-with-divergence, no overclaim.
  introduction_taxonomy.tex   -- Taxonomy, jingle-jangle, incommensurability (wulff-mata-2025, wulff-mata-2026,
        stanghellini-etal-2024); clinical / psychopathology structure from embeddings (kambeitz-etal-2025,
        kojima-etal-2026); embeddings recover social/occupational structure (kmetty-etal-2021); semantic norms
        (buchanan-etal-2001); construct-dependence of the relation (zhang-qiu-2026).
  introduction_pipelines.tex  -- Methods & pipelines and the wider applied wave: SQuID centering (pellert-etal-2026),
        CFA/Monte-Carlo calibration with embeddings (pokropek-2026), Unique Variable Analysis (christensen-etal-2023),
        EGA on embeddings (golino-2026, garrido-etal-2025), response-free short forms (jung-seo-2025, wang-etal-2026),
        tutorials/overviews (hussain-etal-2024, low-etal-preprint); generative psychometrics / LLM item generation
        (russell-lasalandra-etal-2026, huang-etal-2025, liu-etal-2025, maharjan-etal-2025, lim-etal-2025,
        bunt-etal-2025, eberhardt-etal-2025, grobelny-etal-2025, westwood-2025); text-based item-parameter modeling
        (peters-etal-2025 difficulty predictable, han-etal-2025 discrimination largely not, ormerod-2026 simulation
        recovers discrimination); the prior "semantic factor analysis" coinage (muller-2026); and the critique that
        statistics is not measurement (uher-2025) -- engage seriously, not dismissively. Method-provenance to be
        previewed where the package's tools are named: parallel analysis (horn-1965), t-SNE (vandermaaten-hinton-2008),
        adjusted Rand index (hubert-arabie-1985), NLI/SNLI basis (bowman-etal-2015), semantic projection (grand-etal-2022).

PILLAR 3 -- transition only (the demonstration itself is already written):
  introduction_package.tex -- Position \\texttt{semanticfa} (yanitski-westbury-2026) as the first general-purpose,
        response-free toolkit unifying these threads in one reproducible R workflow (rcoreteam-2025, revelle-2026,
        golino-christensen-2026); name its five stages; preview the Big-Five demonstration validated against
        874,434 Open-Source Psychometrics respondents (openpsychometrics-2018). Lead directly into the Method.

=== HARD RULES ===
- SOURCES ARE THE PROVIDED PDFs ONLY (text in papers_txt/). Never cite from memory/web. If a needed source
  is not among the papers, use \\MISSINGCITE{...} and note it; do not invent a bib entry.
- REUSE existing bibkeys (full list below). The ONLY new bib entries are joos-1950 and harris-1954.
- EVERY paper in papers_txt must be cited at least once in the Introduction (curated to be relevant; each has a
  natural home above). Match citations to method/claim, not mere topic.
- Concise and direct: the WHOLE Introduction must compile to <= 10 pages. It is a guided review, not exhaustive.

EXISTING BIBKEYS (reuse; do not duplicate): ${BIBKEYS}
`

// ---------------------------------------------------------------------------
// SCHEMAS
// ---------------------------------------------------------------------------
const BIB_ITEMS = {
  type: 'array',
  items: { type: 'object', required: ['bibkey', 'bibtex', 'status'],
    properties: { bibkey: { type: 'string' }, bibtex: { type: 'string' }, status: { type: 'string', enum: ['existing', 'new'] } } },
}
const CONTRIB = {
  type: 'object',
  required: ['bib_entries', 'contributions', 'summary'],
  properties: {
    bib_entries: BIB_ITEMS,
    contributions: { type: 'array',
      items: { type: 'object', required: ['target', 'prose', 'citations'],
        properties: { target: { type: 'string', description: 'target introduction subsection file' },
          prose: { type: 'string', description: 'drop-in LaTeX prose grounded in the read papers' },
          citations: { type: 'array', items: { type: 'string' } } } } },
    missing: { type: 'array', items: { type: 'string' }, description: 'MISSINGCITE proposals if any' },
    summary: { type: 'string' },
  },
}
const SECTION = {
  type: 'object', required: ['files_written', 'bib_entries', 'summary'],
  properties: { files_written: { type: 'array', items: { type: 'string' } }, bib_entries: BIB_ITEMS,
    summary: { type: 'string' } },
}
const REPORT = {
  type: 'object', required: ['ok', 'intro_pages', 'total_pages', 'uncited_papers', 'undefined_citations', 'stray_initials', 'summary'],
  properties: { ok: { type: 'boolean' }, intro_pages: { type: 'number' }, total_pages: { type: 'number' },
    uncited_papers: { type: 'array', items: { type: 'string' } }, undefined_citations: { type: 'number' },
    stray_initials: { type: 'array', items: { type: 'string' } }, summary: { type: 'string' } },
}

// ---------------------------------------------------------------------------
// READER GROUPS (file paths absolute; each <= ~95K tokens). Wittgenstein solo.
// ---------------------------------------------------------------------------
const G = [
  { label: 'wittgenstein', theme: 'P1 meaning-as-use (book; read Part I, the cited sections in full)',
    files: [`${TXT}/Wittgenstein_1953.txt`],
    note: 'This is Philosophical Investigations (a BOOK, ~166K tokens). Read Part I in full (one Read, large limit); it contains the cited sections. Extract the EXACT wording of \\S43 ("the meaning of a word is its use in the language") and \\S\\S66--67 (family resemblance: no single shared feature, only overlapping similarities). Cite as \\citep[\\S43]{wittgenstein-1953} / \\citep[\\S\\S66--67]{wittgenstein-1953}, never the whole volume.' },
  { label: 'distributional', theme: 'P1 distributional semantics + classic embeddings -> introduction_meaning.tex',
    files: [`${TXT}/Joos_1950.txt`, `${P}/dev/intro_sources/Harris_1954_essay.txt`, `${TXT}/Mikolov_Etal_2013.txt`, `${TXT}/Pennington_Etal_2014.txt`],
    note: 'joos-1950 and harris-1954 are NEW entries -- build them from the text (Joos header gives J. Acoust. Soc. Am. 22, 701-707, 1950, doi 10.1121/1.1906674; the Harris file is the 1981 reprint of the canonical Harris, Z. S. (1954), Distributional structure, Word, 10(2-3), 146-162 -- build an @article for the 1954 Word paper and note the reprint provenance). Harris section 2.3 "Meaning as a Function of Distribution" has the key line that difference of meaning correlates with difference of distribution. mikolov/pennington = word embeddings compress co-occurrence into vector geometry.' },
  { label: 'construct-validity', theme: 'P1 nomological network + validity -> introduction_constructs.tex',
    files: [`${TXT}/Cronbach_Meehl_1955.txt`, `${TXT}/Messick_1995.txt`, `${TXT}/Bollen_Lennox_1991.txt`],
    note: 'cronbach-meehl-1955 = constructs defined by lawful relations / nomological network. messick-1995 = content aspect of validity, wording. bollen-lennox-1991 = reflective vs formative indicators.' },
  { label: 'realism-wording', theme: 'P1 realism about constructs + items/wording -> introduction_constructs.tex / two_structures',
    files: [`${TXT}/Borsboom_2008.txt`, `${TXT}/Borsboom_Etal_2004.txt`, `${TXT}/Clark_Watson_2019.txt`],
    note: 'borsboom-etal-2004 / borsboom-2008 = realist theory of measurement and validity (the caveat: coherence is evidential, not existence-certifying; causal symptom networks predict lower coherence). clark-watson-2019 = scale construction, exact wording shapes the measured construct.' },
  { label: 'fa-origins', theme: 'P1/P2 lexical tradition + factor-analysis origins -> introduction_two_structures.tex',
    files: [`${TXT}/Spearman_1904.txt`, `${TXT}/Galton_1884.txt`, `${TXT}/Goldberg_1990.txt`],
    note: 'galton-1884 = lexical hypothesis origin (character from the dictionary). spearman-1904 = correlational psychology / common-factor logic. goldberg-1990 = Big Five robust across lexical sampling; the demonstration scale is the IPIP Big-Five markers.' },
  { label: 'fa-methods', theme: 'P2 factor-analysis method provenance -> introduction_two_structures / package',
    files: [`${TXT}/Joreskog_1969.txt`, `${TXT}/Kaiser_1958.txt`, `${TXT}/Kaiser_1960.txt`, `${TXT}/Horn_1965.txt`, `${TXT}/Lara_Etal_1992.txt`, `${TXT}/Hubert_Arabie_1985.txt`],
    note: 'joreskog-1969 = confirmatory FA. kaiser-1958 = varimax. kaiser-1960 = eigenvalue>1 rule. horn-1965 = parallel analysis (primary source for sfa_parallel). hubert-arabie-1985 = adjusted Rand index (primary source for sfa_congruence ARI). lara-etal-1992 = early vision of computable/semantic factor interpretation.' },
  { label: 'nlp-transformers', theme: 'P1/P2 transformers, BERT, SBERT, NLI -> introduction_meaning / pipelines',
    files: [`${TXT}/Vaswani_Etal_2017.txt`, `${TXT}/Devlin_Etal_2019.txt`, `${TXT}/Reimers_Gurevych_2019.txt`, `${TXT}/Bowman_Etal_2015.txt`],
    note: 'vaswani-etal-2017 = self-attention/transformer. devlin-etal-2019 = BERT contextual embeddings. reimers-gurevych-2019 = Sentence-BERT (sentence embeddings comparable by cosine). bowman-etal-2015 = SNLI corpus (primary source for the NLI basis of sfa_nli_matrix).' },
  { label: 'distributional-psych', theme: 'P1 distributional models in psychology / packaged semantic spaces -> introduction_meaning',
    files: [`${TXT}/Gunther_Etal_2019.txt`, `${TXT}/Mandera_Etal_2017.txt`],
    note: 'gunther-etal-2019 = vector-space models of meaning in psychology (review). mandera-etal-2017 = prediction-based (word2vec) distributional semantics + packaged/precomputed spaces with a query interface (the precedent for shipping a reusable semantic-space tool).' },
  { label: 'embedding-geometry', theme: 'P1/P2 embedding geometry: projection, t-SNE, Westbury -> meaning / package',
    files: [`${TXT}/vanderMaaten_Hinton_2008.txt`, `${TXT}/Westbury_Preprint.txt`, `${TXT}/Grand_Etal_2022.txt`],
    note: 'vandermaaten-hinton-2008 = t-SNE (primary source for the t-SNE option in sfa_itemplot). grand-etal-2022 = semantic projection recovers human knowledge along named dimensions (primary source for sfa_project). westbury-preprint = author group prior work on semantic representation; place where it fits its method.' },
  { label: 'predict-structure', theme: 'P2 embeddings predict factor structure / pseudo-FA -> introduction_prediction.tex',
    files: [`${TXT}/Milano_Etal_2025a.txt`, `${TXT}/Milano_Etal_2025b.txt`, `${TXT}/Casella_Etal_2024.txt`, `${TXT}/Guenole_Etal_Preprint.txt`],
    note: 'milano/casella = embedding similarity recovers a-priori factor structure of personality tests. guenole-etal-2025 = pseudo-factor analysis of embedding similarity matrices; substitutability assumption; Tucker congruence with empirical loadings often short of equivalence (informative divergence).' },
  { label: 'predict-correlations', theme: 'P2 embeddings predict item correlations / CFA convergence -> introduction_prediction.tex',
    files: [`${TXT}/Hommel_Arslan_2025.txt`, `${TXT}/Schoenegger_Etal_2025.txt`, `${TXT}/Ravenda_Etal_2025.txt`, `${TXT}/Feraco_Toffalini_2025.txt`],
    note: 'hommel-arslan-2025 = LMs infer item/scale correlations from text alone (also the polarity-calibrated NLI similarity idea). schoenegger/ravenda = related prediction. feraco-toffalini-2025 = CFA on embedding similarity; semantic fit implies empirical fit but ~half of semantic misfits are false alarms (convergence-with-divergence; do not overclaim).' },
  { label: 'jingle-jangle', theme: 'P2 taxonomy / jingle-jangle / incommensurability -> introduction_taxonomy.tex',
    files: [`${TXT}/Wulff_Mata_2025.txt`, `${TXT}/Wulff_Mata_2025_Supplementary.txt`, `${TXT}/Wulff_Mata_2026.txt`],
    note: 'wulff-mata-2025 = embeddings reveal/address taxonomic incommensurability (the supplementary belongs to the same key wulff-mata-2025, bibtex ""). wulff-mata-2026 = escaping the jingle-jangle jungle with LLMs (primary source for sfa_jinglejangle).' },
  { label: 'clinical', theme: 'P2 clinical / psychopathology structure from embeddings -> introduction_taxonomy.tex',
    files: [`${TXT}/Kambeitz_Etal_2025.txt`, `${TXT}/Kojima_Etal_2026.txt`, `${TXT}/Stanghellini_Etal_2024.txt`],
    note: 'kambeitz/kojima = psychopathology structure recovered from item embeddings; kojima-etal-2026 = near-interchangeability at the general factor but specific-factor divergences of non-semantic origin (the canonical convergence-with-divergence cite). stanghellini-etal-2024 = semantic loadings / cognitive networks for depression-anxiety-stress.' },
  { label: 'pipelines-core', theme: 'P2 core method pipelines (SQuID, CFA-calibration, UVA) -> introduction_pipelines.tex',
    files: [`${TXT}/Pellert_Etal_2026.txt`, `${TXT}/Pokropek_2026.txt`, `${TXT}/Christensen_Etal_2023.txt`],
    note: 'pellert-etal-2026 = SQuID centering (primary source for the squid encoding). pokropek-2026 = CFA with embeddings + Monte-Carlo null calibration of fit (primary source for calibrate=TRUE). christensen-etal-2023 = Unique Variable Analysis (primary source for sfa_redundancy).' },
  { label: 'pipelines-ega-short', theme: 'P2 EGA on embeddings + response-free short forms -> introduction_pipelines.tex',
    files: [`${TXT}/Golino_Preprint.txt`, `${TXT}/Garrido_Etal_Preprint.txt`, `${TXT}/Jung_Seo_2025.txt`, `${TXT}/Wang_Preprint.txt`],
    note: 'golino-2026 = dynamic EGA on LLM embeddings (depth optimization; primary source for sfa_dimselect / EGA retention). garrido-etal-2025 = EGA recovers embedding dimensionality better than component rules. jung-seo-2025 / wang-etal-2026 = response-free short-form construction (primary sources for sfa_simplify).' },
  { label: 'pipelines-overviews', theme: 'P2 tutorials / overviews -> introduction_pipelines.tex',
    files: [`${TXT}/Hussian_Etal_2024.txt`, `${TXT}/Low_Etal_Preprint.txt`],
    note: 'hussain-etal-2024 = tutorial: locally runnable models for transparency/reproducibility; cosine among item embeddings tracks response correlations (more for larger models). low-etal-preprint = overview: language-based assessment rarely faces the validity/reliability evaluation demanded of rating scales; embeddings-in-FA flagged as an open direction.' },
  { label: 'critique-coinage', theme: 'P2 critique (statistics is not measurement) + prior coinage -> introduction_pipelines.tex / package',
    files: [`${TXT}/Uher_2025.txt`, `${TXT}/Muller_2026.txt`],
    note: 'uher-2025 = the critique that statistics is not measurement; engage seriously as a boundary condition on what SFA can claim. muller-2026 = the OTHER independent coinage of "semantic factor analysis" (Word2Vec adjectives weighted by sample means -> PCA -> k-means; the term was coined twice, independently -- by Mueller and by us).' },
  { label: 'item-parameters', theme: 'P2 text-based item-parameter modeling -> introduction_pipelines.tex',
    files: [`${TXT}/Peters_Etal_2025.txt`, `${TXT}/Han_Etal_2025.txt`, `${TXT}/Ormerod_2026.txt`],
    note: 'peters-etal-2025 = item DIFFICULTY predictable from text (review of difficulty modeling). han-etal-2025 = item DISCRIMINATION largely resists text prediction. ormerod-2026 = simulation-based reconstruction of full item characteristic curves recovers discrimination more readily (it is the counter-case; do not bundle it as agreeing that discrimination is unpredictable).' },
  { label: 'generative-psychometrics', theme: 'P2 generative psychometrics / LLM item generation -> introduction_pipelines.tex',
    files: [`${TXT}/Russell-Lasalandra_Etal_2026.txt`, `${TXT}/Huang_Etal_2025.txt`, `${TXT}/Liu_Etal_2025.txt`],
    note: 'russell-lasalandra-etal-2026 = AIGENIE: LLM drafts an item pool, embedded + network-psychometric pruning (EGA/UVA/bootEGA) in silico -- generation-centric; semanticfa is its analysis-centric complement. huang/liu = LLM-based item or scale generation/validation.' },
  { label: 'applications-a', theme: 'P2 recent embedding-psychometrics applications -> introduction_taxonomy / pipelines',
    files: [`${TXT}/Bunt_Etal_2025.txt`, `${TXT}/Eberhardt_Etal_2025.txt`, `${TXT}/Grobelny_Etal_2025.txt`],
    note: 'recent applications of embeddings to psychometric structure/measurement; place each by what it does (read to determine the precise claim before citing).' },
  { label: 'applications-b', theme: 'P2 further applications + semantic norms / social structure -> introduction_taxonomy / pipelines',
    files: [`${TXT}/Lim_Etal_2025.txt`, `${TXT}/Maharjan_Etal_2025.txt`, `${TXT}/Westwood_2025.txt`, `${TXT}/Zhang_Qiu_2026.txt`, `${TXT}/Buchanan_Etal_2001.txt`, `${TXT}/Kmetty_Etal_2021.txt`],
    note: 'zhang-qiu-2026 = the embedding-response relation is construct-dependent (weakest where baseline inter-item cohesion is strongest) -- a divergence-condition cite. kmetty-etal-2021 = word embeddings recover occupational structure/prestige (with connotational divergence from survey standing). buchanan-etal-2001 = semantic feature/normative resources. lim/maharjan/westwood = read to place precisely.' },
]

// ===========================================================================
phase('Read')

const reads = await parallel(G.map((g) => () => agent(
`${FRAMING}

=== YOUR ROLE: Introduction literature reader "${g.label}" (theme: ${g.theme}) ===
Read EVERY assigned paper IN FULL -- one Read call per file with a large limit (e.g. limit 1000000), no offset; do NOT skim. ${g.note}

Assigned files (read each completely):
${g.files.map((f) => '  - ' + f).join('\n')}

Then return citation-grounded material for the Introduction. Do NOT write any files. For each discrete point, give a drop-in LaTeX prose block (2-5 sentences, scholarly APA voice, matching the manuscript's terminology and thesis above), tagged with the target introduction subsection file it belongs to, and the exact bibkeys it uses. Cite a paper for a claim that USES ITS METHOD/finding, never mere topic overlap. Respect the convergence-with-divergence framing; do not overclaim. Keep prose tight (the whole Introduction must fit in 10 pages, so give the assembler dense, already-condensed material, not long paragraphs).

Return:
- contributions: array of { target (subsection filename), prose (LaTeX), citations (bibkeys) }.
- bib_entries: one per paper you cite. status "existing" with bibtex "" if the bibkey is in the provided list; status "new" with a complete @article/@incollection bibtex (INITIALS ONLY) ONLY for joos-1950 and harris-1954.
- missing: any \\MISSINGCITE proposals (a claim that genuinely needs a source not in your papers).
- summary: what you contributed and any placement notes.`,
  { label: 'read:' + g.label, phase: 'Read', model: MODEL, schema: CONTRIB })))

const R = reads.filter(Boolean)
const allContribs = R.flatMap((r, i) => (r.contributions || []).map((c) => ({ ...c, from: G[i] ? G[i].label : '?' })))
const newBib = []
for (const r of R) for (const b of (r.bib_entries || [])) if (b && b.status === 'new' && b.bibtex && b.bibtex.trim()) newBib.push(b)
const missing = R.flatMap((r) => r.missing || [])

// group contributions by target subsection for the assembler
const byTarget = {}
for (const c of allContribs) { (byTarget[c.target] = byTarget[c.target] || []).push(c) }
const material = Object.keys(byTarget).sort().map((t) =>
  `### ${t}\n` + byTarget[t].map((c) => `- (${(c.citations || []).join(', ')}) [${c.from}]\n${c.prose}`).join('\n\n')
).join('\n\n========\n\n')

const newBibBlock = newBib.map((b) => b.bibtex.trim()).join('\n\n')
log(`Readers done: ${R.length}/${G.length} returned; ${allContribs.length} contributions; ${newBib.length} new bib entries; ${missing.length} MISSINGCITE proposals`)

// ===========================================================================
phase('Assemble')

const assemble = await agent(
`${FRAMING}

=== YOUR ROLE: INTRODUCTION ASSEMBLER ===
FIRST, load the FULL existing manuscript into your context: read EVERY file under ${SEC}/ that is NOT an introduction file -- abstract.tex, methods_*.tex, results_*.tex, discussion_*.tex, conclusion*.tex, supplementary*.tex, and the coordinators methods.tex/results.tex/discussion.tex/conclusion.tex -- so the Introduction's voice, terminology, and claims match the rest exactly and flow directly into the Method. Also read ${P}/latex/references.bib to confirm exact bibkeys.

Then WRITE the Introduction as the eight subsection files named in the framing (introduction_overview, introduction_meaning, introduction_constructs, introduction_two_structures, introduction_prediction, introduction_taxonomy, introduction_pipelines, introduction_package -- each a \\subsection{...} with a neutral parallel title), and OVERWRITE the coordinator ${SEC}/introduction.tex so it \\inputs them in order (keep its leading comment; NO \\section heading). Weave the readers' contributions below into ONE coherent guided review with a clear through-line: Pillar 1 (theory: meaning-as-use -> distributional semantics -> embeddings; constructs as nomological relations; items as semantic proposals; the relational parallel; two structures; semantic-behavioral coherence) flowing into Pillar 2 (the LM-and-psychometrics literature organized by what each line of work does) and closing on Pillar 3 (positioning semanticfa and previewing the demonstration, leading into the Method).

REQUIREMENTS:
- CITE EVERY PAPER: every bibkey in the provided list that corresponds to a papers_txt source must appear at least once (the readers covered all of them; ensure none is dropped in synthesis). Match each citation to the claim that uses its method/finding.
- Concise and direct. The ENTIRE Introduction must compile to <= 10 pages (double-spaced apa7). Prefer dense, well-cited sentences over expansive paragraphs; this is a guided review, not an exhaustive survey.
- Use \\citep[\\S43]{wittgenstein-1953} and \\citep[\\S\\S66--67]{wittgenstein-1953} for Wittgenstein; never the whole volume.
- Do not introduce citations beyond the readers' material except a \\MISSINGCITE{...} where a transition genuinely needs an unprovided source. Reuse existing bibkeys; the only new entries are joos-1950 and harris-1954.
- APPEND the two new bib entries to ${P}/latex/references.bib if not already present (initials only):
${newBibBlock || '(none returned -- build joos-1950 and harris-1254 from the framing notes if the meaning subsection needs them)'}
- Then add \\input{sections/introduction_overview} ... in ${SEC}/introduction.tex (the coordinator) and ensure ${P}/latex/manuscript.tex already \\input{sections/introduction} (it does -- do not change manuscript.tex).

Reader contributions, grouped by target subsection:
${material}

${missing.length ? 'Open MISSINGCITE proposals from readers (resolve or carry as \\MISSINGCITE):\n' + missing.map((m, i) => (i + 1) + '. ' + m).join('\n') : ''}

After writing, COMPILE from ${P}/latex (pdflatex -> bibtex -> pdflatex -> pdflatex, nonstopmode) and fix any errors you introduced. Return files_written, bib_entries (the new ones you added), and summary (the through-line you built + the compiled total page count + where Method now starts).`,
  { label: 'assemble', phase: 'Assemble', model: MODEL, effort: 'high', schema: SECTION })

// ===========================================================================
phase('Verify')

const verify = await agent(
`Verify the newly written Introduction of the semanticfa manuscript at ${P}/latex.

1. COMPILE from ${P}/latex: pdflatex -> bibtex -> pdflatex -> pdflatex (nonstopmode). Report 0/n LaTeX errors and undefined-citation count.
2. INTRO PAGE SPAN: the Introduction is the body text from the start of the document body (after the title/abstract) up to the "\\section{Method}" heading (rendered "Method"). Using pdftotext, find the PDF page where the Introduction begins (first body page) and the page where "Method" begins; intro_pages = methodPage - introStartPage (+1). Report intro_pages and total_pages. FLAG ok=false if intro_pages > 10.
3. COVERAGE: every paper in ${TXT} must be cited. Build the list of cited bibkeys: grep -roh '\\\\cite[a-z]*\\(\\[[^]]*\\]\\)\\{0,2\\}{[^}]*}' ${SEC}/ | sed 's/.*{//; s/}//' | tr ',' '\\n' | sed 's/^ *//; s/ *\\$//' | sort -u. Map each papers_txt filename to its bibkey (the mapping is the obvious lowercase-hyphen form; Wulff_Mata_2025_Supplementary -> wulff-mata-2025; Harris_1954 -> harris-1954; Joos_1950 -> joos-1950). Report uncited_papers = any papers_txt source whose bibkey is NOT cited anywhere in the manuscript sections.
4. STRAY INITIALS: render the PDF text and grep for apacite-injected in-text initials (e.g. "[A-Z]\\. [A-Z][a-z]+" inside citations) that signal an author-name mismatch; report any. Also confirm joos-1950 and harris-1954 resolve in the bibliography (no "[??]").
5. Confirm exactly the intended \\MISSINGCITE flags (ideally zero) remain: grep -rn MISSINGCITE ${SEC}/.

Return ok, intro_pages, total_pages, uncited_papers, undefined_citations, stray_initials, summary.`,
  { label: 'verify', phase: 'Verify', model: MODEL, schema: REPORT })

// ===========================================================================
phase('Trim')

let trim = null
if (verify && (verify.intro_pages > 10 || (verify.uncited_papers && verify.uncited_papers.length) || verify.undefined_citations > 0 || (verify.stray_initials && verify.stray_initials.length))) {
  trim = await agent(
`Finalize the semanticfa Introduction at ${P}/latex/sections (introduction_*.tex). The verifier reported: intro_pages=${verify.intro_pages} (target <=10), uncited papers=${JSON.stringify(verify.uncited_papers || [])}, undefined_citations=${verify.undefined_citations}, stray_initials=${JSON.stringify(verify.stray_initials || [])}.

Do the following with MINIMAL, surgical edits to the introduction_*.tex files only:
1. If intro_pages > 10, CONDENSE the Introduction prose to fit <= 10 pages WITHOUT dropping any citation (merge sentences, cut redundancy and signposting, tighten citation framing; never delete a bibkey -- a citation may move but must remain). Keep every distinct claim and the through-line.
2. For each uncited paper, add a tight, correctly-placed sentence citing it where its method/finding fits (do not force it; find its natural home using the framing notes). Every papers_txt paper must end up cited.
3. Fix any undefined citation (correct the bibkey) and any stray-initials author mismatch (unify the author's initials in references.bib).
Then recompile (pdflatex -> bibtex -> pdflatex -> pdflatex) and report the final intro page span, total pages, remaining uncited papers (should be none), undefined citations (0), and stray initials (none).

Return ok, intro_pages, total_pages, uncited_papers, undefined_citations, stray_initials, summary.`,
    { label: 'trim', phase: 'Trim', model: MODEL, effort: 'high', schema: REPORT })
}

return {
  readers: { returned: R.length, of: G.length, contributions: allContribs.length, newBib: newBib.map((b) => b.bibkey), missing },
  assemble,
  verify,
  trim,
}
