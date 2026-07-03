## Script to regenerate big5.rda from the IPIP Big-Five Factor Markers (50 items)
## with precomputed Qwen3-Embedding-8B item embeddings.
##
## The embeddings were generated offline with Qwen/Qwen3-Embedding-8B and stored
## as a NumPy .npz, so this script loads them directly (no model download or GPU
## needed) and packages them with the item metadata. Vectors are rounded to 4
## decimal places before bundling: this shrinks the compressed .rda to ~280 KB
## with no practical loss (the cosine-similarity matrix is unchanged to 5 dp).
## Requires: reticulate + a Python with numpy.

library(reticulate)

items_csv <- read.csv(
  "/Users/devon7y/VS_Code/LLM_Factor_Analysis/scale_items/Big5_items.csv",
  stringsAsFactors = FALSE
)

npz_path <- "/Users/devon7y/VS_Code/LLM_Factor_Analysis/embeddings/Big5FM_items_8B.npz"
np <- import("numpy")
z  <- np$load(npz_path, allow_pickle = TRUE)

emb       <- py_to_r(z[["embeddings"]])           # 50 x 4096 (Qwen3-Embedding-8B)
npz_codes <- as.character(py_to_r(z[["codes"]]))
npz_items <- as.character(py_to_r(z[["items"]]))

## Alignment guard: the .npz must match the CSV item set and order exactly,
## so the embedding rows line up with items/codes/factors/scoring. Only the
## embeddings are taken from the .npz; item/code/factor/scoring metadata comes
## from the CSV (the .npz carries different factor-label wording).
stopifnot(
  identical(npz_codes, items_csv$code),
  identical(npz_items, items_csv$item),
  nrow(emb) == 50L
)

storage.mode(emb) <- "double"
emb <- round(emb, 4)            # ~280 KB compressed; cosine sim identical to 5 dp
rownames(emb) <- items_csv$code

big5 <- list(
  items      = items_csv$item,
  codes      = items_csv$code,
  factors    = items_csv$factor,
  scoring    = as.numeric(items_csv$scoring),
  embeddings = emb
)

save(big5, file = "data/big5.rda", compress = "xz")
