## Script to regenerate big5.rda from the IPIP Big Five 50-item inventory
## Requires: reticulate, Python sentence-transformers, all-MiniLM-L6-v2

library(reticulate)

items_csv <- read.csv(
  "/Users/devon7y/VS_Code/LLM_Factor_Analysis/scale_items/Big5_items.csv",
  stringsAsFactors = FALSE
)

st <- import("sentence_transformers")
encoder <- st$SentenceTransformer("all-MiniLM-L6-v2")
emb <- encoder$encode(items_csv$item, show_progress_bar = FALSE)
emb <- py_to_r(emb)
storage.mode(emb) <- "double"
rownames(emb) <- items_csv$code

big5 <- list(
  items      = items_csv$item,
  codes      = items_csv$code,
  factors    = items_csv$factor,
  scoring    = as.numeric(items_csv$scoring),
  embeddings = emb
)

save(big5, file = "data/big5.rda", compress = "xz")
