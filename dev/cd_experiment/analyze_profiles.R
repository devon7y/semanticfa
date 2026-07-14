r <- readRDS("dev/cd_experiment/cd_results.rds")
base <- c("emb_06B", "emb_4B", "emb_8B", "human_n2500")
for (nm in intersect(base, names(r))) {
  m <- r[[nm]]$median_rmse
  norm <- m / m[1]
  imp <- -diff(m) / head(m, -1) * 100
  cat(sprintf("\n%s\n  norm RMSR : %s\n  rel impr %%: %s\n", nm,
      paste(sprintf("%.3f", norm), collapse = " "),
      paste(sprintf("%5.1f", imp), collapse = " ")))
}
