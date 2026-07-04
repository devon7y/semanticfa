# Build release manifests for pool assets: sha256, byte size, and 2 GB
# part-splitting for GitHub release assets. Run from the artifacts dir.
LIMIT <- 2 * 1024^3 - 1e6   # stay safely under the 2 GB asset cap

make_manifest <- function(npy, scales = NULL) {
  size <- file.size(npy)
  base <- basename(npy)
  parts <- character(0)
  if (size > LIMIT) {
    n_parts <- ceiling(size / LIMIT)
    con <- file(npy, "rb")
    for (i in seq_len(n_parts)) {
      part <- sprintf("%s.part%d", base, i)
      out <- file(part, "wb")
      remaining <- min(LIMIT, size - (i - 1) * LIMIT)
      while (remaining > 0) {
        chunk <- readBin(con, "raw", n = min(64 * 1024^2, remaining))
        writeBin(chunk, out)
        remaining <- remaining - length(chunk)
      }
      close(out)
      parts <- c(parts, part)
    }
    close(con)
  } else {
    parts <- base
  }
  man <- list(parts = as.list(parts),
              total_bytes = size,
              sha256 = digest::digest(file = npy, algo = "sha256"),
              scales_file = if (!is.null(scales)) basename(scales))
  dest <- sub("\\.npy$", ".manifest.rds", base)
  saveRDS(man, dest)
  message(base, ": ", length(parts), " part(s), ",
          round(size / 1e9, 2), " GB")
}

for (f in list.files(".", pattern = "^pool_.*_fp16\\.npy$")) {
  make_manifest(f)
}
for (f in list.files(".", pattern = "^pool_.*_int8\\.npy$")) {
  make_manifest(f, scales = sub("\\.npy$", ".scales.npy", f))
}
