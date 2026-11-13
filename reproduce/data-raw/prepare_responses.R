## prepare_responses.R -- prepare the response file of a further inventory
## (Section 11d of reproduce.R) from its public data file.
##
##   Rscript data-raw/prepare_responses.R NAME path/to/data.csv [out_dir]
##
## NAME is DASS, ECR, KIMS, HSQ, NIS, RSE, TMA, or BAI. For the first seven,
## data.csv is the data file inside the inventory's zip from
## https://openpsychometrics.org/_rawdata/ (DASS_data_21.02.19.zip,
## ECR-data-1March2018.zip, KIMS.zip, HSQ.zip, NIS_data.zip, RSE.zip,
## TMA.zip). The script keeps the item columns, which these files name Q1,
## Q2, ... or, where the answer is stored beside its response time and
## position, Q1A, Q2A, ..., in that order. For the BAI, data.csv is
## dataset_filtered_n1507_CSV.csv of https://doi.org/10.17632/2f6t9pff4n.8
## (Mendeley Data, Version 8), whose BAI items are the columns QID58_1 to
## QID58_21. The script renames the item columns to the item codes of
## data-raw/items/NAME_items.csv and writes NAME_data.csv to out_dir
## (default data/), with every record kept: reproduce.R removes the records
## with a skipped or out-of-range response.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("usage: prepare_responses.R NAME data.csv [out_dir]")
name <- args[1]
out_dir <- if (length(args) > 2) args[3] else "data"
codes <- read.csv(file.path("data-raw", "items", sprintf("%s_items.csv", name)))$code

## Some free-text answers of the BAI file contain line breaks. A quoted
## answer with a line break is one field, so the lines from the one that opens
## its quote to the one that closes it are kept together. A line break that is
## not quoted splits a field in two, so a record is complete once its lines
## hold the header's number of fields plus one for every such break. The
## lines of each record are joined with a space before the file is parsed.
read_split_csv <- function(path) {
  con <- file(path, encoding = "UTF-8-BOM")
  lines <- readLines(con, warn = FALSE)
  close(con)
  quotes <- lengths(regmatches(lines, gregexpr("\"", lines, fixed = TRUE)))
  open_after <- cumsum(quotes) %% 2 == 1
  grp <- cumsum(c(TRUE, !open_after[-length(lines)]))
  lines <- vapply(split(lines, grp), paste, "", collapse = "\n")
  n <- vapply(lines, function(x) length(scan(text = x, what = "", sep = ",",
                                               quote = "\"", quiet = TRUE,
                                               na.strings = character(0))), 0L)
  width <- n[1]
  rec <- integer(length(lines))
  r <- 0L
  have <- 0L
  for (i in seq_along(lines)) {
    if (n[i] == 0L) {
      rec[i] <- max(r, 1L)  # an empty line adds no field
      next
    }
    if (have == 0L) {
      r <- r + 1L
      have <- n[i]
    } else {
      have <- have + n[i] - 1L
    }
    rec[i] <- r
    if (have >= width) {
      stopifnot(have == width)
      have <- 0L
    }
  }
  stopifnot(have == 0L)
  joined <- vapply(split(lines, rec), paste, "", collapse = " ")
  read.csv(text = joined, header = TRUE, colClasses = "character",
           check.names = FALSE)
}

if (name == "BAI") {
  raw <- read_split_csv(args[2])
  stopifnot(anyDuplicated(raw$ResponseId) == 0, all(grepl("^R_", raw$ResponseId)))
  cols <- paste0("QID58_", seq_along(codes))
  if (!all(cols %in% names(raw)))
    stop("item columns QID58_1..QID58_", length(codes), " not found in ", args[2])
  items <- as.data.frame(lapply(raw[, cols], as.integer))
} else {
  first <- readLines(args[2], n = 1)
  sep <- if (lengths(strsplit(first, "\t")) > lengths(strsplit(first, ","))) "\t" else ","
  raw <- read.table(args[2], header = TRUE, sep = sep, quote = "\"",
                    comment.char = "", check.names = FALSE, fill = TRUE)
  q <- paste0("Q", seq_along(codes))
  cols <- if (all(q %in% names(raw))) q else paste0(q, "A")
  if (!all(cols %in% names(raw)))
    stop("item columns Q1..Q", length(codes), " (or Q1A..) not found in ", args[2])
  items <- raw[, cols]
}
names(items) <- codes
write.csv(items, file.path(out_dir, sprintf("%s_data.csv", name)), row.names = FALSE)
cat(sprintf("%s: %d records, %d items -> %s\n", name, nrow(items), ncol(items),
            file.path(out_dir, sprintf("%s_data.csv", name))))
