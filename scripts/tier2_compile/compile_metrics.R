PRODUCTS <- c("A", "B", "C")
KEYS <- c("record", "n", "n_pairs", "n_freq", "span_days", "tau")

compile_metrics <- function(csv_in, out = "results/metrics_long.csv",
                           wide = NULL, exclude = character(0)) {
  rows <- read.csv(csv_in, check.names = FALSE, colClasses = "character",
                   stringsAsFactors = FALSE)
  cols <- names(rows)
  metrics <- setdiff(cols, KEYS)

  keep <- !(rows$record %in% exclude)
  parts <- regmatches(rows$record, regexpr("_[^_]*$", rows$record))
  prod <- sub("^_", "", parts)
  subj <- substr(rows$record, 1, nchar(rows$record) - nchar(parts))
  keep <- keep & (prod %in% PRODUCTS)
  rows <- rows[keep, , drop = FALSE]
  subj <- subj[keep]
  prod <- prod[keep]

  have <- split(prod, subj)
  complete <- names(have)[vapply(have, function(p)
    all(PRODUCTS %in% p), logical(1))]

  ord <- order(subj, prod)
  rows <- rows[ord, , drop = FALSE]
  subj <- subj[ord]
  prod <- prod[ord]

  num_ok <- function(x) {
    v <- suppressWarnings(as.numeric(x))
    !is.na(v) & is.finite(v)
  }

  long <- data.frame(
    subject = rep(subj, each = length(metrics)),
    product = rep(prod, each = length(metrics)),
    record = rep(rows$record, each = length(metrics)),
    complete_triple = rep(ifelse(subj %in% complete, "YES", "NO"),
                          each = length(metrics)),
    n_readings = rep(rows$n, each = length(metrics)),
    n_pairs = rep(rows$n_pairs, each = length(metrics)),
    span_days = rep(rows$span_days, each = length(metrics)),
    metric = rep(metrics, times = nrow(rows)),
    value = as.vector(t(as.matrix(rows[, metrics, drop = FALSE]))),
    stringsAsFactors = FALSE, check.names = FALSE)
  long$value[!num_ok(long$value)] <- ""

  dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
  write.csv(long, out, row.names = FALSE, quote = FALSE, na = "")

  if (!is.null(wide)) {
    w <- data.frame(subject = subj, product = prod, record = rows$record,
                    complete_triple = ifelse(subj %in% complete, "YES", "NO"),
                    stringsAsFactors = FALSE, check.names = FALSE)
    w <- cbind(w, rows[, setdiff(cols, "record"), drop = FALSE])
    dir.create(dirname(wide), showWarnings = FALSE, recursive = TRUE)
    write.csv(w, wide, row.names = FALSE, quote = FALSE, na = "")
  }

  cat(sprintf("records %d   subjects %d   complete triples %d\n",
              nrow(rows), length(have), length(complete)))
  cat(sprintf("metrics %d   rows %d\n", length(metrics), nrow(long)))
  cat(paste0(out, if (!is.null(wide)) paste0("   ", wide) else "", "\n"))
  invisible(long)
}

.parse_args <- function() {
  a <- commandArgs(trailingOnly = TRUE)
  get1 <- function(flag, default) {
    i <- match(flag, a)
    if (is.na(i) || i == length(a)) default else a[i + 1]
  }
  getn <- function(flag) {
    i <- match(flag, a)
    if (is.na(i)) return(character(0))
    j <- i + 1
    out <- character(0)
    while (j <= length(a) && !startsWith(a[j], "-")) { out <- c(out, a[j]); j <- j + 1 }
    out
  }
  pos <- a[!startsWith(a, "-")]
  pos <- setdiff(pos, c(get1("-o", ""), get1("-w", ""), getn("-x")))
  list(csv = if (length(pos)) pos[1] else "results/record_metrics.csv",
       out = get1("-o", "results/metrics_long.csv"),
       wide = if (is.na(match("-w", a))) NULL else get1("-w", NULL),
       exclude = getn("-x"))
}

.a <- .parse_args()
compile_metrics(.a$csv, .a$out, .a$wide, .a$exclude)
