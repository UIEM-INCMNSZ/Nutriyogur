.args  <- commandArgs(trailingOnly = TRUE)
REPO   <- if (length(.args) >= 1) .args[1] else "."
OUTDIR <- if (length(.args) >= 2) .args[2] else "results"
EXCLUDE <- if (length(.args) >= 3) strsplit(.args[3], ",")[[1]] else character(0)

T1 <- file.path(REPO, "scripts", "tier1_timeseries", "record_metrics.R")
for (e in parse(T1)) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      !identical(e[[2]], as.name(".a"))) eval(e, globalenv())
}
stopifnot(exists("read_record"), exists("episodes"))

files <- sort(list.files(file.path(REPO, "data/cgm"), "\\.xlsx$", full.names = TRUE))
rows <- lapply(files, function(f) {
  rec <- read_record(f)
  v <- rec$v[!is.na(rec$v)]
  l1  <- episodes(rec, 70, above = FALSE, min_min = 14.999)
  l2  <- episodes(rec, 54, above = FALSE, min_min = 14.999)
  data.frame(record = sub("\\.xlsx$", "", basename(f)),
             n_readings = length(v), min_glucose = min(v),
             n_below54 = sum(v < 54), n_at_floor40 = sum(v <= 40),
             level1_events = length(l1), level2_events = length(l2),
             longest_below54_min = if (length(l2)) max(l2) else 0,
             prolonged_events = sum(l2 >= 120), stringsAsFactors = FALSE)
})
R <- do.call(rbind, rows)
R$subject <- sub("_[^_]*$", "", R$record)
R$product <- sub("^.*_", "", R$record)
U <- R[!(R$record %in% EXCLUDE), ]

L <- character(0)
say <- function(...) { s <- paste0(...); cat(s, "\n", sep = ""); L <<- c(L, s) }
chk <- function(ok, msg) { say(sprintf("  [%s] %s", if (ok) "PASS" else "FAIL", msg)); ok }

say("NUTRIYOGUR — HYPOGLYCAEMIA FROM THE CGM RECORDS")
say(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
say("")
ok <- c(
  chk(all(R$n_readings > 0), sprintf("%d records read, every one with readings", nrow(R))),
  chk(nrow(U) == nrow(R) - length(EXCLUDE),
      sprintf("%d usable records from %d participants after excluding %s",
              nrow(U), length(unique(U$subject)),
              if (length(EXCLUDE)) paste(EXCLUDE, collapse = ", ") else "none")),
  chk(all(U$level2_events <= U$level1_events),
      "every level 2 event lies inside a level 1 event"))

t1 <- read.csv(file.path(REPO, "results", "record_metrics.csv"),
               check.names = FALSE, stringsAsFactors = FALSE)
m <- merge(U, t1[, c("record", "span_days", "ep_hypo54_per_day")], by = "record")
re <- vapply(m$record, function(r) {
  rec <- read_record(file.path(REPO, "data/cgm", paste0(r, ".xlsx")))
  length(episodes(rec, 54, above = FALSE)) }, numeric(1))
ok <- c(ok, chk(isTRUE(all.equal(unname(re / m$span_days), m$ep_hypo54_per_day, tolerance = 1e-12)),
  sprintf("the episode function reproduces Tier 1's ep_hypo54_per_day on all %d records", nrow(m))))

by_prod <- function(col, f) tapply(U[[col]], U$product, f)
s_l2 <- tapply(U$level2_events > 0, U$subject, any)
s_pr <- tapply(U$prolonged_events > 0, U$subject, any)
say("")
say(sprintf("COHORT, %d records / %d participants", nrow(U), length(unique(U$subject))))
say(sprintf("  lowest reading                          %d mg/dL", min(U$min_glucose)))
say(sprintf("  readings at the 40 mg/dL sensor floor   %d, in %d records",
            sum(U$n_at_floor40), sum(U$n_at_floor40 > 0)))
say(sprintf("  participants with a level 2 event       %d of %d", sum(s_l2), length(s_l2)))
say(sprintf("  records with a level 2 event            %d of %d", sum(U$level2_events > 0), nrow(U)))
say(sprintf("  level 2 events, total                   %d", sum(U$level2_events)))
say(sprintf("  longest continuous time < 54 mg/dL      %.0f min (%s)",
            max(U$longest_below54_min), U$record[which.max(U$longest_below54_min)]))
say(sprintf("  prolonged events (< 54 for >= 120 min)  %d, in %d participants",
            sum(U$prolonged_events), sum(s_pr)))
say("")
say("BY PRODUCT")
for (p in c("A", "B", "C")) {
  k <- U$product == p
  say(sprintf("  %s  records %2d  with level 2 event %2d  level 2 events %2d  longest < 54 %4.0f min  prolonged %d",
              p, sum(k), sum(U$level2_events[k] > 0), sum(U$level2_events[k]),
              max(U$longest_below54_min[k]), sum(U$prolonged_events[k])))
}

dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)
write.csv(R, file.path(OUTDIR, "hypoglycemia.csv"), row.names = FALSE)
say("")
say(file.path(OUTDIR, "hypoglycemia.csv"))
if (!all(ok)) stop("a check failed")
