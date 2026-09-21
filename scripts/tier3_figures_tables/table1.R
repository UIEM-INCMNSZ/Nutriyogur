.args <- commandArgs(trailingOnly = TRUE)
BASE <- if (length(.args) >= 1) .args[1] else "data/baseline.csv"
T1 <- if (length(.args) >= 2) .args[2] else "results/record_metrics.csv"
EXCLUDE <- if (length(.args) >= 3) strsplit(.args[3], ",")[[1]] else character(0)
OUT <- if (length(.args) >= 4) .args[4] else "results/table1.csv"

say <- function(...) cat(paste0(...), "\n", sep = "")
chk <- function(ok, msg) { say(sprintf("  [%s] %s", if (ok) "PASS" else "FAIL", msg)); ok }

b <- read.csv(BASE, colClasses = c(participant = "character"), na.strings = "",
              stringsAsFactors = FALSE)
m <- read.csv(T1, check.names = FALSE, stringsAsFactors = FALSE)
m <- m[!(m$record %in% EXCLUDE), ]
m$participant <- sub("_[^_]*$", "", m$record)
cgm_mean <- tapply(m$mean_tw, m$participant, mean)
cgm_gmi <- tapply(3.31 + 0.02392 * m$mean_tw, m$participant, mean)

qt <- function(v, p) as.numeric(quantile(v, p, type = 7, names = FALSE))
row_num <- function(v, label, unit, dec = 1, form = "median") {
  v <- v[is.finite(v)]
  val <- if (form == "median")
    sprintf("%.*f [%.*f–%.*f] (%.*f–%.*f)", dec, median(v), dec, qt(v, .25),
            dec, qt(v, .75), dec, min(v), dec, max(v))
  else sprintf("%.*f ± %.*f", dec, mean(v), dec, sd(v))
  data.frame(characteristic = label, unit = unit, n = length(v), value = val,
             stringsAsFactors = FALSE)
}
tab <- rbind(
  row_num(b$age_years, "Age", "years, median [IQR] (range)"),
  row_num(b$age_at_t2d_dx_years, "Age at type 2 diabetes diagnosis", "years, median [IQR] (range)"),
  row_num(b$diabetes_years, "Duration of type 2 diabetes", "years, median [IQR] (range)"),
  row_num(b$bmi_kg_m2, "Body mass index", "kg/m², mean ± SD", 1, "mean"),
  row_num(b$waist_cm, "Waist circumference", "cm, mean ± SD", 1, "mean"),
  row_num(b$hba1c_pct, "Glycated haemoglobin", "%, mean ± SD", 1, "mean"),
  row_num(cgm_mean, "Mean interstitial glucose, CGM", "mg/dL, mean ± SD", 0, "mean"),
  row_num(cgm_gmi, "Glucose management indicator, CGM", "%, mean ± SD", 1, "mean"))

say("NUTRIYOGUR TIER 3 (R) — TABLE 1")
ok <- c(
  chk(setequal(b$participant, names(cgm_mean)),
      sprintf("the baseline table and the CGM records cover the same %d participants", nrow(b))),
  chk(all(abs(m$GMI - (3.31 + 0.02392 * m$mean_tw)) < 1e-9),
      "GMI is the time-weighted mean glucose re-expressed, record by record"),
  chk(nrow(tab) == 8 && all(tab$n > 0), "eight rows, each with its own n"))
for (i in seq_len(nrow(tab)))
  say(sprintf("  %-34s n = %2d  %s", tab$characteristic[i], tab$n[i], tab$value[i]))
write.csv(tab, OUT, row.names = FALSE)
say(OUT)
if (!interactive() && !all(ok)) quit(status = 1)
