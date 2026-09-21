.args <- commandArgs(trailingOnly = TRUE)
if (length(.args)) setwd(.args[1])

RSCRIPT <- file.path(R.home("bin"), "Rscript")
STAGES <- list(
  c("scripts/tier1_timeseries/record_metrics.R", "--selftest"),
  c("scripts/tier1_timeseries/record_metrics.R", "data/cgm",
    "-o", "figures/per_record",
    "-c", "results/record_metrics.csv",
    "-d", "results/daily_metrics.csv"),
  c("scripts/tier2_compile/compile_metrics.R", "results/record_metrics.csv",
    "-o", "results/metrics_long.csv",
    "-w", "results/metrics_wide.csv"),
  c("scripts/tier2_compile/compile_metrics.R", "results/record_metrics.csv",
    "-x", "18009_C",
    "-o", "results/metrics_long_paired.csv",
    "-w", "results/metrics_wide_paired.csv"),
  c("scripts/tier3_figures_tables/summary_table.R",
    "results/metrics_long_paired.csv", "--all",
    "-o", "results/summary_paired.tsv"),
  c("scripts/tier3_figures_tables/table1.R", "data/baseline.csv",
    "results/record_metrics.csv", "18009_C", "results/table1.csv"),
  c("scripts/tier3_figures_tables/hypoglycemia.R", ".", "results", "18009_C"),
  c("scripts/tier3_figures_tables/figures.R"))

dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
for (s in STAGES) {
  cat("\n>", paste(s, collapse = " "), "\n")
  if (system2(RSCRIPT, s, env = "LC_ALL=C") != 0) stop("stage failed: ", s[1])
}
cat("\nresults/ and figures/ written\n")
