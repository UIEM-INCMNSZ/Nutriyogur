PRODUCTS <- c("A", "B", "C")
PAIRS <- list(c("A", "B"), c("A", "C"), c("B", "C"))
N_PERM <- 10000L
SEED <- 42
ORDER <- c("mean", "median", "SD", "CV", "TIR", "TBR", "TBR54", "TAR", "TAR250",
           "GMI", "Sk", "k", "alpha", "SD1", "SD2", "SD1/SD2", "CCM", "Slope",
           "Slope2", "Break", "Peak")

lcg_new <- function(seed) {
  e <- new.env(parent = emptyenv())
  e$x <- seed %% 4294967296
  e
}
lcg_next <- function(e) {
  e$x <- (1664525 * e$x + 1013904223) %% 4294967296
  e$x / 4294967296
}
lcg_shuffle <- function(v, e) {
  n <- length(v)
  if (n > 1) for (i in n:2) {
    j <- floor(lcg_next(e) * i) + 1
    tmp <- v[i]; v[i] <- v[j]; v[j] <- tmp
  }
  v
}

qtile <- function(v, p) as.numeric(quantile(v, p, type = 7, names = FALSE))

quade_stat <- function(blocks) {
  n <- nrow(blocks); k <- ncol(blocks)
  Q <- rank(apply(blocks, 1, max) - apply(blocks, 1, min))
  S <- t(vapply(seq_len(n), function(i)
    Q[i] * (rank(blocks[i, ]) - (k + 1) / 2), numeric(k)))
  Sj <- colSums(S)
  A <- sum(S * S)
  B <- sum(Sj * Sj) / n
  if (A == B) Inf else (n - 1) * B / (A - B)
}

friedman_stat <- function(blocks) {
  n <- nrow(blocks); k <- ncol(blocks)
  R <- colSums(t(apply(blocks, 1, rank)))
  12 / (n * k * (k + 1)) * sum(R * R) - 3 * n * (k + 1)
}

perm_p <- function(blocks, stat, n_perm = N_PERM, seed = SEED) {
  e <- lcg_new(seed)
  obs <- stat(blocks)
  if (!is.finite(obs)) return(0)
  ge <- 0L
  sh <- blocks
  for (b in seq_len(n_perm)) {
    for (i in seq_len(nrow(blocks))) sh[i, ] <- lcg_shuffle(blocks[i, ], e)
    if (stat(sh) >= obs - 1e-12) ge <- ge + 1L
  }
  (ge + 1) / (n_perm + 1)
}

signflip_p <- function(d) {
  d <- d[d != 0]
  n <- length(d)
  if (n < 3) return(NA_real_)
  r <- rank(abs(d))
  wplus <- sum(r[d > 0])
  centre <- sum(r) / 2
  obs <- abs(wplus - centre)
  kk <- as.integer(round(2 * r))
  counts <- numeric(sum(kk) + 1L)
  counts[1] <- 1
  for (k in kk) {
    nxt <- numeric(length(counts))
    idx <- which(counts != 0)
    nxt[idx] <- nxt[idx] + counts[idx]
    nxt[idx + k] <- nxt[idx + k] + counts[idx]
    counts <- nxt
  }
  sm <- (seq_along(counts) - 1)
  sum(counts[abs(sm / 2 - centre) >= obs - 1e-12]) / 2^n
}

rank_biserial <- function(a, b) {
  d <- b - a
  d <- d[d != 0]
  if (length(d) < 3) return(NA_real_)
  r <- rank(abs(d))
  tot <- sum(r)
  if (tot == 0) return(NA_real_)
  (sum(r[d > 0]) - sum(r[d < 0])) / tot
}

fmt <- function(x) {
  if (is.na(x)) return("")
  a <- abs(x)
  if (a >= 100) return(sprintf("%.0f", x))
  if (a >= 10)  return(sprintf("%.1f", x))
  if (a >= 1)   return(sprintf("%.2f", x))
  sprintf("%.3g", x)
}

summarise_metrics <- function(csv_in, out = "results/summary_paired.tsv",
                             metrics = NULL, all = FALSE) {
  raw <- read.csv(csv_in, check.names = FALSE, colClasses = "character",
                  stringsAsFactors = FALSE)
  raw <- raw[raw$complete_triple == "YES" & raw$value != "", , drop = FALSE]
  raw$value <- as.numeric(raw$value)

  by <- split(raw, raw$metric)
  wanted <- if (!is.null(metrics)) metrics else if (all) sort(names(by)) else
    ORDER[ORDER %in% names(by)]

  rows <- list()
  for (m in wanted) {
    d <- by[[m]]
    if (is.null(d)) next
    tab <- tapply(d$value, list(d$subject, d$product), function(z) z[1])
    if (!all(PRODUCTS %in% colnames(tab))) next
    tab <- tab[, PRODUCTS, drop = FALSE]
    ok <- stats::complete.cases(tab)
    subs <- sort(rownames(tab)[ok])
    if (length(subs) < 4) next
    blocks <- tab[subs, , drop = FALSE]
    storage.mode(blocks) <- "double"

    row <- list(metric = m, n = length(subs))
    for (p in PRODUCTS) {
      v <- blocks[, p]
      row[[paste0(p, "_mean_SD")]] <- paste0(fmt(mean(v)), " ± ", fmt(sd(v)))
      row[[paste0(p, "_median_IQR")]] <- paste0(
        fmt(median(v)), " [", fmt(qtile(v, .25)), "–", fmt(qtile(v, .75)), "]")
    }
    row$p_omnibus_quade <- sprintf("%.4f", perm_p(blocks, quade_stat))
    row$kendall_W <- sprintf("%.3f", friedman_stat(blocks) / (length(subs) * 2))
    for (ab in PAIRS) {
      a <- ab[1]; b <- ab[2]
      pv <- signflip_p(blocks[, b] - blocks[, a])
      row[[paste0("p_", a, "v", b)]] <- if (is.na(pv)) "" else sprintf("%.4f", pv)
      rb <- rank_biserial(blocks[, a], blocks[, b])
      row[[paste0("rbc_", a, "v", b)]] <- if (is.na(rb)) "" else sprintf("%+.3f", rb)
    }
    rows[[length(rows) + 1L]] <- row
  }

  cols <- c("metric", "n")
  for (p in PRODUCTS) cols <- c(cols, paste0(p, "_mean_SD"), paste0(p, "_median_IQR"))
  cols <- c(cols, "p_omnibus_quade", "kendall_W")
  for (ab in PAIRS) cols <- c(cols, paste0("p_", ab[1], "v", ab[2]),
                              paste0("rbc_", ab[1], "v", ab[2]))

  out_df <- as.data.frame(do.call(rbind, lapply(rows, function(r)
    vapply(cols, function(c) as.character(r[[c]]), character(1)))),
    stringsAsFactors = FALSE)
  names(out_df) <- cols

  dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
  con <- file(out, "wb")
  writeLines(c(paste(cols, collapse = "\t"),
               apply(out_df, 1, paste, collapse = "\t")), con, sep = "\n")
  close(con)

  wm <- max(nchar(out_df$metric))
  cat(sprintf(paste0("%-", wm, "s %3s "), "metric", "n"),
      paste(sprintf("%16s", paste(PRODUCTS, "mean±SD")), collapse = " "),
      sprintf(" %8s %7s %7s %7s\n", "omnibus", "AvB", "AvC", "BvC"), sep = "")
  for (i in seq_len(nrow(out_df))) {
    r <- out_df[i, ]
    cat(sprintf(paste0("%-", wm, "s %3s "), r$metric, r$n),
        paste(sprintf("%16s", c(r$A_mean_SD, r$B_mean_SD, r$C_mean_SD)),
              collapse = " "),
        sprintf(" %8s %7s %7s %7s\n", r$p_omnibus_quade, r$p_AvB, r$p_AvC, r$p_BvC),
        sep = "")
  }
  cat(out, "\n")
  invisible(out_df)
}

.parse_args <- function() {
  a <- commandArgs(trailingOnly = TRUE)
  get1 <- function(flag, default) {
    i <- match(flag, a)
    if (is.na(i) || i == length(a)) default else a[i + 1]
  }
  getn <- function(flag) {
    i <- match(flag, a)
    if (is.na(i)) return(NULL)
    j <- i + 1; out <- character(0)
    while (j <= length(a) && !startsWith(a[j], "-")) { out <- c(out, a[j]); j <- j + 1 }
    out
  }
  pos <- setdiff(a[!startsWith(a, "-")], c(get1("-o", ""), getn("-m")))
  list(csv = if (length(pos)) pos[1] else "results/metrics_long_paired.csv",
       out = get1("-o", "results/summary_paired.tsv"),
       metrics = getn("-m"),
       all = "--all" %in% a)
}

.a <- .parse_args()
summarise_metrics(.a$csv, .a$out, .a$metrics, .a$all)
