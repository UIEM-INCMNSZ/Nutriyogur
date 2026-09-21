DATA    <- "data/cgm"
OUTDIR  <- "results"
FIGDIR  <- "figures"
T1_CSV  <- file.path(OUTDIR, "record_metrics.csv")
T2_WIDE <- file.path(OUTDIR, "metrics_wide_paired.csv")
SENSORY <- file.path("data", "sensory.csv")
BRACKETS <- file.path(OUTDIR, "pairwise_contrasts.csv")
NOTES_MD <- file.path(FIGDIR, "FIGURE_NOTES.md")
POINTSIZE <- 8
FIG_WIDTH <- 7.5

PRODUCTS <- c("A", "B", "C")
PAIRS <- list(c("A", "B"), c("A", "C"), c("B", "C"))
PROD_COLOR <- c(A = "#6A3D9A", B = "#E66100", C = "#1B9E9E")
PROD_NAME <- c(A = "Nutriyogur", B = "Commercial Greek", C = "Non-Greek")
PROD_AXIS <- c(A = "Nutriyogur", B = "Greek", C = "Non-Greek")
INK <- "#1a1a1a"; INK2 <- "#5c5c5c"; GRID <- "#e6e6e6"; SURFACE <- "#ffffff"
CLINICAL <- c(54, 250)
FENCE <- 3.0
CLOCK_BIN_MIN <- 15
LIGHT_HOUR <- 6.0
NOMINAL_STEP_MIN <- 15.0
N_PERM <- 10000L; SEED <- 42
NU <- 5.0; MAXIT <- 25L; TOL <- 0.01; LEVEL <- 0.95
EXCEL_EPOCH <- as.Date("1899-12-30"); EXCEL_TO_POSIX_DAYS <- 25569
FIG <- new.env(parent = emptyenv()); FIG$y <- c(0, 1); FIG$cur <- ""; FIG$notes <- list()

CYCLIC <- c(
  "#6276BA", "#6278BB", "#637ABB", "#647CBC", "#647EBC", "#6580BD",
  "#6683BD", "#6785BE", "#6887BE", "#6989BE", "#6A8BBF", "#6B8DBF",
  "#6C8FBF", "#6E91C0", "#6F93C0", "#7195C0", "#7297C1", "#7499C1",
  "#769BC1", "#779DC2", "#799FC2", "#7BA1C2", "#7DA3C3", "#7FA5C3",
  "#81A6C3", "#84A8C4", "#86AAC4", "#88ACC4", "#8AAEC5", "#8DB0C5",
  "#8FB1C6", "#92B3C6", "#95B5C7", "#97B7C7", "#9AB8C7", "#9BB8C6",
  "#9CB7C4", "#9DB7C3", "#9EB7C2", "#A0B7C1", "#A1B6BF", "#A2B6BE",
  "#A4B6BD", "#A5B6BD", "#A6B5BC", "#A7B5BB", "#A8B5BA", "#AAB4BA",
  "#ABB4B9", "#ACB4B9", "#ADB4B8", "#AEB3B8", "#AFB3B8", "#B0B3B7",
  "#B1B3B7", "#B1B2B7", "#B2B2B7", "#B3B2B7", "#B4B2B7", "#B4B1B7",
  "#B5B1B7", "#B6B1B8", "#B7B1B8", "#B7B0B8", "#B8B0B8", "#B8B0B7",
  "#B8B1B6", "#B8B1B5", "#B8B1B5", "#B9B1B4", "#B9B1B3", "#B9B1B2",
  "#BAB0B1", "#BAB0B0", "#BBB0AF", "#BCB0AE", "#BCB0AD", "#BDB0AC",
  "#BEB0AA", "#BFB0A9", "#C0B0A8", "#C1B0A6", "#C2AFA5", "#C3AFA3",
  "#C4AFA2", "#C5AFA0", "#C6AF9E", "#C8AE9D", "#C9AE9B", "#CBAE9A",
  "#CCAE98", "#CEAD97", "#CEAC94", "#CEA991", "#CDA78E", "#CCA58B",
  "#CCA287", "#CBA085", "#CB9D82", "#CA9B7F", "#CA997C", "#C99679",
  "#C89477", "#C89174", "#C78F72", "#C78C6F", "#C68A6D", "#C6876B",
  "#C58569", "#C48267", "#C48065", "#C37D63", "#C27B62", "#C17960",
  "#C0765F", "#C0745D", "#BF715C", "#BE6F5B", "#BD6C5A", "#BC6A58",
  "#BB6857", "#BA6557", "#B96356", "#B86155", "#B65E54", "#B55C54",
  "#B45A53", "#B35752", "#B15552", "#B05351", "#AF5151", "#AD4E51",
  "#AC4C50", "#AA4A50", "#A94850", "#A74650", "#A54450", "#A44250",
  "#A24050", "#A03E50", "#9F3C50", "#9D3A50", "#9B3850", "#993650",
  "#983651", "#973653", "#963655", "#953656", "#943658", "#933659",
  "#92365B", "#90375C", "#8F375D", "#8E375F", "#8D3760", "#8B3861",
  "#8A3862", "#893863", "#873964", "#863964", "#843A65", "#833A65",
  "#813A66", "#7F3B66", "#7E3B67", "#7C3C67", "#7B3C67", "#793D67",
  "#783D67", "#763D67", "#753E67", "#733E67", "#723F67", "#703F67",
  "#6F4066", "#6E4066", "#6C4166", "#6B4166", "#6A4165", "#684265",
  "#674265", "#664364", "#654364", "#644364", "#634463", "#624463",
  "#614463", "#604563", "#5F4562", "#5E4562", "#5D4662", "#5B4661",
  "#5D4662", "#5E4564", "#5E4565", "#5F4467", "#604468", "#604469",
  "#61436B", "#62436C", "#63436E", "#63426F", "#644271", "#654173",
  "#664175", "#674177", "#674079", "#68407B", "#693F7D", "#693F7F",
  "#6A3F81", "#6A3E83", "#6B3E85", "#6B3E87", "#6B3D89", "#6B3D8B",
  "#6B3D8D", "#6B3D8F", "#6B3D91", "#6A3D93", "#6A3D94", "#693D96",
  "#693D97", "#683D99", "#673D9A", "#663D9B", "#653D9D", "#643D9E",
  "#623E9F", "#613EA0", "#603EA0", "#5E3FA1", "#5D40A2", "#5D42A4",
  "#5E45A6", "#5E47A7", "#5E49A9", "#5E4CAA", "#5E4EAB", "#5E51AD",
  "#5E53AE", "#5F55AF", "#5F58B0", "#5F5AB1", "#5F5DB2", "#5F5FB3",
  "#5F61B4", "#5F64B5", "#6066B6", "#6068B6", "#606AB7", "#606DB8",
  "#616FB9", "#6171B9", "#6173BA", "#6276BA")

hour_col <- function(h) CYCLIC[pmin(pmax(round(h / 24 * 255) + 1L, 1L), 256L)]

HAVE_READXL <- requireNamespace("readxl", quietly = TRUE)

.pack <- function(day, sec, val)
  list(t = (day - EXCEL_TO_POSIX_DAYS) * 86400 + sec,
       date = as.Date(day, origin = EXCEL_EPOCH), hour = sec / 3600, v = val)

read_record_xml <- function(path) {
  d <- file.path(tempdir(), paste0("cgm_", basename(path)))
  unlink(d, recursive = TRUE); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(path, exdir = d)
  sheets <- list.files(file.path(d, "xl", "worksheets"), pattern = "\\.xml$",
                       full.names = TRUE)
  xml <- paste(readLines(sheets[1], warn = FALSE), collapse = "")
  rows <- regmatches(xml, gregexpr("<row[^>]*>.*?</row>", xml))[[1]]
  n <- length(rows)
  ser <- rep(NA_real_, n); val <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    r <- rows[i]
    rn <- suppressWarnings(as.integer(sub('^<row[^>]*\\br="([0-9]+)".*$', "\\1", r)))
    if (is.na(rn) || rn < 4L) next
    cells <- regmatches(r, gregexpr(
      '<c [^>]*r="[A-Z]+[0-9]+"[^>]*>.*?</c>|<c [^>]*r="[A-Z]+[0-9]+"[^>]*/>', r))[[1]]
    for (cc in cells) {
      col <- sub('^<c [^>]*r="([A-Z]+)[0-9]+".*$', "\\1", cc)
      if (!col %in% c("C", "E")) next
      if (grepl('t="inlineStr"', cc, fixed = TRUE)) next
      v <- regmatches(cc, regexpr("<v>[^<]*</v>", cc))
      if (!length(v)) next
      num <- suppressWarnings(as.numeric(sub("<v>([^<]*)</v>", "\\1", v)))
      if (col == "C") ser[i] <- num else val[i] <- num
    }
  }
  keep <- !is.na(ser); ser <- ser[keep]; val <- val[keep]
  day <- floor(ser)
  .pack(day, round((ser - day) * 86400), val)
}

read_record_readxl <- function(path) {
  d <- readxl::read_excel(path, sheet = 1, skip = 3, col_names = FALSE,
                          .name_repair = "minimal")
  ts <- d[[3]]; val <- suppressWarnings(as.numeric(unlist(d[[5]])))
  if (inherits(ts, "POSIXct")) {
    sa <- as.numeric(ts)
    day <- floor(sa / 86400) + EXCEL_TO_POSIX_DAYS
    sec <- round(sa - (day - EXCEL_TO_POSIX_DAYS) * 86400)
  } else {
    ser <- suppressWarnings(as.numeric(unlist(ts)))
    day <- floor(ser); sec <- round((ser - day) * 86400)
  }
  keep <- !is.na(day)
  .pack(day[keep], sec[keep], val[keep])
}

read_record <- function(path)
  if (HAVE_READXL) read_record_readxl(path) else read_record_xml(path)

lsp <- function(t, y) {
  n <- length(y); tt <- t - t[1]
  span <- tt[n] - tt[1]
  nout <- floor(0.5 * n)
  freqs <- seq_len(nout) / span
  m <- mean(y); vr <- var(y); yc <- y - m
  P <- numeric(nout)
  for (j in seq_len(nout)) {
    w <- 2 * pi * freqs[j]
    tau <- 0.5 * atan2(sum(sin(2 * w * tt)), sum(cos(2 * w * tt))) / w
    ang <- w * (tt - tau); cs <- cos(ang); sn <- sin(ang)
    A <- sum(yc * cs); B <- sum(cs * cs); C <- sum(yc * sn); D <- sum(sn * sn)
    P[j] <- (A * A / B + C * C / D) / (2 * vr)
  }
  list(freqs = freqs, P = P)
}

cpt_amoc <- function(x) {
  n <- length(x); cs <- c(0, cumsum(x)); k <- seq_len(n - 1)
  k[which.max(cs[k + 1]^2 / k + (cs[n + 1] - cs[k + 1])^2 / (n - k))]
}

ols <- function(x, y) sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)

spectral <- function(rec) {
  ok <- !is.na(rec$v)
  sp <- lsp(rec$t[ok], rec$v[ok])
  pos <- sp$P > 0
  lx <- log10(sp$freqs[pos]); ly <- log10(sp$P[pos])
  tau <- cpt_amoc(lx)
  list(lx = lx, ly = ly, tau = tau,
       Slope = ols(lx[seq_len(tau - 1)], ly[seq_len(tau - 1)]),
       Slope2 = ols(lx[tau:length(lx)], ly[tau:length(ly)]))
}

betacf <- function(a, b, x) {
  qab <- a + b; qap <- a + 1; qam <- a - 1
  c0 <- 1; d <- 1 - qab * x / qap
  if (abs(d) < 1e-30) d <- 1e-30
  d <- 1 / d; h <- d
  for (m in 1:300) {
    m2 <- 2 * m
    aa <- m * (b - m) * x / ((qam + m2) * (a + m2))
    d <- 1 + aa * d; if (abs(d) < 1e-30) d <- 1e-30
    c0 <- 1 + aa / c0; if (abs(c0) < 1e-30) c0 <- 1e-30
    d <- 1 / d; h <- h * d * c0
    aa <- -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2))
    d <- 1 + aa * d; if (abs(d) < 1e-30) d <- 1e-30
    c0 <- 1 + aa / c0; if (abs(c0) < 1e-30) c0 <- 1e-30
    d <- 1 / d; del <- d * c0; h <- h * del
    if (abs(del - 1) < 3e-16) break
  }
  h
}

betainc <- function(a, b, x) {
  if (x <= 0) return(0); if (x >= 1) return(1)
  lb <- lgamma(a + b) - lgamma(a) - lgamma(b) + a * log(x) + b * log(1 - x)
  if (x < (a + 1) / (a + b + 2)) exp(lb) * betacf(a, b, x) / a
  else 1 - exp(lb) * betacf(b, a, 1 - x) / b
}

f_cdf <- function(x, d1, d2)
  if (x <= 0) 0 else betainc(d1 / 2, d2 / 2, d1 * x / (d1 * x + d2))

qf_ <- function(p, d1, d2) {
  lo <- 0; hi <- 1
  while (f_cdf(hi, d1, d2) < p) hi <- hi * 2
  for (i in 1:200) {
    mid <- 0.5 * (lo + hi)
    if (f_cdf(mid, d1, d2) < p) lo <- mid else hi <- mid
  }
  0.5 * (lo + hi)
}

cov_trob <- function(px, py, nu = NU, maxit = MAXIT, tol = TOL) {
  n <- length(px); p <- 2
  w <- rep(1 + p / nu, n); cx <- mean(px); cy <- mean(py)
  for (it in seq_len(maxit)) {
    w0 <- w; sw <- sum(w)
    s11 <- sum(w * (px - cx)^2) / sw
    s12 <- sum(w * (px - cx) * (py - cy)) / sw
    s22 <- sum(w * (py - cy)^2) / sw
    det <- s11 * s22 - s12 * s12
    u <- px - cx; v <- py - cy
    q <- (s22 * u * u - 2 * s12 * u * v + s11 * v * v) / det
    w <- (nu + p) / (nu + q); sw <- sum(w)
    cx <- sum(w * px) / sw; cy <- sum(w * py) / sw
    if (max(abs(w - w0)) < tol) break
  }
  list(center = c(cx, cy),
       S = c(sum(w * (px - cx)^2) / n,
             sum(w * (px - cx) * (py - cy)) / n,
             sum(w * (py - cy)^2) / n))
}

eig2 <- function(S) {
  a <- S[1]; b <- S[2]; cc <- S[3]
  tr <- a + cc; det <- a * cc - b * b
  disc <- sqrt(max(tr * tr / 4 - det, 0))
  list(l = c(tr / 2 + disc, max(tr / 2 - disc, 0)), ang = 0.5 * atan2(2 * b, a - cc))
}

poincare_points <- function(rec) {
  v <- rec$v; n <- length(v)
  if (n < 3) return(NULL)
  i <- seq_len(n - 2)
  k <- !is.na(v[i]) & !is.na(v[i + 1]) & !is.na(v[i + 2])
  list(x = v[i][k], y = v[i + 1][k], t = rec$t[i][k], hour = rec$hour[i][k])
}

ccm <- function(x, y, l1, l2) {
  n <- length(x); i <- seq_len(n - 2)
  100 * sum(abs((x[i + 1] - x[i]) * (y[i + 2] - y[i]) -
                (x[i + 2] - x[i]) * (y[i + 1] - y[i])) / 2) / (pi * l1 * l2 * (n - 2))
}

poincare_fit <- function(rec) {
  pp <- poincare_points(rec)
  fit <- cov_trob(pp$x, pp$y)
  ev <- eig2(fit$S)
  fac <- 2 * qf_(LEVEL, 2, length(pp$x) - 1)
  sd1 <- sqrt(ev$l[2] * fac); sd2 <- sqrt(ev$l[1] * fac)
  list(pp = pp, center = fit$center, ang = ev$ang, SD1 = sd1, SD2 = sd2,
       ratio = sd1 / sd2, CCM = ccm(pp$x, pp$y, sd1, sd2))
}

lcg_new <- function(seed) { e <- new.env(parent = emptyenv()); e$x <- seed %% 4294967296; e }
lcg_next <- function(e) { e$x <- (1664525 * e$x + 1013904223) %% 4294967296; e$x / 4294967296 }
lcg_shuffle <- function(v, e) {
  n <- length(v)
  if (n > 1) for (i in n:2) {
    j <- floor(lcg_next(e) * i) + 1
    tmp <- v[i]; v[i] <- v[j]; v[j] <- tmp
  }
  v
}

quade_stat <- function(blocks) {
  n <- nrow(blocks); k <- ncol(blocks)
  Q <- rank(apply(blocks, 1, max) - apply(blocks, 1, min))
  S <- t(vapply(seq_len(n), function(i) Q[i] * (rank(blocks[i, ]) - (k + 1) / 2),
                numeric(k)))
  Sj <- colSums(S); A <- sum(S * S); B <- sum(Sj * Sj) / n
  if (A == B) Inf else (n - 1) * B / (A - B)
}

friedman_stat <- function(blocks) {
  n <- nrow(blocks); k <- ncol(blocks)
  R <- colSums(t(apply(blocks, 1, rank)))
  12 / (n * k * (k + 1)) * sum(R * R) - 3 * n * (k + 1)
}

perm_p <- function(blocks, stat = quade_stat, n_perm = N_PERM, seed = SEED) {
  e <- lcg_new(seed)
  obs <- stat(blocks)
  if (!is.finite(obs)) return(0)
  ge <- 0L; sh <- blocks
  for (b in seq_len(n_perm)) {
    for (i in seq_len(nrow(blocks))) sh[i, ] <- lcg_shuffle(blocks[i, ], e)
    if (stat(sh) >= obs - 1e-12) ge <- ge + 1L
  }
  (ge + 1) / (n_perm + 1)
}

signflip_p <- function(d) {
  d <- d[d != 0]; n <- length(d)
  if (n < 3) return(NA_real_)
  r <- rank(abs(d))
  centre <- sum(r) / 2
  obs <- abs(sum(r[d > 0]) - centre)
  kk <- as.integer(round(2 * r))
  counts <- numeric(sum(kk) + 1L); counts[1] <- 1
  for (k in kk) {
    nxt <- numeric(length(counts)); idx <- which(counts != 0)
    nxt[idx] <- nxt[idx] + counts[idx]
    nxt[idx + k] <- nxt[idx + k] + counts[idx]
    counts <- nxt
  }
  sm <- seq_along(counts) - 1
  sum(counts[abs(sm / 2 - centre) >= obs - 1e-12]) / 2^n
}

rank_biserial <- function(a, b) {
  d <- b - a; d <- d[d != 0]
  if (length(d) < 3) return(NA_real_)
  r <- rank(abs(d)); tot <- sum(r)
  if (tot == 0) return(NA_real_)
  (sum(r[d > 0]) - sum(r[d < 0])) / tot
}

stars <- function(p) {
  if (is.null(p) || is.na(p)) return(NA_character_)
  if (p < 0.0001) return("****")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  NA_character_
}

BRACKET_ROWS <- new.env(parent = emptyenv()); BRACKET_ROWS$rows <- list()

paired_blocks <- function(bysp) {
  subs <- sort(names(bysp)[vapply(bysp, function(d)
    all(PRODUCTS %in% names(d)) && all(is.finite(unlist(d[PRODUCTS]))), logical(1))])
  if (!length(subs)) return(list(subs = character(0), blocks = NULL))
  m <- t(vapply(subs, function(s) unlist(bysp[[s]][PRODUCTS]), numeric(3)))
  dimnames(m) <- list(subs, PRODUCTS)
  list(subs = subs, blocks = m)
}

inference <- function(metric, bysp, source) {
  pb <- paired_blocks(bysp)
  out <- list(metric = metric, n = length(pb$subs), quade_p = NA_real_,
              kendall_W = NA_real_, p = c(), rbc = c())
  if (length(pb$subs) < 4) return(out)
  blocks <- pb$blocks
  out$quade_p <- perm_p(blocks)
  out$kendall_W <- friedman_stat(blocks) / (length(pb$subs) * 2)
  for (ab in PAIRS) {
    a <- ab[1]; b <- ab[2]; key <- paste0(a, b)
    d <- blocks[, b] - blocks[, a]
    pv <- signflip_p(d); rb <- rank_biserial(blocks[, a], blocks[, b])
    out$p[[key]] <- pv; out$rbc[[key]] <- rb
    st <- stars(pv)
    BRACKET_ROWS$rows[[length(BRACKET_ROWS$rows) + 1L]] <- list(
      figure_source = source, metric = metric, n = length(pb$subs),
      contrast = paste(a, "vs", b),
      median_difference = formatC(median(d), format = "g", digits = 6),
      rank_biserial = if (is.na(rb)) "" else sprintf("%.4f", rb),
      p_uncorrected = if (is.na(pv)) "" else formatC(pv, format = "g", digits = 6),
      exact = "YES", stars = if (is.na(st)) "ns" else st,
      drawn_on_figure = if (is.na(st)) "no" else "YES",
      quade_p_uncorrected = formatC(out$quade_p, format = "g", digits = 6))
  }
  out
}

panel <- function(x0, x1, y0, y1, mar = c(3.8, 4.2, 2.2, 1.0)) {
  m <- function(y) min(max((y - FIG$y[1]) / diff(FIG$y), 0), 1)
  par(fig = c(x0, x1, m(y0), m(y1)), mar = mar, new = TRUE)
  plot.new()
}

plotbox <- function() {
  f <- par("fig"); p <- par("plt")
  c(f[1] + p[1] * (f[2] - f[1]), f[1] + p[2] * (f[2] - f[1]),
    f[3] + p[3] * (f[4] - f[3]), f[3] + p[4] * (f[4] - f[3]))
}

style <- function(xlim, ylim, xaxt = "s", yaxt = "s", xlab = "", ylab = "",
                  letter = NULL, right = NULL, centre = NULL, ylas = 0,
                  xat = NULL, xlabels = NULL, xcex = 1.0, xcol = INK2, xline = 2.3) {
  plot.window(xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i")
  rect(xlim[1], ylim[1], xlim[2], ylim[2], col = SURFACE, border = NA)
  abline(h = axTicks(2), col = GRID, lwd = 0.6)
  abline(v = if (is.null(xat)) axTicks(1) else xat, col = GRID, lwd = 0.6)
  if (xaxt == "s") {
    if (is.null(xat)) axis(1, col = INK, col.axis = xcol, cex.axis = xcex,
                           lwd = 1.0, tck = -0.018)
    else {
      axis(1, at = xat, labels = FALSE, col = INK, lwd = 1.0, tck = -0.018)
      mtext(xlabels, side = 1, at = xat, line = 0.7, padj = 1, cex = xcex, col = xcol)
    }
  }
  else axis(1, labels = FALSE, col = INK, lwd = 1.0, tck = -0.018)
  if (yaxt == "s") axis(2, col = INK, col.axis = INK2, cex.axis = 1.0,
                        lwd = 1.0, tck = -0.018, las = ylas)
  else axis(2, labels = FALSE, col = INK, lwd = 1.0, tck = -0.018)
  if (nzchar(xlab)) mtext(xlab, 1, line = xline, cex = 1.0, col = INK2)
  if (nzchar(ylab)) mtext(ylab, 2, line = 2.5, cex = 1.0, col = INK)
  if (!is.null(letter)) mtext(letter, 3, line = 0.5, adj = 0, cex = 1.35,
                              font = 2, col = INK)
  if (!is.null(right)) mtext(right, 3, line = 0.5, adj = 1, cex = 1.0, col = INK2)
  if (!is.null(centre)) mtext(centre, 3, line = 0.5, adj = 0.5, cex = 1.15,
                              font = 2, col = INK)
}

qtile <- function(v, p) as.numeric(quantile(v, p, type = 7, names = FALSE))

swarm <- function(vals, n_bins = 34L) {
  if (!length(vals)) return(numeric(0))
  lo <- min(vals); hi <- max(vals); span <- if (hi > lo) hi - lo else 1
  b <- pmin(as.integer((vals - lo) / span * n_bins), n_bins - 1L)
  off <- numeric(length(vals))
  for (bb in unique(b)) {
    idx <- order(vals)[b[order(vals)] == bb]
    k <- length(idx)
    if (k < 2) next
    step <- min(0.44 / (k - 1), 0.075)
    off[idx] <- (seq_len(k) - 1 - (k - 1) / 2) * step
  }
  off
}

beeswarm <- function(vals, max_half = 0.30, bin_w = 3.0) {
  if (!length(vals)) return(numeric(0))
  b <- round(vals / bin_w)
  biggest <- max(table(b))
  spacing <- 2 * max_half / max(biggest - 1, 1)
  off <- numeric(length(vals))
  for (bb in unique(b)) {
    idx <- which(b == bb); k <- length(idx)
    off[idx] <- (seq_len(k) - 1 - (k - 1) / 2) * spacing
  }
  off
}

robust_limits <- function(flat, fence = FENCE) {
  flat <- flat[is.finite(flat)]
  if (length(flat) < 4) return(NULL)
  if (all(abs(flat - round(flat)) < 1e-9) && diff(range(flat)) <= 20) return(NULL)
  q1 <- qtile(flat, .25); q3 <- qtile(flat, .75); w <- fence * (q3 - q1)
  inside <- flat[flat >= q1 - w & flat <= q3 + w]
  if (!length(inside) || length(inside) == length(flat)) return(NULL)
  pad <- 0.08 * diff(range(inside)); if (pad == 0) pad <- 1
  c(min(inside) - pad, max(inside) + pad)
}

footer <- function(notes = character(0), h) FIG$notes[[FIG$cur]] <- unique(notes)


draw_brackets <- function(inf, ylim, positions = 1:3, pad_frac = 0.055,
                          step_frac = 0.085, cols = NULL) {
  span <- diff(ylim)
  drawn <- Filter(function(ab) !is.na(stars(inf$p[[paste0(ab[1], ab[2])]])), PAIRS)
  if (!length(drawn)) return(invisible(NULL))
  drawn <- drawn[order(vapply(drawn, function(ab)
    abs(match(ab[2], PRODUCTS) - match(ab[1], PRODUCTS)), numeric(1)))]
  y <- ylim[2] + span * pad_frac
  for (ab in drawn) {
    x1 <- positions[match(ab[1], PRODUCTS)]; x2 <- positions[match(ab[2], PRODUCTS)]
    h <- span * 0.018
    col <- if (is.null(cols)) INK else cols
    lines(c(x1, x1, x2, x2), c(y, y + h, y + h, y), col = col, lwd = 1.0, xpd = NA)
    text((x1 + x2) / 2, y + h * 1.15, stars(inf$p[[paste0(ab[1], ab[2])]]),
         adj = c(0.5, 0), cex = 1.15, col = col, xpd = NA)
    y <- y + span * step_frac
  }
  invisible(NULL)
}

bracket_head <- function(inf, pad_frac = 0.055, step_frac = 0.085) {
  k <- sum(vapply(PAIRS, function(ab)
    !is.na(stars(inf$p[[paste0(ab[1], ab[2])]])), logical(1)))
  if (!k) return(0)
  pad_frac + step_frac * (k - 1) + 0.05
}

boxpanel <- function(vals, inf, ylabel, letter, subs = character(0),
                     xlab_extra = NULL, centre = NULL, show_right = TRUE) {
  flat <- unlist(vals[PRODUCTS])
  lim <- robust_limits(flat)
  if (is.null(lim)) {
    r <- range(flat); pad <- 0.08 * diff(r); if (pad == 0) pad <- 1
    lim <- c(r[1] - pad, r[2] + pad)
  }
  head <- bracket_head(inf)
  ylim <- c(lim[1], lim[2] + diff(lim) * head)
  q <- inf$quade_p
  style(c(0.45, 3.55), ylim,
        xlab = if (is.null(xlab_extra))
          sprintf("n = %d", inf$n) else xlab_extra,
        ylab = ylabel, letter = letter, centre = centre,
        xat = 1:3, xlabels = PROD_AXIS[PRODUCTS], xcex = 1.0, xcol = INK,
        xline = 2.5, right = NULL)
  out <- list()
  for (i in seq_along(PRODUCTS)) {
    p <- PRODUCTS[i]; v <- vals[[p]]
    if (!length(v)) next
    q1 <- qtile(v, .25); md <- median(v); q3 <- qtile(v, .75)
    iqr <- q3 - q1
    lo <- min(v[v >= q1 - 1.5 * iqr]); hi <- max(v[v <= q3 + 1.5 * iqr])
    rect(i - .275, q1, i + .275, q3, col = adjustcolor(PROD_COLOR[p], .28),
         border = PROD_COLOR[p], lwd = 1.0)
    lines(c(i, i), c(q3, hi), col = INK, lwd = 1.0)
    lines(c(i, i), c(q1, lo), col = INK, lwd = 1.0)
    lines(c(i - .11, i + .11), c(hi, hi), col = INK, lwd = 1.0)
    lines(c(i - .11, i + .11), c(lo, lo), col = INK, lwd = 1.0)
    lines(c(i - .275, i + .275), c(md, md), col = INK, lwd = 1.8)
    ids <- if (length(subs) == length(v)) subs else rep("", length(v))
    off <- swarm(v)
    for (j in seq_along(v)) {
      if (v[j] < lim[1] || v[j] > lim[2]) {
        edge <- if (v[j] < lim[1]) lim[1] else lim[2]
        points(i + off[j], edge, pch = if (v[j] < lim[1]) 25 else 24,
               bg = PROD_COLOR[p], col = PROD_COLOR[p], cex = 0.85, xpd = NA)
        out[[length(out) + 1L]] <- sprintf("%s participant %s, %s = %.4g", letter, ids[j], PROD_NAME[[p]], v[j])
      } else {
        points(i + off[j], v[j], pch = 21, bg = PROD_COLOR[p], col = "white",
               lwd = 0.5, cex = 0.62)
      }
    }
  }
  draw_brackets(inf, lim)
  box(bty = "l", col = INK, lwd = 1.0)
  unlist(out)
}

hour_key <- function() {
  plot.window(c(0, 24), c(0, 1), xaxs = "i", yaxs = "i")
  ras <- as.raster(matrix(CYCLIC, nrow = 1))
  rasterImage(ras, 0, 0, 24, 1, interpolate = TRUE)
  axis(1, at = seq(0, 24, 3), labels = sprintf("%02d", seq(0, 24, 3)),
       col = NA, col.axis = INK2, cex.axis = 1.0, tck = -0.14, lwd = 0)
  mtext("Hour of day", 1, line = 2.1, cex = 1.0, col = INK2)
}

colorbar <- function() {
  plot.window(c(0, 1), c(0, 24), xaxs = "i", yaxs = "i")
  rasterImage(as.raster(matrix(rev(CYCLIC), ncol = 1)), 0, 0, 1, 24,
              interpolate = TRUE)
  axis(4, at = seq(0, 24, 6), labels = sprintf("%02d", seq(0, 24, 6)),
       col = NA, col.axis = INK2, cex.axis = 1.0, tck = -0.25, lwd = 0, las = 1)
}

ellipse_xy <- function(cx, cy, a, b, ang, n = 240) {
  th <- seq(0, 2 * pi, length.out = n)
  list(x = cx + a * cos(th) * cos(ang) - b * sin(th) * sin(ang),
       y = cy + a * cos(th) * sin(ang) + b * sin(th) * cos(ang))
}

read_tab <- function(p) read.csv(p, check.names = FALSE, colClasses = "character",
                                 stringsAsFactors = FALSE)

load_records <- function(dir) {
  files <- sort(list.files(dir, pattern = "\\.xlsx$", full.names = TRUE))
  recs <- list()
  for (f in files) recs[[sub("\\.[^.]*$", "", basename(f))]] <- read_record(f)
  recs
}

wide_rows <- function() {
  w <- read_tab(T2_WIDE)
  w[w$complete_triple == "YES", , drop = FALSE]
}

by_subject_product <- function(w, metric) {
  v <- suppressWarnings(as.numeric(w[[metric]]))
  ok <- is.finite(v)
  out <- list()
  for (i in which(ok)) {
    s <- w$subject[i]
    if (is.null(out[[s]])) out[[s]] <- list()
    out[[s]][[w$product[i]]] <- v[i]
  }
  out
}

sensory_by_subject_product <- function(field) {
  s <- read_tab(SENSORY)
  v <- suppressWarnings(as.numeric(s[[field]]))
  ok <- is.finite(v)
  acc <- list()
  for (i in which(ok)) {
    key <- paste(s$subject[i], s$product[i], sep = "|")
    acc[[key]] <- c(acc[[key]], v[i])
  }
  out <- list()
  for (key in names(acc)) {
    parts <- strsplit(key, "|", fixed = TRUE)[[1]]
    if (is.null(out[[parts[1]]])) out[[parts[1]]] <- list()
    out[[parts[1]]][[parts[2]]] <- median(acc[[key]])
  }
  out
}

vals_of <- function(bysp) {
  pb <- paired_blocks(bysp)
  list(subs = pb$subs,
       vals = if (length(pb$subs))
         setNames(lapply(PRODUCTS, function(p) as.numeric(pb$blocks[, p])), PRODUCTS)
       else setNames(rep(list(numeric(0)), 3), PRODUCTS))
}

open_dev <- function(path, w, h, ext) {
  if (ext == "pdf") grDevices::pdf(paste0(path, ".pdf"), width = w, height = h,
                                   pointsize = POINTSIZE)
  else grDevices::tiff(paste0(path, ".tif"), width = w, height = h, units = "in",
                       res = 300, compression = "lzw", pointsize = POINTSIZE,
                       type = "cairo")
  par(bg = SURFACE, family = "sans", xpd = FALSE, lend = 1, mar = c(0, 0, 0, 0))
  plot.new()
}

clock_bin_medians <- function(recs, rec_prod, n_bins = 1440 / CLOCK_BIN_MIN) {
  out <- list()
  for (p in PRODUCTS) {
    acc <- vector("list", n_bins)
    for (rid in names(recs)) {
      if (!identical(rec_prod[[rid]], p)) next
      r <- recs[[rid]]; ok <- !is.na(r$v)
      b <- as.integer(r$hour[ok] * n_bins / 24) %% n_bins + 1L
      for (k in seq_along(b)) acc[[b[k]]] <- c(acc[[b[k]]], r$v[ok][k])
    }
    keep <- which(lengths(acc) > 0)
    out[[p]] <- data.frame(hour = (keep - 0.5) * 24 / n_bins,
                           med = vapply(acc[keep], median, numeric(1)),
                           n = lengths(acc[keep]))
  }
  out
}

clock_profile <- function(recs, rec_prod, n_bins = 1440 / CLOCK_BIN_MIN) {
  out <- list()
  for (p in PRODUCTS) {
    per <- matrix(NA_real_, 0, n_bins)
    for (rid in names(recs)) {
      if (!identical(rec_prod[[rid]], p)) next
      r <- recs[[rid]]; ok <- !is.na(r$v)
      b <- as.integer(r$hour[ok] * n_bins / 24) %% n_bins + 1L
      row <- rep(NA_real_, n_bins)
      sp <- split(r$v[ok], b)
      row[as.integer(names(sp))] <- vapply(sp, mean, numeric(1))
      per <- rbind(per, row)
    }
    cnt <- colSums(!is.na(per))
    keep <- which(cnt >= 3)
    mu <- apply(per[, keep, drop = FALSE], 2, mean, na.rm = TRUE)
    sd_ <- apply(per[, keep, drop = FALSE], 2, sd, na.rm = TRUE)
    out[[p]] <- data.frame(hour = (keep - 0.5) * 24 / n_bins, mu = mu,
                           se = sd_ / sqrt(cnt[keep]))
  }
  out
}

FIG2_SPECS <- list(
  list("median", "Median interstitial glucose (mg/dL)", "C"),
  list("k", "Excess kurtosis", "D"),
  list("Sk", "Skewness", "E"))

draw_fig2 <- function(recs, rec_prod, w, infs, prof, binned, geom) {
  fh <- 0.048
  rowtop <- 0.905; rowbot <- 0.455
  devH <- par("din")[2] / diff(FIG$y); lin <- par("csi")
  ybot_A <- rowbot + (3.6 * lin + 0.14) / devH

  flat_all <- unlist(lapply(PRODUCTS, function(p) binned[[p]]$med))
  limB <- robust_limits(flat_all)
  a_lo <- min(unlist(lapply(prof, function(d) d$mu - d$se)))
  a_hi <- max(unlist(lapply(prof, function(d) d$mu + d$se)))
  bb <- if (is.null(limB)) range(flat_all) else limB
  pad <- 0.05 * (max(a_hi, bb[2]) - min(a_lo, bb[1]))
  shared <- c(min(a_lo, bb[1]) - pad, max(a_hi, bb[2]) + pad)

  marA <- c(0.6, 4.2, 2.2, 1.0)
  panel(0.004, 0.663, ybot_A, rowtop, mar = marA)
  style(c(0, 24), shared, xaxt = "n",
        ylab = "Interstitial glucose (mg/dL)",
        letter = "A")
  for (lim in c(180, 70)) abline(h = lim, col = INK2, lty = "44", lwd = 0.8)
  for (p in PRODUCTS) {
    d <- prof[[p]]
    polygon(c(d$hour, rev(d$hour)), c(d$mu - d$se, rev(d$mu + d$se)),
            col = adjustcolor(PROD_COLOR[p], .18), border = NA)
  }
  for (p in PRODUCTS) {
    d <- prof[[p]]
    lines(d$hour, d$mu, col = PROD_COLOR[p], lwd = 2.0)
  }
  legend("topleft", legend = PROD_NAME[PRODUCTS], col = PROD_COLOR[PRODUCTS],
         lwd = 2, bty = "n", horiz = TRUE, cex = 1.0, seg.len = 1.4)
  box(bty = "l", col = INK, lwd = 1.0)
  geom$A <- plotbox()

  panel(0.004, 0.663, rowbot, ybot_A, mar = c(3.4, 4.2, 0.2, 1.0))
  hour_key()
  geom$K <- plotbox()

  extra <- (ybot_A - rowbot) * devH / lin
  panel(0.672, 0.996, rowbot, rowtop, mar = marA + c(extra, 0, 0, 0))
  style(c(0.45, 3.55), shared,
        ylab = "Interstitial glucose (mg/dL)", letter = "B",
        xat = 1:3, xlabels = PROD_AXIS[PRODUCTS], xcex = 1.0, xcol = INK)
  for (lim in c(180, 70)) abline(h = lim, col = INK2, lty = "44", lwd = 0.8)
  offnote <- character(0)
  for (i in seq_along(PRODUCTS)) {
    p <- PRODUCTS[i]; d <- binned[[p]]
    off <- beeswarm(d$med, max_half = 0.24, bin_w = 2.0)
    for (j in seq_len(nrow(d))) {
      if (!is.null(limB) && (d$med[j] < limB[1] || d$med[j] > limB[2])) {
        edge <- if (d$med[j] > limB[2]) limB[2] else limB[1]
        points(i + off[j], edge, pch = if (d$med[j] > limB[2]) 24 else 25,
               bg = PROD_COLOR[p], col = PROD_COLOR[p], cex = 0.7)
        offnote <- c(offnote, sprintf("B bin %.2f h, %s = %.4g", d$hour[j], PROD_NAME[[p]], d$med[j]))
      } else {
        points(i + off[j], d$med[j], pch = 21, bg = hour_col(d$hour[j]),
               col = "white", lwd = 0.4, cex = 0.72)
      }
    }
    q1 <- qtile(d$med, .25); q3 <- qtile(d$med, .75); md <- median(d$med)
    rect(i - .34, q1, i + .34, q3, border = INK, lwd = 1.3)
    lines(c(i - .34, i + .34), c(md, md), col = INK, lwd = 2.4)
    text(i, shared[1] + diff(shared) * 0.015, nrow(d), adj = c(0.5, 0),
         cex = 1.0, col = INK2)
  }
  box(bty = "l", col = INK, lwd = 1.0)
  geom$B <- plotbox()

  notes <- character(0)
  for (j in seq_along(FIG2_SPECS)) {
    sp <- FIG2_SPECS[[j]]
    panel((j - 1) / 3 + 0.004, j / 3 - 0.004, fh + 0.02, rowbot - 0.015)
    vv <- vals_of(by_subject_product(w, sp[[1]]))
    notes <- c(notes, boxpanel(vv$vals, infs[[sp[[1]]]], sp[[2]], sp[[3]], vv$subs))
  }
  footer(c(offnote, notes), fh)
  geom
}

REP_METRICS <- c("median", "SD", "SD1", "SD2", "SD1/SD2", "Slope", "CV")

representative <- function(w) {
  m <- vapply(REP_METRICS, function(k) suppressWarnings(as.numeric(w[[k]])),
              numeric(nrow(w)))
  keep <- stats::complete.cases(m)
  m <- m[keep, , drop = FALSE]; subj <- w$subject[keep]
  rk <- apply(m, 2, function(v) (rank(v) - 1) / (length(v) - 1))
  dev <- rowMeans(abs(rk - 0.5))
  agg <- tapply(dev, subj, function(z) if (length(z) == 3) mean(z) else NA_real_)
  names(agg)[which.min(agg)]
}

FIG3_SPECS <- list(
  list("Slope", "PSD slope", "G"),
  list("SD1/SD2", "SD1/SD2", "H"),
  list("CCM", "CCM (%)", "I"))

draw_fig3 <- function(recs, w, subj, infs, geom) {
  fh <- 0.040
  rows <- list(c(0.690, 0.925), c(0.395, 0.660), c(0.070, 0.355))
  cols <- list(c(0.010, 0.312), c(0.320, 0.622), c(0.630, 0.932))
  MAR <- c(3.6, 4.2, 2.2, 0.6)

  sp <- lapply(PRODUCTS, function(p) spectral(recs[[paste0(subj, "_", p)]]))
  names(sp) <- PRODUCTS
  allx <- unlist(lapply(sp, `[[`, "lx")); ally <- unlist(lapply(sp, `[[`, "ly"))
  padx <- 0.02 * diff(range(allx)); pady <- 0.05 * diff(range(ally))
  XL <- range(allx) + c(-padx, padx); YL <- range(ally) + c(-pady, pady)
  for (j in seq_along(PRODUCTS)) {
    p <- PRODUCTS[j]; s <- sp[[p]]
    panel(cols[[j]][1], cols[[j]][2], rows[[1]][1], rows[[1]][2], mar = MAR)
    style(XL, YL, yaxt = if (j == 1) "s" else "n",
          xlab = expression(log[10] * " frequency (Hz)"),
          ylab = if (j == 1) expression(log[10] * " power") else "",
          letter = paste0(c("A", "B", "C")[j], "   ", PROD_NAME[[p]]))
    lines(s$lx, s$ly, col = "#9a9a9a", lwd = 0.5)
    points(s$lx, s$ly, pch = 16, cex = 0.22, col = adjustcolor(INK, .75))
    tau <- s$tau; n <- length(s$lx)
    for (z in list(list(1, tau - 1, 1), list(tau, n, 2))) {
      xs <- s$lx[z[[1]]:z[[2]]]; ys <- s$ly[z[[1]]:z[[2]]]
      if (length(xs) < 3) next
      sl <- ols(xs, ys); ic <- mean(ys) - sl * mean(xs)
      lines(range(xs), ic + sl * range(xs), col = PROD_COLOR[p], lwd = 2.0,
            lty = if (z[[3]] == 1) 1 else "44")
    }
    abline(v = s$lx[tau - 1], col = "#2e7d32", lty = "53", lwd = 1.2)
    if (j == 1)
      legend("bottomleft", inset = 0.005, x.intersp = 0.5,
             legend = c("segment 1", "segment 2", "change point"),
             col = c(PROD_COLOR[p], PROD_COLOR[p], "#2e7d32"),
             lty = c("solid", "44", "53"), lwd = c(2, 2, 1.2), bty = "n",
             cex = 1.0, seg.len = 1.2)
    box(bty = "l", col = INK, lwd = 1.0)
    geom[[paste0("r1c", j)]] <- plotbox()
  }

  for (j in seq_along(PRODUCTS)) {
    p <- PRODUCTS[j]
    pf <- poincare_fit(recs[[paste0(subj, "_", p)]])
    panel(cols[[j]][1], cols[[j]][2], rows[[2]][1], rows[[2]][2], mar = MAR)
    style(c(0, 350), c(0, 350), yaxt = if (j == 1) "s" else "n",
          xlab = "glucose (mg/dL) at t",
          ylab = if (j == 1) "glucose (mg/dL) at t+1" else "",
          letter = paste0(c("D", "E", "F")[j], "   ", PROD_NAME[[p]]))
    for (lim in CLINICAL) {
      abline(v = lim, col = INK2, lty = "44", lwd = 0.8)
      abline(h = lim, col = INK2, lty = "44", lwd = 0.8)
    }
    abline(0, 1, col = INK2, lwd = 0.7)
    points(pf$pp$x, pf$pp$y, pch = 16, cex = 0.30, col = hour_col(pf$pp$hour))
    el <- ellipse_xy(pf$center[1], pf$center[2], pf$SD2, pf$SD1, pf$ang)
    lines(el$x, el$y, col = PROD_COLOR[p], lwd = 1.9)
    box(bty = "l", col = INK, lwd = 1.0)
    geom[[paste0("r2c", j)]] <- plotbox()
  }
  panel(0.940, 0.999, rows[[2]][1], rows[[2]][2], mar = c(3.6, 0.2, 2.2, 2.2))
  colorbar()

  notes <- character(0)
  for (j in seq_along(FIG3_SPECS)) {
    s <- FIG3_SPECS[[j]]
    panel(cols[[j]][1], cols[[j]][2], rows[[3]][1], rows[[3]][2], mar = MAR + c(0.2, 0, 0, 0))
    vv <- vals_of(by_subject_product(w, s[[1]]))
    notes <- c(notes, boxpanel(vv$vals, infs[[s[[1]]]], s[[2]], s[[3]], vv$subs))
    geom[[paste0("r3c", j)]] <- plotbox()
  }

  footer(notes, fh)
  geom
}

TIMES <- list(c("saciedad_t0", "T0"), c("saciedad_t1", "T1"), c("saciedad_t2", "T2"))
TIME_PAIRS <- list(c(1, 2), c(2, 3), c(1, 3))

satiety_blocks <- function() {
  per <- lapply(TIMES, function(z) sensory_by_subject_product(z[1]))
  out <- list()
  for (p in PRODUCTS) {
    subs <- character(0); m <- matrix(NA_real_, 0, 3)
    for (s in sort(names(per[[1]]))) {
      trio <- vapply(per, function(q) {
        v <- q[[s]][[p]]
        if (is.null(v)) NA_real_ else v
      }, numeric(1))
      if (all(is.finite(trio))) { subs <- c(subs, s); m <- rbind(m, trio) }
    }
    out[[p]] <- list(subs = subs, blocks = m)
  }
  out
}

satiety_stats <- function(blocks) {
  if (nrow(blocks) < 4) return(NULL)
  out <- list(n = nrow(blocks), quade_p = perm_p(blocks), p = c())
  for (ij in TIME_PAIRS)
    out$p[[paste0(ij[1], ij[2])]] <- signflip_p(blocks[, ij[2]] - blocks[, ij[1]])
  out
}

draw_fig4 <- function(inf_liking, sb, stats) {
  fh <- 0.075
  notes <- character(0)

  panel(0.006, 0.360, fh + 0.02, 0.885)
  vv <- vals_of(sensory_by_subject_product("agrado"))
  notes <- c(notes, boxpanel(vv$vals, inf_liking, "Liking", "A",
                             vv$subs,
                             xlab_extra = sprintf("n = %d",
                                                  inf_liking$n),
                             centre = "Liking", show_right = FALSE))

  offs <- c(A = -0.26, B = 0.0, C = 0.26)
  ymax <- max(unlist(lapply(PRODUCTS, function(p) sb[[p]]$blocks)))
  ymin <- min(unlist(lapply(PRODUCTS, function(p) sb[[p]]$blocks)))
  nmark <- max(vapply(PRODUCTS, function(p)
    if (is.null(stats[[p]])) 0L else
      sum(vapply(TIME_PAIRS, function(ij)
        !is.na(stars(stats[[p]]$p[[paste0(ij[1], ij[2])]])), logical(1))), integer(1)))
  lo <- ymin - 0.5
  step <- (ymax - lo) * 0.15
  ytop <- ymax + step * (0.45 + 1.15 * max(nmark, 1))

  panel(0.380, 0.800, fh + 0.02, 0.885)
  qtxt <- paste(vapply(PRODUCTS, function(p)
    if (is.null(stats[[p]])) "" else sprintf("%s: Quade p = %.4f", p, stats[[p]]$quade_p),
    character(1)), collapse = "   ")
  style(c(0.55, 3.45), c(lo, ytop),
        xlab = sprintf("Evaluation      n = %d per product",
                       min(vapply(PRODUCTS, function(p) stats[[p]]$n, numeric(1)))),
        ylab = "Satiety", letter = "B", centre = "Satiety",
        xat = 1:3, xlabels = vapply(TIMES, `[`, character(1), 2), xcex = 1.1,
        xcol = INK)
  axis(2, at = seq(ceiling(ymin), floor(ymax)), col = INK, col.axis = INK2,
       cex.axis = 1.0, lwd = 1.0, tck = -0.018)
  for (p in PRODUCTS) {
    B <- sb[[p]]$blocks
    for (ti in 1:3) {
      v <- B[, ti]; x <- ti + offs[[p]]
      q1 <- qtile(v, .25); q3 <- qtile(v, .75); md <- median(v)
      iqr <- q3 - q1
      hi <- max(v[v <= q3 + 1.5 * iqr]); lw <- min(v[v >= q1 - 1.5 * iqr])
      rect(x - .11, q1, x + .11, q3, col = adjustcolor(PROD_COLOR[p], .30),
           border = PROD_COLOR[p], lwd = 1.2)
      lines(c(x, x), c(q3, hi), col = PROD_COLOR[p], lwd = 1.2)
      lines(c(x, x), c(q1, lw), col = PROD_COLOR[p], lwd = 1.2)
      lines(c(x - .11, x + .11), c(md, md), col = INK, lwd = 1.6)
      off <- swarm(v)
      points(x + off * 0.42, v, pch = 21, bg = PROD_COLOR[p], col = "white",
             lwd = 0.4, cex = 0.5)
    }
  }
  y <- ymax + step * 0.45
  for (p in PRODUCTS) {
    st_ <- stats[[p]]
    if (is.null(st_)) next
    drew <- FALSE
    for (ij in TIME_PAIRS[order(vapply(TIME_PAIRS, function(z) z[2] - z[1], numeric(1)))]) {
      mk <- stars(st_$p[[paste0(ij[1], ij[2])]])
      if (is.na(mk)) next
      x1 <- ij[1] + offs[[p]]; x2 <- ij[2] + offs[[p]]; h <- step * 0.13
      lines(c(x1, x1, x2, x2), c(y, y + h, y + h, y), col = PROD_COLOR[p], lwd = 1.4)
      text((x1 + x2) / 2, y + h * 1.05, mk, adj = c(0.5, 0), cex = 1.1,
           col = PROD_COLOR[p], xpd = NA)
      drew <- TRUE
    }
    if (drew) y <- y + step
  }
  box(bty = "l", col = INK, lwd = 1.0)

  panel(0.810, 0.999, fh + 0.02, 0.885, mar = c(0, 0, 0, 0))
  plot.window(c(0, 1), c(0, 1), xaxs = "i", yaxs = "i")
  for (k in seq_along(PRODUCTS)) {
    p <- PRODUCTS[k]
    rect(0.02, 0.62 - (k - 1) * 0.09, 0.14, 0.675 - (k - 1) * 0.09,
         col = adjustcolor(PROD_COLOR[p], .30), border = PROD_COLOR[p], lwd = 1.2)
    text(0.18, 0.648 - (k - 1) * 0.09, PROD_NAME[[p]], adj = c(0, 0.5), cex = 1.0, col = INK)
  }

  footer(notes, fh)
  invisible(notes)
}

CHECKS <- new.env(parent = emptyenv()); CHECKS$rows <- list()
chk <- function(label, ok, detail = "") {
  CHECKS$rows[[length(CHECKS$rows) + 1L]] <- list(label = label, ok = isTRUE(ok),
                                                  detail = detail)
  isTRUE(ok)
}

run_figures <- function(root = ".") {
  owd <- setwd(root); on.exit(setwd(owd), add = TRUE)
  dir.create(FIGDIR, showWarnings = FALSE, recursive = TRUE)

  t1 <- read_tab(T1_CSV)
  w <- wide_rows()
  rec_prod <- setNames(as.list(sub("^.*_", "", t1$record)), t1$record)
  cat("reading", nrow(t1), "records from", DATA, "\n")
  recs <- load_records(DATA)

  worst <- 0; ncmp <- 0L
  for (rid in t1$record) {
    r <- recs[[rid]]
    if (is.null(r)) next
    s <- spectral(r); pf <- poincare_fit(r)
    got <- c(Slope = s$Slope, Slope2 = s$Slope2, SD1 = pf$SD1, SD2 = pf$SD2,
             `SD1/SD2` = pf$ratio, CCM = pf$CCM)
    i <- match(rid, t1$record)
    for (k in names(got)) {
      ref <- as.numeric(t1[[k]][i])
      worst <- max(worst, abs(got[[k]] - ref) / max(abs(ref), 1e-12))
      ncmp <- ncmp + 1L
    }
  }
  ok_dup <- chk(paste("the estimators copied from tier 1 reproduce tier 1's own table",
                      "for every record"),
                worst < 1e-9,
                sprintf("worst relative difference %.2e over %d values", worst, ncmp))
  if (!ok_dup) stop("the copied estimators have drifted from tier 1; nothing drawn")

  metrics <- unique(c("Slope", "SD1/SD2", "CCM",
                      vapply(FIG2_SPECS, `[[`, character(1), 1),
                      vapply(FIG3_SPECS, `[[`, character(1), 1)))
  infs <- list()
  for (m in metrics) infs[[m]] <- inference(m, by_subject_product(w, m), if (m %in% c("median", "k", "Sk")) "Figure 2" else "Figure 3")
  inf_liking <- inference("liking", sensory_by_subject_product("agrado"), "Figure 4")
  sb <- satiety_blocks()
  stats <- lapply(PRODUCTS, function(p) satiety_stats(sb[[p]]$blocks))
  names(stats) <- PRODUCTS

  prof <- clock_profile(recs, rec_prod)
  binned <- clock_bin_medians(recs, rec_prod)
  subj <- representative(w)

  geom <- new.env(parent = emptyenv())
  paths <- c(fig2 = file.path(FIGDIR, "Fig2"),
             fig3 = file.path(FIGDIR, "Fig3"),
             fig4 = file.path(FIGDIR, "Fig4"))
  band <- list(fig2 = c(0.058, 0.915), fig3 = c(0.060, 0.935), fig4 = c(0.085, 0.895))
  height <- c(fig2 = 5.0, fig3 = 7.6, fig4 = 3.0)
  fig_open <- function(id, ext) {
    FIG$y <- band[[id]]; FIG$cur <- id
    open_dev(paths[[id]], FIG_WIDTH, height[[id]], ext)
  }
  g2 <- list(); g3 <- new.env(parent = emptyenv())
  for (ext in c("pdf", "tif")) {
    fig_open("fig2", ext)
    g2 <- draw_fig2(recs, rec_prod, w, infs, prof, binned, new.env(parent = emptyenv()))
    dev.off()
    fig_open("fig3", ext)
    g3 <- draw_fig3(recs, w, subj, infs, new.env(parent = emptyenv())); dev.off()
    fig_open("fig4", ext);  draw_fig4(inf_liking, sb, stats); dev.off()
  }
  FIG$y <- c(0, 1)
  nm <- c(fig2 = "Figure 2", fig3 = "Figure 3", fig4 = "Figure 4")
  md <- c("# Figure notes", "",
          sprintf("Representative participant in Figure 3 A-F: %s.", subj), "",
          "Values outside the plotted y range, drawn as triangles at the axis limit:", "")
  for (id in names(nm)) {
    v <- FIG$notes[[id]]
    md <- c(md, sprintf("- %s: %s", nm[[id]],
                        if (length(v)) paste(v, collapse = "; ") else "none"))
  }
  writeLines(md, NOTES_MD)

  rows <- BRACKET_ROWS$rows
  cols <- names(rows[[1]])
  led <- as.data.frame(do.call(rbind, lapply(rows, function(r)
    vapply(cols, function(c) as.character(r[[c]]), character(1)))),
    stringsAsFactors = FALSE)
  names(led) <- cols
  write.csv(led, BRACKETS, row.names = FALSE, quote = FALSE, na = "")

  made <- sort(list.files(FIGDIR, pattern = "\\.(pdf|tif)$"))
  chk("all three figures written in both formats", length(made) == 6,
      sprintf("%d files", length(made)))
  szp <- file.size(file.path(FIGDIR, made))
  ispdf <- grepl("\\.pdf$", made)
  chk("every figure file is non-trivial in size",
      all(szp[ispdf] > 8000) && all(szp[!ispdf] > 50000),
      sprintf("smallest PDF %.0f kB, smallest TIFF %.0f kB",
              min(szp[ispdf]) / 1000, min(szp[!ispdf]) / 1000))

  dyA <- max(abs(g2$A[3] - g2$B[3]), abs(g2$A[4] - g2$B[4]))
  dxK <- max(abs(g2$A[1] - g2$K[1]), abs(g2$A[2] - g2$K[2]))
  chk(paste("figure 2 panels A and B occupy the same band of the figure, so 180",
            "mg/dL and the floor read straight across"),
      dyA < 1e-9 && dxK < 1e-9,
      sprintf("A/B floor and ceiling differ by %.2e of figure height, key/A left and right edge by %.2e of figure width",
              dyA, dxK))

  spread <- 0
  for (r in 1:3) for (cc in 1:3) {
    a <- get(paste0("r", r, "c", cc), envir = g3); b <- get(paste0("r1c", cc), envir = g3)
    spread <- max(spread, abs(a[1] - b[1]), abs(a[2] - b[2]))
  }
  chk(paste("figure 3 is a true 3x3 grid: every column shares its left and right",
            "edge across all three rows"),
      spread < 1e-9, sprintf("worst column edge offset %.2e of figure width", spread))

  drawn <- led[led$drawn_on_figure == "YES", , drop = FALSE]
  chk("every bracket drawn has p < 0.05 uncorrected",
      all(as.numeric(drawn$p_uncorrected) < 0.05), sprintf("%d drawn", nrow(drawn)))
  chk("all post-hoc p are exact sign-flip, not sampled", all(led$exact == "YES"))

  tsvp <- file.path(OUTDIR, "summary_paired.tsv")
  if (file.exists(tsvp)) {
    tsv <- read.delim(tsvp, check.names = FALSE, colClasses = "character")
    wq <- 0; wp <- 0; nn <- 0L
    for (m in names(infs)) {
      i <- match(m, tsv$metric)
      if (is.na(i)) next
      wq <- max(wq, abs(infs[[m]]$quade_p - as.numeric(tsv$p_omnibus_quade[i])))
      for (ab in PAIRS) {
        col <- paste0("p_", ab[1], "v", ab[2])
        v <- suppressWarnings(as.numeric(tsv[[col]][i]))
        if (is.na(v)) next
        wp <- max(wp, abs(infs[[m]]$p[[paste0(ab[1], ab[2])]] - v))
        nn <- nn + 1L
      }
    }
    chk("the figures' inference reproduces tier 3's summary table",
        wq < 5e-5 && wp < 5e-5,
        sprintf("worst |dp| omnibus %.1e, post-hoc %.1e, over %d contrasts%s",
                wq, wp, nn, ""))
  } else {
    chk("the figures' inference reproduces tier 3's summary table", FALSE,
        "summary table not present")
  }

  chk("every boxplot metric comes from tier 2's table",
      all(metrics %in% names(w)),
      paste(metrics, collapse = ", "))

  L <- c("NUTRIYOGUR TIER 3 (R) — FIGURES 2, 3 AND 4",
         paste("run:", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")), "")
  for (r in CHECKS$rows)
    L <- c(L, paste0("  [", if (r$ok) "PASS" else "FAIL", "] ", r$label,
                     if (nzchar(r$detail)) paste0("   (", r$detail, ")") else ""))
  L <- c(L, "",
         paste("representative participant:", subj),
         sprintf("CGM cohort n = %d; sensory cohort n = %d",
                 infs[[metrics[1]]]$n, inf_liking$n),
         "",
         "EVERY CONTRAST, DRAWN OR NOT  (p uncorrected, exact sign-flip)",
         sprintf("  %-9s %-10s %3s  %-9s %10s  %s", "source", "metric", "n",
                 "contrast", "p_uncorr", "mark"))
  for (i in seq_len(nrow(led)))
    L <- c(L, sprintf("  %-9s %-10s %3s  %-9s %10.5f  %s", led$figure_source[i],
                      led$metric[i], led$n[i], led$contrast[i],
                      as.numeric(led$p_uncorrected[i]), led$stars[i]))
  L <- c(L, "", sprintf("MARKS DRAWN: %d of %d contrasts", nrow(drawn), nrow(led)))
  for (i in seq_len(nrow(drawn)))
    L <- c(L, sprintf("  %-9s %-10s %-9s p=%.5f %-5s (rank-biserial %s)",
                      drawn$figure_source[i], drawn$metric[i], drawn$contrast[i],
                      as.numeric(drawn$p_uncorrected[i]), drawn$stars[i],
                      drawn$rank_biserial[i]))
  L <- c(L, "", "SATIETY, WITHIN EACH PRODUCT ACROSS EVALUATIONS")
  for (p in PRODUCTS) if (!is.null(stats[[p]]))
    L <- c(L, sprintf("  %s  n=%d  Quade p=%.4f   T0-T1 %.4f   T1-T2 %.4f   T0-T2 %.4f",
                      p, stats[[p]]$n, stats[[p]]$quade_p, stats[[p]]$p[["12"]],
                      stats[[p]]$p[["23"]], stats[[p]]$p[["13"]]))
  L <- c(L, "",
         paste("files:", paste(made, collapse = ", ")),
         paste("bracket ledger:", BRACKETS))
  cat(paste(L, collapse = "\n"), "\n")
  sum(vapply(CHECKS$rows, function(r) !r$ok, logical(1)))
}

.a <- commandArgs(trailingOnly = TRUE)
.failed <- run_figures(if (length(.a) && !startsWith(.a[1], "-")) .a[1] else ".")
if (!interactive() && .failed) quit(status = 1)
