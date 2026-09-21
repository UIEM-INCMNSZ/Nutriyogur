NU <- 5.0; MAXIT <- 25L; TOL <- 0.01
LEVEL <- 0.95
MAX_GAP_MIN <- 60.0
THR <- c(54.0, 70.0, 180.0, 250.0)
EXCEL_EPOCH <- as.Date("1899-12-30")
LIGHT_HOUR <- 6.0

HAVE_READXL <- requireNamespace("readxl", quietly = TRUE)
.READER_CHECKED <- new.env(parent = emptyenv()); .READER_CHECKED$done <- FALSE

EXCEL_TO_POSIX_DAYS <- 25569

.pack <- function(day, sec, val) {
  list(t = (day - EXCEL_TO_POSIX_DAYS) * 86400 + sec,
       date = as.Date(day, origin = EXCEL_EPOCH),
       hour = sec / 3600,
       v = val)
}

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
    cells <- regmatches(r, gregexpr('<c [^>]*r="[A-Z]+[0-9]+"[^>]*>.*?</c>|<c [^>]*r="[A-Z]+[0-9]+"[^>]*/>',
                                    r))[[1]]
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
  keep <- !is.na(ser)
  ser <- ser[keep]; val <- val[keep]
  day <- floor(ser)
  .pack(day, round((ser - day) * 86400), val)
}

read_record_readxl <- function(path) {
  d <- readxl::read_excel(path, sheet = 1, skip = 3, col_names = FALSE,
                          .name_repair = "minimal")
  if (ncol(d) < 5) stop("unexpected sheet shape in ", basename(path))
  ts <- d[[3]]
  val <- suppressWarnings(as.numeric(unlist(d[[5]])))
  if (inherits(ts, "POSIXct")) {
    sec_all <- as.numeric(ts)
    day <- floor(sec_all / 86400) + EXCEL_TO_POSIX_DAYS
    sec <- round(sec_all - (day - EXCEL_TO_POSIX_DAYS) * 86400)
  } else {
    ser <- suppressWarnings(as.numeric(unlist(ts)))
    day <- floor(ser); sec <- round((ser - day) * 86400)
  }
  keep <- !is.na(day)
  .pack(day[keep], sec[keep], val[keep])
}

read_record <- function(path) {
  if (!HAVE_READXL) return(read_record_xml(path))
  out <- read_record_readxl(path)
  if (!.READER_CHECKED$done) {
    .READER_CHECKED$done <- TRUE
    ref <- read_record_xml(path)
    same <- length(ref$t) == length(out$t) &&
      all(ref$t == out$t) && all(ref$date == out$date) &&
      isTRUE(all.equal(ref$v, out$v)) && isTRUE(all.equal(ref$hour, out$hour))
    cat(sprintf("  [%s] readxl and the built-in reader agree on %s (%d samples)\n",
                if (same) "PASS" else "FAIL", basename(path), length(out$t)))
    if (!same) stop("the two readers disagree on ", basename(path),
                    "; run with the built-in reader by detaching readxl")
  }
  out
}

window_rec <- function(rec, start = NULL, end = NULL) {
  if (is.null(start) && is.null(end)) return(rec)
  lo <- if (!is.null(start)) as.Date(start) else as.Date("0001-01-01")
  hi <- if (!is.null(end)) as.Date(end) else as.Date("9999-12-31")
  k <- rec$date >= lo & rec$date <= hi
  lapply(rec, function(z) z[k])
}

moments <- function(v) {
  n <- length(v); m <- mean(v); s <- sd(v)
  m2 <- sum((v - m)^2) / n; m3 <- sum((v - m)^3) / n; m4 <- sum((v - m)^4) / n
  f <- (n - 1) / n
  sk <- m3 / m2^1.5 * f^1.5
  ku <- m4 / m2^2 * f * f - 3
  cv <- 100 * s / m
  list(n = n, median = median(v), mean = m, SD = s, CV = cv, Sk = sk, k = ku,
       alpha = sqrt(cv * cv + sk * sk + ku * ku))
}

ranges_readings <- function(v) {
  n <- length(v)
  list(TIR_readings = 100 * sum(v >= 70 & v <= 180) / n,
       TBR_readings = 100 * sum(v < 70) / n,
       TAR_readings = 100 * sum(v > 180) / n)
}

daily_cv <- function(rec) {
  ok <- !is.na(rec$v)
  d <- rec$date[ok]; v <- rec$v[ok]
  sp <- split(v, format(d))
  cvs <- unlist(lapply(sp, function(z)
    if (length(z) >= 8 && mean(z) > 0) 100 * sd(z) / mean(z) else NULL))
  list(CV_daily_readings = if (length(cvs)) mean(cvs) else NA_real_,
       n_days = length(sp))
}

akima_segments <- function(t, y) {
  n <- length(t)
  if (n < 2) return(NULL)
  m <- diff(y) / diff(t)
  if (n == 2) {
    d <- c(m[1], m[1])
  } else {
    e <- c(2 * m[1] - m[2], 2 * m[1] - m[2], m,
           2 * m[n - 1] - m[n - 2], 2 * m[n - 1] - m[n - 2])
    e[1] <- 2 * e[3] - e[4]
    e[2] <- 2 * m[1] - m[2]
    e[length(e)] <- 2 * e[length(e) - 2] - e[length(e) - 3]
    d <- numeric(n)
    for (i in seq_len(n)) {
      a <- e[i]; b <- e[i + 1]; cc <- e[i + 2]; dd <- e[i + 3]
      w1 <- abs(dd - cc); w2 <- abs(b - a)
      d[i] <- if ((w1 + w2) > 1e-12) (w1 * b + w2 * cc) / (w1 + w2) else 0.5 * (b + cc)
    }
  }
  h <- diff(t)
  cbind(t0 = t[-n], h = h, a0 = y[-n], a1 = d[-n],
        a2 = (3 * m - 2 * d[-n] - d[-1]) / h,
        a3 = (d[-n] + d[-1] - 2 * m) / (h * h))
}

cubic_roots <- function(a3, a2, a1, a0) {
  cbrt <- function(x) sign(x) * abs(x)^(1 / 3)
  if (abs(a3) < 1e-14) {
    if (abs(a2) < 1e-14) return(if (abs(a1) < 1e-14) numeric(0) else -a0 / a1)
    disc <- a1 * a1 - 4 * a2 * a0
    if (disc < 0) return(numeric(0))
    r <- sqrt(disc)
    return(c((-a1 - r) / (2 * a2), (-a1 + r) / (2 * a2)))
  }
  b <- a2 / a3; cc <- a1 / a3; dd <- a0 / a3
  p <- cc - b * b / 3
  q <- 2 * b^3 / 27 - b * cc / 3 + dd
  disc <- q * q / 4 + p^3 / 27
  sh <- -b / 3
  if (disc > 1e-18) {
    r <- sqrt(disc)
    return(cbrt(-q / 2 + r) + cbrt(-q / 2 - r) + sh)
  }
  if (abs(p) < 1e-18) return(sh)
  rr <- 2 * sqrt(-p / 3)
  ph <- acos(max(-1, min(1, 3 * q / (p * rr))))
  rr * cos((ph - 2 * pi * (0:2)) / 3) + sh
}

time_above <- function(segs, thr) {
  if (is.null(segs)) return(0)
  tot <- 0
  for (i in seq_len(nrow(segs))) {
    h <- segs[i, "h"]; a0 <- segs[i, "a0"]; a1 <- segs[i, "a1"]
    a2 <- segs[i, "a2"]; a3 <- segs[i, "a3"]
    rts <- cubic_roots(a3, a2, a1, a0 - thr)
    rts <- rts[rts > 1e-12 & rts < h - 1e-12]
    cuts <- sort(c(0, h, rts))
    lo <- cuts[-length(cuts)]; hi <- cuts[-1]
    x <- 0.5 * (lo + hi)
    tot <- tot + sum((hi - lo)[a0 + a1 * x + a2 * x * x + a3 * x^3 > thr])
  }
  tot
}

seg_moments <- function(segs) {
  if (is.null(segs)) return(c(0, 0, 0))
  h <- segs[, "h"]; a0 <- segs[, "a0"]; a1 <- segs[, "a1"]
  a2 <- segs[, "a2"]; a3 <- segs[, "a3"]
  i1 <- sum(a0 * h + a1 * h^2 / 2 + a2 * h^3 / 3 + a3 * h^4 / 4)
  cf <- cbind(a0 * a0, 2 * a0 * a1, a1 * a1 + 2 * a0 * a2,
              2 * a0 * a3 + 2 * a1 * a2, a2 * a2 + 2 * a1 * a3,
              2 * a2 * a3, a3 * a3)
  i2 <- 0
  for (k in 0:6) i2 <- i2 + sum(cf[, k + 1] * h^(k + 1) / (k + 1))
  c(sum(h), i1, i2)
}

runs_of <- function(t, v, max_gap_min = MAX_GAP_MIN) {
  ok <- !is.na(v)
  t <- t[ok]; v <- v[ok]
  o <- order(t, v)
  t <- t[o]; v <- v[o]
  if (!length(t)) return(list())
  brk <- c(TRUE, diff(t) > max_gap_min * 60)
  g <- cumsum(brk)
  out <- list()
  for (k in unique(g)) {
    i <- which(g == k)
    if (length(i) >= 2) out[[length(out) + 1L]] <- list(t = t[i], v = v[i])
  }
  out
}

segs_of <- function(t, v) {
  rs <- runs_of(t, v)
  if (!length(rs)) return(NULL)
  do.call(rbind, lapply(rs, function(r) akima_segments(r$t, r$v)))
}

integral_ranges <- function(rec) {
  segs <- segs_of(rec$t, rec$v)
  if (is.null(segs)) return(list())
  sm <- seg_moments(segs); T <- sm[1]
  if (T <= 0) return(list())
  above <- sapply(THR, function(th) time_above(segs, th))
  names(above) <- as.character(THR)
  tw <- sm[2] / T
  vr <- max(sm[3] / T - tw * tw, 0)
  pct <- function(x) 100 * x / T
  list(TIR = pct(above["70"] - above["180"]),
       TBR = pct(T - above["70"]),
       TBR54 = pct(T - above["54"]),
       TAR = pct(above["180"]),
       TAR250 = pct(above["250"]),
       GMI = 3.31 + 0.02392 * tw,
       mean_tw = tw,
       CV_tw = if (tw != 0) 100 * sqrt(vr) / tw else NA_real_,
       coverage_days = T / 86400)
}

integral_daily_cv <- function(rec) {
  ok <- !is.na(rec$v)
  key <- format(rec$date[ok]); tt <- rec$t[ok]; vv <- rec$v[ok]
  per <- c()
  for (d in sort(unique(key))) {
    i <- which(key == d)
    segs <- segs_of(tt[i], vv[i])
    if (is.null(segs)) next
    sm <- seg_moments(segs); T <- sm[1]
    if (T < 8 * 3600) next
    m <- sm[2] / T
    vr <- max(sm[3] / T - m * m, 0)
    if (m > 0) per <- c(per, 100 * sqrt(vr) / m)
  }
  list(CV_daily_mean = if (length(per)) mean(per) else NA_real_,
       n_days_cv = length(per))
}

pyfmt <- function(x, d) {
  s <- formatC(round(x, d), format = "f", digits = d)
  s <- sub("0+$", "", s)
  sub("\\.$", ".0", s)
}

day_table <- function(rec, name) {
  ok <- !is.na(rec$v)
  key <- format(rec$date[ok]); tt <- rec$t[ok]; vv <- rec$v[ok]
  out <- list()
  for (d in sort(unique(key))) {
    i <- which(key == d)
    segs <- segs_of(tt[i], vv[i])
    if (is.null(segs)) next
    sm <- seg_moments(segs); T <- sm[1]
    if (T <= 0) next
    m <- sm[2] / T
    if (m <= 0) next
    cv <- 100 * sqrt(max(sm[3] / T - m * m, 0)) / m
    a54 <- time_above(segs, 54); a70 <- time_above(segs, 70)
    a180 <- time_above(segs, 180); a250 <- time_above(segs, 250)
    tar <- 100 * a180 / T; tar250 <- 100 * a250 / T
    parts <- strsplit(name, "_")[[1]]
    out[[length(out) + 1L]] <- data.frame(
      record = name, subject = parts[1], product = parts[length(parts)],
      date = d, hours = pyfmt(T / 3600, 3), mean_tw = pyfmt(m, 2),
      CV_day = pyfmt(cv, 3), CV_day_over_36 = if (cv >= 36) "YES" else "no",
      TIR_day = pyfmt(100 * (a70 - a180) / T, 2),
      TBR_day = pyfmt(100 * (T - a70) / T, 2),
      TBR54_day = pyfmt(100 * (T - a54) / T, 2),
      TAR_day = pyfmt(tar, 2), TAR250_day = pyfmt(tar250, 2),
      TAR_day_over_25 = if (tar > 25) "YES" else "no",
      TAR250_day_over_5 = if (tar250 > 5) "YES" else "no",
      stringsAsFactors = FALSE)
  }
  if (length(out)) do.call(rbind, out) else NULL
}

episodes <- function(rec, thresh, above = TRUE, min_min = 20) {
  ok <- !is.na(rec$v)
  t <- rec$t[ok]; v <- rec$v[ok]
  hit <- if (above) v > thresh else v < thresh
  if (!any(hit)) return(numeric(0))
  g <- cumsum(!hit)
  keep <- c()
  for (k in unique(g[hit])) {
    i <- which(hit & g == k)
    d <- (t[i[length(i)]] - t[i[1]]) / 60 + 15
    if (d > min_min) keep <- c(keep, d)
  }
  keep
}

episode_stats <- function(rec, span_days) {
  out <- list()
  spec <- list(c("hyper180", 180, TRUE), c("hyper250", 250, TRUE),
               c("hypo70", 70, FALSE), c("hypo54", 54, FALSE))
  for (s in spec) {
    d <- episodes(rec, as.numeric(s[[2]]), as.logical(s[[3]]))
    out[[paste0("ep_", s[[1]], "_per_day")]] <-
      if (span_days) length(d) / span_days else NA_real_
    out[[paste0("ep_", s[[1]], "_mean_min")]] <- if (length(d)) mean(d) else 0
  }
  out
}

betacf <- function(a, b, x) {
  qab <- a + b; qap <- a + 1; qam <- a - 1
  c_ <- 1; d <- 1 - qab * x / qap
  if (abs(d) < 1e-300) d <- 1e-300
  d <- 1 / d; h <- d
  for (m in 1:299) {
    m2 <- 2 * m
    aa <- m * (b - m) * x / ((qam + m2) * (a + m2))
    d <- 1 + aa * d; c_ <- 1 + aa / c_
    if (abs(d) < 1e-300) d <- 1e-300
    if (abs(c_) < 1e-300) c_ <- 1e-300
    d <- 1 / d; h <- h * d * c_
    aa <- -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2))
    d <- 1 + aa * d; c_ <- 1 + aa / c_
    if (abs(d) < 1e-300) d <- 1e-300
    if (abs(c_) < 1e-300) c_ <- 1e-300
    d <- 1 / d; de <- d * c_; h <- h * de
    if (abs(de - 1) < 3e-16) break
  }
  h
}

betainc <- function(a, b, x) {
  if (x <= 0) return(0)
  if (x >= 1) return(1)
  lb <- lgamma(a + b) - lgamma(a) - lgamma(b) + a * log(x) + b * log1p(-x)
  if (x < (a + 1) / (a + b + 2)) exp(lb) * betacf(a, b, x) / a
  else 1 - exp(lb) * betacf(b, a, 1 - x) / b
}

f_cdf <- function(x, d1, d2) if (x <= 0) 0 else betainc(d1 / 2, d2 / 2,
                                                        d1 * x / (d1 * x + d2))

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
  w <- rep(1 + p / nu, n)
  cx <- mean(px); cy <- mean(py)
  for (it in seq_len(maxit)) {
    w0 <- w
    sw <- sum(w)
    s11 <- sum(w * (px - cx)^2) / sw
    s12 <- sum(w * (px - cx) * (py - cy)) / sw
    s22 <- sum(w * (py - cy)^2) / sw
    det <- s11 * s22 - s12 * s12
    u <- px - cx; v <- py - cy
    q <- (s22 * u * u - 2 * s12 * u * v + s11 * v * v) / det
    w <- (nu + p) / (nu + q)
    sw <- sum(w)
    cx <- sum(w * px) / sw
    cy <- sum(w * py) / sw
    if (max(abs(w - w0)) < tol) break
  }
  s11 <- sum(w * (px - cx)^2) / n
  s12 <- sum(w * (px - cx) * (py - cy)) / n
  s22 <- sum(w * (py - cy)^2) / n
  list(center = c(cx, cy), S = c(s11, s12, s22))
}

eig2 <- function(S) {
  a <- S[1]; b <- S[2]; cc <- S[3]
  tr <- a + cc; det <- a * cc - b * b
  disc <- sqrt(max(tr * tr / 4 - det, 0))
  list(l = c(tr / 2 + disc, max(tr / 2 - disc, 0)),
       ang = 0.5 * atan2(2 * b, a - cc))
}

poincare_points <- function(rec) {
  v <- rec$v; n <- length(v)
  if (n < 3) return(NULL)
  i <- seq_len(n - 2)
  k <- !is.na(v[i]) & !is.na(v[i + 1]) & !is.na(v[i + 2])
  list(x = v[i][k], y = v[i + 1][k], t = rec$t[i][k], hour = rec$hour[i][k])
}

ccm <- function(x, y, l1, l2) {
  n <- length(x)
  i <- seq_len(n - 2)
  tot <- sum(abs((x[i + 1] - x[i]) * (y[i + 2] - y[i]) -
                 (x[i + 2] - x[i]) * (y[i + 1] - y[i])) / 2)
  100 * tot / (pi * l1 * l2 * (n - 2))
}

lsp <- function(t, y) {
  n <- length(y)
  tt <- t - t[1]
  span <- tt[n] - tt[1]
  step <- 1 / span
  nout <- floor(0.5 * n)
  freqs <- step * seq_len(nout)
  m <- mean(y); vr <- var(y)
  yc <- y - m
  P <- numeric(nout)
  for (j in seq_len(nout)) {
    w <- 2 * pi * freqs[j]
    tau <- 0.5 * atan2(sum(sin(2 * w * tt)), sum(cos(2 * w * tt))) / w
    ang <- w * (tt - tau)
    cs <- cos(ang); sn <- sin(ang)
    A <- sum(yc * cs); B <- sum(cs * cs); C <- sum(yc * sn); D <- sum(sn * sn)
    P[j] <- (A * A / B + C * C / D) / (2 * vr)
  }
  list(freqs = freqs, P = P)
}

cpt_amoc <- function(x) {
  n <- length(x)
  cs <- c(0, cumsum(x))
  k <- seq_len(n - 1)
  obj <- cs[k + 1]^2 / k + (cs[n + 1] - cs[k + 1])^2 / (n - k)
  k[which.max(obj)]
}

ols <- function(x, y) sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)

spectral <- function(rec) {
  ok <- !is.na(rec$v)
  sp <- lsp(rec$t[ok], rec$v[ok])
  pos <- sp$P > 0
  lx <- log10(sp$freqs[pos]); ly <- log10(sp$P[pos])
  tau <- cpt_amoc(lx)
  list(freqs = sp$freqs, power = sp$P, lx = lx, ly = ly, tau = tau,
       Slope = ols(lx[seq_len(tau - 1)], ly[seq_len(tau - 1)]),
       Slope2 = ols(lx[tau:length(lx)], ly[tau:length(ly)]),
       Peak = sp$freqs[which.max(sp$P)] * 1e6,
       Break = (1 / sp$freqs[tau]) / 60)
}

hour_colour <- function(h, light_hour = LIGHT_HOUR) {
  phase <- ((h - light_hour) / 24) %% 1
  lum <- 0.30 + 0.40 * (1 + cos(2 * pi * phase)) / 2
  hue <- (0.62 + phase) %% 1
  hsv(h = hue, s = 0.55, v = lum + 0.25)
}

figure <- function(name, sp, pp, fit, res, path) {
  grDevices::pdf(paste0(path, ".pdf"), width = 11.6, height = 5.2)
  on.exit(grDevices::dev.off(), add = TRUE)
  par(mfrow = c(1, 2), mar = c(4.2, 4.4, 2.6, 1.2), col.axis = "#5c5c5c",
      col.lab = "#1a1a1a", cex.axis = 0.8, cex.lab = 0.9)
  plot(sp$lx, sp$ly, pch = 16, cex = 0.35, col = "#9a9a9a",
       xlab = expression(log[10] ~ frequency ~ (Hz)),
       ylab = expression(log[10] ~ power), bty = "l")
  title("A   Lomb-Scargle periodogram", adj = 0, cex.main = 1.0)
  tau <- sp$tau
  i1 <- seq_len(tau - 1); i2 <- tau:length(sp$lx)
  for (ii in list(i1, i2)) {
    b <- ols(sp$lx[ii], sp$ly[ii])
    a <- mean(sp$ly[ii]) - b * mean(sp$lx[ii])
    lines(range(sp$lx[ii]), a + b * range(sp$lx[ii]), col = "#6A3D9A", lwd = 2)
  }
  abline(v = sp$lx[tau], col = "#2e7d32", lty = 2)
  hi <- max(350, ceiling(max(c(pp$x, pp$y)) / 50) * 50)
  plot(pp$x, pp$y, pch = 16, cex = 0.45, col = hour_colour(pp$hour),
       xlim = c(0, hi), ylim = c(0, hi), asp = 1, bty = "l",
       xlab = "glucose (mg/dL) at t", ylab = "glucose (mg/dL) at t+1")
  title("B   Poincare return map", adj = 0, cex.main = 1.0)
  abline(0, 1, col = "#5c5c5c", lwd = 0.7)
  abline(v = c(54, 250), h = c(54, 250), col = "#5c5c5c", lty = 2, lwd = 0.8)
  ev <- eig2(fit$S)
  fac <- sqrt(2 * qf_(LEVEL, 2, length(pp$x) - 1))
  th <- seq(0, 2 * pi, length.out = 200)
  ex <- sqrt(ev$l[1]) * fac * cos(th); ey <- sqrt(ev$l[2]) * fac * sin(th)
  lines(fit$center[1] + ex * cos(ev$ang) - ey * sin(ev$ang),
        fit$center[2] + ex * sin(ev$ang) + ey * cos(ev$ang),
        col = "#6A3D9A", lwd = 2)
  legend("topleft", bty = "n", cex = 0.75,
         legend = c(sprintf("SD1 = %.2f", res$SD1), sprintf("SD2 = %.2f", res$SD2),
                    sprintf("SD1/SD2 = %.3f", res$`SD1/SD2`),
                    sprintf("CCM = %.3f %%", res$CCM)))
  invisible(path)
}

analyse <- function(path, start = NULL, end = NULL, outdir = NULL) {
  name <- sub("\\.[^.]*$", "", basename(path))
  rec <- window_rec(read_record(path), start, end)
  vals <- rec$v[!is.na(rec$v)]
  if (length(vals) < 30) return(NULL)
  res <- moments(vals)
  res <- c(res, ranges_readings(vals), integral_ranges(rec))
  pp <- poincare_points(rec)
  fit <- cov_trob(pp$x, pp$y)
  ev <- eig2(fit$S)
  fac <- 2 * qf_(LEVEL, 2, length(pp$x) - 1)
  sd1 <- sqrt(ev$l[2] * fac); sd2 <- sqrt(ev$l[1] * fac)
  res <- c(res, list(n_pairs = length(pp$x), SD1 = sd1, SD2 = sd2,
                     `SD1/SD2` = sd1 / sd2, CCM = ccm(pp$x, pp$y, sd1, sd2)))
  sp <- spectral(rec)
  res <- c(res, list(Slope = sp$Slope, Slope2 = sp$Slope2, Peak = sp$Peak,
                     Break = sp$Break, n_freq = length(sp$lx), tau = sp$tau))
  ts <- rec$t[!is.na(rec$v)]
  res$span_days <- (ts[length(ts)] - ts[1]) / 86400
  res <- c(res, episode_stats(rec, res$span_days), daily_cv(rec),
           integral_daily_cv(rec))
  res$record <- name
  if (!is.null(outdir)) {
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    figure(name, sp, pp, fit, res, file.path(outdir, name))
  }
  res
}

COLS <- c("record", "n", "n_pairs", "n_freq", "span_days", "median", "mean", "SD",
          "CV", "CV_tw", "CV_daily_mean", "CV_daily_readings", "n_days", "n_days_cv",
          "Sk", "k", "alpha", "TIR", "TBR", "TBR54", "TAR", "TAR250", "GMI",
          "mean_tw", "coverage_days", "TIR_readings", "TBR_readings", "TAR_readings",
          "ep_hyper180_per_day", "ep_hyper180_mean_min", "ep_hyper250_per_day",
          "ep_hyper250_mean_min", "ep_hypo70_per_day", "ep_hypo70_mean_min",
          "ep_hypo54_per_day", "ep_hypo54_mean_min",
          "SD1", "SD2", "SD1/SD2", "CCM", "Slope", "Slope2", "tau", "Break", "Peak")

selftest <- function() {
  mk <- function(vals, step = 15) {
    n <- length(vals)
    sec <- (seq_len(n) - 1) * step * 60
    list(t = sec, date = as.Date("2023-01-01") + sec %/% 86400,
         hour = (sec %% 86400) / 3600, v = vals)
  }
  ok <- list()
  add <- function(lab, good) ok[[length(ok) + 1L]] <<- list(lab, isTRUE(good))

  r <- integral_ranges(mk(rep(120, 97)))
  add("flat 120 gives TIR 100%", abs(r$TIR - 100) < 1e-9)
  add("flat 120 gives CV 0%", abs(r$CV_tw) < 1e-9)
  add("flat 120 time-weighted mean is 120", abs(r$mean_tw - 120) < 1e-9)
  r <- integral_ranges(mk(60 + (0:60) * 2))
  add("linear ramp 60->180: Akima reproduces the line, mean = 120",
      abs(r$mean_tw - 120) < 1e-6)
  add("linear ramp: fraction below 70 equals the analytic 10/120",
      abs(r$TBR - 100 * 5 / 60) < 0.05)
  r <- integral_ranges(mk(140 + 60 * sin(2 * pi * (0:192) / 96)))
  add("sine 140+-60: symmetric, TAR180 approx TBR100 by symmetry",
      abs(r$TAR - (100 - r$TIR - r$TBR)) < 1e-6)
  add("sine 140+-60: time-weighted mean approx 140", abs(r$mean_tw - 140) < 1)
  g <- mk(rep(120, 20)); g2 <- mk(rep(120, 20))
  gap <- list(t = c(g$t, 48 * 3600 + (0:19) * 3600),
              date = as.Date("2023-01-01") + c(g$t, 48 * 3600 + (0:19) * 3600) %/% 86400,
              hour = 0, v = rep(120, 40))
  r <- integral_ranges(gap)
  add("a 2-day gap is excluded from the denominator", r$coverage_days < 1)

  for (z in ok) cat(sprintf("  [%s] %s\n", if (z[[2]]) "PASS" else "FAIL", z[[1]]))
  sum(!vapply(ok, function(z) z[[2]], logical(1)))
}

run_tier1 <- function(paths, outdir = "results", csv_out = NULL,
                      days_out = NULL, start = NULL, end = NULL,
                      figures = TRUE) {
  files <- character(0)
  for (p in paths) {
    if (dir.exists(p)) {
      files <- c(files, sort(list.files(p, pattern = "\\.xlsx$", full.names = TRUE)))
    } else files <- c(files, p)
  }
  files <- files[!grepl("^~\\$", basename(files))]
  rows <- list(); dayrows <- list()
  for (f in files) {
    rec <- window_rec(read_record(f), start, end)
    dt <- day_table(rec, sub("\\.[^.]*$", "", basename(f)))
    if (!is.null(dt)) dayrows[[length(dayrows) + 1L]] <- dt
    r <- analyse(f, start, end, if (figures) outdir else NULL)
    if (!is.null(r)) {
      rows[[length(rows) + 1L]] <- r
      cat(sprintf("%s  n=%d  mean=%.4g  TIR=%.4g  Slope=%.4g\n",
                  r$record, r$n, r$mean, r$TIR, r$Slope))
    }
  }
  if (length(dayrows) && !is.null(days_out)) {
    dd <- do.call(rbind, dayrows)
    dir.create(dirname(days_out), recursive = TRUE, showWarnings = FALSE)
    write.csv(dd, days_out, row.names = FALSE, quote = FALSE)
    cat(sprintf("day table: %d days, CV>=36%% %d, TAR>25%% %d, TAR250>5%% %d  -> %s\n",
                nrow(dd), sum(dd$CV_day_over_36 == "YES"),
                sum(dd$TAR_day_over_25 == "YES"),
                sum(dd$TAR250_day_over_5 == "YES"), days_out))
  }
  out <- if (is.null(csv_out)) file.path(outdir, "record_metrics.csv") else csv_out
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  m <- do.call(rbind, lapply(rows, function(r)
    vapply(COLS, function(c_) {
      z <- r[[c_]]
      if (is.null(z) || length(z) == 0) "" else
        if (is.character(z)) z else if (is.na(z)) "" else format(z, digits = 17)
    }, character(1))))
  colnames(m) <- COLS
  con <- file(out, "wb")
  writeLines(c(paste(COLS, collapse = ","),
               apply(m, 1, paste, collapse = ",")), con, sep = "\n")
  close(con)
  cat(out, "\n")
  invisible(out)
}

.parse_args <- function() {
  a <- commandArgs(trailingOnly = TRUE)
  g1 <- function(flag, default) {
    i <- match(flag, a)
    if (is.na(i) || i == length(a)) default else a[i + 1]
  }
  flags <- c("-o", "-c", "-d", "--start", "--end")
  taken <- unlist(lapply(flags, function(f) {
    i <- match(f, a); if (is.na(i)) NULL else a[c(i, i + 1)]
  }))
  pos <- setdiff(a[!startsWith(a, "-")], taken)
  list(paths = if (length(pos)) pos else "data/cgm",
       outdir = g1("-o", "results"),
       csv_out = if (is.na(match("-c", a))) NULL else g1("-c", NULL),
       days_out = if (is.na(match("-d", a))) NULL else g1("-d", NULL),
       start = if (is.na(match("--start", a))) NULL else g1("--start", NULL),
       end = if (is.na(match("--end", a))) NULL else g1("--end", NULL),
       selftest = "--selftest" %in% a,
       figures = !("--no-figures" %in% a))
}

.a <- .parse_args()
if (.a$selftest) {
  quit(status = if (selftest()) 1L else 0L)
} else {
  run_tier1(.a$paths, .a$outdir, .a$csv_out, .a$days_out, .a$start, .a$end,
            .a$figures)
}
