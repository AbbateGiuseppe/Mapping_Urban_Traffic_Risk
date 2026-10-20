library(dplyr)
library(sf)
library(fda)

dat <- dataset_quartieri_sf %>%
  st_drop_geometry() %>%
  mutate(
    Month_num = match(Month, month.abb),
    Hour_int  = as.integer(substr(Hour, 1, 2))
  )

quartieri <- sort(unique(dat$DEN_Z_URB))





# ============================================================
# 1. BUILD POINT x CURVE MATRICES (rows = points, columns = neighborhoods)
#    - monthly: average accidents per month (averaged over years), grid 1..12
#    - hourly : count per hour, grid 0..23
# ============================================================

# --- MONTHLY profile: averaged over years ---
mese_mat <- sapply(quartieri, function(q) {
  d <- dat %>% filter(DEN_Z_URB == q) %>%
    count(Year, Month_num, name = "n") %>%
    group_by(Month_num) %>% summarise(m = mean(n), .groups = "drop")
  v <- setNames(rep(0, 12), 1:12)            # full 1..12 grid
  v[as.character(d$Month_num)] <- d$m
  v
})  # -> 12 rows (months) x N neighborhoods

# --- HOURLY profile: count per hour ---
ora_mat <- sapply(quartieri, function(q) {
  d <- dat %>% filter(DEN_Z_URB == q, !is.na(Hour_int)) %>%
    count(Hour_int, name = "n")
  v <- setNames(rep(0, 24), 0:23)            # full 0..23 grid
  v[as.character(d$Hour_int)] <- d$n
  v
})  # -> 24 rows (hours) x N neighborhoods


# ============================================================
# 2. FDA SMOOTHING (BLOCK A) — Fourier basis since the data is periodic
# ============================================================

# --- MONTHLY: domain [0,12], Fourier basis ---
rng_mese   <- c(0, 12)
argvals_m  <- 1:12
basis_mese <- create.fourier.basis(rangeval = rng_mese, nbasis = 7)   # odd, as required by Fourier
fd_mese    <- Data2fd(y = mese_mat, argvals = argvals_m, basisobj = basis_mese)

# --- HOURLY: domain [0,24], Fourier basis ---
rng_ora    <- c(0, 24)
argvals_o  <- 0:23
basis_ora  <- create.fourier.basis(rangeval = rng_ora, nbasis = 9)
fd_ora     <- Data2fd(y = ora_mat, argvals = argvals_o, basisobj = basis_ora)

# ============================================================
# 3. PLOT — one smooth curve per neighborhood
# ============================================================
par(mfrow = c(1, 2))

plot.fd(fd_mese, xlab = "Month", ylab = "Average Accidents",
        main = "Functional Month")
plot.fd(fd_ora, xlab = "hour", ylab = "Average Accidents",
        main = "Functional Hour")

par(mfrow = c(1, 1))





par(mfrow = c(1, 2))

# monthly
media_m <- mean.fd(fd_mese)
std_m   <- sd.fd(fd_mese)
plot.fd(fd_mese, col = "grey80", lty = 1,
        main = "Monthly: mean ± sd", xlab = "Month", ylab = "Average Accidents")
lines(media_m, lwd = 3, col = "black")
lines(media_m + std_m, lwd = 2, lty = 2, col = "red")
lines(media_m - std_m, lwd = 2, lty = 2, col = "red")

# hourly
media_o <- mean.fd(fd_ora)
std_o   <- sd.fd(fd_ora)
plot.fd(fd_ora, col = "grey80", lty = 1,
        main = "Hour: mean ± sd", xlab = "Hour", ylab = "Average Accidents")
lines(media_o, lwd = 3, col = "black")
lines(media_o + std_o, lwd = 2, lty = 2, col = "red")
lines(media_o - std_o, lwd = 2, lty = 2, col = "red")

par(mfrow = c(1, 1))


# monthly
pca_m <- pca.fd(fd_mese, nharm = 3, centerfns = TRUE)
cat("Monthly explained variance:", 
    round(cumsum(pca_m$values[1:3]) / sum(pca_m$values[pca_m$values > 0]), 3), "\n")

par(mfrow = c(1, 3))
plot.pca.fd(pca_m, nx = 100, harm = c(1, 2, 3))
par(mfrow = c(1, 1))

# hourly
pca_o <- pca.fd(fd_ora, nharm = 3, centerfns = TRUE)
cat("Hourly explained variance:",
    round(cumsum(pca_o$values[1:3]) / sum(pca_o$values[pca_o$values > 0]), 3), "\n")

par(mfrow = c(1, 3))
plot.pca.fd(pca_o, nx = 100, harm = c(1, 2, 3))
par(mfrow = c(1, 1))


par(mfrow = c(1, 2))

# monthly
plot(pca_m$scores[, 1], pca_m$scores[, 2], type = "n",
     xlab = "FPC1", ylab = "FPC2", main = "Monthly Scores")
text(pca_m$scores[, 1], pca_m$scores[, 2], labels = quartieri, cex = 0.7)

# hourly
plot(pca_o$scores[, 1], pca_o$scores[, 2], type = "n",
     xlab = "FPC1", ylab = "FPC2", main = "Hourly Scores per Neighborhood")
text(pca_o$scores[, 1], pca_o$scores[, 2], labels = quartieri, cex = 0.7)

par(mfrow = c(1, 1))



# ============================================================
# WEEKLY PROFILE per neighborhood
# ============================================================

# canonical day order (Mon -> Sun)
giorni_ord <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")

# --- 7 x n_neighborhoods matrix ---
day_mat <- sapply(quartieri, function(q) {
  d <- dat %>%
    filter(DEN_Z_URB == q) %>%
    mutate(Day = factor(Day, levels = giorni_ord)) %>%
    count(Day, .drop = FALSE, name = "n")   # .drop=FALSE keeps the zero-count days
  d$n
})
rownames(day_mat) <- giorni_ord

# --- FDA: Fourier basis, domain [0,7], 7 points ---
rng_day   <- c(0, 7)
argvals_d <- 1:7          # Mon=1, Tue=2, ..., Sun=7
basis_day <- create.fourier.basis(rangeval = rng_day, nbasis = 5)  # max 5 for 7 points
fd_day    <- Data2fd(y = day_mat, argvals = argvals_d, basisobj = basis_day)

# --- functional curve plot ---
par(mfrow = c(1, 1))
plot.fd(fd_day,
        xlab = "Day of the week", ylab = "Number of accidents",
        main = "Functional Day Profile per neighbourhood",
        xaxt = "n")
axis(1, at = 1:7, labels = giorni_ord)

# --- functional mean + sd ---
media_d <- mean.fd(fd_day)
std_d   <- sd.fd(fd_day)

plot.fd(fd_day, col = "grey80", lty = 1,
        main = "Daily profile: mean ± sd",
        xlab = "Day of the week", ylab = "Accidents",
        xaxt = "n")
axis(1, at = 1:7, labels = giorni_ord)
lines(media_d, lwd = 3, col = "black")
lines(media_d + std_d, lwd = 2, lty = 2, col = "red")
lines(media_d - std_d, lwd = 2, lty = 2, col = "red")

# --- FPCA ---
pca_d <- pca.fd(fd_day, nharm = 3, centerfns = TRUE)
cat("Weekly explained variance:",
    round(cumsum(pca_d$values[1:3]) / sum(pca_d$values[pca_d$values > 0]), 3), "\n")

par(mfrow = c(1, 3))
plot.pca.fd(pca_d, nx = 100, harm = c(1, 2, 3))
par(mfrow = c(1, 1))

# --- scores scatter plot ---
plot(pca_d$scores[, 1], pca_d$scores[, 2], type = "n",
     xlab = "FPC1", ylab = "FPC2",
     main = "Weekly profile scores per neighbourhood")
text(pca_d$scores[, 1], pca_d$scores[, 2], labels = quartieri, cex = 0.7)
