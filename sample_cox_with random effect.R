set.seed(10)
N <- 250
dat <- data.frame(ID = factor(1:N), age = rnorm(N, mean = 45, sd = 5), sex = sample(0:1, 
      N, TRUE), basemort = rnorm(N, sd = 3))
interval <- matrix(sample(2:14, N * 3, replace = TRUE), N)

windows <- t(apply(cbind(0, interval), 1, cumsum))
windows <- rbind(windows[, 1:2], windows[, 2:3], windows[, 3:4])

colnames(windows) <- c("time1", "time2")
dat <- cbind(do.call(rbind, rep(list(dat), 3)), windows)
dat <- dat[order(dat$ID), ]
dat$assessment <- rep(1:3, N)
rownames(dat) <- NULL
head(dat)

# simulate survival (mortality) data
attach(dat)
transplant <- with(dat, {
  mu <- (0.05 * age) + (0.3 * time2)
  lp <- rnorm(N * 3, mean = mu, sd = 1)
  as.integer(lp > quantile(lp, probs = 0.65))
})
# ensure that transplants do not revert
transplant <- as.integer(ave(transplant, dat$ID, FUN = cumsum) >= 1)

# simulate survival (mortality) data
mortality <- with(dat, {
  mu <- basemort + (0.05 * age) - (2.5 * sex) + (0.3 * time2)
  lp <- rnorm(N * 3, mean = mu, sd = 1)
  as.integer(lp > median(lp))
})

# ensure that once someone dies, he or she stays dead
mortality <- as.integer(ave(mortality, dat$ID, FUN = cumsum) >= 1)

# ensure no one dead at baseline
mortality[dat$assessment == 1] <- 0


