## bptest() for weighted regressions is the test applied to the equivalent
## unweighted regression of sqrt(w) * y on sqrt(w) * X, i.e., based on the
## residuals sqrt(w) * (y - X b).
library("lmtest")

set.seed(1)
d <- data.frame(x = runif(100), z = rnorm(100), w = runif(100, 0.5, 3))
d$y <- 1 + d$x + rnorm(100, sd = (1 + d$x) / sqrt(d$w))

m <- lm(y ~ x + z, data = d, weights = w)
bptest(m)
bptest(m, studentize = FALSE)

## hand computation of Koenker's statistic n * R^2 on scaled residuals
r <- sqrt(d$w) * residuals(m)
koenker <- nrow(d) * summary(lm(r^2 ~ x + z, data = d))$r.squared

stopifnot(
  all.equal(unname(bptest(m)$statistic), koenker),
  ## rescaling all weights does not change the fitted model or the test
  all.equal(bptest(m)$statistic, bptest(update(m, weights = 10 * w))$statistic),
  all.equal(bptest(m, studentize = FALSE)$statistic,
            bptest(update(m, weights = 10 * w), studentize = FALSE)$statistic),
  ## formula interface with weights
  all.equal(bptest(m)$statistic, bptest(y ~ x + z, data = d, weights = d$w)$statistic),
  ## zero weights are equivalent to dropping the observations
  all.equal(bptest(update(m, weights = ifelse(seq_len(100) <= 10, 0, w)))$statistic,
            bptest(lm(y ~ x + z, data = d[-(1:10), ], weights = w))$statistic),
  ## unit weights give the unweighted test
  all.equal(bptest(update(m, weights = rep(1, 100)))$statistic,
            bptest(lm(y ~ x + z, data = d))$statistic)
)

## varformula with missing values: model refitted on the common rows
d$v <- d$z
d$v[c(5, 9)] <- NA
mv <- lm(y ~ x + z, data = d[-c(5, 9), ], weights = w)
r <- sqrt(mv$weights) * residuals(mv)
stopifnot(all.equal(unname(bptest(m, ~ v, data = d)$statistic),
                    98 * summary(lm(r^2 ~ d$v[-c(5, 9)]))$r.squared))
