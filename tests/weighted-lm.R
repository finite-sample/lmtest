## Diagnostic tests that do not use regression weights must not silently
## test the unweighted regression when given a weighted "lm" object.
library("lmtest")

set.seed(1)
d <- data.frame(x = runif(50), z = rnorm(50), w = runif(50, 0.5, 3))
d$y <- 1 + d$x + rnorm(50, sd = 1 / sqrt(d$w))
mw <- lm(y ~ x + z, data = d, weights = w)
m1 <- lm(y ~ x + z, data = d, weights = rep(1, 50))
m <- lm(y ~ x + z, data = d)

fails <- function(expr) {
  e <- try(expr, silent = TRUE)
  inherits(e, "try-error") && grepl("weighted regressions", e)
}
stopifnot(
  fails(bgtest(mw)), fails(gqtest(mw)), fails(harvtest(mw)), fails(hmctest(mw)),
  fails(raintest(mw)), fails(resettest(mw)), fails(dwtest(mw)),
  fails(jtest(mw, lm(y ~ x, data = d))), fails(coxtest(lm(y ~ x, data = d), mw)),
  fails(petest(lm(exp(y) ~ x, data = d, weights = w), lm(y ~ x, data = d)))
)

## unit weights are still accepted and give the unweighted results
stopifnot(
  all.equal(bgtest(m1)$statistic, bgtest(m)$statistic),
  all.equal(gqtest(m1)$statistic, gqtest(m)$statistic),
  all.equal(resettest(m1)$statistic, resettest(m)$statistic),
  all.equal(jtest(m1, lm(y ~ x, data = d)), jtest(m, lm(y ~ x, data = d)))
)
