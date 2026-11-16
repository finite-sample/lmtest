## Reduced models specified by an update must be refitted on exactly the
## observations of the full model, also when the reduced model alone would
## use more observations and itself drops rows with missing values.
library("lmtest")

set.seed(1)
d <- data.frame(x1 = rnorm(60), x2 = rnorm(60))
d$y <- 1 + 0.5 * d$x1 + d$x2 + rnorm(60)
d$y[c(3, 17, 40)] <- NA
d$x1[c(8, 50)] <- NA

m1 <- lm(y ~ x1 + x2, data = d)
m0 <- lm(y ~ x2, data = d[row.names(model.frame(m1)), ])

lr <- lrtest(m1, m0)
lrtest(m1, "x1")
lrtest(m1, . ~ . - x1)
waldtest(m1, "x1")
stopifnot(
  all.equal(lrtest(m1, "x1")[2, "Chisq"], lr[2, "Chisq"]),
  all.equal(lrtest(m1, . ~ . - x1)[2, "Chisq"], lr[2, "Chisq"]),
  all.equal(lrtest(m1, 1)[2, "Chisq"], lr[2, "Chisq"]),
  all.equal(waldtest(m1, "x1")[2, "Res.Df"], df.residual(m0))
)

## same with non-default row names (data subset beforehand)
d2 <- d[-(1:5), ]
m1 <- lm(y ~ x1 + x2, data = d2)
m0 <- lm(y ~ x2, data = d2[row.names(model.frame(m1)), ])
stopifnot(all.equal(lrtest(m1, "x1")[2, "Chisq"], lrtest(m1, m0)[2, "Chisq"]))
