# From Ben Bolker

library(lme4)
set.seed(1001)

x <- rnorm(10, 20, 1)
y <- x + rnorm(10, 2, 1)
t1 <- t.test(x, y, paired = TRUE)

alldat <- rbind(data.frame(obs=x,id=1:10,var="x"),
               data.frame(obs=y,id=1:10,var="y"))
m1 <- lmer(obs ~ var + (1 | id) , data = alldat, REML = TRUE)

all.equal(abs(t1$statistic), summary(m1)@coefs["vary","t value"])

# The t statistic is (almost) the same. (all.equal() reports a relative
# difference of 4.618215e-07). REML=TRUE isn't necessary (it's the
# default) but it emphasizes the fact that the paired t test is exactly
# equivalent to REML.