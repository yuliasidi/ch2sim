library(tidyr, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(bin2mi)
library(m2imp)

alpha <- 0.025
power <- 0.85
pc <- 0.8
pt <- 0.8
#M1 is calculated based on the meta-analysis of the historical trials
m1 <- 0.232

#M2 is  based on clinical expertise: assume that the whole population of clinical experts correcpond to
# 1000 clinicians
# set.seed(120487)
# m2p_unif <- round(runif(n = 100, min = 0.6, max = 0.8), 2)
set.seed(120488)
m2p_norm <- round(rnorm(n = 1000, mean = 0.7, sd =  0.03), 2)
# set.seed(120489)
# m2p_exp <- round(rbeta(n = 1000, shape1 = 7, shape2 = 3), 2)

#estimate mu_lambda from the populaiton:
mu_l <- mean(m2p_norm)

#minimal fraction preservation and ss: 369
m2_max <- (1 - min(m2p_norm))*m1
ss_min <- ss_wald(alpha = alpha, power = power, pc = pc, pt = pt, m2 = m2_max)

#maximal fraction preservation and ss: 1210
m2_min <-  (1 - max(m2p_norm))*m1
ss_max <- ss_wald(alpha = alpha, power = power, pc = pc, pt = pt, m2 = m2_min)

#mean fraction preservation and ss: 589
ss_mean <- ss_wald(alpha = alpha, power = power, pc = pc, pt = pt, m2 = (1- mu_l)*m1)

##ss example in the draft
ss_wald(alpha = alpha, power = power, pc = pc, pt = pt, m2 = (1- 0.7)*m1)
ss_wald(alpha = alpha, power = power, pc = pc, pt = pt, m2 = (1- 0.71)*m1)

