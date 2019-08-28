library(tidyr, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(bin2mi)
library(m2imp)

alpha <- 0.025
pc <- 0.8
pt <- 0.8
#M1 is calculated based on the meta-analysis of the historical trials
m1 <- 0.232
#M2 that was used in RENOVATE studies
m2_ren <- round((1- 2/3)*m1,3)
#reproduce SS calculation for RENOVATE studies
pc_rmle =  p_rmle(m2_ren, 1, 1, pc = pc, pt = pt)
ss_fm(alpha = alpha, power = 0.954, pc = pc, pt = pt, m2 = m2_ren, pc_r = pc_rmle, pt_r = pc_rmle - m2_ren)
