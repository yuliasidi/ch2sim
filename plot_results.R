library(dplyr)
library(ggplot2)

source("funs/plot_ch2.R")

mdsum <- readRDS("sums/sum_mdsu_obs3.rds")

mdsum <-res_sum1

plot_ch2(mdsum)

#pdf("plots/plot_mdsur_obs3.pdf")
plot_mdsur_obs3
#dev.off()