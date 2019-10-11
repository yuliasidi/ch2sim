library(dplyr)
library(ggplot2)

source("funs/plot_ch2.R")
source("funs/plotd_ch2.R")
source("funs/tab_sum.R")
source("funs/tab_minfo.R")


mdsum <- readRDS("sums/sum_mdsu_obs3.rds")
mdrsum <- readRDS("sums/sum_mdsu_obs3r.rds")
mdsum_mcar <- readRDS("sums/sum_mdsu_obs3_smcar.rds")
mdsum_mar <- readRDS("sums/sum_mdsu_obs3_smar.rds")

minfo <- readRDS("sums/minfo_mdsu_obs3.rds")
minfor <- readRDS("sums/minfo_mdsu_obs3r.rds")
minfo_mcar <- readRDS("sums/minfo_mdsu_obs3_smcar.rds")
minfo_mar <- readRDS("sums/minfo_mdsu_obs3_smar.rds")


###############################
## Plots - Difference vs POP ##
###############################

pdf("plots/plotd_mdsur_obs3.pdf")
plotd_ch2(mdsum)
dev.off()

pdf("plots/plotd_mdsur_obs3r.pdf")
plotd_ch2(mdrsum)
dev.off()

pdf("plots/plotd_mdsur_obs3_mcar.pdf")
plotd_ch2(mdsum_mcar)
dev.off()


pdf("plots/plotd_mdsur_obs3_mar.pdf")
plotd_ch2(mdsum_mar)
dev.off()


#####################################
# Tables with results- % Rejections #
#####################################

mdsum_tab <- tab_sum(mdsum)
print(xtable::xtable(mdsum_tab, digits=c(0,3,0,1,1,1,1,1)), include.rownames=FALSE)


mdrsum_tab <- tab_sum(mdrsum)
print(xtable::xtable(mdrsum_tab, digits=c(0,3,0,1,1,1,1,1)), include.rownames=FALSE)


mdsum_mcar_tab <- tab_sum(mdsum_mcar)
print(xtable::xtable(mdsum_mcar_tab, digits=c(0,3,0,1,1,1,1,1)), include.rownames=FALSE)

mdsum_mar_tab <- tab_sum(mdsum_mar)
print(xtable::xtable(mdsum_mar_tab, digits=c(0,3,0,1,1,1,1,1)), include.rownames=FALSE)

#####################################
# Tables with results- Missing Info #
#####################################
minfo_tab <- tab_minfo(minfo)
print(xtable::xtable(minfo_tab, digits=c(0,3,1,0,4)), include.rownames=FALSE)

minfor_tab <- tab_minfo(minfor)
print(xtable::xtable(minfor_tab, digits=c(0,3,1,0,4)), include.rownames=FALSE)

minfo_tab_mcar <- tab_minfo(minfo_mcar, subj_miss = T)
print(xtable::xtable(minfo_tab_mcar, digits=c(0,3,1,0,4,4)), include.rownames=FALSE)

minfo_tab_mar <- tab_minfo(minfo_mar, subj_miss = T)
print(xtable::xtable(minfo_tab_mar, digits=c(0,3,1,0,4,4)), include.rownames=FALSE)

