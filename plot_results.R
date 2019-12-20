library(dplyr)
library(ggplot2)

source("funs/plot_ch2.R")
source("funs/plotd_ch2.R")
source("funs/tab_sum.R")
source("funs/tab_minfo.R")

setting <- readRDS("setting.rds")

mdsum <- readRDS("sums/sum_mdsu_obs3_cart.rds")
mdrsum <- readRDS("sums/sum_mdsu_obs3r_cart.rds")
mdsum_mcar <- readRDS("sums/sum_mdsu_obs3_smcar_cart.rds")
mdsum_mar <- readRDS("sums/sum_mdsu_obs3_smar_cart.rds")

minfo <- readRDS("sums/minfo_mdsu_obs3_cart.rds")
minfor <- readRDS("sums/minfo_mdsu_obs3r_cart.rds")
minfo_mcar <- readRDS("sums/minfo_mdsu_obs3_smcar_cart.rds")
minfo_mar <- readRDS("sums/minfo_mdsu_obs3_smar_cart.rds")


###############################
## Plots - Difference vs POP ##
###############################

pdf("plots/plotd_mdsur_obs3_cart.pdf")
plotd_ch2(mdsum)
dev.off()

pdf("plots/plotd_mdsur_obs3r_cart.pdf")
plotd_ch2(mdrsum)
dev.off()

pdf("plots/plotd_mdsur_obs3_mcar_cart.pdf")
plotd_ch2(mdsum_mcar)
dev.off()


pdf("plots/plotd_mdsur_obs3_mar_cart.pdf")
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

#combine mcar and mar
minfo_tab_both <- minfo_tab_mcar%>%
  dplyr::left_join(minfo_tab_mar%>%
                     dplyr::rename(mminfo_mar = mminfo,
                                   mmsinfo_mar = mmsinfo), by = c('pt', 'cor_xl', 'n_obs'))

print(xtable::xtable(minfo_tab_both, digits=c(0,3,1,0,4,4,4,4)), include.rownames=FALSE)


##################################################################################
# Min/Max differences per method across different scenarios/types of missingness #
##################################################################################

allres <- 
  bind_rows(mdsum%>%
            mutate(miss = 'm2'),
          mdrsum%>%
            mutate(miss = 'm2_r'),
          mdsum_mcar%>%
            mutate(miss = 'm2_smcar'),
          mdsum_mar%>%
            mutate(miss = 'm2_smar'))%>%
  mutate(ni_p = 100 * (ni_desy/n_sim))

allres%>% 
  dplyr::select(sur, set_n, ni_p, miss)%>%
  tidyr::spread(key = 'sur', value = 'ni_p')%>%
  dplyr::mutate(MI = all - mi, OBS = all - obs, MIN = all - `sing min`, MAX = all - `sing max`)%>%
  dplyr::select(set_n, miss, MI, OBS, MIN, MAX)%>%
  tidyr::gather(key = 'method', value = 'diff', -c(set_n, miss))%>%
  dplyr::group_by(method)%>%
  dplyr::summarise(min_diff = min(abs(diff)), max_diff = max(abs(diff)))

allres%>% 
  dplyr::select(sur, set_n, ni_p, miss)%>%
  tidyr::spread(key = 'sur', value = 'ni_p')%>%
  dplyr::mutate(MI = abs(all - mi), OBS = abs(all - obs), MIN = abs(all - `sing min`), MAX = abs(all - `sing max`))%>%
  dplyr::filter(miss %in% c('m2', 'm2_r'))%>%
  dplyr::summarise_at(.vars = c('MI','OBS', 'MAX', 'MIN'), .funs = c('min', 'max'))

#take a look at this data
tmp <- allres%>% 
  dplyr::select(sur, set_n, ni_p, miss, cor_xl)%>%
  dplyr::filter(cor_xl ==0.7)%>%
  tidyr::spread(key = 'sur', value = 'ni_p')%>%
  dplyr::mutate(MI = abs(all - mi), OBS = abs(all - obs), MIN = abs(all - `sing min`), MAX = abs(all - `sing max`))%>% 
  filter(miss %in% c('m2', 'm2_r'))

tmp1 <- allres%>% 
  dplyr::select(sur, set_n, ni_p, miss, cor_xl)%>%
  dplyr::filter(cor_xl ==0.7)%>%
  tidyr::spread(key = 'sur', value = 'ni_p')%>%
  dplyr::mutate(MI = abs(all - mi), OBS = abs(all - obs), MIN = abs(all - `sing min`), MAX = abs(all - `sing max`))%>% 
  filter(miss %in% c('m2_smcar'))

tmp2 <- allres%>% 
  dplyr::select(sur, set_n, ni_p, miss, cor_xl)%>%
  dplyr::filter(cor_xl ==0.4)%>%
  tidyr::spread(key = 'sur', value = 'ni_p')%>%
  dplyr::mutate(MI = abs(all - mi), OBS = abs(all - obs), MIN = abs(all - `sing min`), MAX = abs(all - `sing max`))%>% 
  filter(miss %in% c('m2_smar'))


allres%>% 
  dplyr::select(sur, set_n, ni_p, miss)%>%
  tidyr::spread(key = 'sur', value = 'ni_p')%>%
  dplyr::mutate(MI = all - mi, OBS = all - obs, MIN = all - `sing min`, MAX = all - `sing max`)%>%
  dplyr::filter(miss %in% c('m2_smcar'))%>%
  dplyr::summarise_at(.vars = c('MI','OBS', 'MAX', 'MIN'), .funs = c('min', 'max'))
