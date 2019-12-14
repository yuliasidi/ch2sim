library(dplyr)

setting <- readRDS("setting.rds")

pc <- setting$pc[setting$set_n==1]
pt <- setting$pt[setting$set_n==1]
n_obs <- setting$n_obs[setting$set_n==1]
cor_xl <- setting$cor_xl[setting$set_n==1]

x1_m50 <- readRDS("results/mcar/mdsu_obs5_smcar_sc1_cart_sm50.rds")

x1 <- readRDS("results/mcar/mdsu_obs5_smcar_sc1_cart.rds")

x2 <- x1%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')

x2_m50 <- x1_m50%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')

check_mcar <- bind_rows(x2%>%
                         dplyr::filter(sur=='mi')%>%
                         dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                                          n_sim = n())%>%
                         dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl,
                                       ni_desp = ni_desy/n_sim, m = 20),
                      x2_m50%>%
                         dplyr::filter(sur=='mi')%>%
                         dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                                          n_sim = n())%>%
                         dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl,
                                       ni_desp = ni_desy/n_sim, m = 50)
)


saveRDS(check_mcar, "checks/ch_outs/ch_mcar_ms.rds")

###########################
## Same as above for MAR ##
###########################

x1_m50 <- readRDS("results/mar/mdsu_obs5_smar_sc1_cart_sm50.rds")

x1 <- readRDS("results/mar/mdsu_obs5_smar_sc1_cart.rds")

x2 <- x1%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')

x2_m50 <- x1_m50%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')

check_mar <- bind_rows(x2%>%
                          dplyr::filter(sur=='mi')%>%
                          dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                                           n_sim = n())%>%
                          dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl,
                                        ni_desp = ni_desy/n_sim, m = 20),
                        x2_m50%>%
                          dplyr::filter(sur=='mi')%>%
                          dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                                           n_sim = n())%>%
                          dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl,
                                        ni_desp = ni_desy/n_sim, m = 50)
)


saveRDS(check_mar, "checks/ch_outs/ch_mar_ms.rds")

