library(dplyr)

setting <- readRDS("setting.rds")

pc <- setting$pc[setting$set_n==1]
pt <- setting$pt[setting$set_n==1]
n_obs <- setting$n_obs[setting$set_n==1]
cor_xl <- setting$cor_xl[setting$set_n==1]

x1_m50 <- readRDS("results/mdsu_obs3_sc1_m50.rds")
x1_m20 <- readRDS("results/mdsu_obs3_sc1.rds")

x1_m10 <- readRDS("results/mdsu_obs3_sc1_m10.rds")

x2_m10 <- x1_m10%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')

x2_m20 <- x1_m20%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')

x2_m50 <- x1_m50%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')

check_m <- bind_rows(x2_m10%>%
  dplyr::filter(sur=='mi')%>%
  dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                   n_sim = n())%>%
  dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl,
                ni_desp = ni_desy/n_sim, m = 10),
    x2_m20%>%
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
                  ni_desp = ni_desy/n_sim, m = 50))
  


saveRDS(check_m, "checks/ch_m.rds")
