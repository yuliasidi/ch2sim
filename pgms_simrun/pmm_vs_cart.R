sc1_pmm <- readRDS('results/mdsu_obs3_sc1_pmm.rds')
sc1_cart <- readRDS('results/mdsu_obs3_sc1_cart_min2.rds')


sc1_pmm%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
  dplyr::filter(sur=='mi')%>%
  dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                   md_m2 = mean(mean_l),
                   md_se = mean(se_l),
                   mu = mean(ubar),
                   mb = mean(b))
sc1_cart%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
  dplyr::filter(sur=='mi')%>%
  dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                   md_m2 = mean(mean_l),
                   md_se = mean(se_l),
                   mu = mean(ubar),
                   mb = mean(b))
