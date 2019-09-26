
x1_10k <- readRDS("results//mdsu_obs3_sc1_x10k.rds")

x1 <- readRDS("results//mdsu_obs3_sc1.rds")

x2 <- x1%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')

x2_10k <- x1_10k%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')

check_10k <- bind_rows(x2%>%
  dplyr::group_by(sur)%>%
  dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                   n_sim = n())%>%
  dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl,
                ni_desp = ni_desy/n_sim),
x2_10k%>%
  dplyr::group_by(sur)%>%
  dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                   n_sim = n())%>%
  dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl,
                ni_desp = ni_desy/n_sim))

saveRDS(check_10k, "checks/ch_10k_vs_1k.rds")
