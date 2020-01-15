setting <- readRDS("setting.rds")

pc <- setting$pc[setting$set_n==1]
pt <- setting$pt[setting$set_n==1]
n_obs <- setting$n_obs[setting$set_n==1]
cor_xl <- setting$cor_xl[setting$set_n==1]

x1_10k <- readRDS("results/mdsu_obs3_sc1_cart_min2_10k.rds")

x1 <- readRDS("results/mdsu_obs3_sc1_cart_min2.rds")

x2 <- x1%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')

x2_10k <- x1_10k%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')

check_10k <- bind_rows(
  x2%>%
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

check_10k <- check_10k%>%
  dplyr::select(n_sim, sur, ni_desp)%>%
  tidyr::spread(key = 'n_sim', value = 'ni_desp')

saveRDS(check_10k, "checks/ch_outs/ch_10k_vs_5k.rds")
