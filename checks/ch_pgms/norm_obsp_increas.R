library(dplyr)

x0 <- readRDS('results/mdsu_obs3_sc1_cart_min2.rds')
x1 <- readRDS('checks/ch_outs/obs10_cart.rds')

norm3 <- readRDS('checks/ch_outs/obs3_norm.rds')
norm10 <- readRDS('checks/ch_outs/obs10_norm.rds')
norm20 <- readRDS('checks/ch_outs/obs20_norm.rds')
norm50 <- readRDS('checks/ch_outs/obs50_norm.rds')
norm60 <- readRDS('checks/ch_outs/obs60_norm.rds')
norm80 <- readRDS('checks/ch_outs/obs80_norm.rds')


cart_obs3 <- x0%>%
  purrr::map_df(.f = function(x) x$ct_des)%>%
  dplyr::filter(sur=='mi')%>%
  summarise_at(c('mean_l', 'se_l', 'b', 'ubar', 't'), 'mean')%>%
  dplyr::mutate(obs = 9, meth = 'cart')

norm_obs3 <- norm3%>%
  purrr::map_df(.f = function(x) x$md)%>%
  dplyr::filter(sur=='mi')%>%
  summarise_at(c('mean_l', 'se_l', 'b', 'ubar', 't'), 'mean')%>%
  dplyr::mutate(obs = 9, meth = 'norm')

norm_obs10 <- norm10%>%
  purrr::map_df(.f = function(x) x$md)%>%
  dplyr::filter(sur=='mi')%>%
  summarise_at(c('mean_l', 'se_l', 'b', 'ubar', 't'), 'mean')%>%
  dplyr::mutate(obs = 30, meth = 'norm')

norm_obs20 <- norm20%>%
  purrr::map_df(.f = function(x) x$md)%>%
  dplyr::filter(sur=='mi')%>%
  summarise_at(c('mean_l', 'se_l', 'b', 'ubar', 't'), 'mean')%>%
  dplyr::mutate(obs = 60, meth = 'norm')

norm_obs50 <- norm50%>%
  purrr::map_df(.f = function(x) x$md)%>%
  dplyr::filter(sur=='mi')%>%
  summarise_at(c('mean_l', 'se_l', 'b', 'ubar', 't'), 'mean')%>%
  dplyr::mutate(obs = 150, meth = 'norm')



all_comp <- dplyr::bind_rows(cart_obs3, norm_obs3, norm_obs10, norm_obs20, norm_obs50)%>%
  dplyr::select(meth, obs, mean_l, se_l, t, b, ubar)


print(xtable::xtable(all_comp, digits = c(0,0,0,3,3,5,5,5)), include.rownames=FALSE)

