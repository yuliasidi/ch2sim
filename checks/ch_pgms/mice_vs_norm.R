library(dplyr, warn.conflicts = F, quietly = T)
library(ggplot2, warn.conflicts = F, quietly = T)

q_res <- function(result){
  result%>%
    purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
    dplyr::filter(sim_id <=1000)%>%

    dplyr::group_by(sur)%>%
    dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                     n_sim = n(),
                     md_n = mean(n_l),
                     m_sel = mean(se_l),
                     md_m2 = mean(mean_l),
                     pp = round(100*ni_desy/n_sim,1))
}

x1_cart <- readRDS("results/mdsu_obs3_sc1_cart.rds")
x1_norm <- readRDS("results/mdsu_obs3_sc1_norm.rds")
x1_pnorm <- readRDS("results/mdsu_obs3_sc1_pnorm.rds")
x1_pnorm1 <- readRDS("results/mdsu_obs3_sc1_pnorm1.rds")
x1_pnorm2 <- readRDS("results/mdsu_obs3_sc1_pnorm2.rds")
x1_pnorm1_m50 <- readRDS("results/mdsu_obs3_sc1_pnorm1_m50.rds")
x1_pnorm1_m100 <- readRDS("results/mdsu_obs3_sc1_pnorm1_m100.rds")

x1_pnorm_chains <- readRDS("results/mdsu_obs3_sc1_pnorm_chains1.rds")


q_res(x1_cart)
q_res(x1_pnorm)
q_res(x1_norm)

q_res(x1_pnorm1)
q_res(x1_pnorm2)
q_res(x1_pnorm1_m50)
q_res(x1_pnorm1_m100)
q_res(x1_pnorm_chains)


diff_l <- x1_pnorm2 %>%
  purrr::keep(.p = function(x) length(x)>0) %>%
  purrr::map_dbl(.f=function(x) {
    xx <- x$diff_last
    xx[[1]]})

sum(as.numeric(399999 == diff_l))

prob_id <- which(399999 == diff_l)%>%
  tibble::enframe()%>%
  dplyr::rename(sim_id = value)

tmp <- x1_pnorm2 %>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
  dplyr::anti_join(prob_id, by = 'sim_id')

tmp%>% dplyr::group_by(sur)%>%
  dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                   n_sim = n(),
                   md_n = mean(n_l),
                   md_m2 = mean(mean_l),
                   pp = round(100*ni_desy/n_sim,1))  


check_ub <- function(result){
  result%>%
    purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
    dplyr::filter(sim_id <= 1000)%>%
    dplyr::filter(sur == 'mi')%>%
    dplyr::select(mean_l, ubar, b, t, v)%>%
    dplyr::summarise_all(.funs = mean)
  
}

check_ub(x1_cart)
check_ub(x1_norm)
check_ub(x1_pnorm1)
check_ub(x1_pnorm2)
check_ub(x1_pnorm1_m50)
check_ub(x1_pnorm1_m100)
check_ub(x1_pnorm_chains)


check_ub(x1_cart, nm = 'cart')%>%
  summarise_at(.vars = c('ubar', 'b'), .funs = mean)

check_ub(x1_norm, nm = 'cart')%>%
  summarise_at(.vars = c('ubar', 'b'), .funs = mean)
check_ub(x1_pnorm1, nm = 'cart')%>%
  summarise_at(.vars = c('ubar', 'b'), .funs = mean)
check_ub(x1_pnorm_chains, nm = 'cart')%>%
  summarise_at(.vars = c('ubar', 'b'), .funs = mean)

#ubar
check_ub(x1_cart, nm = 'cart')%>%
  dplyr::bind_rows(check_ub(x1_norm, nm = 'norm'))%>%
  dplyr::bind_rows(check_ub(x1_pnorm1k, nm = 'pnorm1k'))%>%
  dplyr::bind_rows(check_ub(x1_pnorm5k, nm = 'pnorm5k'))%>%
  ggplot(aes(x = sim, y = b)) +
  geom_point() +
  facet_wrap(~name, nrow = 4, ncol = 1, scales = 'free') +
  scale_x_discrete(breaks = seq(1,5000,500)) +
  labs(x = 'Simulation ID')


check_ub(x1_cart, nm = 'cart')%>%
  dplyr::bind_rows(check_ub(x1_norm, nm = 'norm'))%>%
  dplyr::bind_rows(check_ub(x1_pnorm1k, nm = 'pnorm1k'))%>%
  dplyr::bind_rows(check_ub(x1_pnorm5k, nm = 'pnorm5k'))%>%
  dplyr::mutate(sim = as.numeric(sim))%>%
  dplyr::filter(sim<100)%>%
  ggplot(aes(x = b)) +
  geom_histogram(bins = 50) +
  facet_wrap(~name, nrow = 4, ncol = 1)
  

#b
check_ub(x1_cart, nm = 'cart')%>%
  dplyr::bind_rows(check_ub(x1_norm, nm = 'norm'))%>%
  dplyr::bind_rows(check_ub(x1_pnorm1k, nm = 'pnorm1k'))%>%
  dplyr::bind_rows(check_ub(x1_pnorm5k, nm = 'pnorm5k'))%>%
  ggplot(aes(x = sim, y = b)) +
  geom_point() +
  facet_wrap(~name, nrow = 4, ncol = 1, scales = 'free')


  