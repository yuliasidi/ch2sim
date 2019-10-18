#check for larger estimated differences in proportions, when subject level data are MCAR

x1 <- readRDS("results/mcar/mdsu_obs3_smcar_test1.rds")
x2 <- readRDS("results/mcar/mdsu_obs3_smcar_test2.rds")
x3 <- readRDS("results/mcar/mdsu_obs3_smcar_test3.rds")
x4 <- readRDS("results/mcar/mdsu_obs3_smcar_test4.rds")

test_res <- function(dt, pt){

  dt%>%
    purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
    dplyr::filter(sur %in% c('all', 'mi', 'obs'))%>%
    dplyr::group_by(sur)%>%
    dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                     n_sim = n(),
                     n_pi = ni_desy/n_sim)%>%
    dplyr::mutate(pt = pt)  
}

test_res(x1, 0.9)
test_res(x2, 0.8)
test_res(x3, 0.75)
test_res(x4, 0.85)

  