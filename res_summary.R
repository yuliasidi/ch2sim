library(dplyr)
library(purrr)

setting <- readRDS("setting.rds")

ll <- seq(1,12,1)

res_sum <-
  map_df(ll, 
         .f = function(sc) {
           x1 <- readRDS(list.files("results", paste0("mdsu_obs3_sc", sc, ".rds"), full.names = T))
           x2 <- x1%>%
             purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')
           
           x2%>%
             dplyr::group_by(sur)%>%
             dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                              n_sim = n())%>%
             dplyr::mutate(set_n = sc)%>%
             dplyr::left_join(setting, by = "set_n")%>%
             dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl)
           
         })

saveRDS(res_sum, "results/sum_mdsu_obs3.rds")
  