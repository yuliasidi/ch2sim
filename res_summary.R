library(dplyr)
library(purrr)

setting <- readRDS("setting.rds")

################################
# All patient data is observed #
################################

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
                              n_sim = n(),
                              md_n = mean(n_l),
                              md_m2 = mean(mean_l))%>%
             dplyr::mutate(set_n = sc)%>%
             dplyr::left_join(setting, by = "set_n")%>%
             dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl)
           
         })

res_sum%>%dplyr::select(sur, md_m2, set_n)%>%filter(sur%in%c('all', 'obs', 'mi'))%>%spread(key='sur', value='md_m2')
saveRDS(res_sum, "sums/sum_mdsu_obs3.rds")
  


#########################################################################
# All patient data is observed  and subsample of MDs is totally random  #
#########################################################################

ll <- seq(1,12,1)

res_sum1 <-
  map_df(ll, 
         .f = function(sc) {
           x1 <- readRDS(list.files("results", paste0("mdsu_obs3r_sc", sc, ".rds"), full.names = T))
           x2 <- x1%>%
             purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')
           
           x2%>%
             dplyr::group_by(sur)%>%
             dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                              n_sim = n(),
                              md_n = mean(n_l),
                              md_m2 = mean(mean_l))%>%
             dplyr::mutate(set_n = sc)%>%
             dplyr::left_join(setting, by = "set_n")%>%
             dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl)
           
         })

res_sum1%>%dplyr::select(sur, md_m2, set_n)%>%filter(sur%in%c('all', 'obs', 'mi'))%>%spread(key='sur', value='md_m2')

saveRDS(res_sum1, "sums/sum_mdsu_obs3r.rds")

################################
# Patient data are MCAR        #
################################

ll <- seq(1,12,1)

do_check <- map_df(ll, 
       .f = function(sc) {
         x1 <- readRDS(list.files("results/mcar", paste0("mdsu_obs3_smcar_sc", sc, ".rds"), full.names = T))
         x2 <- x1%>% purrr::map_df(.f=function(x) x$do_ch, .id = 'sim')%>%
           group_by(trt)%>%summarise(mean(do))
         
       })

x_check <- map_df(ll, 
                   .f = function(sc) {
                     x1 <- readRDS(list.files("results/mcar", paste0("mdsu_obs3_smcar_sc", sc, ".rds"), full.names = T))
                     x2 <- x1%>%
                       purrr::map_df(.f=function(x) x$p_ch, .id = 'sim')%>%
                       group_by(trt,r)%>%summarise_at(.vars = c('xmean', 'pcca', 'pfull'), .funs = 'mean')%>%
                       mutate(set_n = sc)%>%
                       left_join(setting, by = 'set_n')
                     
                   })


res_sum2 <-
  map_df(ll, 
         .f = function(sc) {
           x1 <- readRDS(list.files("results/mcar", paste0("mdsu_obs3_smcar_sc", sc, ".rds"), full.names = T))
           x2 <- x1%>%
             purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')
           
           x2%>%
             dplyr::group_by(sur)%>%
             dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                              n_sim = n(),
                              md_n = mean(n_l),
                              md_m2 = mean(mean_l))%>%
             dplyr::mutate(set_n = sc)%>%
             dplyr::left_join(setting, by = "set_n")%>%
             dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl)
           
         })

res_sum2%>%dplyr::select(sur, md_m2, set_n)%>%filter(sur%in%c('all', 'obs', 'mi'))%>%spread(key='sur', value='md_m2')

saveRDS(res_sum2, "sums/sum_mdsu_obs3_smcar.rds")


################################
# Patient data are MAR        #
################################

ll <- seq(2,8,1)

do_check <- map_df(ll, 
                   .f = function(sc) {
                     x1 <- readRDS(list.files("results/mar", paste0("mdsu_obs3_smar_sc", sc, ".rds"), full.names = T))
                     x2 <- x1%>% purrr::map_df(.f=function(x) x$do_ch, .id = 'sim')%>%
                       group_by(trt)%>%summarise(mean(do))
                     
                   })

x_check <- map_df(ll, 
                  .f = function(sc) {
                    x1 <- readRDS(list.files("results/mar", paste0("mdsu_obs3_smar_sc", sc, ".rds"), full.names = T))
                    x2 <- x1%>%
                      purrr::map_df(.f=function(x) x$p_ch, .id = 'sim')%>%
                      group_by(trt,r)%>%summarise_at(.vars = c('xmean', 'pcca', 'pfull'), .funs = 'mean')
                    
                  })


res_sum3 <-
  map_df(ll, 
         .f = function(sc) {
           x1 <- readRDS(list.files("results/mar", paste0("mdsu_obs3_smar_sc", sc, ".rds"), full.names = T))
           x2 <- x1%>%
             purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')
           
           x2%>%
             dplyr::group_by(sur)%>%
             dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                              n_sim = n(),
                              md_n = mean(n_l))%>%
             dplyr::mutate(set_n = sc)%>%
             dplyr::left_join(setting, by = "set_n")%>%
             dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl)
           
         })

saveRDS(res_sum3, "sums/sum_mdsu_obs3_smar.rds")


