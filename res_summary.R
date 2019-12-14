library(dplyr)
library(purrr)
library(tidyr)

setting <- readRDS("setting.rds")

################################
# All patient data is observed #
################################

ll <- seq(1,12,1)

res_sum <-
  map_df(ll, 
         .f = function(sc) {
           x1 <- readRDS(list.files("results", paste0("mdsu_obs3_sc", sc, "_cart.rds"), full.names = T))
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

ch_ch <- map_df(ll, 
       .f = function(sc) {
         x1 <- readRDS(list.files("results", paste0("mdsu_obs3_sc", sc, "_cart.rds"), full.names = T))
         
        x1%>%
          purrr::map_df(.f=function(x) x$ch, .id = 'sim')%>%
          group_by(x_20)%>%
          summarise_at(.vars = c('ml', 'rm', 'xm'), .funs = 'mean')})


saveRDS(res_sum, "sums/sum_mdsu_obs3_cart.rds")


minfo <-
  map_df(ll, 
         .f = function(sc) {
   x1 <- readRDS(list.files("results", paste0("mdsu_obs3_sc", sc, "_cart.rds"), full.names = T))
   x2 <- x1%>%
    purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
     filter(sur%in%c('mi'))%>%
     mutate(minfo = b/(ubar + b))%>%
     dplyr::select(sim, minfo)%>%
     dplyr::mutate(set_n = sc)%>%
     dplyr::left_join(setting, by = "set_n")%>%
     dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl)
           
  })

saveRDS(minfo, "sums/minfo_mdsu_obs3_cart.rds")


#########################################################################
# All patient data is observed  and subsample of MDs is totally random  #
#########################################################################

ll <- seq(1,12,1)

res_sum1 <-
  map_df(ll, 
         .f = function(sc) {
           x1 <- readRDS(list.files("results/mdsu_obs3r", paste0("mdsu_obs3r_sc", sc, "_cart.rds"), full.names = T))
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

saveRDS(res_sum1, "sums/sum_mdsu_obs3r_cart.rds")


minfo1 <-
  map_df(ll, 
         .f = function(sc) {
           x1 <- readRDS(list.files("results/mdsu_obs3r", paste0("mdsu_obs3r_sc", sc, "_cart.rds"), full.names = T))
           x2 <- x1%>%
             purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
             filter(sur%in%c('mi'))%>%
             mutate(minfo = b/(ubar + b))%>%
             dplyr::select(sim, minfo)%>%
             dplyr::mutate(set_n = sc)%>%
             dplyr::left_join(setting, by = "set_n")%>%
             dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl)
           
         })

saveRDS(minfo1, "sums/minfo_mdsu_obs3r_cart.rds")

################################
# Patient data are MCAR        #
################################

ll <- seq(1,12,1)

do_check <- 
map_df(ll, 
       .f = function(sc) {
         x1 <- readRDS(list.files("results/mcar", paste0("mdsu_obs3_smcar_sc", sc, "_cart.rds"), full.names = T))
         x2 <- x1%>% purrr::map_df(.f=function(x) x$do_ch, .id = 'sim')%>%
           group_by(trt)%>%summarise(mean(do))
         
       })

x_check <- 
map_df(ll, 
       .f = function(sc) {
         x1 <- readRDS(list.files("results/mcar", paste0("mdsu_obs3_smcar_sc", sc, "_cart.rds"), full.names = T))
         x2 <- x1%>%
           purrr::map_df(.f=function(x) x$p_ch, .id = 'sim')%>%
           group_by(trt,r)%>%summarise_at(.vars = c('xmean', 'pcca', 'pfull'), .funs = 'mean')%>%
           mutate(set_n = sc)%>%
           left_join(setting, by = 'set_n')
         
      })


res_sum2 <-
  map_df(ll, 
         .f = function(sc) {
           x1 <- readRDS(list.files("results/mcar", paste0("mdsu_obs3_smcar_sc", sc, "_cart.rds"), full.names = T))
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

saveRDS(res_sum2, "sums/sum_mdsu_obs3_smcar_cart.rds")


minfo2 <-
  map_df(ll, 
         .f = function(sc) {
           x1 <- readRDS(list.files("results/mcar", paste0("mdsu_obs3_smcar_sc", sc, "_cart.rds"), full.names = T))
           x2 <- x1%>%
             purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
             filter(sur%in%c('mi'))%>%
             mutate(minfo = b/(ubar + b),
                    msinfo = b_subj/(u_subj + b_subj))%>%
             dplyr::select(sim, minfo, msinfo)%>%
             dplyr::mutate(set_n = sc)%>%
             dplyr::left_join(setting, by = "set_n")%>%
             dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl)
           
         })

saveRDS(minfo2, "sums/minfo_mdsu_obs3_smcar_cart.rds")

################################
# Patient data are MAR        #
################################

ll <- seq(1,12,1)

do_check <- map_df(ll, 
                   .f = function(sc) {
                     x1 <- readRDS(list.files("results/mar", paste0("mdsu_obs3_smar_sc", sc, "_cart.rds"), full.names = T))
                     x2 <- x1%>% purrr::map_df(.f=function(x) x$do_ch, .id = 'sim')%>%
                       group_by(trt)%>%summarise(mean(do))
                     
                   })

x_check <- map_df(ll, 
                  .f = function(sc) {
                    x1 <- readRDS(list.files("results/mar", paste0("mdsu_obs3_smar_sc", sc, "_cart.rds"), full.names = T))
                    x2 <- x1%>%
                      purrr::map_df(.f=function(x) x$p_ch, .id = 'sim')%>%
                      group_by(trt,r)%>%summarise_at(.vars = c('xmean', 'pcca', 'pfull'), .funs = 'mean')%>%
                      mutate(set_n = sc)%>%
                      left_join(setting, by = 'set_n')
                    
                    
                  })


res_sum3 <-
  map_df(ll, 
         .f = function(sc) {
           x1 <- readRDS(list.files("results/mar", paste0("mdsu_obs3_smar_sc", sc, "_cart.rds"), full.names = T))
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

saveRDS(res_sum3, "sums/sum_mdsu_obs3_smar_cart.rds")

minfo3 <-
  map_df(ll, 
         .f = function(sc) {
           x1 <- readRDS(list.files("results/mar", paste0("mdsu_obs3_smar_sc", sc, "_cart.rds"), full.names = T))
           x2 <- x1%>%
             purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
             filter(sur%in%c('mi'))%>%
             mutate(minfo = b/(ubar + b),
                    msinfo = b_subj/(u_subj + b_subj))%>%
             dplyr::select(sim, minfo, msinfo)%>%
             dplyr::mutate(set_n = sc)%>%
             dplyr::left_join(setting, by = "set_n")%>%
             dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl)
           
         })

saveRDS(minfo3, "sums/minfo_mdsu_obs3_smar_cart.rds")



