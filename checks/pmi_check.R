library(dplyr)

#check bias in mi, cca (mcar/mar) vs full data estimates for difference in proportions

#######################
# MCAR ################
#######################

ll <- seq(1,1,1)

bias_mcar <-
  map_df(ll, 
         .f = function(sc) {
           x1 <- readRDS(list.files("results/mcar", paste0("mdsu_obs3_smcar_sc", sc, "_newx.rds"), full.names = T))
           x2 <- x1%>%
             purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
             dplyr::filter(sur %in% c('all', 'mi', 'obs'))%>%
             dplyr::select(sim, sur, phat_d)%>%
             tidyr::spread(key = 'sur', value = 'phat_d')%>%
             dplyr::mutate(bias_mi = all - mi,
                           bias_cca = all - obs)
          

           
          x2%>%
             dplyr::summarise_at(.vars = c('bias_mi', "bias_cca"), .funs = 'mean')

         })

#######################
# MAR #################
#######################

ll <- seq(1,1,1)

bias_mar <-
  map_df(ll, 
         .f = function(sc) {
           x1 <- readRDS(list.files("results/mar", paste0("mdsu_obs3_smar_sc", sc, ".rds"), full.names = T))
           x2 <- x1%>%
             purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
             dplyr::filter(sur %in% c('all', 'mi', 'obs'))%>%
             dplyr::select(sim, sur, phat_d)%>%
             tidyr::spread(key = 'sur', value = 'phat_d')%>%
             dplyr::mutate(bias_mi = all - mi,
                           bias_cca = all - obs)
           
           
           
           x2%>%
             dplyr::summarise_at(.vars = c('all','obs','bias_mi', "bias_cca"), .funs = 'mean')
        
           
         })
