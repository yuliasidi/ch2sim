library(dplyr)

#check bias cca (mcar/mar) vs full data estimates for difference in proportions

#######################
# MCAR ################
#######################

ll <- seq(1,12,1)

bias_mcar <-
  map_df(ll, 
         .f = function(sc) {
           x1 <- readRDS(list.files("results/mcar", paste0("mdsu_obs3_smcar_sc", sc, ".rds"), full.names = T))
           x2 <- x1%>%
             purrr::map_df(.f=function(x) x$p_ch, .id = 'sim')
          
           cca_diff <-x2%>%
             dplyr::filter(is.na(pcca)==F)%>%
             dplyr::select(sim, trt, pcca)%>%
             tidyr::spread(key = 'trt', value='pcca')%>%
             dplyr::mutate(diff_cca = c - t)%>%
             dplyr::select(sim, diff_cca)
           
           full_diff <- x2%>%
             dplyr::filter(is.na(pcca)==F)%>%
             dplyr::select(sim, trt, pfull)%>%
             tidyr::spread(key = 'trt', value='pfull')%>%
             dplyr::mutate(diff_full = c - t)%>%
             dplyr::select(sim, diff_full)
           
           
           out <- full_diff%>%
             dplyr::left_join(cca_diff, by = 'sim')%>%
             dplyr::mutate(diff_bias = diff_cca - diff_full)%>%
             dplyr::summarize(mean_bias = mean(diff_bias))
        return(out)
           
         })

#######################
# MAR #################
#######################

ll <- seq(1,12,1)

bias_mar <-
  map_df(ll, 
         .f = function(sc) {
           x1 <- readRDS(list.files("results/mar", paste0("mdsu_obs3_smar_sc", sc, ".rds"), full.names = T))
           x2 <- x1%>%
             purrr::map_df(.f=function(x) x$p_ch, .id = 'sim')
           
           cca_diff <-x2%>%
             dplyr::filter(is.na(pcca)==F)%>%
             dplyr::select(sim, trt, pcca)%>%
             tidyr::spread(key = 'trt', value='pcca')%>%
             dplyr::mutate(diff_cca = c - t)%>%
             dplyr::select(sim, diff_cca)
           
           full_diff <- x2%>%
             dplyr::filter(is.na(pcca)==F)%>%
             dplyr::select(sim, trt, pfull)%>%
             tidyr::spread(key = 'trt', value='pfull')%>%
             dplyr::mutate(diff_full = c - t)%>%
             dplyr::select(sim, diff_full)
           
           
           out <- full_diff%>%
             dplyr::left_join(cca_diff, by = 'sim')%>%
             dplyr::mutate(diff_bias = diff_cca - diff_full)%>%
             dplyr::summarize(mean_bias = mean(diff_bias))
           return(out)
           
         })
