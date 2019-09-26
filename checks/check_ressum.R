library(dplyr)
library(purrr)

setting <- readRDS("setting.rds")

ll <- seq(1,12,1)

#check the mean number of observed MDs
map_df(ll, 
       .f = function(sc) {
         x1 <- readRDS(list.files("results", paste0("mdsu_obs3_sc", sc, ".rds"), full.names = T))
         x2 <- x1%>%
           purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')
         
         x2%>%
           dplyr::filter(sur=='obs')%>%
           dplyr::summarise(mean(n_l))
       })

#check the mean mean_lambda of observed vs all MDs
ch_ml <- map_df(ll, 
       .f = function(sc) {
         x1 <- readRDS(list.files("results", paste0("mdsu_obs3_sc", sc, ".rds"), full.names = T))
         x2 <- x1%>%
           purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')
         
         x2%>%
           dplyr::filter(sur%in%c('obs', 'all'))%>%
           dplyr::group_by(sur)%>%
           dplyr::summarise(ml = mean(mean_l))%>%
           dplyr::mutate(sc = sc)
       })

ch_ml%>%tidyr::spread(key = 'sur', value = 'ml')

