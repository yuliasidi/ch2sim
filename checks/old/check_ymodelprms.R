library(dplyr)
library(furrr)

setting <- readRDS("setting.rds")

future::plan(multicore)

#check_phats <- 
  setting%>%
  tibble::as.tibble()%>%
  dplyr::mutate(test = purrr::pmap(list(pc = pc, pt = pt, n_obs = n_obs), .f = function(nsim, pc, pt, n_obs){
    
    furrr::future_map_dfr(1:100,.f =function(x, pc, pt, n_obs){
      set.seed(200*1 + x)
      dt_subj(bx = -0.04, pc = pc, pt = pt, n_obs = n_obs)%>%
      dplyr::group_by(trt)%>%summarise(my = mean(y))
                               
                             },pc = pc, pt = pt, n_obs = n_obs,.id = 'iter')
  }))%>%
  tidyr::unnest()%>%
  group_by(set_n, trt)%>%
  summarise(mmy = mean(my))%>%
  spread(key = 'trt', value = 'mmy')%>%
  left_join(setting%>%dplyr::select(set_n, pc, pt, n_obs), by = 'set_n')

saveRDS(check_phats, "checks/check_phats.rds")

#check_xyrel <- 
  setting%>%
  tibble::as.tibble()%>%
  dplyr::mutate(test = purrr::pmap(list(pc = pc, pt = pt, n_obs = n_obs), .f = function(nsim, pc, pt, n_obs){
    
    furrr::future_map_dfr(1:1000,.f =function(x,pc, pt, n_obs){
      set.seed(200*1 + x)
      dt_subj(bx = -0.04, pc = pc, pt = pt, n_obs = n_obs)%>%
        dplyr::group_by(trt, y)%>%summarise(mx = mean(x))
      
    },pc = pc, pt = pt, n_obs = n_obs,.id = 'iter')
  }))%>%
  tidyr::unnest()%>%
  group_by(set_n, trt, y)%>%
  summarise(mmx = mean(mx))%>%
  spread(key = 'trt', value = 'mmx')%>%
  left_join(setting%>%dplyr::select(set_n, pc, pt, n_obs), by = 'set_n')

saveRDS(check_xyrel, "checks/check_xyrel.rds")
