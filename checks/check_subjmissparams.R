#check missingness parameters for subject level missingness
library(tidyr, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(MASS, warn.conflicts = F, quietly = T)
library(bin2mi)
library(m2imp)

pc <- 0.8
pt <- 0.825
n_obs <- 250
do_rate <- 0.2

x1 <- parallel::mclapply(X = 1:1000, 
                         mc.cores = 7,
                         FUN= function(i){
                           
  set.seed(200*1 + i)
  dt0 <- bin2mi::dt_p2(n = n_obs, pc = pc, pt = pt, add_xcont = TRUE)
  
  
  #impose missingness in the subject level data based on x
  dt0_miss <- dt_miss_pert(dt0, do_ratec = do_rate, do_ratet = do_rate, bxmc = -0.015, bxmt = 0.02)
  
  dt0_miss%>%
  group_by(trt)%>%
  summarise(pcca = mean(y, na.rm=T))
  
  do_ch <- dt0_miss%>%group_by(trt)%>%summarise(do=mean(r))
  p_bias <- dt0_miss%>%
  group_by(trt)%>%
  summarise(pcca = mean(y, na.rm=T))
  
  ch <- dt0%>%
  group_by(trt)%>%
  summarise(pcca = mean(y, na.rm=T))
  
  
  out <- list(do_ch, p_bias, ch )%>%
  purrr::set_names("do_ch", "p_bias", "ch")
  return(out)
   
                           
                         })

# x1%>%
#   purrr::map_df(.f=function(x) x$ch, .id = 'sim')%>%
#   group_by(trt, y)%>%
#   summarise(mean(mx))

x1%>%
  purrr::map_df(.f=function(x) x$do_ch, .id = 'sim')%>%
  group_by(trt)%>%
  summarise(mean(do))


x1%>%
  purrr::map_df(.f=function(x) x$p_bias, .id = 'sim')%>%
  group_by(trt)%>%
  summarise(mean(pcca))

x1%>%
  purrr::map_df(.f=function(x) x$ch, .id = 'sim')%>%
  group_by(trt)%>%
  summarise(mean(pcca))

