#check missingness parameters for subject level missingness

pc <- 0.8
pt <- 0.825
n_obs <- 250
do_rate <- 0.2

x1 <- parallel::mclapply(X = 1:10000, 
                         mc.cores = 7,
                         FUN= function(x){
                           
set.seed(200*1 + x)
dt0 <- bin2mi::dt_p2(n = n_obs, pc = pc, pt = pt, add_xcont = T)

#impose missingness in the subject level data based on x
b1_subj <- -0.2
b3_subj <-  0.22
int_subj <- log(do_rate/(1 - do_rate)) - b1_subj*mean(dt0$x) - b3_subj*mean(dt0$x)*0.5

dt0_miss <- dt0%>%
  dplyr::mutate(trtn = ifelse(trt=='t', 1, 0),
                pmiss = 1/(1 + exp(- int_subj - b1_subj*x - b3_subj*x*trtn)),
                pthresh = runif(n()),
                r = ifelse(pmiss > pthresh, 1, 0))#%>%
  #dplyr::select(-c(pmiss, pthresh))

do_ch <- dt0_miss%>%group_by(trt)%>%summarise(do=mean(r))
p_ch <- dt0_miss%>%group_by(trt,r)%>%summarise(pcca=mean(y))

out <- list(do_ch, p_ch)%>%
  purrr::set_names("do_ch", "p_ch")
return(out)


})

x1%>%
  purrr::map_df(.f=function(x) x$do_ch, .id = 'sim')%>%
  group_by(trt)%>%
  summarise(mean(do))
  

x1%>%
  purrr::map_df(.f=function(x) x$p_ch, .id = 'sim')%>%
  group_by(trt,r)%>%
  summarise(mean(pcca))

