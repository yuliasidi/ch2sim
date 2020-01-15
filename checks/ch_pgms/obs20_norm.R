library(tidyr, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(MASS, warn.conflicts = F, quietly = T)
library(bin2mi)
library(m2imp)

alpha <- 0.025
power <- 0.85
cor_xl <- 0.4
pc <- 0.8
pt <- 0.825
m1 <- 0.23
n_obs <- 250

mu_x <- 20
mu_lambda <- 0.7
sd_x <- 7
sd_lambda <- 0.12

xcov <- matrix(c(sd_x^2, sd_x*sd_lambda*cor_xl, sd_x*sd_lambda*cor_xl, sd_lambda^2), 2, 2)

#number of imputations for the MDs survey
num_m_md <- 20 

x1 <- parallel::mclapply(X = 1:5000, 
                         mc.cores = 24,
                         FUN= function(i){
                           
#population of physicians consists of 1000 doctors
set.seed(100*1 + i)
dt_pop0 <- mvrnorm(1000, mu = c(mu_x, mu_lambda), Sigma = xcov)

   dt_pop <- tibble::tibble(x = dt_pop0[,1],
                            lambda = dt_pop0[,2],
                            ph_id = seq(1, length(dt_pop0[,1])))%>%
     dplyr::mutate(lambda = ifelse(lambda<0.40, 0.4, ifelse(lambda>0.95, 0.95, lambda)), #cut-off lambda values to be between 0.40 and 0.95
                   x = ifelse(x < 0, 0, x)) #cut-off x values at 0
   
   #representative sample of MDs - 30% of the population
   dt_sample <- dt_pop%>%
     dplyr::sample_frac(size = 0.3)%>%
     dplyr::mutate(x_20 = ifelse(x > 20, 1, 0)) #introduce cut-off value of x at 20 
   
   #observe only k physicians
   dt_all <- 
     dt_sample%>%
     dplyr::mutate(pmiss = ifelse(x_20 == 1, 0.65, 0.95))%>%
     split(.$pmiss)%>%
     purrr::map(.f = function(x){ x%>%
         dplyr::mutate(r = stats::rbinom(dplyr::n(), 1, pmiss))})%>%
     dplyr::bind_rows()%>%
     dplyr::select(-pmiss)
   
  
   #the below condition added in order to make sure that at least 4 responses are observed in the survey
   while(length(dt_all$r[dt_all$r==0])<4){
     
     dt_all <- 
       dt_sample%>%
       dplyr::mutate(pmiss = ifelse(x_20 == 1, 0.65, 0.95))%>%
       split(.$pmiss)%>%
       purrr::map(.f = function(x){ x%>%
           dplyr::mutate(r = stats::rbinom(dplyr::n(), 1, pmiss))})%>%
       dplyr::bind_rows()%>%
       dplyr::select(-pmiss)
     
   }
   
  #mean/sd lambda for the whole representitive sample of MDs
   mdsur_all <- dt_all%>%
     dplyr::summarise(mean_l = mean(lambda), sd_l = sd(lambda), n_l = n())%>%
     dplyr::mutate(se_l = sd_l/sqrt(n_l))
   
   #mean/sd lambda for the observed sample of MDs
   mdsur_obs <- dt_all%>%
     dplyr::filter(r==0)%>%
     dplyr::summarise(mean_l = mean(lambda), sd_l = sd(lambda), n_l = n())%>%
     dplyr::mutate(se_l = sd_l/sqrt(n_l))
   
   ch <- dt_all%>%
     group_by(x_20)%>%
     summarise(ml = mean(lambda), rm = mean(r), xm = mean(x))
   
   #mask unobserved values from the sample of MDs
   dt_obs <- dt_all%>%
     dplyr::mutate(lambda = ifelse(r==0, lambda, NA))%>%
     dplyr::select(-x_20)
   
   mdsur_mi <- m2_mi(dt_obs, num_m = num_m_md, mi_method = 'norm')%>%
     dplyr::rename(mean_l = qbar)%>%
     dplyr::mutate(sd_l = sqrt(t), se_l = sd_l)
   
   md <- bind_rows(mdsur_all%>%mutate(sur = 'all'),
                   mdsur_obs%>%mutate(sur = 'obs'),
                   mdsur_mi%>%mutate(sur = 'mi'))%>%
     dplyr::mutate(sim_id = i)
   
 
   out <- list(md)%>%
     purrr::set_names("md")
   return(out)
                        
})

saveRDS(x1, "results/obs20_norm.rds")  

