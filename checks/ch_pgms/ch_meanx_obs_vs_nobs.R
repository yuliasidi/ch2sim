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

mu_x <- 15
mu_lambda <- 0.7
sd_x <- 4
sd_lambda <- 0.05

#rate of clinical experts opinios we observe
obs_rate <- 0.05
#parameters tbu in the clinical experts opinions model (to calculate probability to be non/observed) 
b1 <- - 0.08
xcov <- matrix(c(sd_x^2, sd_x*sd_lambda*cor_xl, sd_x*sd_lambda*cor_xl, sd_lambda^2), 2, 2)

#number of imputations for the MDs survey
num_m_md <- 20 

x1 <- parallel::mclapply(X = 1:5000, 
                         mc.cores = 7,
                         FUN= function(x){
                           
                           #population of physicians consists of 1000 doctors
   set.seed(100*1 + x)
   dt_pop0 <- mvrnorm(1000, mu = c(mu_x, mu_lambda), Sigma = xcov)
   
   dt_pop <- tibble::tibble(x = dt_pop0[,1],
                            lambda = dt_pop0[,2],
                            ph_id = seq(1, length(dt_pop0[,1])))
   
   dt_sample <- dt_pop%>%
     dplyr::sample_frac(size = 0.3)
   
   int <- log((1 - obs_rate)/obs_rate) - b1*mean(dt_sample$x)
   
   #observe only k physicians
   dt_all <- dt_sample%>%
     dplyr::mutate(pmiss = 1/(1 + exp(- int - b1*x)),
                   pthresh = runif(n()),
                   r = ifelse(pmiss > pthresh, 1, 0))%>%
     dplyr::select(-c(pmiss, pthresh))
   
   #the below condition added in order to make sure that at least 4 responses are observed in the survey
   while(length(dt_all$r[dt_all$r==0])<4){
     
     dt_all <- dt_sample%>%
       dplyr::mutate(pmiss = 1/(1 + exp(- int - b1*x)),
                     pthresh = runif(n()),
                     r = ifelse(pmiss > pthresh, 1, 0))%>%
       dplyr::select(-c(pmiss, pthresh))
     
   }
   out <- dt_all%>%group_by(r)%>%summarise(mean_x = mean(x))
   out <- list(out)%>%purrr::set_names('out')
   return(out)
   
 })

x1%>%
  purrr::map_df(.f=function(x) x$out, .id = 'sim')%>%
  dplyr::group_by(r)%>%
  dplyr::summarise(mean(mean_x))

