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

#rate of clinical experts opinios we observe
obs_rate <- 0.05
#parameters tbu in the clinical experts opinions model (to calculate probability to be non/observed) 
#b1 <- - 0.08
xcov <- matrix(c(sd_x^2, sd_x*sd_lambda*cor_xl, sd_x*sd_lambda*cor_xl, sd_lambda^2), 2, 2)

#number of imputations for the MDs survey
num_m_md <- 20

x1 <- parallel::mclapply(X = 1:100, 
                         mc.cores = 7,
                         FUN= function(i){
                           
#population of physicians consists of 1000 doctors
set.seed(100*1 + i)
dt_pop0 <- MASS::mvrnorm(1000, mu = c(mu_x, mu_lambda), Sigma = xcov)
                           
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
  dplyr::mutate(pmiss = ifelse(x_20 == 1, 0.95, 0.99))%>%
  #dplyr::mutate(pmiss = ifelse(x_20 == 1, 0.93, 0.97))%>%
  #dplyr::mutate(pmiss = ifelse(x_20 == 1, 0.85, 0.95))%>%
  #dplyr::mutate(pmiss = ifelse(x_20 == 1, 0.4, 0.8))%>%
  split(.$pmiss)%>%
  purrr::map(.f = function(x){ x%>%
      dplyr::mutate(r = stats::rbinom(dplyr::n(), 1, pmiss))})%>%
  dplyr::bind_rows()%>%
  dplyr::select(-pmiss)

# dt_boot <- purrr::rerun(.n = 1000, dt_sample%>%
#                dplyr::sample_frac(size = obs_rate)%>%
#                dplyr::summarise_at(.vars= 'lambda', .funs = c('mean','sd')))%>%
#   dplyr::bind_rows()%>%
#   dplyr::summarise(m_mean = mean(mean), m_sd = mean(sd))
                

#the below condition added in order to make sure that at least 4 responses are observed in the survey
while(length(dt_all$r[dt_all$r==0])<4){

  dt_all <- 
    dt_sample%>%
    dplyr::mutate(pmiss = ifelse(x_20 == 1, 0.95, 0.99))%>%
    #dplyr::mutate(pmiss = ifelse(x_20 == 1, 0.93, 0.97))%>%
    #dplyr::mutate(pmiss = ifelse(x_20 == 1, 0.85, 0.95))%>%
    #dplyr::mutate(pmiss = ifelse(x_20 == 1, 0.4, 0.8))%>%
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

mdsur_mi <- m2_mi(dt_obs, num_m = num_m_md, use_pckg = 'norm')%>%
  dplyr::rename(mean_l = qbar)%>%
  dplyr::mutate(sd_l = sqrt(t), se_l = sd_l)

# mdsur_smin <- dt_all%>%
#   dplyr::summarise(mean_l = min(lambda, na.rm = T))%>%
#   dplyr::mutate(sd_l = 0, n_l = 1)
# 
# mdsur_smax <- dt_all%>%
#   dplyr::summarise(mean_l = max(lambda, na.rm = T))%>%
#   dplyr::mutate(sd_l = 0, n_l = 1)

#generate trial data:
# set.seed(200*1 + i)
# dt0 <- bin2mi::dt_p2(n = n_obs, pc = pc, pt = pt)
# 
# #calculate ci and derive decision based on the full/obs/mi/sing cohort of MDs
# mdall_des  <- ci_sur(mdsur_all, dt0, type = 'all')
# mdobs_des  <- ci_sur(mdsur_obs, dt0, type = 'obs') 
# mdmi_des   <- ci_sur(mdsur_mi, dt0, type = 'mi')
# mdmin_des <- ci_sur(mdsur_smin, dt0, type = 'sing min')
# mdmax_des <- ci_sur(mdsur_smax, dt0, type = 'sing max')

# ct_des <- bind_rows(mdall_des, mdobs_des, mdmi_des, mdmin_des, mdmax_des)%>%
#   dplyr::mutate(sim_id = x)

ct_des <- bind_rows(mdsur_all%>%mutate(meth = 'all'), mdsur_obs%>%mutate(meth = 'obs'),
                    mdsur_mi%>%mutate(meth = 'mi'))%>%
  dplyr::mutate(sim_id = i)

# ct_des1 <- bind_rows(mdall_des, mdobs_des, mdmi_des)%>%
#   dplyr::mutate(sim_id = i)

out <- list(ct_des, ch)%>%
       purrr::set_names("ct_des", 'ch')
     return(out)
                       
})

#saveRDS(x1, "results/mdsu_obs3_sc1_norm.rds")  



xx1 <- x1%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
  mutate(ci_u = mean_l + se_l*ifelse(meth == 'mi', qt(0.975, df = v), qnorm(0.975)))%>%
  group_by(meth)%>%
  summarise(mean_lambda=round(mean(mean_l),4), mean_n_mds=round(mean(n_l),1), 
            mean_se_lambda = round(mean(se_l), 5), mean_upper_ci = round(mean(ci_u),4))

knitr::kable(xx1,format = 'latex')%>%texPreview::tex_preview(returnType = 'html')

x1%>%
  purrr::map_df(.f=function(x) x$ct_des1, .id = 'sim')%>%
  mutate(ci_uall = mean_l + cutv*ifelse(sur == 'mi', sqrt(var_d + m1*sd_l^2), sqrt(var_d + m1*sd_l^2/n_l)),
         ci_u = mean_l + ifelse(sur == 'mi', qt(0.975, df = v)*sqrt(t), qnorm(0.975)*sd_l/sqrt(n_l)))%>%
  dplyr::select(sim, sur, ci_uall, mean_l, cutv, ci_u)%>%
  group_by(sur)%>%
  summarise(mean(ci_uall), mean(ci_u))
  

x1%>%
  purrr::map_df(.f=function(x) x$ch, .id = 'sim')%>%
  group_by(x_20)%>%
  summarise_at(.vars = c('ml', 'rm', 'xm'), .funs = 'mean')

x2 <- x1%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
  dplyr::filter(meth == 'mi')

boot_sum <- x1%>%
  purrr::map_df(.f=function(x) x$dt_boot, .id = 'sim')

x1%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')

x1%>%
  purrr::map_df(.f=function(x) x$ct_des1, .id = 'sim')%>%
  dplyr::group_by(sur)%>%
  dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                   n_sim = n(),
                   md_n = mean(n_l),
                   md_m2 = mean(mean_l))
