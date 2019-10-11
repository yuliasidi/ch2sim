library(tidyr, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(bin2mi)
library(m2imp)

phsur_or3_all <- readRDS('results/phsur_or3_all.rds')


#mean/sd lambda for the whole representitive sample of MDs
phsur_all <- phsur_or3_all%>%
  dplyr::group_by(sim)%>%
  dplyr::summarise(mean_l = mean(lambda), sd_l = sd(lambda), n_l = n())

#mean/sd lambda for the observed sample of MDs
phsur_obs <- phsur_or3_all%>%
  dplyr::filter(r==0)%>%
  dplyr::group_by(sim)%>%
  dplyr::summarise(mean_l = mean(lambda), sd_l = sd(lambda), n_l = n())

alpha <- 0.025
power <- 0.85
pc <- 0.8
pt <- 0.75
m1 <- 0.23
n_obs <- 800

#generate trial data:
set.seed(120487)
dt0 <- bin2mi::dt_p2(n = n_obs, pc = pc, pt = pt)

#for each simulated survey derive decision based on the full/observed cohort of MDs
phall_des <- phsur_all%>%
  dplyr::mutate(p_sum = purrr::pmap(list(m2 = mean_l), p2_mle, dt = dt0))%>%
  tidyr::unnest()%>%
  dplyr::mutate(ci_u = phat_d - (1 - mean_l)*m1 + qnorm(1-alpha)*sqrt(var_d + m1*sd_l^2/n_l),
                ci_l = phat_d - (1 - mean_l)*m1 - qnorm(1-alpha)*sqrt(var_d + m1*sd_l^2/n_l),
                ni_des = ifelse(ci_u < 0, 1, 0))

phobs_des <- phsur_obs%>%
  dplyr::mutate(p_sum = purrr::pmap(list(m2 = mean_l), p2_mle, dt = dt0))%>%
  tidyr::unnest()%>%
  dplyr::mutate(ci_u = phat_d - (1 - mean_l)*m1 + qnorm(1-alpha)*sqrt(var_d + m1*sd_l^2/n_l),
                ci_l = phat_d - (1 - mean_l)*m1 - qnorm(1-alpha)*sqrt(var_d + m1*sd_l^2/n_l),
                ni_des = ifelse(ci_u < 0, 1, 0))

x1_pop <- parallel::mclapply(X = 1:10000, 
                             mc.cores = 7,
                             FUN= function(x){
     
     set.seed(100 + x)
       
     #mle summaries for dt0:
     p_sum <- bin2mi::p2_mle(dt0, m2 = m2_popest_mean)
     
     dtout <- p_sum%>%
       dplyr::mutate(lower_bound = phat_d - m2_popest_mean - qnorm(1-alpha)*sqrt(var_d + m2_popest_se),
                     upper_bound = phat_d - m2_popest_mean + qnorm(1-alpha)*sqrt(var_d + m2_popest_se),
                     sim_id = x,
                     rej_h0 = ifelse(upper_bound<0, 1, 0))
     
     out <- list(dtout)%>%
       purrr::set_names("res")
     return(out)
                       
})

x2_pop <- x1_pop%>%purrr::map_df(.f=function(x) x$res, .id = 'sim')
mean(x2_pop$rej_h0)

#############################################

set.seed(4581)
l_k10 <- sample(l_pop, 2)

#mle estimates of mean M2 and its SE from the population
m2_k10_mean <- (1 - mean(l_k10))*m1
m2_k10_se <- m1^2*var(l_k10)/length(l_k10)

n_obs <- ss_wald(alpha = alpha, power = power, pc = pc, pt = pt, m2 = m2_k10_mean)


x1_pop <- parallel::mclapply(X = 1:10000, 
                             mc.cores = 7,
                             FUN= function(x){
                               
   set.seed(100 + x)
   #generate full data under: pt = pc -  m2_truemean:
   dt0 <- bin2mi::dt_p2(n = n_obs, pc = pc, pt = pc - m2_truemean)  
   #mle summaries for dt0:
   p_sum <- bin2mi::p2_mle(dt0, m2 = m2_k10_mean)
   
   dtout <- p_sum%>%
     dplyr::mutate(lower_bound = phat_d - m2_k10_mean - qnorm(1-alpha)*sqrt(var_d + m2_k10_se),
                   upper_bound = phat_d - m2_k10_mean + qnorm(1-alpha)*sqrt(var_d + m2_k10_se),
                   sim_id = x,
                   rej_h0 = ifelse(upper_bound<0, 1, 0))
   
   out <- list(dtout)%>%
     purrr::set_names("res")
   return(out)
   
 })

x2_pop <- x1_pop%>%purrr::map_df(.f=function(x) x$res, .id = 'sim')
mean(x2_pop$rej_h0)

x <- runif(n = 1000, min = 1, max = 40)
lambda <- rnorm(n = 1000, mean = 0.7, sd = 0.03) 

ub_cor <- cor(sort(runif(n = 100000, min = 1, max = 40)), 
              sort(rnorm(n = 100000, mean = 0.7, sd = 0.03) ))

dt_pop <- tibble::tibble(ph_id = seq(1, length(x),1),
                         x = c(sort(x[1:floor(length(x)*cor_xl/ub_cor)]), 
                               x[(floor(length(x)*cor_xl/ub_cor) + 1):length(x)]),
                         lambda = c(sort(lambda[1:floor(length(lambda)*cor_xl/ub_cor)]), 
                                    lambda[(floor(length(lambda)*cor_xl/ub_cor) + 1):length(lambda)]))


dt_all1 <- dt_sample%>%
  dplyr::sample_n(10, weight = x)%>%
  dplyr::mutate(r = 1)

dt_all2 <- dplyr::bind_rows(dt_all1,
                            dt_sample%>%
                              dplyr::anti_join(dt_all1%>%
                                                 dplyr::select(ph_id), by = 'ph_id')%>%
                              dplyr::mutate(r = 0))

dt_all2%>%group_by(r)%>%summarise_at(.vars = c('x', 'lambda'), .funs = mean)
cor(dt_all$x[dt_all$r==1], dt_all$lambda[dt_all$r==1])
cor(dt_all$x[dt_all$r==0], dt_all$lambda[dt_all$r==0])

