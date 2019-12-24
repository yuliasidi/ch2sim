library(dplyr, warn.conflicts = F, quietly = T)
library(ggplot2, warn.conflicts = F, quietly = T)


x1_cart <- readRDS("results/mdsu_obs3_sc1_cart.rds")
x1_norm <- readRDS("results/mdsu_obs3_sc1_norm.rds")


x2_cart <- 
  x1_cart%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')

x2_norm <- 
  x1_norm%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')

x3_cart <- 
  x2_cart%>%
  dplyr::filter(sur == 'mi')%>%
  dplyr::select(sim, mean_l, ubar, b, t, v)

x3_norm <- 
  x2_norm%>%
  dplyr::filter(sur == 'mi')%>%
  dplyr::select(sim, mean_l, ubar, b, t, v)

x_both <- dplyr::bind_rows(x3_cart%>%
                             dplyr::mutate(mi_method = 'cart'),
                           x3_norm%>%
                             dplyr::mutate(mi_method = 'norm'))
x_both1 <- x_both%>%
  dplyr::select(-c(t, mean_l))%>%
  tidyr::gather(key = 'var_term', value = 'val', -c(sim, mi_method))

x_both1%>%
  dplyr::filter(var_term!='v')%>%
  ggplot(aes(x = sim, y = val)) +
  geom_point(aes(color = var_term)) +
  facet_wrap(~ mi_method, scales = 'free')

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



xcov <- matrix(c(sd_x^2, sd_x*sd_lambda*cor_xl, sd_x*sd_lambda*cor_xl, sd_lambda^2), 2, 2)

#number of imputations for the MDs survey
num_m_md <- 20

i <- 11
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
  split(.$pmiss)%>%
  purrr::map(.f = function(x){ x%>%
      dplyr::mutate(r = stats::rbinom(dplyr::n(), 1, pmiss))})%>%
  dplyr::bind_rows()%>%
  dplyr::select(-pmiss)


#the below condition added in order to make sure that at least 4 responses are observed in the survey
while(length(dt_all$r[dt_all$r==0])<4){
  
  dt_all <- 
    dt_sample%>%
    dplyr::mutate(pmiss = ifelse(x_20 == 1, 0.95, 0.99))%>%
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

predM <- mice::make.predictorMatrix(data=dt_obs)
predM[, "ph_id"] <- 0
predM[, "r"] <- 0

mice_out_norm <- mice::mice(
data = dt_obs,
m=num_m_md,
maxit = 20,
method = 'norm',
predictorMatrix=predM,
printFlag = FALSE
)

mice_out_cart <- mice::mice(
data = dt_obs,
m=num_m_md,
maxit = 20,
method = 'cart',
predictorMatrix=predM,
printFlag = FALSE
)


#use norm package
norm::rngseed(seed = 666*i)
dt_in <- dt_obs
data1 <- as.matrix(dt_in%>%
                     dplyr::select(x, lambda))
s <- norm::prelim.norm(data1)

n_iter <- 100000

thetahat <- norm::em.norm(s, showits = FALSE)
thetahat <- norm::da.norm(s, thetahat, steps = 1, showits = FALSE)

# param_out <- norm::getparam.norm(s, thetahat)
# da_out <- tibble::tibble(mu1 = param_out$mu[1],
#                          mu2 = param_out$mu[2],
#                          sd1 = param_out$sigma[1,1],
#                          sd2 = param_out$sigma[2,2],
#                          cov12 = param_out$sigma[1,2],
#                          theta = thetahat,
#                          iter_num = 1)%>%
#   tidyr::nest(theta)


xx <- 2
diff <- matrix(NA_real_,nrow = n_iter,ncol = 4)
diff[1,c(1,2)] <- 100
thetahat_old <- thetahat

while( xx < n_iter & (diff[xx-1,1] > 1e-7 & diff[xx-1,2] > 1e-10)){
  thetahat <- norm::da.norm(s, thetahat_old, steps = 1, showits = FALSE)
  param_out_old <- norm::getparam.norm(s, thetahat_old)
  param_out <- norm::getparam.norm(s, thetahat)
  
  diff[xx,1] <- abs(param_out$mu[2] - param_out_old$mu[2])
  diff[xx,2] <- abs(param_out$sigma[2,2] - param_out_old$sigma[2,2])
  diff[xx,3] <- param_out$mu[2]
  diff[xx,4] <- param_out$sigma[2,2]
  
  thetahat_old <- thetahat
  xx <- xx + 1
}

plot(2:tail(which(!is.na(mi_out$diff[,1])),1),mi_out$diff[2:tail(which(!is.na(mi_out$diff[,1])),1),3])
plot(2:tail(which(!is.na(mi_out$diff[,1])),1),mi_out$diff[2:tail(which(!is.na(mi_out$diff[,1])),1),4])

tail(which(!is.na(mi_out$diff[,1])),1)
mi_out$diff[tail(which(!is.na(mi_out$diff[,1])),1),1]
mi_out$diff[tail(which(!is.na(mi_out$diff[,1])),1),2]



imp_n <- tibble::tibble(i = seq(1, num_m_md, 1))

system.time({mi_sum <- purrr::pmap(imp_n, .f=function(i){
  
  
  dt_mi <- norm::imp.norm(s, thetahat, data1)%>%
    tibble::as_tibble()%>%
    dplyr::bind_cols(dt_in%>%
                       dplyr::select(-c(lambda, x)))%>%
    dplyr::summarise(qhat = mean(lambda), u = stats::var(lambda)/dplyr::n())
  
  out <- list(dt_mi, da_out%>%
                dplyr::select(-data))%>%purrr::set_names('dt_mi', 'da_out')
  
  return(out)
})})

chains <- mi_sum%>%purrr::map_df(.f = function(x) x$da_out, .id = 'i')
library(ggplot2)

chains %>%
  ggplot( aes(x=iter_num, y=mu2, group=i, color=i)) +
  geom_line()

chains %>%
  ggplot( aes(x=iter_num, y=sd2, group=i, color=i)) +
  geom_line()

plot(mice_out_norm)
plot(mice_out_cart)

tmp <- tibble::tibble(i = seq(1, num_m_md,1))

dt_mice_norm <- purrr::pmap_dfr(tmp, .f=function(i){
  dt <- mice::complete(mice_out_norm, i)
}, .id = "i")

dt_mice_cart <- purrr::pmap_dfr(tmp, .f=function(i){
  dt <- mice::complete(mice_out_cart, i)
}, .id = "i")


dt_mice_norm%>%
  dplyr::group_by(i)%>%
  dplyr::summarise(qhat = mean(lambda), u = stats::var(lambda)/dplyr::n())%>%
  dplyr::ungroup()%>%
  dplyr::summarise(qbar = mean(qhat),
                   ubar = mean(u),
                   b = stats::var(qhat))%>%
  dplyr::mutate(t = ubar + (1 + 1/num_m_md)*b,
                v = (num_m_md - 1)*(1 + ubar/(b*(1 + 1/num_m_md)))^2,
                minfo = b/(ubar + b))
dt_mice_cart%>%
  dplyr::group_by(i)%>%
  dplyr::summarise(qhat = mean(lambda), u = stats::var(lambda)/dplyr::n())%>%
  dplyr::ungroup()%>%
  dplyr::summarise(qbar = mean(qhat),
                   ubar = mean(u),
                   b = stats::var(qhat))%>%
  dplyr::mutate(t = ubar + (1 + 1/num_m_md)*b,
                v = (num_m_md - 1)*(1 + ubar/(b*(1 + 1/num_m_md)))^2,
                minfo = b/(ubar + b))
mi_sum%>%purrr::map_df(.f = function(x) x$dt_mi, .id = 'i')%>%
dplyr::summarise(qbar = mean(qhat),
                 ubar = mean(u),
                 b = stats::var(qhat))%>%
  dplyr::mutate(t = ubar + (1 + 1/num_m_md)*b,
                v = (num_m_md - 1)*(1 + ubar/(b*(1 + 1/num_m_md)))^2,
                n_l = length(dt_in$ph_id),
                minfo = b/(ubar + b))

#compare a fully simulated data
sc1_norm <- readRDS('results/mdsu_obs5_sc1_norm_m100.rds')
sc1_cart <- readRDS('results/mdsu_obs3_sc1_cart_m50.rds')
sc1_norm_m50 <- readRDS('results/mdsu_obs3_sc1_norm_m50.rds')
sc1_norm_it50 <- readRDS('results/mdsu_obs5_sc1_norm_m100.rds')

sc1_norm%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
  filter(sur%in%c('mi'))%>%
  mutate(info = b/(ubar + b))%>%
  summarise(mean(info))

sc1_norm_m50%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
  filter(sur%in%c('mi'))%>%
  mutate(info = b/(ubar + b))%>%
  summarise(mean(info))

sc1_norm_it50%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
  filter(sur%in%c('mi'))%>%
  mutate(info = b/(ubar + b))%>%
  summarise(mean(info))


sc1_cart%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
  filter(sur%in%c('mi'))%>%
  mutate(info = b/(ubar + b))%>%
  summarise(mean(info))

sc1_norm%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
  dplyr::group_by(sur)%>%
  dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                   n_sim = n(),
                   md_n = mean(n_l),
                   md_m2 = mean(mean_l))

sc1_norm_m50%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
  dplyr::group_by(sur)%>%
  dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                   n_sim = n(),
                   md_n = mean(n_l),
                   md_m2 = mean(mean_l))

sc1_norm_it50%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
  dplyr::group_by(sur)%>%
  dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                   n_sim = n(),
                   md_n = mean(n_l),
                   md_m2 = mean(mean_l))

sc1_cart%>%
  purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%
  dplyr::group_by(sur)%>%
  dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                   n_sim = n(),
                   md_n = mean(n_l),
                   md_m2 = mean(mean_l))
