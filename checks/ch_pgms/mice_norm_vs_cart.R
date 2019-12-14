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



#rate of clinical experts opinios we observe
obs_rate <- 0.03
#parameters tbu in the clinical experts opinions model (to calculate probability to be non/observed) 
b1 <- - 0.08
xcov <- matrix(c(sd_x^2, sd_x*sd_lambda*cor_xl, sd_x*sd_lambda*cor_xl, sd_lambda^2), 2, 2)

#number of imputations for the MDs survey
num_m_md <- 20 

x <- 1
#population of physicians consists of 1000 doctors
set.seed(100*1 + x)
dt_pop0 <- mvrnorm(1000, mu = c(mu_x, mu_lambda), Sigma = xcov)

dt_pop <- tibble::tibble(x = dt_pop0[,1],
                    lambda = dt_pop0[,2],
                    ph_id = seq(1, length(dt_pop0[,1])))%>%
dplyr::mutate(x = ifelse(x < 0, 0, x), #years of exp. cannot be negative
           lambda = ifelse(lambda < 0.4, 0.4, ifelse(lambda > 0.95, 0.95, lambda))) #set up cut-off values so that lambda cannot be below 0.4 or above 0.95, as preserving less than 40% or more than 95% does not make sense 


dt_sample <- dt_pop%>%
dplyr::sample_frac(size = 0.3)


#observe only k physicians
dt_all <- dt_sample%>%
dplyr::mutate(pmiss = 1/(1 + exp(- log((1 - obs_rate)/obs_rate) - b1*(x - mean(dt_sample$x)))),
           pthresh = runif(n()),
           r = ifelse(pmiss > pthresh, 1, 0))%>%
dplyr::select(-c(pmiss, pthresh))

# plot(dt_all$x, dt_all$pmiss)
# sum(dt_all$r)
# mean(dt_all$lambda[dt_all$r==0])
# cor(dt_all$x, dt_all$lambda)
# cor(dt_all$x[dt_all$r==0], dt_all$lambda[dt_all$r==0])

#the below condition added in order to make sure that at least 4 responses are observed in the survey
while(length(dt_all$r[dt_all$r==0])<4){

dt_all <- dt_sample%>%
dplyr::mutate(pmiss = 1/(1 + exp(- log((1 - obs_rate)/obs_rate) - b1*(x - mean(dt_sample$x)))),
             pthresh = runif(n()),
             r = ifelse(pmiss > pthresh, 1, 0))%>%
dplyr::select(-c(pmiss, pthresh))

}
#mean/sd lambda for the whole representitive sample of MDs
#mask unobserved values from the sample of MDs
dt_obs <- dt_all%>%
dplyr::mutate(lambda = ifelse(r==0, lambda, NA))

predM <- mice::make.predictorMatrix(data=dt_obs)
predM[, "ph_id"] <- 0
predM[, "r"] <- 0

mice_out_norm <- mice::mice(
data = dt_obs,
m=num_m_md,
maxit = 50,
method = 'norm',
predictorMatrix=predM,
printFlag = FALSE
)

mice_out_cart <- mice::mice(
data = dt_obs,
m=num_m_md,
maxit = 50,
method = 'cart',
predictorMatrix=predM,
printFlag = FALSE
)

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
