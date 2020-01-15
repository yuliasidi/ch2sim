x1 <- readRDS('checks/ch_outs/obs10_cart.rds')

norm10 <- readRDS('checks/ch_outs/obs10_norm.rds')
norm20 <- readRDS('checks/ch_outs/obs20_norm.rds')
norm50 <- readRDS('checks/ch_outs/obs50_norm.rds')
norm60 <- readRDS('checks/ch_outs/obs60_norm.rds')
norm80 <- readRDS('checks/ch_outs/obs80_norm.rds')


x1%>%
  purrr::map_df(.f = function(x) x$md)%>%
  group_by(sur)%>%
  summarise_at(c('mean_l', 'se_l'), 'mean')

norm10%>%
  purrr::map_df(.f = function(x) x$md)%>%
  group_by(sur)%>%
  summarise_at(c('mean_l', 'se_l', 'b', 'ubar'), 'mean')
norm20%>%
  purrr::map_df(.f = function(x) x$md)%>%
  group_by(sur)%>%
  summarise_at(c('mean_l', 'se_l', 'b', 'ubar'), 'mean')
norm50%>%
  purrr::map_df(.f = function(x) x$md)%>%
  group_by(sur)%>%
  summarise_at(c('mean_l', 'se_l', 'b', 'ubar'), 'mean')
norm60%>%
  purrr::map_df(.f = function(x) x$md)%>%
  group_by(sur)%>%
  summarise_at(c('mean_l', 'se_l', 'b', 'ubar'), 'mean')
norm80%>%
  purrr::map_df(.f = function(x) x$md)%>%
  group_by(sur)%>%
  summarise_at(c('mean_l', 'se_l', 'b', 'ubar'), 'mean')
