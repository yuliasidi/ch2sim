#simulation set-up
library(dplyr)

setting <- expand.grid(cor_xl = c(0.4, 0.7),
                       pc = 0.8,
                       pt = 0.8 - seq(-0.025,0.025, 0.025),
                       n_obs = c(250,500))

setting <- setting%>%
  dplyr::mutate(set_n = seq(1, length(setting$pc), 1))

saveRDS(setting, 'setting.rds')
