library(dplyr)
library(purrr)

setting <- readRDS("setting.rds")

setting.l <- as.list(setting)

purrr::pwalk(.l = setting.l,
             .f = function(cor_xl, pc, pt, n_obs, set_n){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/mdsur_obs3.tmpl'),
                   data = list(
                     cor_xl = cor_xl,
                     pc = pc,
                     pt = pt,
                     n_obs = n_obs,
                     set_n = set_n)
                 ),
                 file = file.path('pgms_simrun',
                                  sprintf("mdsur_obs3_sc%s.R",
                                          set_n)
                 ),
                 sep='\n')
             })
