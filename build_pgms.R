library(dplyr)
library(purrr)


###########################################
## Subject level data is fully opbserved ##
###########################################

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

#check dfs from margin MI for fully observed subject level data 
purrr::pwalk(.l = setting.l,
             .f = function(cor_xl, pc, pt, n_obs, set_n){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/mdsur_obs3_checkdf.tmpl'),
                   data = list(
                     cor_xl = cor_xl,
                     pc = pc,
                     pt = pt,
                     n_obs = n_obs,
                     set_n = set_n)
                 ),
                 file = file.path('checks/dfchecks/pgms',
                                  sprintf("mdsur_obs3_dfcheck_sc%s.R",
                                          set_n)
                 ),
                 sep='\n')
             })

######################################################################
## Subject level data is fully opbserved random small subset of MDs ##
######################################################################

setting <- readRDS("setting.rds")

setting.l <- as.list(setting)

purrr::pwalk(.l = setting.l,
             .f = function(cor_xl, pc, pt, n_obs, set_n){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/mdsur_obs3r.tmpl'),
                   data = list(
                     cor_xl = cor_xl,
                     pc = pc,
                     pt = pt,
                     n_obs = n_obs,
                     set_n = set_n)
                 ),
                 file = file.path('pgms_simrun',
                                  sprintf("mdsur_obs3r_sc%s.R",
                                          set_n)
                 ),
                 sep='\n')
             })


###########################################
## Subject level data MCAR               ##
###########################################

setting <- readRDS("setting.rds")

setting.l <- as.list(setting)

purrr::pwalk(.l = setting.l,
             .f = function(cor_xl, pc, pt, n_obs, set_n){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/mdsur_obs3_smcar.tmpl'),
                   data = list(
                     cor_xl = cor_xl,
                     pc = pc,
                     pt = pt,
                     n_obs = n_obs,
                     set_n = set_n)
                 ),
                 file = file.path('pgms_simrun/mcar',
                                  sprintf("mdsur_obs3_smcar_sc%s.R",
                                          set_n)
                 ),
                 sep='\n')
             })


###########################################
## Subject level data MAR               ##
###########################################

setting <- readRDS("setting.rds")

setting.l <- as.list(setting)

purrr::pwalk(.l = setting.l,
             .f = function(cor_xl, pc, pt, n_obs, set_n){
               cat(
                 whisker::whisker.render(
                   readLines('tmpls/mdsur_obs3_smar.tmpl'),
                   data = list(
                     cor_xl = cor_xl,
                     pc = pc,
                     pt = pt,
                     n_obs = n_obs,
                     set_n = set_n)
                 ),
                 file = file.path('pgms_simrun/mar',
                                  sprintf("mdsur_obs3_smar_sc%s.R",
                                          set_n)
                 ),
                 sep='\n')
             })
