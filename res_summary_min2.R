library(dplyr)
library(purrr)
library(tidyr)

setting <- readRDS("setting.rds")

################################
# All patient data is observed #
################################

ll <- seq(2,12,1)

file_prefix <- "mdsu_obs3"

create_res_sum(ll, load_rds = FALSE, file_prefix = file_prefix)

create_ch_sum(ll, load_rds = FALSE, file_prefix = file_prefix)

create_minfo_sum(ll, load_rds = FALSE, file_prefix = file_prefix)

#########################################################################
# All patient data is observed  and subsample of MDs is totally random  #
#########################################################################

ll <- seq(1,12,1)

in_dir <- "results/mdsu_obs3r"
file_prefix <- "mdsu_obs3r"
out_dir <- "sums/mdsu_obs3r"

create_res_sum(ll, load_rds = FALSE, file_prefix = file_prefix, in_dir = in_dir, out_dir = out_dir)

create_ch_sum(ll, load_rds = FALSE, file_prefix = file_prefix, in_dir = in_dir)

create_minfo_sum(ll, load_rds = FALSE, file_prefix = file_prefix, in_dir = in_dir, out_dir = out_dir)

################################
# Patient data are MCAR        #
################################

ll <- seq(1,12,1)

in_dir <- "results/mcar"
file_prefix <- "mdsu_obs3_smcar"
file_suffix <- "_cart_2min"
out_dir <- "sums/mcar"

create_res_sum(ll, load_rds = FALSE, file_prefix = file_prefix, file_suffix = file_suffix,
               in_dir = in_dir, out_dir = out_dir)

create_ch_sum(ll, load_rds = FALSE, file_prefix = file_prefix, file_suffix = file_suffix, in_dir = in_dir)

create_minfo_sum(ll, load_rds = FALSE, file_prefix = file_prefix, file_suffix = file_suffix,
                 in_dir = in_dir, out_dir = out_dir)


################################
# Patient data are MAR        #
################################

ll <- seq(1,12,1)

in_dir <- "results/mar"
file_prefix <- "mdsu_obs3_smar"
out_dir <- "sums/mar"

create_res_sum(ll, load_rds = FALSE, file_prefix = file_prefix, in_dir = in_dir, out_dir = out_dir)

create_ch_sum(ll, load_rds = FALSE, file_prefix = file_prefix, in_dir = in_dir)

create_minfo_sum(ll, load_rds = FALSE, file_prefix = file_prefix, in_dir = in_dir, out_dir = out_dir)

