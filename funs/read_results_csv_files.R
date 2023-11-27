create_res_sum <- function(ll, load_rds = FALSE, file_prefix = "mdsu_obs3", file_suffix = "_cart_min2", in_dir = "results", out_dir = "sums") {
  res_sum <-
    map_df(ll,
           .f = function(sc) {
             if (load_rds) {
               x1 <- readRDS(list.files(in_dir, paste0(file_prefix, "_sc", sc, file_suffix, ".rds"), full.names = T))
               x2 <- x1%>%
                 purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')
             }
             else {
               x2 <- readr::read_csv(file.path(in_dir, paste0(file_prefix, "_sc", sc, file_suffix, "_ct_des.csv")))
             }



             x2%>%
               dplyr::group_by(sur)%>%
               dplyr::summarise(ni_desy = sum(ni_des, na.rm = T),
                                n_sim = n(),
                                md_n = mean(n_l),
                                md_m2 = mean(mean_l),
                                md_se = mean(se_l))%>%
               dplyr::mutate(set_n = sc)%>%
               dplyr::left_join(setting, by = "set_n")%>%
               dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl,
                             dd = ni_desy/n_sim)

           })

  res_sum%>%dplyr::select(sur, md_m2, set_n)%>%filter(sur%in%c('all', 'obs', 'mi'))%>%spread(key='sur', value='md_m2')

  f_out <- paste0("sum_", file_prefix, "_cart_min2")
  print(file.path("sums", f_out))
  saveRDS(res_sum, file = file.path("sums", paste0(f_out, ".rds")))
  readr::write_csv(res_sum, file = file.path("sums", paste0(f_out, ".csv")))
}

create_minfo_sum <- function(ll, load_rds = FALSE, file_prefix = "mdsu_obs3", file_suffix = "_cart_min2", in_dir = "results", out_dir = "sums") {
  minfo <-
    map_df(ll,
           .f = function(sc) {
             if (load_rds) {
               x1 <- readRDS(list.files(in_dir, paste0(file_prefix, "_sc", sc, file_suffix, ".rds"), full.names = T))
               x2 <- x1 %>%
                 purrr::map_df(.f=function(x) x$ct_des, .id = 'sim')%>%filter(sur%in%c('mi'))%>%
                 mutate(minfo = b/(ubar + b))%>%
                 dplyr::select(sim_id, minfo)%>%
                 dplyr::mutate(set_n = sc)%>%
                 dplyr::left_join(setting, by = "set_n")%>%
                 dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl)
             }

             else {
               x1 <- readr::read_csv(file.path(in_dir, paste0(file_prefix, "_sc", sc, file_suffix, "_ct_des.csv")))

               x2 <- x1%>%filter(sur%in%c('mi'))%>%
                 mutate(minfo = b/(ubar + b))%>%
                 dplyr::select(sim_id, minfo)%>%
                 dplyr::mutate(set_n = sc)%>%
                 dplyr::left_join(setting, by = "set_n")%>%
                 dplyr::mutate(pc = pc, pt = pt, n_obs = n_obs, cor_xl = cor_xl)
             }

             x2
           })
  f_out <- paste0("minfo_", file_prefix, "_cart_min2")
  print(file.path("sums", f_out))
  saveRDS(res_sum, file = file.path("sums", paste0(f_out, ".rds")))
  readr::write_csv(res_sum, file = file.path("sums", paste0(f_out, ".csv")))
}

create_ch_sum <- function(ll, load_rds = FALSE, file_prefix = "mdsu_obs3", file_suffix = "_cart_min2", in_dir = "results") {
  ch_ch <- map_df(ll,
                  .f = function(sc) {

                    if (load_rds) {
                      x1 <- readRDS(list.files(in_dir, paste0(file_prefix, "_sc", sc, file_suffix, ".rds"), full.names))

                      x2 <- x1 %>%
                        purrr::map_df(.f=function(x) x$ch, .id = 'sim')%>%
                        group_by(x_20)%>%
                        summarise_at(.vars = c('ml', 'rm', 'xm'), .funs = 'mean')
                    }
                    else {
                      x2 <- readr::read_csv(file.path(in_dir, paste0(file_prefix, "_sc", sc, file_suffix, "_ch.csv")))

                      x2 <- x2%>%
                        group_by(x_20)%>%
                        summarise_at(.vars = c('ml', 'rm', 'xm'), .funs = 'mean')
                    }
                    return(x2)

                    })
  print(ch_ch)
}
