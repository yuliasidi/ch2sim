
tab_sum <- function(dt, rho = 0.4){
 
   dt%>%
    dplyr::mutate(ni_p = 100 * (ni_desy/n_sim))%>%
    dplyr::select(sur, set_n, ni_p)%>%
    tidyr::spread(key = 'sur', value = 'ni_p')%>%
    dplyr::rename(FULL = all, MI = mi , OBS = obs, MIN = `sing min`, MAX = `sing max`)%>%
    dplyr::left_join(setting, by = 'set_n')%>%
    dplyr::filter(cor_xl==rho)%>%
    dplyr::arrange(pt)%>%
    dplyr::select(pt, n_obs, FULL, MI, OBS, MIN, MAX)
  
}

