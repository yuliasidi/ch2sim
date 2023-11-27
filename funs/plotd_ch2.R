plotd_ch2 <- function(dt, cor_val = 0.4){

  pdat <- dt%>%
    dplyr::mutate(ni_p = 100 * (ni_desy/n_sim))%>%
    dplyr::select(sur, set_n, ni_p)%>%
    tidyr::spread(key = 'sur', value = 'ni_p')%>%
    dplyr::mutate(MI = all - mi, OBS = all - obs, MIN = all - `sing min`, MAX = all - `sing max`, HALF = all - `sing half`)%>%
    dplyr::select(set_n, MI, OBS, MIN, MAX, HALF)%>%
    tidyr::gather(key = 'method', value = 'diff', -set_n)%>%
    dplyr::left_join(setting, by = 'set_n')%>%
    dplyr::filter(cor_xl == cor_val)%>%
    tibble::as_tibble()%>%
    dplyr::mutate(n_obs = paste0('n=', n_obs))%>%
    dplyr::group_by(set_n)%>%
    dplyr::mutate(
      pt_nudge = pt + c(-0.5,-.25,0, 0.25, 0.5)/100
    )%>%
    dplyr::ungroup()

  plot_mdsur <-
    ggplot() +
    geom_bar(
      aes(x=pt_nudge, y = diff, fill = method),
      data = pdat, stat = 'identity') +
    #scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = unique(pdat$pt), labels = scales::percent_format(accuracy = 0.1)) +
    labs(x = 'Event probability in new treatment group', y = 'Difference in % rejections vs OBJ',
         fill = 'Method') +
    facet_grid(. ~ n_obs) +
    theme_bw(base_size = 15) +
    theme(legend.position = 'bottom')

  return(plot_mdsur)


}
