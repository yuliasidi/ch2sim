plot_ch2 <- function(dt){
  pdat <- dt%>%
    tibble::as_tibble()%>%
    dplyr::mutate(
      y = ni_desy/n_sim
    )%>%
    dplyr::group_by(set_n)%>%
    dplyr::mutate(
      pt_nudge = pt + c(0,-0.75,-.25, 0.25,0.75)/100
    )%>%
    dplyr::ungroup()
  
  plot_mdsur <- ggplot() + 
    geom_segment(
      aes(x=pt-0.01,xend=pt+0.01,y=y,yend=y,colour = factor(cor_xl)),
      data = pdat%>%dplyr::filter(sur=='all'),
      alpha = 0.5
    ) +
    geom_point(
      aes(x=pt_nudge,y=y,shape=sur,colour = factor(cor_xl)),
      data = pdat%>%dplyr::filter(sur!='all')
    ) + 
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = unique(pdat$pt)) + 
    labs(x = 'Pt', y = '% Rejection',shape = 'Method',colour = 'Correlation') +
    facet_grid(. ~ n_obs) + 
    theme_bw() + 
    theme(legend.position = 'bottom')
  
  return(plot_mdsur)
  
}
