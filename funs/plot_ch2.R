plot_ch2 <- function(dt){
  pdat <- dt%>%
    tibble::as_tibble()%>%
    dplyr::mutate(
      y = ni_desy/n_sim,
      n_obs = paste0('n=', n_obs)
    )%>%
    dplyr::group_by(set_n)%>%
    dplyr::mutate(
      pt_nudge = pt + c(0,-0.75,-.25, 0.25,0.75)/100
    )%>%
    dplyr::ungroup()
  
  plot_mdsur <- ggplot() + 
    geom_segment(
      aes(x=pt-0.01,xend=pt+0.01,y=y,yend=y),
      data = pdat%>%dplyr::filter(sur=='all'),
      alpha = 0.5
    ) +
    geom_point(
      aes(x=pt_nudge,y=y,shape=factor(cor_xl), colour = sur),
      data = pdat%>%dplyr::filter(sur!='all')
    ) + 
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = unique(pdat$pt)) + 
    labs(x = 'Probability of event in standard treatment group', y = '% Rejection',
         colour = 'Method', shape = 'Correlation') +
    facet_grid(. ~ n_obs) + 
    theme_bw(base_size = 15) + 
    theme(legend.position = 'bottom',
          panel.grid.minor.x =   element_line(colour = "#54545B")) +
    scale_color_discrete(label = c("FULL", 'MI','OBS',"MAX", "MIN"))
  
  return(plot_mdsur)
  
}
