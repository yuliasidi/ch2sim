dt_subj <- function( bx, pc = pc, pt = pt, n_obs = n_obs, muxc = 4, sdxc = 8, muxt = 2, sdxt = 4){
  
  dtc <- tibble::tibble(x = rnorm(n_obs, mean = muxc, sd = sdxc))
  dtt <- tibble::tibble(x = rnorm(n_obs, mean = muxt, sd = sdxt))
  
  dt <- dplyr::bind_rows(dtc%>%dplyr::mutate(trt='c', trtn = 0),
                         dtt%>%dplyr::mutate(trt='t', trtn = 1))
  
  bt <- log(pt*(1 - pc)/(pc*(1 - pt)))
  
  int_dts <- log(pc/(1-pc)) - bx*muxc
  bxt <- - (bx * (muxt - muxc)/muxt)
  
  dt <- dt%>%dplyr::mutate(py = 1/(1 + exp(-int_dts - bt*trtn - bx*x - bxt*x*trtn)),
                           pthresh = runif(dplyr::n()),
                           y = ifelse(py > pthresh, 1, 0))%>%
    dplyr::select(-c(py, pthresh))
  
  dt

}
