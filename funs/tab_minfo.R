tab_minfo <- function(dt, subj_miss = FALSE){

  if(!subj_miss){
    dt%>%
      dplyr::group_by(set_n)%>%
      dplyr::summarise(mminfo = mean(minfo))%>%
      dplyr::left_join(setting, by ='set_n')%>%
      dplyr::select(pt, cor_xl, n_obs, mminfo)%>%
      dplyr::arrange(pt)
  }
  else{
    dt%>%
      dplyr::group_by(set_n)%>%
      dplyr::summarise(mminfo = mean(minfo), mmsinfo = mean(msinfo))%>%
      dplyr::left_join(setting, by ='set_n')%>%
      dplyr::select(pt, cor_xl, n_obs, mminfo, mmsinfo)%>%
      dplyr::arrange(pt)
  }
    
  
}
