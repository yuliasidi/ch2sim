rbind_results <- function(results_list) {

  ct_des <- lapply(results_list, function(x) {x[["ct_des"]]}) |> dplyr::bind_rows()
  ch <- lapply(results_list, function(x) {x[["ch"]]}) |> dplyr::bind_rows()

  return(list(ct_des = ct_des, ch = ch))
}

