load_state <- function(state_index, .class) {
  ## Read PCIS pqt into env ####
  pcis_loc <- file.path(pqt_dir, pcis_pqt[[state_index]])
  
  pcis_obj <- 
    read_parquet(pcis_loc) %>%
    dplyr::select(!`__index_level_0__`)
  
  ## Read sales data ####
  sale_obj <- 
    read_parquet(file.path(nolte.dir, all_sale[[state_index]]))
  
  ## Read salepid crosswalk ####
  salepid_obj <- 
    read_parquet(file.path(nolte.dir, all_sale_pids[[state_index]]))
  
  list(
    sale_obj = sale_obj,
    salepid_obj = salepid_obj,
    pcis_obj = pcis_obj
  )
  
  
}
