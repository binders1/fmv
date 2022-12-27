
load_state <- function(state_index) {
  UseMethod("state_index")
}

load_state.pc <- function(state_index) {
  
  ## Read parcel data 
  pc_loc <- 
    file.path(nolte.dir, all_pc[[state_index]]) %>%
    read_parquet() %>%
    # Remove vars not used in sale-based model
    dplyr::select(
      !c(bld_yr_eff, dplyr::matches("^mv_.*_za$"),
         dplyr::matches("^p_(a|u)_lcmap_"))
      )
  
  ## Read PCIS data
  pcis_loc <- 
    file.path(pqt_dir, pcis_pqt[[state_index]]) %>%
    read_parquet() %>%
    dplyr::select(!`__index_level_0__`)
  
  
  # Return list of pc and pcis dataframes, with class pc
  # for initial_merge method dispatch
  structure(
    list(
      pc_obj = pc_obj,
      pcis_obj = pcis_obj
    ),
    class = "pc"
  )
  
}
