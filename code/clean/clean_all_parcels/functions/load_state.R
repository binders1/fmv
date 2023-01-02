
load_state.pc <- function(state_index) {
  
  ## Read parcel data 
  pc_obj <- 
    all_pc[[state_index]] %>%
    read_parquet() %>%
    
    # Remove vars not used in sale-based model
    dplyr::select(
      !any_of(c("bld_yr_eff")),
      !c(dplyr::matches("^mv_.*_za$"), dplyr::matches("^p_(a|u)_lcmap_"))
    )
  
  ## Read PCIS data
  pcis_obj <- 
    pcis_pqt[[state_index]] %>%
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
