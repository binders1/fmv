
process_state <- function(state_index) {
  
  #============================================================
  # 01) Load data, merge, and perform minor cleaning steps
  #============================================================
  
  clean_base <-
    
    # Load sales, sale <-> parcel crosswalk, and processed climate/soil vars
    load_state(state_index = state_index) %>%
    
    # merge all together
    initial_merge() %>% 
    
    # perform inflation adjustments 
    clean_inflation() %>%

    # Add HPI index ####
    clean_HPI() %>%
    
    # Create logged $/ha
    clean_logprice()
  
  #================================================================
  # 02) Clean and aggregate: irrigation, soil, climate, and others
  #================================================================
  
    # clean and aggregate irrigation variables
    clean_agg_irrigation <- 
      agg_irrigation(clean_base)
  
    # clean and aggregate soil variables
    clean_agg_soil <-
      agg_soil(clean_base, 
               state_index = state_index)
    
    # clean and aggregate climate variables 
    clean_agg_climate <-
      agg_climate(clean_base)
    
    # agg_mean
  
    # agg_sum
  
    # final_merge 
    
    # write to disk
  
}