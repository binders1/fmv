

# Read in cleaned FMV data by state ========================================= #

read_state_clean <- function(state) {
  
  # Vector of all state pqt files
  all_clean <- 
    list.files(
      clean_dir, 
      full.names = TRUE,
      pattern = "\\.pqt$")
  
  # Current state file path
  state_to_read <- clean_path[grepl(state, clean_path)]
  
  message('\nTrying: ', state)
  
  # Read in current state data safely
  tryCatch(
    arrow::read_parquet(state_to_read),
    error = function(e) 
      message('\nData import error occured in `', state, "`")
  )
}



# Generate character vector of model predictors ============================== #
predictor_set <- 
  function(
    data,                          # modeling data
    geo = c("county", "frr"),      # geographic level of model 
    pred.set = c("full", "nolte"), # predictor set to use
    HPI = TRUE                     # whether HPI should be included
  ) {
    
    # Check args
    geo <- match.arg(geo)
    pred.set <- match.arg(pred.set)
    
    predictors <-
      switch(pred.set,
             # Full set is all columns in dataframe
             full = names(data),
             # Nolte set includes Nolte 2020, plus a few convenience variables
             # HPI is included here, and can 
             nolte = 
               c(nolte2020vars, "log_priceadj_ha", "fips", "sid", "x45", "y45"))
    
    
    # -- Adjustments to predictor set, depending on full/nolte and geo level -- #
    
    # In Full predictor set...
    if (pred.set == "full") {
      
      # ... remove the following:
      switch(geo,
             # 1) `MHV` when modeling at the county level
             county = predictors[!predictors %in% "MedianHomeValue"],
             # 2) `HPI` when modeling at the FRR level
             frr = predictors[!predictors %in% "HPI"])
      
    } else {
      # In Nolte set, we add HPI if desired (only at county level)
      if (HPI & geo == "county") {
        c(predictors, "HPI")
      } else {
        # If no HPI requested, or at the FRR level, then no adjustments
        predictors
      }
    }
    
  }

predictor_set(df_import, geo = "county", pred.set = "full")
predictor_set(df_import, geo = "county", pred.set = "nolte", HPI = FALSE)
predictor_set(df_import, geo = "county", pred.set = "nolte", HPI = TRUE)

predictor_set(df_import, geo = "frr", pred.set = "full")
predictor_set(df_import, geo = "frr", pred.set = "nolte")

