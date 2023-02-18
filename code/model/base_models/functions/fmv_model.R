# ===================================================================
# Model across geo levels, predictor sets, and county restrictions
# 
# Author: mag
# Date: 29 Jan 2023
# ===================================================================

# General wrapper function for dispatching to methods by geo level
fmv_model <- 
  function(
    geo = c("county", "frr"),      # geographic level at which to model
    pred.set = c("full", "nolte"), # predictor set ('nolte': see Nolte, 2020)
    ...                            # add'l args to send to geo-specific method  
    ) {
    
    # Check args
    geo <- match.arg(geo)
    pred.set <- match.arg(pred.set)
  
    # Dispatch method to correct geo level
    geo_model <-
      switch(geo,
             county = fmv_model.county,
             frr = fmv_model.frr)
    
    # Execute geo-specific method
    geo_model(pred.set, ...)
    
}

# County modeling method ======================================================
fmv_model.county <- 
  function(
    pred.set,  # predictor set 
    HPI        # Include HPI in predictor set?
    ) {
    
    if (!is.logical(HPI)) stop("Argument `HPI` must be logical, not ", class(HPI))
    
    # Model all counties, one state at a time
    walk(
      all_states,
      ~ model_state_counties(.x, pred.set, HPI)
    )
}

# FRR modeling method =========================================================
fmv_model.frr <-
  function(
    pred.set,           # Predictor set
    only.nolte.counties # Restricts sample to only counties in Nolte's county model
    ) {
    
    if (!is.logical(only.nolte.counties)) {
      stop("Argument `only.nolte.counties` must be logical, not", class(only.nolte.counties))
    }
    
    # Prep parallel workers
    unregisterCores()
    if (foreach::getDoParWorkers() < 32) doParallel::registerDoParallel(32)
    
    n_iters <- length(frr_key$frr_name)
    
    state_rf_fit <-
      foreach::foreach(
        frr = frr_key$frr_name,
        pred.set = rep(pred.set, n_iters),
        only.nolte.counties = rep(only.nolte.counties, n_iters)) %dopar% {
          
          frr_model(frr = frr, 
                    pred.set = pred.set,
                    only.nolte.counties = only.nolte.counties)
        }
    
    unregisterCores()

}

