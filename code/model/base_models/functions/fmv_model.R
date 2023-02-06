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
    pred.set,    # predictor set 
    HPI = TRUE   # Include HPI in predictor set?
    ) {
    
    if (!is.logical(HPI)) stop("Argument `HPI` must be logical, not ", class(HPI))
    
    # Model all counties, one state at a time
    walk(
      all_states,
      fmv_state_model
    )
}

# FRR modeling method =========================================================
fmv_model.frr <-
  function(
    pred.set,                   # Predictor set
    only.nolte.counties = FALSE # Restricts sample to only counties in Nolte's county model
    ) {
    
    if (!is.logical(only.nolte.counties)) {
      stop("Argument `only.nolte.counties` must be logical")
    }
}

