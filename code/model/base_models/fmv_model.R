# ===================================================================
# Model across geo levels, predictor sets, and county restrictions
# 
# Author: mag
# Date: 29 Jan 2023
# ===================================================================

# General wrapper function for dispatching methods varying by geo level
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


fmv_model.county <- 
  # County modeling method
  function(
    pred.set,    # predictor set 
    HPI = TRUE   # Include HPI in predictor set?
    ) {
    
    if (!is.logical(HPI)) {
      stop("Argument `HPI` must be logical")
    }
    
    # Placeholder for random-forest model county-level function
    if (FALSE) rf_fit()
   
}


fmv_model.frr <-
  # FRR modeling method
  function(
    pred.set,                   # Predictor set
    only.nolte.counties = FALSE # Restricts sample to only counties in Nolte's county model
    ) {
    
    if (!is.logical(only.nolte.counties)) {
      stop("Argument `only.nolte.counties` must be logical")
    }
}

