# FUNCs: loadResults and resultList ####

# Purpose: read in and combine model results of all three types ####

# Args ####


loadResults <- 
  function(
    model        = c("fcb", "ffb", "ffr", "ncb", 
                     "nch", "nfb", "nfr"),
    res_type     = c("importance", "performance", "predictions"),
    include_mod  = TRUE,
    include_type = TRUE
    ) {
    
    # check args
    model <- match.arg(model)
    res_type <- match.arg(res_type)
    
    if (length(res_type) > 1) stop("res_type must be character string of length 1")
    if (length(model) > 1) stop("model must be character string of length 1")
    
    
    # Specify directory containing desired results type 
    model_dir <- "~/fmv/data/model"
    
    full_dir <- file.path(model_dir, "full")
    nolte_dir <- file.path(model_dir, "nolte")
    
    model_dir <- 
      switch(model,
             fcb = file.path(full_dir, "county/base"),
             
             ffb = file.path(full_dir, "frr/base"),
             ffr = file.path(full_dir, "frr/restricted"),
             
             ncb = file.path(nolte_dir, "county/base"),
             nch = file.path(nolte_dir, "county/hpi"),
             
             nfb = file.path(nolte_dir, "frr/base"),
             nfr = file.path(nolte_dir, "frr/restricted")
      )
    
    target_dir <- 
      file.path(model_dir, res_type)
    
    files_to_load <- 
      list.files(target_dir, full.names = TRUE)
    
    res_list <-
      lapply(files_to_load, 
             arrow::read_parquet)
    
    out <-
      data.table::rbindlist(res_list,
                            fill = TRUE)
    
    if (include_mod) {
      out$model <- model
    }
    
    if (include_type) {
      out$type <- res_type
    }
    
    # return tibble dataframe
    dplyr::tibble(out) %>%
      dplyr::relocate(c(model, type))
    
  }




resultList <- function(model) {
  
  outlist <-
    purrr::map2(
      .x = model,
      .y = c("importance", "performance", 
             "predictions"),
      loadResults
    )
  
  names(outlist) <- c("importance", "performance", "predictions") 
  
  outlist
  
}


