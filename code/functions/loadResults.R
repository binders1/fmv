# FUNCs: loadResults and resultList ####

# Purpose: read in and combine model results of all three types ####

# Args ####


loadResults <- 
  function(
    model        = c("fcb", "ffb", "ffr", "ncb", "nch", "nfb", "nfr"),
    res_type     = c("importance", "metrics", "predictions", "predict_all", "performance"),
    include_mod  = TRUE,
    include_type = TRUE,
    archive = FALSE
    ) {
    
    # check args
    model <- match.arg(model)
    res_type <- match.arg(res_type)
    
    # Specify directory containing desired results type
    model_dir <-
      if (archive) {
        "~/fmv/data/model/archive"
      } else {
        "~/fmv/data/model"
      }
    
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
    
    result_list <-
      lapply(files_to_load, 
             arrow::read_parquet)
    
    result_data <-
      data.table::rbindlist(result_list,
                            fill = TRUE)
    
    if (include_mod) result_data$model <- model
    
    if (include_type) result_data$type <- res_type
    
    # return tibble dataframe
    result_data %>%
      dplyr::tibble() %>%
      dplyr::relocate(any_of(c("model", "type")))
  }
