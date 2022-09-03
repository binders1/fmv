# FUNCs: loadResults and resultList ####

# Purpose: read in and combine model results of all three types ####

# Args ####


loadResults <- function(model = c("fcb", "ffb", "ffr", 
                                  "ncb", "nch", "nfb", "nfr"),
                        res_type = c("importance", "performance", 
                                     "predictions"),
                        inc_mod = TRUE, inc_type = TRUE) {
  
  if (length(res_type) > 1) {
    
    stop("res_type must be character string of length 1")
    
  }
  
  if (length(model) > 1) {
    
    stop("model must be character string of length 1")
    
  }
  
  mod_dir <- "~/fmv/data/model"
  
  full_dir <- file.path(mod_dir, "full")
  nolte_dir <- file.path(mod_dir, "nolte")
  
  # full models 
  ## county
  fcb <- file.path(full_dir, "county/base")
  ## frr
  ffb <- file.path(full_dir, "frr/base")
  ffr <- file.path(full_dir, "frr/restricted")
  
  # nolte models
  ## county
  ncb <- file.path(nolte_dir, "county/base")
  nch <- file.path(nolte_dir, "county/hpi")
  ## frr
  nfb <- file.path(nolte_dir, "frr/base")
  nfr <- file.path(nolte_dir, "frr/restricted")
  
  # specify directory containing desired results type
  target_dir <- 
    file.path(get(model), res_type)
  
  files_to_load <- 
    list.files(target_dir, full.names = T)
  
  res_list <-
    lapply(files_to_load, 
           arrow::read_parquet)
  
  out <-
    data.table::rbindlist(res_list,
                          fill = T)
  
  if (inc_mod) {
    out$model <- model
  }
  
  if (inc_type) {
    out$type <- res_type
  }
  
  out <- dplyr::tibble(out) %>%
    dplyr::relocate(c(model, type))
  
  return(out) 
  
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
  
  return(outlist)
  
}


