
make_results_path.state <- 
  function(state, model_abbr = c("fcb", "ncb", "nch")) {
    
    model_abbr <- match.arg(model_abbr)
    
    root_dir <- file.path(ddir, "model")
    model_dir <-
      switch(model_abbr,
             fcb = "full/county/base",
             ncb = "nolte/county/base",
             nch = "nolte/county/hpi")
    
    dest_dir <- file.path(root_dir, model_dir)
  
    if (!dir.exists(dest_dir)) {
      stop("`", dest_dir, "` does not exist")
    }
    
    result_types <- 
      c("predict_all", "predictions", 
        "metrics", "importance")
    
    result_dirs <-
      file.path(dest_dir, result_types)
    
    result_files <- 
      paste0(result_types, "_", model_abbr, "_", state, ".pqt")

    file.path(result_dirs, result_files) %>%
      set_names(result_types) %>% 
      as.list()
}

make_model_abbr <- function(pred.set, HPI) {
  
  pred.set.abbr <- str_sub(pred.set, 1, 1)
  
  model_abbr <- 
    if (pred.set == "nolte" & HPI) {
      paste0(pred.set.abbr, "ch")
    } else {
      paste0(pred.set.abbr, "cb")
    }
  
  model_abbr
}