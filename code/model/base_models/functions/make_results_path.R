
# Generate file path to save model results ====================================

make_results_path <- function(geo = c("county", "frr"), ...) {
  geo <- match.arg(geo)
  path_fn <- switch(geo,
                    county = make_results_path.state,
                    frr = make_results_path.frr)
  path_fn(...)
  
  }

# FRR method ==================================================================
make_results_path.frr <- 
  function(frr, model_abbr = c("ffb", "ffr", "nfb", "nfr")) {
  model_abbr <- match.arg(model_abbr)
  
  root_dir <- file.path(ddir, "model")
  
  model_dir <- switch(model_abbr,
                      ffb = "full/frr/base",
                      ffr = "full/frr/restricted",
                      nfb = "nolte/frr/base",
                      nfr = "nolte/frr/restricted")
  
  frr_num_id <- which(frr_key$frr_name == frr)
  
  dest_dir <- file.path(root_dir, model_dir)
  
  if (!dir.exists(dest_dir)) stop("`", dest_dir, "` does not exist")
  
  result_types <- 
    c("predict_all", "predictions", 
      "metrics", "importance")
  
  result_dirs <-
    file.path(dest_dir, result_types)
  
  result_files <- 
    paste0(result_types, "_", model_abbr, "_", frr_num_id, ".pqt")
  
  file.path(result_dirs, result_files) %>%
    set_names(result_types) %>% 
    as.list()

  }

# County method ===============================================================
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
  
    if (!dir.exists(dest_dir)) stop("`", dest_dir, "` does not exist")
    
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


# Generate model abbreviation ==================================================

# First letter (f/n): predictor set
# Second letter (f/c): geographic level
# Third letter (b/h/r): base, HPI (nolte-county only), restricted (FRR only)

make_model_abbr <- function(geo = c("county", "frr"), ...) {
  geo <- match.arg(geo)
  abbr_fn <- switch(geo, 
                    county = make_model_abbr.county,
                    frr = make_model_abbr.frr)
  abbr_fn(...)
}

make_model_abbr.frr <- function(pred.set, only.nolte.counties) {
  pred.set.abbr <- str_sub(pred.set, 1, 1)
  suffix <- ifelse(only.nolte.counties, "r", "b")
  paste0(pred.set.abbr, "f", suffix)
  }

make_model_abbr.county <- function(pred.set, HPI) {
  
  pred.set.abbr <- str_sub(pred.set, 1, 1)
  
  if (pred.set == "nolte" & HPI) {
    paste0(pred.set.abbr, "ch")
    } else {
      paste0(pred.set.abbr, "cb")
      }
}
