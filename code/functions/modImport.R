# FUNC: modImport ####

# Purpose: reads in and row-binds model performance/predictions/importance dfs ####

# Args ####

## parent_dir: charater string indicating directory storing all stats folders

## stat_dir: character string indicating a folder within parent_dir

## file_prefix: character string indicating the beginning of each 
## file (e.g., "import_frr_") 

## file_suffix: vector of file endings (e.g., c("AL","AR", etc.))

## add_source: optional character string, will add a column indicating model 
## source (e.g., "Nolte" or "Full")


modImport <- function(parent_dir, stat_dir, 
                      file_prefix, file_suffix,
                      add_source=NULL) {
  
  if (!parent_dir %in% list.files(getwd())) {
    
    stop("\n 'parent_dir' not found. Check working directory")
    
  }
  
  
  path <- file.path(parent_dir, stat_dir, file_prefix)
  
  
  tryCatch(
    
    out <- purrr::map_dfr(
      file_suffix,
      ~ arrow::read_parquet(paste0(path, .x, ".pqt"))
    ),
    
    error = function(e)
      cli::cli_alert_warning("Error importing data")
  )
  
  
  
  if(!missing(add_source)) {
    
    out <- dplyr::mutate(out, source = add_source)
    
  }
  
  return(out)
  
}
