# Read in cleaned FMV data from a single state 

read_state_clean <- function(state) {
  
  state <- toupper(state)
  
  # Vector of all state pqt files
  all_clean <- 
    list.files(
      clean_dir, 
      full.names = TRUE,
      pattern = "\\.pqt$")
  
  if (all(!grepl(state, all_clean))) {
    stop("`", state, "` not found in cleaned state data")
  }
  
  # Current state file path
  state_to_read <- all_clean[grepl(state, all_clean)]
  
  message("Loading: ", state)
  
  # Read in current state data safely
  tryCatch(
    arrow::read_parquet(state_to_read),
    error = function(e) 
      message('\nData import error occured in `', state, "`")
  )
}