
source_dir <- function(dir) {
  
  walk(
    list.files(dir, full.names = TRUE),
    source
  )
  
  files_in_dir <- 
    list.files(dir) %>%
    paste0(" *", ., collapse = "\n")
  
  message("Sourced `", dir, "`\n", files_in_dir, "\n")
}