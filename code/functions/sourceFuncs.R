
sourceFuncs <- function() {
  
  func_to_load <- file.path("~/fmv/code/functions", list.files("~/fmv/code/functions"))
  
  walk(func_to_load, source)
  
}

