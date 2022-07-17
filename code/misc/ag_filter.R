

ag_filter <- function(data) {
  
  out <- 
    data 
  
  return(out)
  
}

for (i in seq_len(length(all_sale))) {
  
  state <- str_extract(all_sale[[i]], "[:upper:]{2}")
  
  setwd("~/fmv/data/Nolte")

  data <- read_parquet(all_sale[[i]]) %>%
    dplyr::select(sid, bld_code) %>%
    dplyr::filter(str_detect(bld_code, "AG"))
  
  setwd("~/fmv/data/ag_filter")
  
  write_parquet(data, paste0("ag_filter_", state, ".pqt"))
  
}




