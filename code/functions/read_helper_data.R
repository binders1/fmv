# Read in file from data/helper_dir
# Supports csv and parquet files

read_helper_data <- function(filename, ...) {
  
  ext <- str_extract(filename, "(?<=\\.).+$")
  
  .fn <- switch(ext,
                csv = readr::read_csv,
                pqt = arrow::read_parquet
                )
    .fn(
      file.path(helper_dir, filename),
      ...
      )

}