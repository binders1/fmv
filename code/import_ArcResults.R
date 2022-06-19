#====================
#
#
#   Interact with 
#  ArcResults Drive
#
#
#====================

setwd('data')


## Drive Auth ####

if (!drive_has_token()) {
  
  drive_auth('gold1@roar.stolaf.edu')
  
  }


## Specify Drive Location ####

drive_id_arc <- as_id("https://drive.google.com/drive/folders/1oHbTsYdQMY86T69nszp1wj47X9HfBDiT")


## Retrieve Soil Codes ####

## specify names of all states' soil code csv
soilcodes <- drive_ls(drive_id_arc) %>%
  filter(str_detect(name, "soilcodes.+csv$")) %>%
  .[[1]]

## download from drive
walk(soilcodes,
    ~ drive_download(.x, path = file.path('ArcResults','soilcodes',.x),
                     overwrite = T))

soilcode_csvs <- list.files('ArcResults/soilcodes')

## Find the maximum soil codes across all indiv states
max_n <- map(soilcode_csvs, 
     ~ nrow(read_csv(file.path('ArcResults/soilcodes', .x),
                     show_col_types = F))) %>%
  unlist() %>%
  max()




## Load each state's soil codes 
## and collate them into a single df
soil_tbl <- tibble(rowid = seq_len(max_n))

for (i in seq_len(length(soilcode_csvs))) {
  
  state <- str_extract(soilcode_csvs[[i]], "(?<=_)[:upper:]{2}")
  
  tmp <- read_csv(file.path('ArcResults/soilcodes',
                            soilcode_csvs[[i]]),
                  show_col_types = F) %>%
    arrange(farmlndcl) %>%
    rowid_to_column() %>%
    select(rowid, farmlndcl) %>%
    rename({{ state }} := "farmlndcl")
  
  
  soil_tbl <- soil_tbl %>%
    left_join(tmp)
  
}



## Load Parquet files ####

## vector of all state pqt files
pcis_pqt <- drive_ls(drive_id_arc) %>%
  filter(str_detect(name, "pqt")) %>%
  pull(name) |>
  sort()

## download and read parquet file by state
setwd('/home/rstudio/users/gold1/fmv/data/ArcResults/parquet')

# download from drive...
walk(pcis_pqt,
     ~ drive_download(file = .x, path = .x,
               overwrite = T)
)

# ...then read into env
assign(str_remove(pcis_pqt[[2]], "\\.pqt"), 
       read_parquet(pcis_pqt[[2]]))

# reset wd to fmv
setwd('/home/rstudio/users/gold1/fmv')
