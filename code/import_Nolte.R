
#=========================
#
#       Download
#    Nolte .pqt data
#         and
#   Binder ArcResults
# 
#=========================


## load packages ####
library(tidyverse)
library(googledrive)
library(arrow)


## Google Drive Connection: Nolte ####

if (!drive_has_token()) {
  drive_auth("gold1@roar.stolaf.edu")
}

drive_id_nolte <- as_id("https://drive.google.com/drive/folders/1HQimnfgzAEQz1TYYFieoZJBbsn9Wn79x")

nolte_drive <- drive_ls(drive_id_nolte) |>
  arrange(name)

nolte_noshp <- nolte_drive |>
  filter(!str_detect(name, "(shp|prior|gpkg)"))




#### SALES RECORDS ####

## Download all state sales files from Google Drive ####

# set working directory to be the data folder
getwd()
# setwd("fmv/data")

## download from Drive and unzip
for (i in seq_len(nrow(nolte_noshp))) {
  
  drive_download(nolte_noshp[i,], path = paste0("zipped/",pull(nolte_noshp[i,1])))
  
  }


setwd("/home/rstudio/users/gold1/fmv/data")

nolte_files <- list.files('zipped')

walk(nolte_files, ~ unzip(file.path('zipped', .x),
                         exdir = 'Nolte'))

## File Names ####

### subset to only sale pqt files ####
all_sale <- list.files('Nolte') |>
  tibble() |>
  rename(name = 1) |>
  filter(str_detect(name, "_sale\\.pqt$")) |>
  pull()

### subset to only sale_pids crosswalk files ####
all_sale_pids <- list.files('Nolte') |>
  tibble() |>
  rename(name = 1) |>
  filter(str_detect(name, "sale_pids")) |>
  pull()

setwd("/home/rstudio/users/gold1/fmv")




