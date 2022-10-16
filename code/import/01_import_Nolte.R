
#=========================
#
#       Download
#    Nolte .pqt data
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

nolte_drive <- drive_ls(drive_id_nolte) %>%
  arrange(name)

nolte_noshp <- nolte_drive %>%
  filter(!str_detect(name, "(shp|prior|gpkg)"))




#### SALES RECORDS ####

## Download all state sales files from Google Drive ####
walk(
  nolte_noshp$name,
  ~ drive_download(.x, 
                   path = file.path(z.dir, .x),
                   overwrite = TRUE)
  )

## Unzip

zipped_files <- list.files(z.dir)

walk(zipped_files, 
     ~ unzip(file.path(z.dir, .x),
             exdir = file.path(ddir, "Nolte"))
     )


## Nolte File Names ####

nolte_files <- 
  data.frame(
    filename = list.files(file.path(ddir, "Nolte"))
    )

### extract sale pqt files ####
all_sale <- 
  nolte_files %>%
  filter(str_detect(filename, "_sale\\.pqt$")) %>%
  pull()

### extract sale <-> pids crosswalk files ####
all_sale_pids <- 
  nolte_files %>%
  filter(str_detect(filename, "sale_pids")) %>%
  pull()

