#====================
#
#   Interact with 
#  ArcResults Drive
#
#====================

# load packages
library(tidyverse)
library(googledrive)

# Drive Auth ####

if (!drive_has_token()) {
  
  drive_auth('gold1@roar.stolaf.edu')
  
  }

# Dest dir for pqt files
pqt_dir <- file.path(ddir, "ArcResults", "parquet")

## Specify Drive Location ####

drive_id_arc <- as_id("https://drive.google.com/drive/folders/1oHbTsYdQMY86T69nszp1wj47X9HfBDiT")

## inspect drive contents
drive_contents <- drive_ls(drive_id_arc) 

# Load Parquet files ####

pcis_pqt <- 
  drive_contents %>%
  filter(str_detect(name, "pqt"))

## vector of state pqt files NOT already in folder ####
pcis_to_load <- 
  pcis_pqt %>%
  filter(!is.element(name, list.files(pqt_dir))) %>%
  pull(name)

walk(pcis_to_load,
     ~ drive_download(file = .x, 
                      path = file.path(pqt_dir, .x),
                      overwrite = TRUE)
     )
