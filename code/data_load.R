
# Load packages ####
library(tidyverse)
library(arrow)
library(lubridate)

setwd('/home/rstudio/users/gold1/fmv/code')
source('custom_functions.R')

# set wd to data folder
setwd("/home/rstudio/users/gold1/fmv/data")

## Retrieve vars used in Nolte (2020) ####
nolte2020vars <- googlesheets4::range_read(ss = "1AejaWn1ZaNJBTWG2kFnhfq_rqpDKZETdrfUq41opSVU",
                                           range = "B:B") |>
  dplyr::rename(name = 1) |>
  dplyr::filter(!is.na(name)) |>
  dplyr::filter(!str_detect(name, "\\+")) |>
  dplyr::pull()

## Load CPI data ####
CPI <- read_csv('CPIAUCSL.csv') %>%
  dplyr::rename(CPI = "CPIAUCSL") %>%
  dplyr::mutate(year = lubridate::year(DATE),
                month = lubridate::month(DATE)) %>%
  dplyr::filter(year >= 2000 & year <= 2020) %>%
  
  dplyr::mutate(CPI = CPI/dplyr::pull(dplyr::filter(., year == 2020 & month == 1),
                                      CPI))

# Read Data: PCIS and Nolte (sales and crosswalk) ####


## Set obj names for programmatic assignment ####
i=4
sale_obj <- paste0(stringr::str_remove(all_sale[[i]], 
                                       "\\.pqt"),
                   "_rm")

salepid_obj <- paste0(str_remove(all_sale_pids[[i]],
                                 "\\.pqt"),
                      "_rm")

state <- str_extract(sale_obj, 
                     ".{2}(?=_)")

mergepid_obj <- paste0("merge", state, "_rm")

pcis_obj <- paste0(str_remove(pcis_pqt[[i]], 
                              "\\.pqt"),
                   "_rm")

df_final <- paste0(state, "_final")


## Read PCIS pqt read into env ####
assign(pcis_obj, 
       read_parquet(file.path('ArcResults/parquet',pcis_pqt[[i]])) %>%
         dplyr::select(!`__index_level_0__`))

## Read sales data ####
assign(sale_obj,
       read_parquet(file.path('Nolte', all_sale[[i]]))
)

## Read salepid crosswalk ####
assign(salepid_obj,
       read_parquet(file.path('Nolte', all_sale_pids[[i]]))
)

# Merge ####

## Merge sale and crosswalk ####
assign(mergepid_obj,
       get(sale_obj) %>%
         mutate(year = year(date)) %>%
         dplyr::filter(year >= 2000 & year <= 2019) %>%
         left_join(get(salepid_obj), by = "sid")
)


## Merge Sales and PCIS ####
assign(
  df_final,
  inner_join(get(mergepid_obj),
             get(pcis_obj))
)


rm(list = ls(pattern = "^.+_rm$"))


# Data Cleaning ####

## Create irrigation indicator variables ####
assign(df_final,
       get(df_final) %>%
         irrFilter('irrEver') %>%
         irrFilter('irrRecent', years = 3)
)

## Clean soil codes ####

##============================##
##============================##


## Inflation Adjustment (Base = Jan 2020) ####

assign(df_final,
       get(df_final) %>%
         mutate(month = month(date)) %>%
         relocate(month, .after = "date") %>%
         left_join(CPI, by = c('year','month')) %>%
         relocate(CPI, .after = "price") %>%
         mutate(price_adj = price*(CPI/100)) %>%
         relocate(price_adj, .after = "CPI")
)


## Create logged $/ha

assign(df_final,
       get(df_final) %>%
         mutate(log_priceadj_ha = log(price_adj/ha)) %>%
         relocate(log_priceadj_ha, .after = 'price_adj')
)
