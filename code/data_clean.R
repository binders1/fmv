#=========================
#
#   Load, clean, and prep
#    state-level dfs for
#        modeling
#
#=========================


# Load packages ####
library(tidyverse)
library(arrow)
library(lubridate)
# setwd("Y:/code")

setwd('/home/rstudio/users/gold1/fmv/code')
source('custom_functions.R')



## Nolte File Names ####
setwd("/home/rstudio/users/gold1/fmv/data")

### subset to only sales data ####
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


## Retrieve vars used in Nolte (2020) ####
nolte2020vars <- googlesheets4::range_read(ss = "1AejaWn1ZaNJBTWG2kFnhfq_rqpDKZETdrfUq41opSVU",
                                           range = "B:B") |>
  dplyr::rename(name = 1) |>
  dplyr::filter(!is.na(name)) |>
  dplyr::filter(!stringr::str_detect(name, "\\+")) |>
  dplyr::pull()

### Specify variables for aggregation ####

# vector of Nolte's variables to average 
noltevars_to_mean <- googlesheets4::range_read(ss = "1AejaWn1ZaNJBTWG2kFnhfq_rqpDKZETdrfUq41opSVU",
                                               range = "B:C") |>
  dplyr::filter(`How to Aggregate`=="Mean") |>
  dplyr::pull(`Matched to Ours`)

setwd('/home/rstudio/users/gold1/fmv/data')
# setwd("Y:/data")
# vector of Temp/Dew/precip vars to (weighted) average
climate_to_mean <- read_parquet('ArcResults/parquet/ParcelClimateIrrSoil_AL.pqt') |>
  dplyr::select(starts_with('Dew'),
         starts_with('Temp'),
         starts_with('Precip')) |>
  names()

# vector of variables to sum
noltevars_to_sum <- googlesheets4::range_read(ss = "1AejaWn1ZaNJBTWG2kFnhfq_rqpDKZETdrfUq41opSVU",
                                              range = "B:C") |>
  filter(`How to Aggregate`=="Sum") %>%
  pull(`Matched to Ours`)



## Load CPI data ####
CPI <- read_csv('CPIAUCSL.csv',
                show_col_types = F) %>%
  dplyr::rename(CPI = "CPIAUCSL") %>%
  dplyr::mutate(year = lubridate::year(DATE),
                month = lubridate::month(DATE)) %>%
  dplyr::filter(year >= 2000 & year <= 2020) %>%
  
  dplyr::mutate(CPI = CPI/dplyr::pull(dplyr::filter(., year == 2020 & month == 1),
                                      CPI))

# Load HPI Index ####
HPI_county <- readr::read_csv('HPIcounty.csv', 
                              show_col_types = F) %>%
  mutate(across(HPI_2000:HPI_2020, ~ .x/HPI_2020)) %>%
  pivot_longer(
    cols = HPI_2000:HPI_2020,
    names_to = "year",
    values_to = "HPI"
  ) %>%
    mutate(year = as.numeric(str_remove(year, "HPI_")))


pcis_pqt <- list.files('ArcResults/parquet')

# Clean Data: State Loop
for (i in seq_len(length(pcis_pqt))) {
  
  cat("\n\nTrying: ", pcis_pqt[[i]], "\n\n", sep = "")
  
  setwd("/home/rstudio/users/gold1/fmv/data")
  
  # Read Data: PCIS and Nolte (sales and crosswalk) ####
  
  state <- str_extract(pcis_pqt[[i]], "(?<=_).{2}(?=\\.pqt)")
  
  ## Read PCIS pqt into env ####
  pcis_obj <- read_parquet(file.path('ArcResults/parquet',pcis_pqt[[i]])) %>%
    dplyr::select(!`__index_level_0__`)
  
  ## Read sales data ####
  sale_obj <- read_parquet(file.path('Nolte', all_sale[[i]]))
  
  ## Read salepid crosswalk ####
  salepid_obj <- read_parquet(file.path('Nolte', all_sale_pids[[i]]))
  
  # Merge ####
  
  ## Merge sale and crosswalk ####
  mergepid_obj <- sale_obj %>%
    mutate(year = lubridate::year(date)) %>%
    dplyr::filter(year >= 2000 & year <= 2019) %>%
    left_join(salepid_obj, by = "sid")
  
  
  ## Merge Sales and PCIS ####
  df_final <- inner_join(mergepid_obj,
                         pcis_obj)
  
  cat('\n\nMerge complete')
  
  
  rm(sale_obj, salepid_obj, mergepid_obj, pcis_obj)
  gc()
  
  # Data Cleaning ####
  
  ## Create irrigation indicator variables ####
  df_final_irr <- df_final %>%
    irrFilter('irrEver') %>%
    irrFilter('irrRecent', years = 3,
              drop_used = T) %>%
    suppressWarnings()
  
  
  
  remove(df_final)
  
  ## Inflation Adjustment (Base = Jan 2020) ####
  
  df_final_inflate <- df_final_irr %>%
    mutate(month = lubridate::month(date)) %>%
    relocate(month, .after = "date") %>%
    left_join(CPI, by = c("year", "month")) %>%
    relocate(CPI, .after = "price") %>%
    mutate(price_adj = price * (CPI / 100)) %>%
    relocate(price_adj, .after = "CPI")
  
  ## Add HPI index ####
  
  df_final_HPI <- df_final_inflate %>%
    left_join(HPI_county, by = c("fips","year")) %>%
    relocate(HPI, .after = "CPI")
  
  
  ## Create logged $/ha
  
  df_final_logprice <- df_final_HPI %>%
    mutate(log_priceadj_ha = log(price_adj/ha)) %>%
    relocate(log_priceadj_ha, .after = 'price_adj')
  
  ## Clean soil ####
  
  setwd("/home/rstudio/users/gold1/fmv/data/ArcResults/soilcodes")
  soil_to_drop <- readr::read_csv(list.files()[i],
                                  show_col_types = F) %>%
    dplyr::mutate(Value = paste0("VALUE_", Value)) %>%
    dplyr::filter(stringr::str_detect(farmlndcl,
                  stringr::regex("(not prime|missing)", 
                                 ignore_case = T))) %>%
    dplyr::pull(Value)                

  df_soil_tmp <- df_final_logprice %>%
    dplyr::select(!any_of(soil_to_drop)) %>%
    pivot_longer(
      cols = starts_with("VALUE"),
      names_to = "type",
      values_to = "soil_area") %>%
    group_by(sid) %>%
    summarise(total_soil_area = sum(soil_area)) %>%
    ungroup() %>%
    select(sid, total_soil_area) 
  
  df_final_soil <- df_final_logprice %>%
    dplyr::select(!any_of(soil_to_drop)) %>%
    left_join(df_soil_tmp) %>%
    # convert sq meters to hectares
    mutate(across(c(starts_with("VALUE"), total_soil_area), 
                  ~ .x * 1e-04))
  
  setwd("/home/rstudio/users/gold1/fmv/data/")
  ## Aggregate across parcels in single sale ####
  
  cat('\n\n Starting aggregation \n')
  
  df_agg_soil <- df_final_soil %>%
    group_by(sid) %>%
    summarise(across(c(starts_with('VALUE'), total_soil_area), sum)) %>%
    ungroup() %>%
    mutate(across(starts_with('VALUE'), 
                  ~ .x / total_soil_area, .names = "{.col}_prop")) %>%
    select(sid, ends_with("prop")) %>%
    mutate(across(starts_with("VALUE"), ~ replace_na(.x, 0)))
  
  cat('\n Finished: soil \n')
  
  df_agg_mean <- df_final_soil %>%
    group_by(sid) %>%
    summarise(across(any_of(noltevars_to_mean), mean))
  
  cat('\n Finished: mean \n')
  
  df_agg_sum <- df_final_soil %>%
    group_by(sid) %>%
    summarise(across(any_of(noltevars_to_sum), sum))
  
  cat('\nFinished: sum')
  
  # aggregate monthly obs across seasons
  # and take weighted average across parcels within sid
  df_agg_climate <- df_final_soil %>%
    select(sid, pid, ha, any_of(climate_to_mean)) %>%
   # slice_sample(n = 1000) %>%
    pivot_longer(
      cols = !c(sid, pid, ha),
      names_to = "var",
      values_to = "value"
    ) %>%
    separate(
      col = var,
      into = c("name", "month"),
      sep = "_"
    ) %>%
    mutate(season = case_when(
      month %in% c('12','01','02') ~ "winter",
      month %in% c('03','04','05') ~ "spring",
      month %in% c('06','07','08') ~ "summer",
      month %in% c('09','10','11') ~ "fall",
      month == "annual" ~ "annual")) %>%
    mutate(var = paste0(name,"_",season)) %>% 
    group_by(sid, pid, var) %>%
    mutate(mean = mean(value)) %>%
    ungroup() %>%
    distinct(sid,pid,ha,var,mean) %>%
    pivot_wider(
      names_from = var,
      values_from = mean
    ) %>%
    group_by(sid) %>%
    summarise(across(!c(pid,ha), 
                     ~ weighted.mean(.x, ha)))
  
  
  cat('\nFinished: climate')
  
  df_agg_irr <- df_final_soil %>%
    group_by(sid) %>%
    summarise(across(starts_with('irr'), max))
  
  cat('\nFinished: irrigation')
  
  df_agg_none <- df_final_soil %>%
    select(c(sid, fips, HPI, log_priceadj_ha, date, ha, x, x45, y, y45)) %>%
    filter(!duplicated(sid))
  
  df_agg_final <- df_agg_none %>%
    left_join(df_agg_soil) %>%
    left_join(df_agg_mean) %>%
    left_join(df_agg_sum) %>%
    left_join(df_agg_climate) %>%
    left_join(df_agg_irr)
  
  cat('\nFinished: merge')
  
  setwd("/home/rstudio/users/gold1/fmv/data/cleaned")
  
  clean_file <- paste0("clean_",state,".pqt")
  
  arrow::write_parquet(df_agg_final, 
                       clean_file)
  
  remove(list = ls(pattern = "^df"))
  gc()
  
  cat("Saved: ",clean_file," | Trying: ", pcis_pqt[[i+1]], "\n", sep = "")
  
  
}


  