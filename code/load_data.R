
# Load packages ####
library(tidyverse)
library(arrow)
library(lubridate)

# set wd to data folder
setwd("/home/rstudio/users/gold1/fmv/data")


# Read Data: PCIS and Nolte (sales and crosswalk) ####


## Set obj names for programmatic assignment ####

sale_obj <- str_remove(all_sale[[2]], "\\.pqt")
salepid_obj <- str_remove(all_sale_pids[[2]], "\\.pqt")

state <- str_extract(sale_obj, ".{2}(?=_)")

mergepid_obj <- paste0("merge", state)

pcis_obj <- str_remove(pcis_pqt[[2]], "\\.pqt")

df_final <- paste0(state, "_final")


## Retrieve vars used in Nolte (2020) ####
nolte2020vars <- googlesheets4::range_read(ss = "1AejaWn1ZaNJBTWG2kFnhfq_rqpDKZETdrfUq41opSVU",
                                           range = "B:B") |>
  rename(name = 1) |>
  filter(!is.na(name)) |>
  filter(!str_detect(name, "\\+")) |>
  pull()




## Read PCIS pqt read into env ####
assign(pcis_obj, 
       read_parquet(file.path('ArcResults/parquet',pcis_pqt[[2]])) %>%
         select(!`__index_level_0__`))

## Read sales data ####
assign(sale_obj,
       read_parquet(file.path('Nolte', all_sale[[2]]))
)

## Read salepid crosswalk ####
assign(salepid_obj,
       read_parquet(file.path('Nolte', all_sale_pids[[2]]))
)

# Merge ####

## Merge sale and crosswalk ####
assign(mergepid_obj,
       get(sale_obj) %>%
         mutate(year = year(date)) %>%
         filter(year >= 2000 & year <= 2019) %>%
         left_join(get(salepid_obj), by = "sid")
       )


## Merge Sales and PCIS ####
assign(
  df_final,
  inner_join(get(mergepid_obj),
             get(pcis_obj))
)



# Data Cleaning ####

## Create irrigation indicator variables ####
assign(df_final,
       get(df_final) %>%
         irrFilter('irrEver') %>%
         irrFilter('irrRecent', years = 3) 
       )

## Clean soil codes ####

AR_soilcode <- read_csv(file.path('ArcResults/soilcodes', soilcode_csvs[[2]])) %>%
  mutate(soil_type = paste0("VALUE_",Value))


soil_test <- get(df_final) %>%
  pivot_longer(
    cols = starts_with('VALUE'),
    names_to = 'soil_type',
    values_to = 'soil_sqm') %>%
  
  slice_sample(n = 50000) %>%
  
  left_join(AR_soilcode %>%
              select(farmlndcl, soil_type)) %>%
  
  mutate(
    soil_sqm_log = case_when(
      soil_sqm > 0 ~ log(soil_sqm),
      soil_sqm == 0 ~ 0),
    price_log = case_when(
      price > 0 ~ log(price),
      price == 0 ~ 0
      )
    ) %>%
  filter(soil_sqm > 0)

soil_test %>%

  ggplot(aes(soil_sqm_log, price_log, color = soil_type))+
  
  geom_point(alpha = 0.6, 
             position = "jitter")+
  
  scale_colour_viridis_d()+
  
  scale_x_continuous(labels = ~ as.integer(.x))+

  facet_wrap(~farmlndcl,
             labeller = labeller(farmlndcl = label_wrap_gen(45)))+
  
  labs(
    title = "Price by Soil Type: Arkansas",
    subtitle = "Logged price (USD) against logged soil (sq. meters), N = 10,434 (sample)",
    y = "Logged Price ($)",
    x = "Logged Soil (sq. m)"
  )+
  
  theme(axis.ticks = element_blank(), 
        text = element_text(size = 16, family = "IBM Plex Sans"),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey92"),
        panel.border = element_blank(), 
        strip.background = element_blank(), 
        plot.background = element_blank())
  


## Inflation Adjustment ####

CPI <- read_csv('data/CPIAUCSL.csv') %>%
  rename(CPI = "CPIAUCSL") %>%
  mutate(year = lubridate::year(DATE)) %>%
  group_by(year) %>%
  summarise(CPI = mean(CPI, na.rm =TRUE)) %>%
  filter(year >= 2000 & year <= 2020)

assign(df_final,
       get(df_final) %>%
         left_join(CPI) %>%
         relocate(CPI, .after = "price") %>%
         mutate(price_adj = price*(CPI/100)) %>%
         relocate(price_adj, .after = "CPI")
       )
       

## Create logged $/ha

assign(df_final,
       get(df_final) %>%
         mutate(log_priceadj_ha = log(price_adj/ha)) %>%
         relocate(log_priceadj_ha, .after = 'price')
       )
