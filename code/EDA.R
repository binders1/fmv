library(googlesheets4)

## Inspect Variable types from overall merge ####
var_types <- map(names(merge_complete),
                 ~ typeof(merge_complete[[.x]])) |>
  unlist()|>
  tibble() |>
  rename(type = 1)

map(names(merge_complete),
    ~ sum(is.na(merge_complete[[.x]]))) |>
  unlist() |>
  tibble() |>
  rename(missing = 1) |>
  bind_cols('variable' = names(merge_complete),
            var_types) |>
  arrange(desc(missing)) |> View()
mutate(missing_percent = round(missing/nrow(merge_complete),2)) |> 
  filter(missing_percent > 0) |>
  write_csv("missingMN.csv")





## Inspect Variable differences across states ####
var_tbl <- tibble('id' = seq_len(62))

for (i in seq_len(length(all_sale))) {
  
  state_name <- str_sub(all_sale[i], 1, 2)
  
  temp <- read_parquet(all_sale[i]) |>
    names() |>
    tibble() |>
    rename(!!state_name := 1) |>
    rowid_to_column('id')
  
  var_tbl <- var_tbl |>
    left_join(temp)
  
}



## County Sale Density Maps ####

setwd('data')

#salepid_nation <- map_dfr(all_sale_pids, ~ read_parquet(.x))

nation_fips <- salepid_nation |>
  filter(!duplicated(sid)) |>
  count(fips) |>
  mutate(n_log = log(n))

remove(salepid_nation)



## Log Sales Density County Map ####

nation_fips_log <- nation_fips |>
  mutate(n_log = log(n))

plot_usmap(data = nation_fips_log,
           values = "n_log",
           regions = c('counties'), 
           exclude = c('Alaska', 'Hawaii'),
           colour = "grey", size = .2)+
  scale_fill_gradientn(
    name = "Obs (logged)",
    colours = noltecolors,
    na.value = "lightgrey",
    labels = c(0,2,4,8,11))+
  labs(
    title = "Unique Sales Records by County",
    subtitle = "N = 6.26mil, 2161 counties"
  )+
  theme(
    legend.position = "left",
    legend.background = element_blank(),
    text = element_text(size = 17)
  )


