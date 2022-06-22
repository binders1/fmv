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


# Upload Times of PCIS Parquet Files ####

times <- map(drive_contents$drive_resource, ~ .x$createdTime) %>% 
  unlist()

size <- map(drive_contents$drive_resource, ~ .x$size) %>%
  unlist()

names <- drive_contents$name

tibble(names = names,
       MB = round(as.numeric(size)/1000000,2),
       times = times) %>%
  filter(str_detect(names, "\\.pqt$")) %>%
  arrange(desc(times)) %>%
  mutate(time_lag = dplyr::lead(times)) %>%
  mutate(times = as_datetime(times),
         time_lag = as_datetime(time_lag)) %>%
  mutate(period = times - time_lag) %>%
  arrange(desc(period)) %>%
  mutate(hours = as.numeric(period/60)) %>%
  mutate(state = str_extract(names, "(?<=_).{2}(?=\\.pqt$)")) %>%
  dplyr::select(state, MB, hours) %>%
  filter(!is.element(state, c('CO', 'MT'))) %>%
  
  ggplot(aes(MB, hours))+
  geom_smooth(method = "lm", se = FALSE, size = 0.3, colour = "deeppink",
              linetype = "dashed", show.legend = T)+
  geom_point(alpha = 0.6, size = 2)+
  coord_fixed(ratio = 25)+
  labs(
    title = "Upload Time by File Size",
    subtitle = "Upload times estimated by intervals between\ncreation in Google Drive",
    y = "Upload Time (hrs)",
    x = "Parquet File Size (mb)"
  )+
  theme_bw(base_family = "IBM Plex Sans", base_size = 20)


# Soil Code Viz ####

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

# HPI 2005 and 2020 ####
VAL_plot_2005 <- med_home_value %>%
  mutate(VAL_log = log(VAL_2005)) %>%
  usmap::plot_usmap(
    data = .,
    values = "VAL_log",
    exclude = c('Alaska','Hawaii'),
    regions = c('counties'),
    colour = "grey",
    size = 0.2
  )+
  scale_fill_gradientn(
    colours = msecolors,  
    na.value = "grey",
    limits = c(10,14)
  )+
  labs(
    title = "2005",
    fill = "Med Value \n($ log)"
  )+
  theme(
    legend.background = element_blank(),
    legend.position = "right",
    plot.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 17)
  )

VAL_plot_2020 <- med_home_value %>%
  mutate(VAL_log = log(VAL_2020)) %>%
  usmap::plot_usmap(
    data = .,
    values = "VAL_log",
    exclude = c('Alaska','Hawaii'),
    regions = c('counties'),
    colour = "grey",
    size = 0.2
  )+
  scale_fill_gradientn(
    colours = msecolors,
    na.value = "grey",
    limits = c(10,14)
    )+
  labs(
    title = "2020"
  )+
  theme(
    legend.background = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 20, face = "bold")
  )




library(patchwork)

VAL_plot_2005 + VAL_plot_2020 +
  plot_annotation(title = "Median Home Value by County\n",
                  caption = "Source: All-Transactions House Price Index (FRED)\nMedian Home Value (Census)",
                  theme = theme(text = element_text(family = "IBM Plex Sans", 
                                                    size = 28),
                                plot.title = element_text(hjust = 0.5),
                                plot.caption = element_text(face = "italic")))


