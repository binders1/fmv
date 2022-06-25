# Soil Type count by parcel ####

setwd('/home/rstudio/users/gold1/fmv/data/cleaned')

all_clean <- list.files()

ex_clean <- map_dfr(all_clean,
                    ~ read_parquet(.x) %>%
                      dplyr::select(sid, starts_with("VALUE"))) %>%
  slice_sample(n = 200000) %>%
  pivot_longer(
    cols = !sid,
    names_to = "soil_type",
    values_to = "prop"
  ) %>%
  filter(!is.na(prop) & prop > 0) %>%
  count(sid)



ex_clean %>%
  ggplot(aes(n, after_stat(count)))+
  geom_histogram(binwidth = 1, color = "darkgrey", fill = "lightgrey")+
  
  scale_y_continuous(labels = scales::comma)+
  
  scale_x_continuous(breaks = seq_len(20))+
  
  labs(
    title = "Number of Soil Types per Sale",
    subtitle = "Random national sample (N = 200,000)",
    y = "Count",
    x = "# of Soil Types Present")+
  
  theme_bw(base_size = 20, base_family = "IBM Plex Sans")

ex_clean %>% nrow()
  


