# Crosswalk from climate month to climate season
climate_season_crosswalk <-
  tibble(
    var = climate_to_mean,
    var_for_sep = climate_to_mean
  ) %>%
  separate(
    col = var_for_sep,
    into = c("name", "month"),
    sep = "_"
  ) %>%
  
  mutate(
    
    season = case_when(
      month %in% c('12','01','02') ~ "winter",
      month %in% c('03','04','05') ~ "spring",
      month %in% c('06','07','08') ~ "summer",
      month %in% c('09','10','11') ~ "fall",
      month == "annual" ~ "annual"),
    
    var_season = paste0(name, "_", season)
    
    ) %>%
  select(var, var_season)
  
  



agg_climate.pc <- function(data) {
  
  data %>%
    select(pid, any_of(climate_to_mean)) %>%
    pivot_longer(
      cols = !pid,
      names_to = "var",
      values_to = "value"
    ) %>%
    left_join(
      climate_season_crosswalk, by = "var"
    ) %>%
    
    select(!var) %>%
    
    group_by(pid, var_season) %>%
    mutate(mean = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    distinct(pid, var_season, mean) %>%
    pivot_wider(
      names_from = var_season,
      values_from = mean
    ) 
  
}
