agg_climate <- function(data) {
  data %>%
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
    
    # aggregate monthly obs across seasons
    # and take weighted average across parcels within sid
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
  
  
}
