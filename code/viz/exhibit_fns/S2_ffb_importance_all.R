ffb_importance_all <- function() {
  
  # Clean FRR Importance data ####
  
  ffb_imp_clean <-
    
    loadResults("ffb", "importance", include_type = FALSE) %>%
    
    pivot_longer(
      cols = !c(frr, model),
      names_to = "Variable",
      values_to = "Importance") %>%
    
    # group climatic variables
    mutate(group = case_when(
      str_detect(Variable, "^Dew") ~ "Dew Temperature",
      str_detect(Variable, "(?<!Dew)Temp") ~ "Temperature",
      str_detect(Variable, "Precip") ~ "Precipitation",
      Variable == "final_mhv" ~ "Median Home Value",
      TRUE ~ as.character(Variable)
    )) %>%
    
    # calc median importance of each group by frr
    group_by(group, frr) %>%
    summarise(
      Importance = mean(Importance, na.rm = T),
      .groups = "keep") %>%
    
    filter(!is.nan(Importance)) %>%
    
    arrange(group,frr) %>%
    
    
    group_by(group) %>%
    mutate(imp_med = median(Importance)) %>%
    
    ungroup() %>%
    
    # bold vars added by this paper; grey out Nolte vars
    mutate(
      
      group = case_when(
        group %in% nolte2020vars ~ 
          paste0("<span style='color:Grey;'>",
                 group,
                 "</span>"),
        TRUE ~ 
          paste0("<span style='font-weight:1000;color:Black;'>",
                 group,
                 "</span>")),
      
      group = fct_reorder(group, imp_med)
      
    )
  
  # VIZ ####
  
  ## Top 20 FRR Importance ####
  
  ffb_imp_clean %>%
    
    ggplot(
      aes(Importance, group, colour = frr)
    ) +
    
    geom_hline(yintercept = 0) +
    
    geom_point(size = 1, alpha = 0.8) +
    
    scale_x_continuous(
      expand = c(0.02,0),
      breaks = seq(0, 1.5, by = 0.25)
    ) +
    scale_y_discrete(expand = c(0.03,0.03)) +
    scale_colour_manual(
      values = frr_colors
    ) +
    
    labs(
      x = "Feature Importance",
      y = NULL,
      colour = "Farm Resource Region") +
    
    fmv_theme +
    
    theme(
      axis.text.y = ggtext::element_markdown(size = 6),
      panel.grid.major.y = element_line(size = 0.2, 
                                        colour = "grey80"),
      legend.key = element_blank(),
      legend.position = c(0.84, .38)
    )
}