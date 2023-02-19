


cost_effective_30by30 <- function() {
  
  sid_ha_data <-
    map_dfr(
      .x = unique(county_frr_crosswalk$stusps),
      ~ read_state_clean(.x) %>%
        select(sid, ha, log_priceadj_ha)
    )
  
  purchase_costs_by_model <-
    c("ground_truth", "ncb", "ffb") %>%
    map_dfr(land_purchase_cost,
            sid_ha_data = sid_ha_data) 
  
  purchase_costs_by_model %>%
    mutate(
      source = case_when(
        source == "ground_truth" ~ "True Cost",
        source == "ncb" ~ "Restricted County Model",
        source == "ffb" ~ "Full FRR Model"),
      source = fct_relevel(
        source,
        c("True Cost", "Full FRR Model", "Restricted County Model")
        )
      ) %>%
    
    # Plot =========================
    
    ggplot(aes(source, total_cost, fill = source)) + 
    
    
    geom_bar(stat = "identity", width = 0.5) + 
    geom_hline(yintercept = 0) +
    
    geom_text(
      aes(
        label =
          label_dollar(scale = 1e-06, suffix = " mil")(total_cost)
        ),
      fontface = "bold",
      vjust = -0.5
      ) +
    
    scale_y_continuous(
      labels = label_number(scale = 1e-06),
      limits = c(0, 150e+06)
    ) +
    
    scale_fill_manual(
      values = 
        c(
          `True Cost` = "grey30",
          `Full FRR Model` = brewer.pal(4, "Paired")[1],
          `Restricted County Model` = brewer.pal(4, "Paired")[3]
          )
      ) +
    
    labs(
      x = NULL,
      y = "Total Cost (millions USD)"
      ) +
    
    fmv_theme +
    theme(
      legend.position = "none"
      )
  
}






land_purchase_cost <- function(model = c("ground_truth", "ncb", "ffb"), sid_ha_data) {

  model <- match.arg(model)  
  
  # Set 30% (of all CONUS data hectares) target for conservation purchasing
  target_ha <- 
    sum(sid_ha_data$ha) * 0.30
  
  purchase_data <-
    if (model != "ground_truth") {
      
      # Join with original data to get hectare measures
      loadResults(model = model, res_type = "predict_all") %>%
        select(-log_priceadj_ha) %>%
        left_join(sid_ha_data, by = "sid")
      } else {
        sid_ha_data
        }
  
  price_variable <-
    switch(model,
           ncb = ,
           ffb = ".pred",
           ground_truth = "log_priceadj_ha")
  
  purchase_data %>%
    
    # Arrange sales by ascending price
    arrange(.data[[price_variable]]) %>%
    
    mutate(
      # Create actual price (delog and multiply by ha)
      actual_price = exp(log_priceadj_ha)*ha,
      # Generate cumsum() column of cumulative land area (measured by ha).
      cumul_ha = cumsum(ha),
      
      target_reached = cumul_ha > target_ha,
      first_over_target = first_appearance(target_reached, TRUE)
    ) %>%
    
    # Cut off data when cumulative hectares purchased exceed 30%.
    filter(
      row_number() <= first_over_target
    ) %>%
    summarise(total_cost = sum(actual_price)) %>%
    
    # Add variable identifying the source of the total purchase price 
    mutate(source = model)
}



first_appearance <- function(vec, value) {
  min(which(vec == value))
}
