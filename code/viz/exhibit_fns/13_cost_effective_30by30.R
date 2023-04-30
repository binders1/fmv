cost_effective_30by30 <- function() {
  
  sid_ha_data <-
    map_dfr(
      .x = unique(county_frr_crosswalk$stusps),
      ~ read_state_clean(.x) %>%
        select(sid, ha, log_priceadj_ha)
    )
  
  purchase_costs_by_model <-
    expand_grid(
      model = c("ground_truth", "ncb", "ffb"),
      ncb_subset = c(TRUE, FALSE)
    ) %>%
    pmap_dfr(
      ~ land_purchase_cost(
        model = .x,
        ncb_subset = .y,
        sid_ha_data = sid_ha_data
      )
    ) 
  
  purchase_costs_by_model_formatted <-
    purchase_costs_by_model %>%
    mutate(
      total_cost = total_cost/1e+09,
      total_cost = round(total_cost, 1),
      source = paste(source, ncb_subset, sep = "_"),
      source = fct_reorder(source, total_cost)
      )
  
  model_plot_labels <-
    c(
      "ground_truth_TRUE" = "True Information*",
      "ground_truth_FALSE" = "True Information",
      "ffb_TRUE" = "Full FRR Model*",
      "ffb_FALSE" = "Full FRR Model",
      "ncb_TRUE" = "Restricted County Model"
    )
  
  model_plot_colors <-
    c(
      "ground_truth_TRUE" = "lightgrey",
      "ground_truth_FALSE" = "grey40",
      "ffb_TRUE" = brewer.pal(4, "Paired")[1],
      "ffb_FALSE" = brewer.pal(4, "Paired")[2],
      "ncb_TRUE" = brewer.pal(4, "Paired")[4]
    )
  
  
  # 30by30 total cost simulation
  simmons_2021_km2 <- 1723452
  simmons_2021_ha <- simmons_2021_km2 * 100
  purchase_costs_by_model_formatted %>%
    mutate(
      simmons_2021_ha = simmons_2021_ha,
      total_30by30_cost = average_cost * simmons_2021_ha,
      total_30by30_cost = label_comma(scale = 1e-09,
                                      prefix = "$",
                                      suffix = " B",
                                      accuracy = 1)(total_30by30_cost)
    )
  
    # Plot =========================
    
  purchase_costs_by_model_formatted %>%
    ggplot(aes(source, 
               average_cost,# total_cost, 
               fill = source)) +
    
    
    geom_bar(stat = "identity", width = 0.6) + 
    geom_hline(yintercept = 0) +
    
    geom_text(
      aes(
        label =
          label_dollar(prefix = "$", accuracy = 1)(average_cost)
        ),
      fontface = "bold",
      vjust = -0.5
      ) +
    
    scale_y_continuous(
      labels = label_dollar(),
      limits = c(0, 1750)
    ) +
    
    scale_x_discrete(
      labels = model_plot_labels
    ) +
    
    scale_fill_manual(
      values = model_plot_colors
    ) +
    labs(
      x = NULL,
      y = "Average Cost per Hectare",
      caption = "\n*Candidate set included only parcels available to the Restricted County Model"
      ) + 
    
    fmv_theme +
    theme(
      legend.position = "none",
      plot.caption = element_text(face = "italic")
      )
  
}


  
  




# Helper functions =============================================================
land_purchase_cost <- 
  function(
    model = c("ground_truth", "ncb", "ffb"),
    sid_ha_data,
    ncb_subset = FALSE # Filters parcels down to ncb_dedup[['sid']] (created in 00_exhibit_prep.R)
    ) {
    
    model <- match.arg(model)  

    # We don't allow ncb to have its full original candidate set
    # we always must filter to ncb_dedup_sid
    if (model == "ncb" & !ncb_subset) return()
    
    price_variable <-
      switch(model,
             ground_truth = "log_priceadj_ha",
             ".pred")
    
  # Set 30% (of all CONUS data hectares) target for conservation purchasing
  target_ha <- 
    sum(sid_ha_data$ha) * 0.30
  
  purchase_data <-
    if (model == "ground_truth") {
      sid_ha_data
    } else {
      # Join with original data to get hectare measures
      loadResults(model = model, res_type = "predict_all") %>%
        select(-log_priceadj_ha) %>%
        left_join(sid_ha_data, by = "sid")
    }

  # Subset data to ncb_dedup
  if (ncb_subset) {
    if (model == "ncb") {
      purchase_data <-
        purchase_data %>%
        filter(paste0(sid, fips) %in% ncb_dedup[['sidfips']])
    } else {
      purchase_data <-
        purchase_data %>%
        filter(sid %in% ncb_dedup[['sid']])
    }
  }
  
  # Check how much total ha is available to the model
  sum(purchase_data$ha) %>% label_comma()(.)
  
  purchase_data %>%
    # Arrange sales by ascending price
    arrange(.data[[price_variable]]) %>%
    
    mutate(
      # Create actual price (delog and multiply by ha)
      actual_cost = exp(log_priceadj_ha)*ha,
      # Generate cumsum() column of cumulative land area (measured by ha).
      cumul_ha = cumsum(ha),
      
      target_reached = cumul_ha > target_ha,
      first_over_target = first_appearance(target_reached, TRUE)
    ) %>%
    
    # Cut off data when cumulative hectares purchased exceed 30%.
    filter(
      row_number() <= first_over_target
    ) %>%
    summarise(total_cost = sum(actual_cost)) %>%
    
    mutate(
      # Identify source of the total purchase price 
      source = model,
      # Whether purchasing occured with out "neighbor_parcels"
      ncb_subset = ncb_subset,
      # Divide by total land area to get avg cost per ha
      average_cost = total_cost / target_ha
      )
}


# Detect first appearance of value in vector
# Returns index (int) 
first_appearance <- function(vec, value) {
  stopifnot(typeof(vec) == typeof(value))
  min(which(vec == value))
}
