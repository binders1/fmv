calc_mse <- function(data) {
  data %>%
    mutate(sq_error = (log_priceadj_ha - .pred)^2) %>%
    group_by(fips) %>%
    summarise(mse = mean(sq_error)) %>%
    summarise(
      mean_mse = mean(mse),
      median_mse = median(mse)
    )
}

percent_change <- function(x1, x2) {
  (x2 - x1) / x2
}

compare_ffb_fcb_mse <- function() {
  
  # Load FRR predictions ####
  
  mods_to_load <- 
    c("ffb", "fcb", "ncb") %>%
    purrr::set_names()
  
  full_predictions <-
    # Load in FFB, FCB, and NCB prediction dataframes, but keep them separate 
    # for now
    map(
      mods_to_load,
      loadResults,
      res_type = "predictions"
    ) %>%
    map(
      ~ .x %>%
      mutate(fips = str_sub(sid, 1, 5)) %>%
        select(fips, model, sid, .pred, log_priceadj_ha)
    )

  
  # Brief analysis aside to produce stats reported in the paper ----------------
  
  # 1. MSE in added counties
  full_predictions$ffb %>%
    filter(! fips %in% full_predictions$fcb$fips) %>%
    calc_mse()
  
  # 2. Compare fcb to ffb MSE across common parcels
  ffb_ncb_common_parcels <-
    common_parcels(full_predictions[c("ffb", "ncb")])
  
  mse_across_common_parcels <-
    full_predictions[c("ffb", "ncb")] %>%
    map(
      ~ .x %>%
        filter(sid %in% ffb_ncb_common_parcels)
    ) %>%
    map(calc_mse) %>%
    map("mean_mse")
  
  # 15 percent change referenced in abstract and end of results section ->
    percent_change(x1 = mse_across_common_parcels$ncb, 
                   x2 = mse_across_common_parcels$ffb)
    
  # ----------------------------------------------------------------------------  
    
  
  # Back to mapping FFB and FCB relative county coverage -----------------------    
    full_predictions <-
      # Grab FFB and FCB from the list of model prediction dataframes...
      full_predictions[c("ffb", "fcb")] %>%
      # ...and row-bind them together, filling in missing county observations with
      # NAs if one model includes that county but the other does not
      data.table::rbindlist(fill = TRUE) %>%
      
      # Calculate MSE 
      mutate(sq_error = (.pred - log_priceadj_ha)^2) %>%
      group_by(fips, model) %>%
      mutate(mse = mean(sq_error)) %>%
      ungroup() %>%
      select(model, fips, mse) %>%
      
      # Clean up model names
      mutate(model = if_else(model=="ffb", "Full FRR", "Full County")) %>%
      distinct()
    
    frr_missing_obs <-
      full_predictions %>%
      
      # We start with a column called "model" that identifies which model 
      # a given prediction came up. Now we pivot that column, along with the 
      # MSE values, to create 2 columns: one for FFB and one for FCB, each 
      # containing the corresponding MSE values in that county. This allows us
      # to determine which counties are not in the FRR model's test set (which
      # is where these predictions come from) but are by definition included in
      # the county's test set, since each county gets its own test and train set
      # Only a handful of counties are absent from the FRR test set in this way. 
      # We plot them at black and note the quirk of our modeling approach in the 
      # figure caption
      pivot_wider(
        names_from = model,
        values_from = mse
      ) %>%
      # This filter call keeps only those *counties* which are missing from the 
      # FRR model in the manner described in the above comment
      filter(is.na(`Full FRR`)) %>%
      select(fips) %>%
      # Join to the counties shapefile for later mapping in black.
      left_join(us_counties,
                by = "fips") %>%
      st_as_sf()
  
  frr_mse_spatial <-
    us_counties %>%
    left_join(full_predictions,
              by = "fips") %>%
    
    mutate(mse_b = cut(mse, breaks = c(0, 0.5, 1, 5, 40)))
  
  # The 90th percentile is calculated relative to the vector of *all* county
  # MSEs. That is, the FFB and FCB models' performances together, not 
  # separately for each model.
  mse_90_pctl <- 
    quantile(frr_mse_spatial$mse, 
             na.rm = T, probs = .9) %>%
    unname()
  
  frr_mse_spatial %<>%
    # Censor MSE values to the 90th percentile for visual clarity in mapping 
    # Here, we take the minimum of each MSE observation and the 90th MSE pctile. 
    # pmin() allows for element-wise comparisons of two vectors. So each entry in 
    # the mse column (which is just a vector) will be compared to the 90th pctile
    # value. In the map, this will render all outliers (>90th pctile) as the
    # same colors as those at the 90th pctile, thereby not omitting them as gray
    # county polygons
    mutate(mse = pmin(mse, mse_90_pctl)) %>%
    na.omit()
  
  
  
  # Map County-level prediction error ####
  
  ggplot() +
    
    geom_sf(data = frr_mse_spatial, 
            aes(fill = mse), colour = NA,
            alpha = 0.8) +
    
    geom_sf(data = frr_missing_obs,
            colour = NA, fill = "black") +
    
    geom_sf(data = us_states, 
            colour = "grey30", 
            size = 0.1, 
            fill = NA) +
    
    facet_wrap(~model) +
    
    scale_fill_gradientn(
      limits = c(0, mse_90_pctl),
      breaks = c(0, 0.5, 1.0, 1.5),
      labels = c("0", "0.5", "1.0", "1.5"),
      colours = rev(brewer.pal(8, "RdYlGn")),
      na.value = "grey90"
      ) +
    
    coord_sf(crs = st_crs(2163)) +
    
    labs(    
      fill = "MSE"
    ) +
    
    guides(
      fill = guide_colorbar(
        barheight = 5,
        barwidth = 0.3,
        frame.colour = "black",
        ticks.colour = "black",
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    
    fmv_theme +
    
    theme(axis.text = element_blank(),
          legend.position = "right")
  
}
