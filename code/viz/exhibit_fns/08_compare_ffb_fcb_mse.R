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
  
  # Load FRR predictions (ffb) ####
  
  mods_to_load <- 
    c("ffb", "fcb", "ncb") %>%
    purrr::set_names()
  
  full_predictions <-
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
  
  # MSE in added counties
  full_predictions$ffb %>%
    filter(! fips %in% full_predictions$fcb$fips) %>%
    calc_mse()

  
  # Compare fcb to ffb MSE across common parcels
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
  
  # 15 percent change referenced in abstract and end of results section
    percent_change(x1 = mse_across_common_parcels$ncb, 
                   x2 = mse_across_common_parcels$ffb)
    
  # Accuracy improvement in $/ha
    full_predictions[c("ffb", "ncb")] %>%
      map(
        ~ .x %>%
          filter(sid %in% ffb_ncb_common_parcels) %>%
          mutate(
            across(
              # Exponentiate to turn logged dollar values into dollar values
              c(.pred, log_priceadj_ha),
              exp
              )
            )
        ) %>%
      map(calc_mse) %>%
      map("mean_mse") %>%
      do.call(`-`, .) %>%
      sqrt() %>%
      label_dollar(scale = 1e-06, suffix = " million")(.)
      
      
      
  
  full_predictions %<>%
    data.table::rbindlist(., fill = T) %>%
    mutate(sq_error = (.pred - log_priceadj_ha)^2) %>%
    group_by(fips, model) %>%
    mutate(mse = mean(sq_error)) %>%
    ungroup() %>%
    select(model, fips, mse) %>%
    mutate(model = if_else(model=="ffb", "Full FRR", "Full County")) %>%
    distinct()
  
  frr_missing_obs <-
    full_predictions %>%
    pivot_wider(
      names_from = model,
      values_from = mse
    ) %>%
    filter(is.na(`Full FRR`)) %>%
    select(fips) %>%
    left_join(us_counties,
              by = "fips") %>%
    st_as_sf()
  
  frr_mse_spatial <-
    us_counties %>%
    left_join(full_predictions,
              by = "fips") %>%
    
    mutate(mse_b = cut(mse, breaks = c(0, 0.5, 1, 5, 40)))
  
  mse_90_pctl <- 
    quantile(frr_mse_spatial$mse, 
             na.rm = T, probs = .9) %>%
    unname()
  
  frr_mse_spatial %<>%
    filter(mse <= mse_90_pctl) %>%
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
      fill = "Mean Sq. Error"
    ) +
    
    guides(
      fill = guide_colorbar(
        barheight = 0.5,
        barwidth = 8,
        frame.colour = "black",
        ticks.colour = "black",
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    
    fmv_theme +
    
    theme(axis.text = element_blank(),
          legend.position = "bottom")
  
}
