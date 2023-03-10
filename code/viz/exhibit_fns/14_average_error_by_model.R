average_error_by_model <- function() {
  
  ncb_ffb_pred_all <-
    map_dfr(
      c("ffb", "ncb"),
      loadResults,
      res_type = "predict_all"
    ) %>%
    mutate(error = .pred - log_priceadj_ha,
           model = 
             if_else(
               model == "ffb", "Full FRR Model", "Restricted County Model"
             ),
           state = str_sub(sid, 1, 2)
           ) %>%
    left_join(
      county_frr_crosswalk %>%
        select(state, stname, stusps) %>%
        distinct(),
      by = "state"
    )
  
  
  ncb_ffb_pred_all %>%
    slice_sample(n = 1000000) %>%
    ggplot(aes(log_priceadj_ha, error, lty = model, color = model)) + 
    geom_hline(yintercept = 0) +
    geom_point(alpha = 0.5, size = 0.3) +
    #geom_smooth(se = FALSE) +
    scale_color_manual(
      values = 
        c(
          `Full FRR Model` = brewer.pal(6, "Paired")[2],
          `Restricted County Model` = brewer.pal(6, "Paired")[4]
        )
    ) +
    
    geofacet::facet_geo(~ stusps,
                        grid = "us_state_contiguous_grid1") +
    labs(
      y = "Prediction Error",
      x = "Actual Price (logged $/ha)",
      color = NULL,
      lty = NULL
    ) +
    fmv_theme +
    theme(
      legend.key = element_blank(),
      legend.position = "top"
    )
  }