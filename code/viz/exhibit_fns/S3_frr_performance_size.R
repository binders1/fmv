frr_performance_size <- function() {
  
  FRR_perform <- 
    loadResults(model = "ffb",
                res_type = "metrics")
  
  
  # R-Sq, MSE, N ####
  FRR_perform %>%
    mutate(frr = fct_reorder(frr, rsq),
           nobs = n_train + n_test) %>%
    select(frr, mse, rsq, nobs) %>%
    pivot_longer(
      cols = !frr,
      names_to = "stat",
      values_to = "value") %>%
    mutate(
      stat = case_when(
        stat == "rsq" ~ "R-Squared",
        stat == "mse" ~ "Mean Sq. Error",
        stat == "nobs" ~ "Sample Size"
        ),
      stat = fct_relevel(stat, 
                         c("R-Squared", "Mean Sq. Error", "Sample Size"))
      ) %>%
    
    ggplot(aes(value, frr, fill = stat)) +
    
    geom_bar(stat = "identity",
             width = 0.8) +
    
    geom_vline(xintercept = 0) +
    
    facet_wrap(~ stat, scales = "free_x") +
    
    scale_fill_manual(
      values = c(
        `R-Squared` = "#3182BD",
        `Mean Sq. Error` = "#FC9272",
        `Sample Size` = "grey40"
      )
    ) +
    
    scale_x_continuous(
      labels = scales::label_comma(),
      expand = c(0,NA)) + 
    
    labs( 
      y = NULL,
      x = NULL
    ) +
    fmv_theme + 
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "grey80")
    )
  
  
}
