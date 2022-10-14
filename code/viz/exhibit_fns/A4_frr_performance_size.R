frr_performance_size <- function() {
  
  FRR_perform <- 
    loadResults(model = "ffb",
                res_type = "performance")
  
  
  # R-Sq, MSE, N ####
  FRR_perform %>%
    select(frr_name, mse, rsq, nobs) %>%
    mutate(frr_name = fct_reorder(frr_name, rsq)) %>%
    pivot_longer(
      cols = !frr_name,
      names_to = "stat",
      values_to = "value") %>%
    mutate(stat = case_when(
      stat == "rsq" ~ "R-Squared",
      stat == "mse" ~ "Mean Sq. Error",
      stat == "nobs" ~ "Sample Size"
    ),
    stat = fct_relevel(stat, c("R-Squared", "Mean Sq. Error", "Sample Size"))) %>%
    
    ggplot(aes(value, frr_name, fill = stat)) +
    
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
      labels = scales::comma,
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
