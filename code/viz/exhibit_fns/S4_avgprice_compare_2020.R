
avgprice_compare_2020 <- function() {
  
  # State abbreviations
  all_states <-
    list.files(
      clean_dir,
      full.names = FALSE,
      pattern = "pqt$"
    ) %>%
    str_extract("[A-Z]{2}(?=\\.pqt$)") %>%
    sort()
  
  # Load all cleaned sales observations by state and row-bind them into a 
  # single dataframe
  clean_sale_data <-
    map_dfr(
      all_states,
      read_state_clean
    )
  
  # Aggregate by year and month
  clean_data_price_by_yrmon <-
    clean_sale_data %>%
    
    # Extract year and month from sale dates
    mutate(
      year = lubridate::year(date),
      month = lubridate::month(date, label = TRUE)
    ) %>%
    
    # If you want to convert the prices back to levels, uncomment this line:
    # (you will also have to rename the variable in the summarise function below)
    # mutate(priceadj_ha = exp(log_priceadj_ha), priceadj = priceadj_ha*ha) %>%
    # Aggregate average sale price by year-month
    dplyr::group_by(year, month) %>%
    summarise(
      mean_price_by_yrmon = mean(log_priceadj_ha, na.rm = TRUE)
    ) %>%
    
    # Convert month to factor, and create indicator for 2020, to allow 
    # 2020 line to be highlighted in the plot below
    mutate(
      month = factor(month),
      is_2020 = if_else(year == 2020, "2020", "2000-2019")
    )
  
  # Plot avg price against month, with lines for each year
  clean_data_price_by_yrmon %>%
    
    ggplot(
      aes(
        x = month,
        y = mean_price_by_yrmon, 
        # Differentiate 2020 line by color and size, customized below
        color = is_2020,
        size = is_2020,
        group = year)
    ) +
    
    geom_line() +
    
    scale_color_manual(
      values = c(
        `2020` = "#33A02C",
        `2000-2019` = "grey"
      )
    ) +
    scale_size_manual(
      values = c(
        `2020` = 1.5,
        `2000-2019` = 0.75
      )
    ) +
    labs(
      x = NULL,
      y = "Average Sale Price (log)",
      color = NULL,
      size = NULL
    ) +
    fmv_theme

}
