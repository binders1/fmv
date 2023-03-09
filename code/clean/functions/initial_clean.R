

clean_inflation <- function(data) {
  
  if (is.null(data)) return(NULL)

  data %>%
    mutate(month = lubridate::month(date)) %>%
    relocate(month, .after = "date") %>%
    left_join(CPI, by = c("year", "month")) %>%
    relocate(CPI, .after = "price") %>%
    mutate(price_adj = price * CPI) %>%
    relocate(price_adj, .after = "CPI")
  
}

clean_HPI <- function(data) {
  
  if (is.null(data)) return(NULL)
  
  data %>%
    left_join(HPI_county,
              by = c("fips","year")) %>%
    relocate(HPI, .after = "CPI")
}

clean_logprice <- function(data) {
  
  if (is.null(data)) return(NULL)
  
  data %>%
    mutate(log_priceadj_ha = log(price_adj/ha)) %>%
    relocate(log_priceadj_ha, .after = 'price_adj') 
}