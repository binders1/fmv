clean_HPI <- function(data) {
  UseMethod("clean_HPI")
}

clean_HPI.pc <- function(data) {
  
  if (is.null(data)) return(NULL)
  
  data %>%
    left_join(HPI_county_pc,
              by = c("fips")) %>%
    relocate(HPI, .after = ha)
}
