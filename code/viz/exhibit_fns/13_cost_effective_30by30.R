

# Read in data from model (let's start by comparing)



# Arrange sales by ascending price
# Generate cumsum() column of cumulative land area (measured by ha).
# Calculate 30% of total area, insert it as single-value (recycled) column
# Filter such that the data stops at the first time the cumulative area exceeds 30%. Use the below code:
  first_appearance <- function(vec, value) {
    min(which(vec == value))
  }

cumsum_data <-
  tibble(x = seq(50)) %>%
  mutate(cumsum_x = cumsum(x), limit = 50)
cumsum_data %>%
  mutate(limit_reached = cumsum_x > limit,
         first_over_limit = first_appearance(limit_reached, TRUE)) %>%
  filter(
    row_number() <= first_over_limit
  )