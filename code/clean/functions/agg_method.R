
agg_by_method <- function(data, vars_to_method, .method) {
  
  data %>%
    group_by(sid) %>%
    summarise(across(any_of(vars_to_method), .method))
  
}
  