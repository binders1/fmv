soil_categories <- 
  soil_counts %>%
  dplyr::select(farmlndcl) %>%
  
  mutate(group = str_extract_all(farmlndcl, 
                "(irrigate|protect|drain|product of I|warm enough)")
  ) %>% 
  mutate(group = na_if(group, "character(0)")) %>%
  
  separate(col = group,
           into = c('group1', 'group2','group3'),
           sep = "\",",
           fill = "right") %>%
  mutate(across(group1:group3,
                ~ str_remove_all(.x, "(c\\(|\"|\"\\)|\\))")
                ),
         across(group1:group3,
                trimws)
         ) %>%
  
  pivot_longer(cols = group1:group3,
               names_to = "group",
               values_to = "value") %>%
  
  filter(!is.na(value)) %>%
  
  mutate(importance = str_extract_all(farmlndcl, 
                                      regex("(statewide|local|(?<!not[:blank:])prime)", 
                                            ignore_case = T)),
         importance = ifelse(importance=="character(0)", NA,importance),
         importance = unlist(importance)) %>%
  dplyr::select(!group) %>%
  relocate(importance, .before = "value") %>%
  mutate(importance = str_to_title(importance)) %>% 
  
  mutate(category = paste0(importance, ", ", value),
         category = str_remove_all(category, "(NA|NA,|, NA)"),
         category = na_if(category, ""),
         category = case_when(
           str_detect(farmlndcl, "missing") ~ "Missing",
           is.na(category) ~ farmlndcl,
           str_detect(farmlndcl, "subsoil") ~ "Other",
           TRUE ~ category),
         category = str_replace_all(category, "product of I", "product of I x C")) %>%
  
  dplyr::select(category, farmlndcl) %>%
  arrange(category)

sheet_write(
  ss = "1AJlJgiMgMQXB9kNKMRuVP6_f5D60-bPmnBMGVYpVPYs",
  soil_categories,
  sheet = "New Categories"
)
  
  
# Merge missing
# Keep missing separate from any "other" category
# Irrigated, drained, protected from flooding, warm enough, product of I x C
# Experiment with lumping unique and prime together