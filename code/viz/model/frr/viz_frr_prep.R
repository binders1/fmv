# Set FRR Colors ####
frr_colors <- c(`Southern Seaboard` = "#A8D3F2", 
                `Eastern Uplands` = "#A5A5A5", 
                `Basin and Range` = "#C3AB1B", 
                `Fruitful Rim` = "#71C276", 
                `Mississippi Portal` = "#AC842B", 
                `Prairie Gateway` = "#EEAB56", 
                `Northern Great Plains` = "#CAC7A1", 
                `Northern Crescent` = "#F3A193", 
                `Heartland` = "#F1EC00")


# Set Directories 
frrdir <- "~/fmv/data/model/all/FRR/rf"

imp_dir <- file.path(frrdir, "importance") 
perf_dir <- file.path(frrdir, "performance")
pred_dir <- file.path(frrdir, "predictions")



frr_sf <- 
  ag_regions_ref %>%
  mutate(
    fips = case_when(
      fips == "46113" ~ "46102",
      TRUE ~ as.character(fips)
    )
  ) %>%
  select(fips, id, frr_name) %>%
  left_join(
    us_counties
  )


frr_shp <- tibble()


for (i in 1:9) {
  
  frr_union <-
    frr_sf %>%
    filter(id == i) %>%
    st_as_sf() %>%
    st_union() %>%
    tibble() %>%
    mutate(id = as.character(i)) %>%
    relocate(id)
  
  
  frr_shp <-
    rbind(
      frr_shp,
      frr_union
    )
  
  cat("Finished:", i, " \n")
  
}

frr_shp %<>%
  st_as_sf() %>%
  left_join(
    ag_regions_key %>%
      mutate(id = as.character(id)),
    by = "id"
  )




