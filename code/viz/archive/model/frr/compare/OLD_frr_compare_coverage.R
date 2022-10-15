# Load Packages ####
library(patchwork)


# Load Fonts ####
font <- "Source Sans Pro"
loadFont(font)


# Load Spatial Files ####

# bad states 
state_rm <-
  c("02", "15", "60", "66", "69", "72", "78")


## State shp #### 
state_shp <- list.files("~/fmv/data/spatial/state_shp", 
                        pattern = ".shp$", 
                        full.names = T)
us_states <- st_read(state_shp)

us_states %<>%
  filter(!(STATEFP %in% c("02", "15", "60", "66", 
                          "69", "72", "78")))
## County shp ####

county_shp <- list.files("~/fmv/data/spatial/county_shp", 
                         pattern = ".shp$", 
                         full.names = T)
# County shp
us_counties <-
  st_read(
    county_shp
  )


us_counties %<>%
  filter(!(STATEFP %in% state_rm)) %>%
  select(
    fips = "GEOID"
  )



# Load predictions ####

mods_to_load <- 
  c("ffb", "ffr")

frr_predictions <-
  map(mods_to_load, 
      ~ loadResults(model = .x, res_type = "predictions"))

names(frr_predictions) <- mods_to_load


n_counties <- 
  map_int(seq_along(mods_to_load), 
          ~ frr_predictions[[.x]]$fips %>% 
          unique() %>% 
          length())

county_coverage <-
  tibble(
    model = mods_to_load,
    n = n_counties
  ) %>%
  mutate(
    author = case_when(
      str_detect(model, "^f") ~ "Full",
      str_detect(model, "^n") ~ "Nolte"),
    type = case_when(
      str_detect(model, "b$") ~ "Unrestricted",
      str_detect(model, "r$") ~ "Restricted"),
    type = fct_relevel(type, c("Unrestricted", "Restricted"))
  )


# VIZ ####

## Bar Chart ####
coverage_bar <-
  county_coverage %>%
  ggplot(aes(type, n, colour = type, fill = type)) +
  
  geom_bar(stat = "identity", 
           position = "dodge",
           width = 0.4) +
  
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.4),
            family = font,
            size = 7,
            vjust = -1) +
  
  scale_colour_manual(values = c("grey20", "grey60")) +

  scale_fill_manual(values = c("grey20", "grey60")) +
  
  scale_y_continuous(expand = c(0, NA), limits = c(0, 2500)) +
  
  labs(
    y = "# Counties in Model",
    x = NULL
  ) +
  
  theme(axis.ticks = element_blank(), 
        text = element_text(size = 24, family = font),
        plot.title = element_text(size = 28, face = "bold"),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 16, face = "italic"),
        plot.background = element_blank(),
        plot.margin = margin(rep(20, 4)),
        legend.title = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey92"),
        panel.border = element_rect(colour = "black", fill = NA), 
        strip.background = element_blank()
  )


## Map ####

present_counties <-
  map(mods_to_load,
      ~ frr_predictions[[.x]] %>%
      select(fips) %>%
      distinct() %>%
      mutate(!!.x := 1)
      )

spatial_coverage <-
  us_counties %>%
  left_join(present_counties[[1]], 
            by = "fips") %>%
  left_join(present_counties[[2]], 
            by = "fips") %>%
  mutate(across(all_of(mods_to_load), ~ replace_na(.x, 0))) %>%
  pivot_longer(
    cols = all_of(mods_to_load),
    names_to = "model",
    values_to = "present"
  ) %>%
  mutate(
    author = case_when(
      str_detect(model, "^f") ~ "Full",
      str_detect(model, "^n") ~ "Nolte"),
    type = case_when(
      str_detect(model, "b$") ~ "Unrestricted",
      str_detect(model, "r$") ~ "Restricted"),
    type = fct_relevel(type, c("Unrestricted", "Restricted"))
  ) %>%
  pivot_wider(
    id_cols = c(fips, geometry, author),
    names_from = type,
    values_from = present
  ) %>%
  mutate(n_present = factor(Unrestricted + Restricted),
         n_present = case_when(
           n_present == "0" ~ NA_character_,
           n_present == "1" ~ "Added with Unrestricted",
           n_present == "2" ~ "Restricted Model"
         )) %>%
  st_as_sf()
 

coverage_map <-
  
  ggplot() +
  
  geom_sf(data = us_counties,
          fill = "grey90",
          colour = "white",
          size = 0) +
  
  geom_sf(data = spatial_coverage,
          aes(fill = n_present), 
          colour = NA,
          alpha = 0.8) +
  
  geom_sf(data = us_states, 
          colour = "grey30", 
          size = 0.3, 
          fill = NA) +
  
  scale_fill_manual(
    values = c(
      `Restricted Model` = "grey60", #brewer.pal(4, "Greens")[4],
      `Added with Unrestricted` = "grey20"
    ),
    na.translate = F
  ) +
  
  coord_sf(crs = st_crs(2163)) +
  
  labs(
    fill = NULL
  ) +
  
  theme(
    text = element_text(size = 25, 
                        family = font, colour = "black"),
    plot.title = element_text(size = 30, 
                              face = "bold"),
    plot.margin = margin(rep(15, 4)),
    panel.background = element_blank(),
    #panel.border = element_rect(fill = NA, 
    #                            colour = "grey30"),
    strip.background = element_blank(),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


# Compose panels ####
coverage_bar + coverage_map









