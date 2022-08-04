# Load Packages ####
library(sf)



# Set Paths ####

ddir <- "~/fmv/data"
mdir <- file.path(ddir, "model")
sdir <- file.path(ddir, "spatial")

pred_dir <- file.path(mdir, "all/FRR/rf/predictions")

state_dir <- file.path(sdir, "state_shp")
county_dir <- file.path(sdir, "county_shp")

## FRR predictions ####
pred_paths <- 
  file.path(mdir, "all/FRR/rf", list.files(pred_dir))



# Load spatial files ####

## Set undesired states/territories ####

state_rm <-
  c("02", "15", "60", "66", "69", "72", "78")

## State .shp ####

state_shp <- list.files(state_dir, pattern = "shp$")

us_states <-
  st_read(
    file.path(state_dir, state_shp)
    )

us_states %<>%
  janitor::clean_names() %>%
  select(
    state_fips = "statefp",
    abbr = "stusps"
  ) %>%
  filter(!(state_fips %in% state_rm))


## County shp ####

county_shp <- list.files(county_dir, pattern = "shp$")

us_counties <-
  st_read(
    file.path(county_dir, 
              county_shp)
    )

us_counties %<>%
  filter(!(STATEFP %in% state_rm)) %>%
  select(
    fips = "GEOID"
  )





# Load all FRR predictions ####
collect_frr_mse <- tibble()

for (i in seq_len(length(pred_paths))) {
  
  frr_pred <- 
    read_parquet(pred_paths[[i]])
  
  frr_pred %<>%
    transmute(
      frr_name = frr_name,
      fips = fips,
      sq_error = (log_priceadj_ha - .pred)^2
    ) %>%
    group_by(fips) %>%
    summarise(mse = mean(sq_error))
  
  collect_frr_mse <- 
    rbind(
      collect_frr_mse,
      frr_pred
      )
  
}



frr_mse_spatial <-
  us_counties %>%
  left_join(collect_frr_mse,
            by = "fips") %>%
  
  mutate(mse_b = cut(mse, breaks = c(0, 0.5, 1, 5, 40)))


# Map County-level prediction error ####

ggplot() +
  
  geom_sf(data = us_counties,
          fill = "grey90",
          colour = NA) +
  
  geom_sf(data = frr_mse_spatial, 
          aes(fill = mse_b), colour = NA,
          alpha = 0.8) +
  
  geom_sf(data = us_states, 
          colour = "grey30", 
          size = 0.3, 
          fill = NA) +
  
 # scale_fill_manual(
#    values = c(
#      `(0,0.5]` = "black",
#      `(0.5,1]` = "grey20", 
#      `(1,5]` = "grey45",
#      `(5,40]` = "grey65"
#    ),
#    na.translate = F
#  ) +
  
   scale_fill_manual(values = rev(brewer.pal(4, "YlGnBu")),
                   na.translate = F) +
  
  scale_colour_manual(values = frr_colors) +
  
  coord_sf(crs = st_crs(2163)) +
  
  labs(    
    title = "Prediction Error by County",
    subtitle = "Farm resource region full model.",
    fill = "MSE",
    colour = "Farm Resource Region"
  ) +
  
  theme(
    text = element_text(size = 25, family = "Source Sans Pro"),
    plot.title = element_text(size = 30, face = "bold"),
    plot.margin = margin(rep(15, 4)),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

  
 




