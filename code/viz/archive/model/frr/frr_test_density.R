# Load packages ####
library(sf)



# Load font ####
font <- "Open Sans"
loadFont(font)

# Set up ####
root <- "~/fmv"
ddir <- file.path(root, "data")
cdir <- file.path(root, "code")

# Source Prep ####
source(file.path(cdir, "functions/sourceFuncs.R"))
source(file.path(cdir, "misc/ag_regions.R"))
source(file.path(cdir, "viz/model/frr/viz_frr_prep.R"))


# Load results ####
ffb_pred <- 
  loadResults("ffb", "predictions")




# EPSG:5070 projection of states
state_5070 <- st_transform(us_states, 
                           crs=st_crs(5070))

# Set cell size
cs <- 10e+03

# Make tesselation of CONUS
state_grid <- sf::st_make_grid(state_5070,
                               cellsize = cs,
                               square = FALSE)

# Retrieve hex's that overlap with states
overlap_idx <- 
  st_intersects(state_grid, state_5070) %>%
  map_int(., 
          length) %>% 
  as.logical()

# Filter to only overlapping hex's
state_tessel <-
  state_grid[overlap_idx]


# Create spatial df of predictions
ffb_pred_proj <- 
  ffb_pred %>%
  select(.pred, x, y) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(5070))

# Bind predicted obs to hex's
ffb_intersect <-
  st_intersects(state_tessel,
                ffb_pred_proj) %>%
  map_int(., length)

# Count obs in each hex
tessel_ffb_count <-
  state_tessel %>%
  st_as_sf() %>%
  mutate(n = ffb_intersect)

# Crop tesselation to CONUS outline
tessel_count_crop <-
  st_intersection(tessel_ffb_count,
                  state_5070) %>%
  st_set_geometry(., "geometry") %>%
  filter(!str_detect(st_geometry_type(.), "POINT"))


# VIZ: obs count by hex
ggplot() +
  
  geom_sf(data = tessel_count_crop,
          aes(fill = n), colour = NA) +
  
  geom_sf(data = state_5070, 
          fill = NA, size = 0.3) +
  
  scale_fill_gradientn(
    colours = c("grey85", brewer.pal(9, "YlOrRd")),
    trans = "pseudo_log",
    breaks = c(0, 10^seq(3))
  ) +
  
  labs(
    title = "FRR Model: Testing Observation Density",
    subtitle = "10km^2 tesselation of the conterminous US",
    fill = "Testing\nObservations"
  ) +
  
  guides(
    fill = guide_colorbar(
      barwidth = 0.6,
      barheight = 12,
      frame.colour = "black",
      ticks.colour = "black"
    )
  ) +
  
  theme(
    text = element_text(family = font, size = 25),
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = ggtext::element_markdown(),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )


