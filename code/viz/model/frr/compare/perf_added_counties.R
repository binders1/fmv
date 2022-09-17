# Load packages ####
library(sf)

# Set up ####
root <- "~/fmv"
ddir <- file.path(root, "data")
cdir <- file.path(root, "code")

# Source Prep ####
source(file.path(cdir, "functions/sourceFuncs.R"))
sourceFuncs()
source(file.path(cdir, "misc/ag_regions.R"))
source(file.path(cdir, "viz/model/frr/viz_frr_prep.R"))

# Load font ####
font <- "Open Sans"
loadFont(font)




# Load results ####
mods_to_load <- c("ffb", "fcb")

ffb_fcb_pred <-
  map_dfr(mods_to_load, 
      loadResults, 
      res_type = "predictions") %>%
  mutate(fips = if_else(model == "ffb", fips,
                        str_sub(sid, 1, 5)))

# FUN: unique vector of fips in a given model ####
modCounties <- function(data, mod) {
  
  out <-
    data %>%
    dplyr::filter(model == mod) %>%
    dplyr::pull(fips) %>%
    unique() %>%
    sort()
  
  return(out)
}

## generate list of ffb and fcb counties ####
ffb_fcb_counties <-
  map(mods_to_load, 
      ~ modCounties(data = ffb_fcb_pred, 
                    mod = .x)
      )

names(ffb_fcb_counties) <- mods_to_load

## create index of ffb counties not in fcb... ####
ffb_only_idx <- 
  !(ffb_fcb_counties$ffb %in% ffb_fcb_counties$fcb)

### ..then select those counties ####
ffb_only_counties <-
  ffb_fcb_counties$ffb[ffb_only_idx]

## Create spatial dataframe of only those counties ####
ffb_only_df <-
  ffb_fcb_pred %>%
  filter(fips %in% ffb_only_counties) %>%
  left_join(us_counties, by = "fips") %>%
  st_as_sf()


# No. of added counties ####
length(ffb_only_counties)

# Added property km^2 ####
sum(ffb_only_df$ha)/100

# Added county km2 ####
added_km2_county <-
  ffb_only_df %>%
  filter(!duplicated(fips)) %>%
  transmute(
    fips,
    area_km = (ALAND + AWATER)/1e+06) %>%
  summarise(added_km = sum(area_km))

ggplot() +
  
  geom_sf(data = frr_shp, aes(fill = frr_name), colour =NA) +
  
  geom_sf(data = ffb_only_df, colour =NA, fill = "black") +
  
  scale_fill_manual(
    values = frr_colors
  )

# Mean squared error by county

ffb_only_mse <-
  ffb_only_df %>%
  mutate(sq_error = (.pred - log_priceadj_ha)^2) %>%
  group_by(fips) %>%
  summarise(mse = mean(sq_error))

# Median, Mean, and Std. Dev of MSE ####

measures <- c("mean", "median", "sd")

mse_stats <-
  map_dbl(measures, rlang::exec, 
        ffb_only_mse$mse, na.rm = TRUE)

names(mse_stats) <- measures

mse_stats


  