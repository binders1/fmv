
# =================================================
# png_wrapper and save_png_custom
#
# Save plots using png graphics device
#
# filename = char string; name to save image as
# .fn = function that generates the image
# ... = additional parameters to send to png()
# =================================================


# Wrapper around png generation function

png_wrapper <- function(filename, ...) {
  
  filename_png <- paste0(filename, ".png")
  
  filepath <- 
    file.path(e.dir, filename_png)
  
  png(filepath, ...)
  
}


# Generate and save png plot image

save_png_custom <- function(filename, .fn, ...) {
  
  on.exit(
    utils::capture.output({
      grDevices::dev.off()
    })
  )
  
  png_wrapper(filename, ...)
  
  p <- .fn()
  
  grid::grid.draw(p)
  
  message("Saved: ", filename, "\n")
  
}



# =================================================
# common_parcels
# Find common sids in a group of models
#
# model_list = list of model predicted value df's
# =================================================

common_parcels <- function(model_list) {

  # make list of each model's unique sids
  unique_sids <-
    map(
      model_list,
      ~ .x[['sid']] %>% unique()
    )
  
  # Return character vector of sids found in all models
  reduce(
    .x = unique_sids,
    .f = base::intersect
  )
  
}




# For ffb_pred_all_*bldg

#========================================================
# Create tessellation 
#========================================================

make_us_tessel <- function(state_sf, 
                           crs = 5070,
                           cs = 10e+03) {
  
  # EPSG:5070 projection of states
  state_5070 <- 
    st_transform(state_sf, crs = st_crs(crs))
  
  # Make hexagonal tessellation of CONUS
  state_grid <- 
    sf::st_make_grid(state_5070, cellsize = cs,
                     square = FALSE, crs = st_crs(crs))
  
  # Retrieve hex's that overlap with states
  overlap_idx <- 
    map_lgl(
      st_intersects(state_grid, state_5070),
      ~ length(.x) %>% as.logical() 
    ) 
  # Filter to only overlapping hex's
  state_tessel <- state_grid[overlap_idx]
  
  # Trim tessellation to fit state polygon extent 
  st_intersection(state_tessel, state_5070)
  
}



#========================================================
# Load predict_everything parquet file and make spatial
#========================================================

find_read_fn <- function(file) {
  
  ext <- tools::file_ext(file)
  
  fn <-
    switch(ext,
           csv = readr::read_csv,
           parquet = ,
           pqt = arrow::read_parquet)
  
  fn
}

load_pred_all <- function(file, ...) {
  
  .fn <- find_read_fn(file)
  
  .fn(file) %>%
    select(...) %>%
    
    # Create spatial points from XY coords
    st_as_sf(coords = c("x", "y"), crs = st_crs(5070))
}


