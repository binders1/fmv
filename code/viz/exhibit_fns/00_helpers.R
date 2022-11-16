

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
# Load predict_everything parquet file
#========================================================

load_pred_all <- function(file) {
  
  file %>%
    read_parquet() %>%
    select(.pred, sid, x, y) %>%
    
    # Create spatial points from XY coords
    st_as_sf(coords = c("x", "y"), crs = st_crs(5070))
}


