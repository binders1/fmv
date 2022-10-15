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



