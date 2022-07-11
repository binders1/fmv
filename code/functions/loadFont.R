# FUNC: Import and activate Google font ####

# Purpose: Loads fonts from google font web repository for
# use in R graphics


loadFont <- function(...) {
  
  f <- str_to_title(
    list(...)
  )
  
  tryCatch(
    
    sysfonts::font_add_google(f),
    
    error = function(e)
      warning("Font not found")
    
  )
  
  showtext::showtext_auto()
}