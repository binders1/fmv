# FUNC: Irrigation Variable Filtering ####  
irrFilter <- function(data, varname, years=NULL, replace_na = TRUE, 
                      na_value=0, drop_used = FALSE) {
  
  tmp <- data |>
    dplyr::select(pid, year, starts_with("Irrigation")) |>
    pivot_longer(
      cols = Irrigation1997:Irrigation2017,
      names_to = "irr_date",
      values_to = "irr_status"
    ) |>
    mutate(irr_date = as.numeric(str_remove_all(irr_date, "Irrigation")),
           diff = year - irr_date) 
  
  if(!is.null(years)) {
    tmp <- tmp |>
      filter(diff <= years)
  }
    
  tmp1 <- tmp |>
    mutate(irr_status = ifelse(diff>=0, irr_status, 0)) |>
    group_by(pid) |>
    filter(irr_status == max(irr_status, na.rm = T)) |>
    ungroup() |>
    filter(diff >= 0) |>
    filter(!duplicated(pid)) |>
    rename({{ varname }} := "irr_status") |>
    ungroup() |>
    dplyr::select(pid, year, {{ varname }})
  
  out <- data |>
          left_join(tmp1)
  
  if(replace_na) {
  
    out[[varname]] <- replace_na(out[[varname]],na_value)

  }
  
  out[[varname]] <- ifelse(is.na(out$Irrigation1997),
                           NA,
                           out[[varname]])
  
  if(drop_used) {
    
    out <- out %>%
      select(!starts_with('Irrigation'))
    
  }
  
  return(out)

  }


# FUNC: Register Google font ####


loadFont <- function(family) {
  
  tryCatch(
    
    sysfonts::font_add_google(family),
    
    error = function(e)
      warning(family, "not found")
    
  )
  
  showtext::showtext_auto()
}


# FUNC: Unregister cores ####
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
