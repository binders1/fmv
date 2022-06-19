#### FUNC: Irrigation Variable Filtering ####  
irrFilter <- function(data, varname, years=NULL, replace_na = TRUE, na_value=0) {
  
  tmp <- data |>
    select(pid, year, starts_with("Irrigation")) |>
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
    filter(irr_status == max(irr_status)) |>
    ungroup() |>
    filter(diff >= 0) |>
    filter(!duplicated(pid)) |>
    rename({{ varname }} := "irr_status") |>
    ungroup() |>
    select(pid, year, {{ varname }})
  
  out <- data |>
          left_join(tmp1)
  
  if(replace_na) {
  
    out[[varname]] <- replace_na(out[[varname]],na_value)

  }
  
  out[[varname]] <- ifelse(is.na(out$Irrigation1997),
                           NA,
                           out[[varname]])
  
  return(out)

  }

