#=========================
#
#   Load, clean, and prep
#    state-level dfs for
#        modeling
#
#=========================


pcis_pqt <- list.files(pqt_dir)

# Clean Data: State Loop ####
for (i in seq_along(pcis_pqt)) {
  
  # Data Cleaning ####

  
 
  
  
  df_agg_mean <- df_final_soil %>%
    group_by(sid) %>%
    summarise(across(any_of(noltevars_to_mean), mean))
  
  cat('\n Finished: mean \n')
  
  df_agg_sum <- df_final_soil %>%
    group_by(sid) %>%
    summarise(across(any_of(noltevars_to_sum), sum))
  
  cat('\nFinished: sum')
  
  
  cat('\nFinished: climate')

  df_agg_none <- 
    df_final_soil %>%
    select(c(sid, fips, HPI, log_priceadj_ha, date, ha, x, x45, y, y45)) %>%
    filter(!duplicated(sid))
  
  df_agg_final <- df_agg_none %>%
    left_join(df_agg_soil) %>%
    left_join(df_agg_mean) %>%
    left_join(df_agg_sum) %>%
    left_join(df_agg_climate) %>%
    left_join(df_agg_irr)
  
  cat('\nFinished: merge')
  
  setwd("~/fmv/data/cleaned")
  
  clean_file <- paste0("clean_",state,".pqt")
  
  arrow::write_parquet(df_agg_final, 
                       clean_file)
  
  remove(list = ls(pattern = "^df"))
  gc()
  
  ptime <- int_length(interval(start, Sys.time())) %>% round(2)
  
  cat("Saved: ", clean_file, sep = "")
  
  cat("\n Process time:", ptime, "secs \n\n")
  
}


  