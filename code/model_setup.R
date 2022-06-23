

setwd("/home/rstudio/users/gold1/fmv/data/cleaned")

all_clean <- list.files()


states <- str_extract(all_clean, "[:upper:]{2}")

collect_stats_base_all <- vector(mode = "list", length = length(all_clean))
names(collect_stats_base_all) <- states

collect_stats_aic_all <- vector(mode = "list", length = length(all_clean))
names(collect_stats_aic_all)<- states


collect_stats_rf_all <- vector(mode = "list", length = length(all_clean))
names(collect_stats_rf_all) <- states

