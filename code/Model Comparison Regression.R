#-----------------------
#
#   Model Selection
#   and Comparison:
#     Regression
#
#-----------------------

# import packages
library(tidyverse)
library(MASS)
library(broom)
library(usmap)
library(RColorBrewer)


####-------Model Selection and Comparison----------####

mod_list <- vector(mode = "list", length = length(5:9))

fips <- usmap::countypop$fips[1:5]

months <- 5:9


for (i in seq_len(length(months))) {
  
  ## filter to current county
  df_filtered <- airquality %>%
    dplyr::filter(Month == months[i]) %>%
    dplyr::select(!Ozone)
  
  ## select optimal AIC model
  stepMod <- stepAIC(lm(Temp~., data = df_filtered), 
                     trace = FALSE, 
                     direction = "both")
  
  ## collect model statistics
  mod_glance <- glance(stepMod)
  
  ## 
  mod_list[[i]] <- mod_glance
  
  names(mod_list)[i] <- fips[i]
  
  cat("Completed: ", fips[i], " |....... ", 
      (i/length(months))*100, "% at ", paste0(Sys.time()),"\n", 
      sep = "")
  
}

## Bind models together
mod_df <- bind_rows(mod_list, .id = "fips")

#### FUNCTION VERSION (for foreach approach) ####

collect_stats <- function(data) {
  
  mod_list <- vector(mode = "list") # figure out how to preallocate length with func arg
  
  ## filter to current county
  df_filtered <- data %>%
    dplyr::filter(county == counties[i])
  
  ## select optimal AIC model
  stepMod <- stepAIC(lm(Sale_Price ~ ., data = df_filtered), 
                     trace = FALSE, 
                     direction = "both")
  
  ## collect model statistics
  mod_glance <- glance(stepMod)
  
  ## 
  mod_list[[i]] <- mod_glance
  
  names(mod_list)[i] <- counties[i]
  
  cat("Completed: ", fips[i], " |....... ", 
      (i/length(nrow(data)))*100, "% at ", paste0(Sys.time()),"\n", 
      sep = "")
  
  return(mod_list)
}

cl <- makeCluster(64)
registerDoParallel(cl)
getDoParWorkers()

foreach(i=seq_len(nrow(data)), .combine = "rbind") %dopar% {
 
  mod_list <- collect_stats(data)
  
  mod_df <- bind_rows(mod_list, .id = "fips")
  
}






####------Plot Model Comparisons------####

## bar chart
mod_df %>%
  ggplot(aes(fips, r.squared))+
  geom_bar(stat = 'identity')


## map
plot_usmap(data = mod_df,
           values = "r.squared",
           regions = c('counties'), colour = "darkgrey", size = .2)+
  scale_fill_gradientn(
    name = "R-Squared",
    colours = c("lightgrey", "gold", "darkgreen"),
    #breaks = c(0,0.2,0.4,0.6,0.8,1),
    na.value = "white")+
  labs(
    title = "Predictive Power by County"
  )+
  theme(
    legend.position = "left",
    legend.background = element_blank(),
    plot.title = element_text(hjust = 0.5, 
                              vjust = -3, 
                              size = 20)
  )











####--------Picking Majority Farm Class--------####

tibble(
  parcel = c('a','b','c'),
  farm1 = c('0.2','0.8','0.3'),
  farm2 = c('0.3','0.1','0.5'),
  farm3 = c('0.5','0.1','0.2')
) |>
  pivot_longer(
    cols = !parcel,
    names_to = "class",
    values_to = "percent"
  ) |>
  group_by(parcel) |>
  filter(percent == max(percent)) |>
  select(!percent)












