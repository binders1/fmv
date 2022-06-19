#============================
#
#    Ranger Random Forest
#
#============================




#### Preamble ####

# load packages

library(arrow)
library(h2o) # parallel computing for faster hyperparameter tuning (NOT YET IMPLEMENTED)
library(rsample)
library(ranger)
library(missRanger) 
library(prob)
library(dplyr)
library(tidyverse)
library(stats)
library(magrittr)


# specify data file location NOT VALID
data_dir <- file.path(getwd(),"fmv", "data")
parcels <- file.path(data_dir, "MN_pc.pqt")
sales <- file.path(data_dir, "MN_sale.pqt")
pids <- file.path(data_dir, "MN_sale_pids.pqt")



# load data
parcels_df <- arrow::read_parquet(parcels)
  
sales_df <- arrow::read_parquet(sales)

pids_df <- arrow::read_parquet(pids)


# merge dfs
master_df <- sales_df |>
  
  dplyr::left_join(pids_df) |>
  
  dplyr::left_join(parcels_df, by = "pid") |>
  
  dplyr::rename_at(
    dplyr::vars(ends_with(".x")),
    ~ stringr::str_remove(., "\\..$") # 
    ) |> 
  dplyr::select_at(
    dplyr::vars(!ends_with(".y"), !`__index_level_0__`)
  )


# Why are there a bunch of duplicate rows that still have different index level 0 values?



#### Cleaning ####

master_clean <- master_df[, sapply(master_df, class) != "character"] |>
  janitor::clean_names()

master_clean <- master_clean[,colSums(is.na(master_clean)) < nrow(master_clean)] |>
  na.omit() |>
  unique()



# impute missing values NOT VALID
# places_full <- missRanger::missRanger(places_numeric[1:1000, 1:100],
#                                        pmm.k = 1,
#                                        num.trees = 50, 
#                                        sample.fraction = 0.1,
#                                        splitrule = "extratrees",
#                                        maxiter = 2,
#                                        max.depth = 6
#                                        )

# specify vector X of explanatory variables
input_features <- prob::setdiff(names(master_clean),"price")



#### Random Forest Model ####


#set seed for later replication
seed <- 150
set.seed(seed)


#split data into training and test subsets
split <- rsample::initial_split(master_clean, prop = .7)
train <- rsample::training(split)
test  <- rsample::testing(split)



#RF w/ Ranger (faster)
ranger_mod <- ranger::ranger(
  formula   = price ~ ., 
  data      = train, 
  num.trees = 500,
  min.node.size = 5,
  mtry      = floor(length(input_features) / 3),
  sample.fraction = 0.7, # do we want to sample with or without replacement?
  importance = 'impurity',
)

print(ranger_mod)






#### Tune Hyperparameters ####

# Advanced tuning w/ Ranger
# Construct hyperparameter grid to search diff combos of parameters
hyper_grid <- expand.grid(
  mtry = seq(5, 20, by = 2),
  node_size = seq(2, 10, by = 1),
  sample_size = seq(0.6, 1, by = 0.1),
  OOB_RMSE = 0 #empty now, will be filled later
)


# perform grid search to compare parameters' performance

for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula = price~.,
    data = train,
    num.trees = 500, # 500 trees was enough to stabilize error
    mtry = hyper_grid$mtry[i],
    min.node.size = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sample_size[i],
    seed = seed #same as above but arbitrary
  )
  
  # extract OOB error from 'model' and add to 'hyper_grid'
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}


# View OOB errors added to hyper_grid to compare hyperparameter tunings

hyper_grid <- hyper_grid %>%
  arrange(OOB_RMSE)

# rerun random forest in ranger using optimal parameters found via grid search
optimal_ranger <- ranger::ranger(
  formula = price ~ .,
  data = train,
  num.trees = 500,
  mtry = hyper_grid$mtry[1],
  min.node.size = hyper_grid$node_size[1],
  sample.fraction = hyper_grid$sample_size[1],
  seed = seed, #same as above but arbitrary
  importance = 'impurity'
) 

print(ranger_mod)
print(optimal_ranger) # we can see that OOB is smaller and R^2 is greater

if (
  (ranger_mod$prediction.error - optimal_ranger$prediction.error) > 0 &
  (optimal_ranger$r.squared - ranger_mod$r.squared) > 0) {
  cat("Tuning improved model performance!")
} else {
  cat("Tuning did not improve model performance")
}


#### Variable Importance Plot ####

# how many variables to plot?
n = 11

# create plot

plot_data <- data.frame('values' = optimal_ranger$variable.importance,
                      'names' = names(optimal_ranger$variable.importance)) |>
  
  dplyr::arrange(desc(values)) |>
  
  dplyr::top_n(n)

plot_data |>
  
  ggplot2::ggplot(aes(stats::reorder(names,values),values))+
  
  #ggplot2::geom_vline(xintercept = plot_data$names, colour = "grey")+
  
  ggplot2::geom_point(shape = 21, size = 3, fill = "white", colour = "black", stroke = 0.5)+
  
  ggplot2::coord_flip()+

  ggplot2::scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
  
  ggplot2::labs(
    title = paste0("NOT VALID Highest Importance Features (Top ",n,")"),
    x = NULL,
    y = "Mean Gini Impurity Decrease"
    )+
  
  ggplot2::theme_linedraw()
  
#ggplot2::theme(
#    panel.background = ggplot2::element_blank(),
#    plot.title = ggplot2::element_text(size = 17),
#    axis.ticks.y = ggplot2::element_blank(),
#    plot.margin = ggplot2::margin(15,15,15,15, unit = "pt")
#  )



ggplot2::ggsave('var_imp_plot.png', 
                path = file.path(getwd(), "fmv", "output"),
                width = 7.5, 
                height = 4, 
                units = "in")


#### Model Testing (NOT VALID) ####

predictions <- stats::predict(optimal_ranger, data = test)



