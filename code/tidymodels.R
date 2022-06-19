#### Install Packages ####

install.packages(c('tidymodels','usemodels','vip','ranger'))


library(tidymodels)
library(usemodels)
library(vip)
library(doParallel)
library(showtext)
tidymodels_prefer()
showtext_auto()

font_add_google("IBM Plex Sans")


#### Load and Clean Data ####

data("Chicago")

glimpse(Chicago)
chicago_clean <- Chicago |>
  dplyr::select(ridership, Austin, Quincy_Wells, Belmont, temp, humidity, wind, percip, Blackhawks_Home, Bulls_Home, Bears_Home) |>
  na.omit()


#### Construct Model ####

## split data
set.seed(319)
rf_split <- initial_split(chicago_clean, strata = ridership)
train <- training(rf_split)
test <- testing(rf_split)


# build resamples for cross-validation during tuning
set.seed(194)

chicago_folds <- vfold_cv(train, strata = ridership,
                          v = 25)

# generate code template for ranger model
use_ranger(ridership ~ . , data = train)


#### Model Workflow ####

# specify general model formula
ranger_recipe <- 
  recipe(formula = ridership ~ ., data = train)
  # preprocessing goes here, if relevant;
  # see https://recipes.tidymodels.org/reference/ for more on preprocessing functions

# specify modelling engine and mode; 
# tune() is inserted as a placeholder for the hyperparameter tuning later
ranger_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>% 
  set_mode("regression") %>% 
  set_engine("ranger")
  # do I need to specify importance method (e.g., impurity) here?


# specify workflow: model, preprocessing (if relevant), etc.
ranger_workflow <- 
  workflow() %>% 
  add_recipe(ranger_recipe) %>% # the recipe in the formula
  add_model(ranger_spec)  # the model are the parameters, engine, importance methods, etc.



#### Hyperparameter Tuning ####

set.seed(15224)
cl <- makeCluster(64)
registerDoParallel(cl)
getDoParWorkers()


# hyperparameter tuning; 
# it takes a while to go through all bootstrapped cross-validation sets
system.time(
  ranger_tune <- tune_grid(ranger_workflow, 
                           resamples = chicago_folds,
                           grid = 11)
)


# inspect the best performing models
show_best(ranger_tune, metric = "rmse")
show_best(ranger_tune, metric = "rsq")

# plot the tuning that you just did to observe 
# how performance varied across parameter values
autoplot(ranger_tune)



# construct a final random forest model with the best hyperparameters from the 
# kfold cross-validation grid-search tuning above
final_rf <- ranger_workflow %>%
  finalize_workflow(select_best(ranger_tune, metric = "rsq"))


### Fitting ####

chicago_fit <- last_fit(final_rf, rf_split)
collect_metrics(chicago_fit)


#### Predicted-Actual Visualization ####

collect_predictions(chicago_fit) %>%
  ggplot(aes(ridership, .pred)) +
  geom_abline(lty = 2, color = "gray50") +
  geom_point(alpha = 0.5, color = "midnightblue") +
  labs(x = "Ridership",y = "Predicted Values")+
  coord_fixed()+
  theme_bw(base_size = 18, base_family = "IBM Plex Sans")

# run this line of code alone to get a dataframe of predicted vs. actual and squared error
collect_predictions(chicago_fit) |>
  mutate(diff = .pred - ridership,
         sq_error = diff^2)


#### Variable Importance ####

# construct variable importance model object
# from the best-performing model, based on initial 
# ranger Random Forest specification above (ranger_spec)
imp_spec <- ranger_spec %>%
  finalize_model(select_best(ranger_tune, metric = "rmse")) %>%
  set_engine("ranger", importance = "impurity")

# visualize variable importance
workflow() %>%
  add_recipe(ranger_recipe) %>%
  add_model(imp_spec) %>%
  fit(test) %>%
  extract_fit_parsnip() %>%
  vip(aesthetics = list(alpha = 0.8, fill = "midnightblue"))+
  theme_bw(base_size = 18, base_family = "IBM Plex Sans")

