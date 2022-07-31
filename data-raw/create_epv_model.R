######################## takes 3 mins to build model, 1 min for rest
# library(devtools)
# devtools::install_github("peteowen1/buddy")
# library(buddy)
library(tidyverse)
library(zoo)
library(tidymodels)
source("./R/load_chains.R")
source("./R/helper_functions.R")
source("./R/clean_model_data.R")

chains <- load_chains(2021:lubridate::year(Sys.Date())) %>%
  janitor::clean_names()

pbp <- clean_pbp(chains)

model_data <- clean_model_data(pbp)
#######################################
##################
nrounds <- 87
params <-
  list(
    booster = "gbtree",
    objective = "multi:softprob",
    eval_metric = c("mlogloss"),
    tree_method = "hist",
    # early_stopping_rounds = 10,
    num_class = 5,
    eta = 0.15, # 0.0685
    gamma = 0,
    subsample = 0.85,
    # monotone_constraints = "(-1,0,0,0,0,0,0,0,0,0,1,1,1,1,0,1)",
    # colsample_bylevel=0.8,
    colsample_bytree = 0.85,
    max_depth = 6,
    min_child_weight = 25
  )

###
full_train <- xgboost::xgb.DMatrix(stats::model.matrix(~ . + 0,
                                                data = model_data %>% select_epv_model_vars()),
                                                label = model_data$label
)

##################################################### UNCOMMENT TO REBUILD THE MODEL
set.seed(1234)
ep_model <- xgboost::xgboost(
  params = params, data = full_train, nrounds = nrounds, print_every_n = 10
)

saveRDS(ep_model, "./data/ep_model.rds")

###########
### TESTING
#######
df <- load_chains(2021, 27) %>%
  janitor::clean_names() %>%
  clean_pbp() %>%
  clean_model_data() %>%
  add_epv_vars()

###################

