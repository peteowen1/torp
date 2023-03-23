###
# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o")#, type="source", repos="https://h2o-release.s3.amazonaws.com/h2o/rel-zumbo/4/R") #INSTALL_opts = '--no-lock'
install.packages("agua")
#############################
# Finally, let's load H2O and start up an H2O cluster
library(h2o)
h2o.init(enable_assertions = FALSE)

# import the cars dataset:
# this dataset is used to classify whether or not a car is economical based on
# the car's displacement, power, weight, and acceleration, and the year it was made
# cars <- h2o.importFile("https://s3.amazonaws.com/h2o-public-test-data/smalldata/junit/cars_20mpg.csv")
#
# # set the predictor names and the response column name
# predictors <- c("displacement","power", "weight", "acceleration", "year")
# response <- "cylinders"
#
# # split into train and validation sets
# cars_splits <- h2o.splitFrame(data =  cars, ratios = 0.8, seed = 1234)
# train <- cars_splits[[1]]
# valid <- cars_splits[[2]]

y <- "score_diff"
predictors <- c("team_type_fac","pred_totshots","pred_shot_diff","pred_conv",
                "bayes_g_diff","bayes_recv_g_diff","bayes_disp_g_diff")

team_mdl_h2o <- as.h2o(team_mdl_df)
# try using the distribution parameter:
# train AutoML
tictoc::tic()
aml_poisson <- h2o.automl(x = predictors, y = y,
                          training_frame = team_mdl_h2o,
                          max_runtime_secs = 120,
                          #validation_frame = valid,
                          distribution = "gaussian",
                          max_models = 15,
                          seed = 1234)
tictoc::toc()

lb <- aml_poisson@leaderboard
print(lb, n = nrow(lb))

# AutoML with tweedie distribution with default value of tweedie_power=1.5
aml_tweedie <- h2o.automl(x = predictors, y = response, training_frame = train,
                          validation_frame = valid,
                          distribution = "tweedie",
                          max_models = 10,
                          seed = 1234)
print(aml_tweedie@leaderboard)

# AutoML with tweedie distribution with a specified value of tweedie_power=1.75
aml_tweedie2 <- h2o.automl(x = predictors, y = response, training_frame = train,
                           validation_frame = valid,
                           distribution = list(type = "tweedie", tweedie_power = 1.75),
                           max_models = 10,
                           seed = 1234)
print(aml_tweedie2@leaderboard)

###
h2o.shutdown(prompt = TRUE)
