library(xgboost)
install.packages("shapr")
library(dplyr)
library(shapr)

library(tidyverse)
library(tidymodels)
library(rsample)
require("data.table")
require("rlist")

require("rpart")
require("parallel")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

require("dplyr")


library("rattle")				
require("treeClust")

#cargo el dataset, aqui debe poner  SU RUTA
dataset <- fread("/Users/dfontenla/Maestria/2022C2/DMEyF/datasets/competencia1_2022.csv")
dataset[,"clase_ternaria"]
#creo la clase_binaria  SI= {BAJA+1, BAJA+2}  NO={CONTINUA}
dataset[ foto_mes==202101, clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]
clase_binaria <- ifelse(dataset$clase_ternaria == "CONTINUA", 0, 1)
dataset$clase_ternaria <- NULL
#defino los datos donde entreno
dlearn <- dataset[ foto_mes==202101, ]
dlearn
str(dlearn)


x_var <- c( "ctrx_quarter")
y_var <- "clase_binaria"

x_train <- as.matrix(dlearn[-1:-6,..x_var])
x_train
y_train <- as.matrix(dlearn[-1:-6, ..y_var])
y_train
x_test <- as.matrix(dlearn[1:6, ..x_var])
x_test


str(x_train)
colnames(x_train)

x_train

model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Prepare the data for explanation
explainer <- shapr(x_train, model)
#> The specified model provides feature classes that are NA. The classes of data are taken as the truth.

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  x_test,
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p
)

# Printing the Shapley values for the test data.
# For more information about the interpretation of the values in the table, see ?shapr::explain.
print(explanation$dt)
#>      none     lstat         rm       dis      indus
#> 1: 22.446 5.2632030 -1.2526613 0.2920444  4.5528644
#> 2: 22.446 0.1671903 -0.7088405 0.9689007  0.3786871
#> 3: 22.446 5.9888016  5.5450861 0.5660136 -1.4304350
#> 4: 22.446 8.2142203  0.7507569 0.1893368  1.8298305
#> 5: 22.446 0.5059890  5.6875106 0.8432240  2.2471152
#> 6: 22.446 1.9929674 -3.6001959 0.8601984  3.1510530

# Plot the resulting explanations for observations 1 and 6
plot(explanation, plot_phi0 = FALSE, index_x_test = c(1, 6))


# Use the Gaussian approach
explanation_gaussian <- explain(
  x_test,
  approach = "gaussian",
  explainer = explainer,
  prediction_zero = p
)

# Plot the resulting explanations for observations 1 and 6
plot(explanation_gaussian, plot_phi0 = FALSE, index_x_test = c(1, 6))


# Use the Gaussian copula approach
explanation_copula <- explain(
  x_test,
  approach = "copula",
  explainer = explainer,
  prediction_zero = p
)

# Plot the resulting explanations for observations 1 and 6, excluding
# the no-covariate effect
plot(explanation_copula, plot_phi0 = FALSE, index_x_test = c(1, 6))

install.packages("partykit")
library("partykit")
# Use the conditional inference tree approach
explanation_ctree <- explain(
  x_test,
  approach = "ctree",
  explainer = explainer,
  prediction_zero = p
)

# Plot the resulting explanations for observations 1 and 6, excluding 
# the no-covariate effect
plot(explanation_ctree, plot_phi0 = FALSE, index_x_test = c(1, 6))


#### shapr currently natively supports explanation of predictions from models fitted with the following functions:

install.packages("gbm")
library(gbm)
#> Loaded gbm 2.1.5

xy_train <- data.frame(x_train,peso = y_train)
xy_train
colnames(xy_train)
form <- as.formula(paste0(y_var,"~",paste0(x_var,collapse="+")))
form
# Fitting a gbm model
set.seed(825)
model <- gbm::gbm(
  form,
  data = xy_train,
  distribution = "gaussian"
)

#### Full feature versions of the three required model functions ####

predict_model.gbm <- function(x, newdata) {
  
  if (!requireNamespace('gbm', quietly = TRUE)) {
    stop('The gbm package is required for predicting train models')
  }

  model_type <- ifelse(
    x$distribution$name %in% c("bernoulli","adaboost"),
    "classification",
    "regression"
  )
  if (model_type == "classification") {

    predict(x, as.data.frame(newdata), type = "response",n.trees = x$n.trees)
  } else {

    predict(x, as.data.frame(newdata),n.trees = x$n.trees)
  }
}

get_model_specs.gbm <- function(x){
  feature_list = list()
  feature_list$labels <- labels(x$Terms)
  m <- length(feature_list$labels)

  feature_list$classes <- attr(x$Terms,"dataClasses")[-1]
  feature_list$factor_levels <- setNames(vector("list", m), feature_list$labels)
  feature_list$factor_levels[feature_list$classes=="factor"] <- NA # the model object doesn't contain factor levels info

  return(feature_list)
}

# Prepare the data for explanation
set.seed(123)
explainer <- shapr(xy_train, model)
#> The columns(s) medv is not used by the model and thus removed from the data.
explainer
p0 <- mean(xy_train[,y_var])
p0
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0)
explanation
# Plot results
plot(explanation)

