#' Random Forest Regression
#'
#' @param x predictor variables
#' @param y response variable
#'
#' @return the best model
#' @export
#'
#' @examples
#' library(GMothDef)
#' rf_reg(v_pred, v_res)

rf_reg <- function(x, y) {

  # A grid can be generated to specify candidate hyper-parameter values for inclusion into the models training
  rf.grid <- expand.grid(mtry=1:24) # number of variables available for splitting at each tree node, can be adjusted to improve model

  # Set up a resampling method in the model training process
  tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                     number = 10, # number of folds
                     repeats = 5, # number of repeats
                     allowParallel = TRUE, # allow use of multiple cores if specified in training
                     verboseIter = TRUE, #print training log
                     p=0.7) # the training percentage

  #########

  #train random forest model
  set.seed(1) #make it reproducible
  rf_model<- caret::train(x = x, y = y,
                          method = "rf", metric="Rsquared", trainControl = tc,
                          tuneGrid = rf.grid)

  return(rf_model)

}

