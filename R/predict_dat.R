#' Make predictions using rf model
#'
#' @param rf_model the model to make predictions
#' @param newd new data for predictions
#' @param ID_plot the field name of plot names in the new data (newd)
#' @param ID_pred the field/file name to record prediction data
#'
#' @return prediction
#' @export
#'
#' @examples
#' library(GMothDef)
#' predict_dat(rf, newd, 'id', 'SR_pred')
predict_dat <- function(rf_model, newd, ID_plot, ID_pred){
  # run prediction
  pred = predict(rf_model, newdata = newd)
  # save the dataset
  eval(parse(text=paste('pred_dat <- data.frame(plot = newd[,"',ID_plot, '"], pred = ', ID_pred,')',  sep = '')))
  eval(parse(text=paste('write.csv(pred_dat, "', ID_pred,'.csv")',  sep = '')))

  return(pred)

}
