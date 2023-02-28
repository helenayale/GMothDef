#' interpolate data
#'
#' @param band_data data to be interpolated
#' @param band_name band name
#'
#' @return interpolated daily time-series data
#' @export
#'
#' @examples
#' library(GMothDef)
#' library(stringr)
#' setwd('C:/YourFolder')
#' VV <- read.csv(file = 'VV.csv')
#' dat_int(VV, 'VV')
dat_int <- function(band_data, band_name) {

  band_days <- as.numeric(str_extract_all(colnames(band_data),"[0-9]+[0-9]"))
  band_days <- band_days[-1]

  min_day <- min(band_days)
  max_day <- max(band_days)
  int_days <- seq(from = min_day, to = max_day)
  date_name <- paste('Day', int_days, sep = '')

  eval(parse(text=paste('colnames(band_data)','<- paste("', band_name, '_", colnames(band_data), sep = "")', sep = '')))
  eval(parse(text=paste('colnames(band_data)[colnames(band_data) == "', band_name, '_id"] <- "id"', sep = '')))

  data.matrix<-matrix(nrow = 1, ncol = length(int_days))
  int <- data.frame(data.matrix)

  colnames(int) <- date_name
  int

  ## interpolate

  band_data_t <- as.data.frame(t(band_data))
  colnames(band_data_t) <-band_data_t[1,]
  band_data_t <- band_data_t[-1,]
  band_data_t$days <- band_days

  int_t <- as.data.frame(t(int))
  int_t$days <- int_days
  join_t <- left_join(int_t, band_data_t , by="days")
  join_t <- join_t[,-1]
  join_t

  join_t <- join_t[,colSums(is.na(join_t)) < nrow(join_t) - 1]

  cor_t <- join_t


  for (i in 2:ncol(join_t)){
    inter <- approx (join_t[,i], y = NULL, method = "linear", n = nrow(join_t), ties = mean)
    cor_t[,i] <-  inter$y
  }

  rownames(cor_t) <- date_name


  return(cor_t)

}
