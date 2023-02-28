#' choose smoothing span
#'
#' @param band_name band name/title of the output plot
#' @param dat daily data to be smoothed e.g. output from dat_int()
#' @param span a vector of spans to be compared
#'
#' @return a plot to help choosing smoothing span
#' @export
#'
#' @examples
#' library(GMothDef)
#' library(stringr)
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#' setwd('C:/YourFolder')
#' VV <- read.csv(file = 'VV.csv')
#' dat_int(VV, 'VV')
#' smth_span('VV', dat_int, c(0.25,0.5,0.75))
smth_span <- function(band_name, dat, span){
  df <- data.frame(DOY = dat[,1], origin = dat[,2])

  for (i in 1:length(span)){

    for (j in 2:ncol(dat)){
      loessMod <- loess(dat[,j] ~ dat[,1], span = span[i]) # smoothing span
      smoothed <- predict(loessMod)
      if (j == 2){
        smth_t <- as.data.frame(dat[,1])
        smth_t <- cbind(smth_t, smoothed)
      }else{
        smth_t <- cbind(smth_t, smoothed)
      }

    }
    colnames(smth_t) <- c('DOY',paste('s', span[i], sep = ''))
    df <- cbind(df,smth_t[2])
  }

  eval(parse(text = paste('df2 <- df %>%
    gather(key = "span", value = ', band_name, ',-DOY)',sep = '')))
  eval(parse(text=paste('p1 <- ggplot(data = df2, aes(x = DOY, y = ', band_name, ', group = span, color = span)) +
    geom_line()', sep = '')))
  p1

  return(p1)
}
