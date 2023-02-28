#' Data extraction from folders
#'
#' @param band_names a vector of folder/band names
#' @param plot_shp shapefile of the plots
#' @param shp_type 'point' or 'polygon
#' @param date_start date start in the file name
#' @param ID_plot the field name of plot id
#'
#' @return files saved in the original directory
#' @export
#'
#' @examples
#' library(GMothDef)
#' library(gtools)
#' library(raster)
#' library(rgdal)
#' library(TDPanalysis)
#' setwd("C:/YourFolder")
#' band <- c('VH', 'VV')
#' dat_folder(band, 'study_area.shp', 'point', 25, 'ID')
dat_folder <- function(band_names, plot_shp, shp_type, date_start, ID_plot) {
  origin_wd <- getwd()
  # load shapefile of plots
  shp <- readOGR(plot_shp)

  for (n in 1:length(band_names)){
    print(band_names[n])
    # set working directory to the folder
    eval(parse(text=paste('setwd("',origin_wd, '/',band_names[n],'")',  sep = '')))

    # load all tiff files
    file_list <-  mixedsort(list.files(pattern = '.tif$', all.files = TRUE, full.names = FALSE))
    # first image
    first_img <- raster(file_list[1])

    test_date <- paste(substr(file_list[1],date_start,date_start+3), '/',substr(file_list[1],date_start+4,date_start+5),'/',substr(file_list[1],date_start+6,date_start+7), sep = '')
    study_plots <- spTransform(shp, CRS(proj4string(first_img)))

    # extract plot id
    eval(parse(text=paste('df <- data.frame(id = study_plots$', ID_plot,')', sep = '')))

    print('start extraction')

    for (i in 1:length(file_list)){
      img <- stack(file_list[i])
      ymd_date <- paste(substr(file_list[i],date_start,date_start+3), '/',substr(file_list[i],date_start+4,date_start+5),'/',substr(file_list[i],date_start+6,date_start+7), sep = '')
      date <- date.to.DOY(ymd_date, format = "yyyy/mm/dd")

      if(i == 1){
        c_date <- date
        j <- 1
        print(date)
      }else{
        if(date == c_date){
          j <- j + 1
        }else{
          c_date <- date
          j <- 1
          print(date)
        }
      }


        if (shp_type == 'point'){
          a <- vector()
          a <- extract(x = img, y = study_plots, fun = NULL, na.rm = TRUE, exact=FALSE, weights=FALSE) # extract from points
          eval(parse(text=paste('df_', k, '$Day', date, '_', j,'<-a', sep = '')))

        }else if(shp_type == 'polygon'){
          a <- vector()
          b <- vector()
          a <- extract(x = img, y = study_plots, fun = NULL, na.rm = TRUE, exact=FALSE, weights=FALSE) # extract from polygon
          b <- unlist(lapply(a, FUN = function(x) { mean(x, na.rm = TRUE) })) # calculate mean value for each polygon
          eval(parse(text=paste('df_', k, '$Day', date, '_', j,'<-b', sep = '')))
        }

      print("start merging")

      col_dat <- colnames(df)
      df_merge <- data.frame(id = df[,'id'])
      df_ncol <- ncol(df)

      for (i in 2:df_ncol){
        date <- str_extract(string = col_dat[i], pattern = "[0-9]+")

        if (i == 2){
          c_date <- date
          j <- 1
          print(date)
        }else{
          if (date == c_date){
            j <- j+1
          }else{
            n <- i - j
            m <- i - 1
            if(n == m){
              dat <- df[,n]
              eval(parse(text=paste('df_merge$Day', c_date,'<- dat', sep = '')))
            }else{
              dat <- df[,n:m]
              eval(parse(text=paste('df_merge$Day', c_date,'<- apply(dat,1,mean,na.rm = TRUE)', sep = '')))
            }


            c_date <- date
            j <- 1
            print(date)
          }
        }
        if (i == df_ncol){
          n <- i - j + 1
          m <- i
          dat <- df[,n:m]
          eval(parse(text=paste('df_merge$Day', c_date,'<- apply(dat,1,mean,na.rm = TRUE)', sep = '')))
        }

      }
      rownames(df_merge) <- df_merge[,'id']

      setwd(origin_wd)

      eval(parse(text=paste('write.csv(df,"',band_names[n],'_origin.csv", row.names = TRUE)', sep = '')))
      eval(parse(text=paste('write.csv(df_merge, "', band_names[n],'.csv", row.names = TRUE)', sep = '')))

      print('data saved')

    }
  }











}



