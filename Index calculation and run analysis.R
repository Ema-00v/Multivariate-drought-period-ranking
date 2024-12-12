pacman::p_load(pacman, SPEI, tidyverse, tidyterra, terra, stars, ggplot2)

#Script to rank drought periods in a multivariate way based on both duration and severity

#Load precipitation data and basins shapefiles
{
  prec <- read_stars("/home/admin/Desktop/Research/ATO4Water/R Index Analysis/NWIOIprecDAY.nc")
  
  #Change the name of the NWIOIprecDAY attribute
  attributes(prec)$names <- "Prec"
  
  #Define reference system
  crs <- st_crs('EPSG:4326')
  prec <- st_set_crs(prec, crs)
  
  #Aggregate on monthly scale
  prec <- aggregate(prec, by="months", FUN=sum)
  prec <- aperm(prec, c(2,3,1)) #Change dimensions back to x,y,time
  
  #Time vector
  t <- time(prec)
  
  #Basins in the Cuneo region
  basins <- vect("/home/admin/OneDrive/ATO4Water/Mappe/bacini_Luca.shp")
  basins <- project(basins, "EPSG:4326")
  
  #Sort in decreasing area order
  basins <- basins[order(basins$area, decreasing = TRUE)]
}

#Choose the basin(s) of interest
basins <- filter(basins, codice=="BANSA")

#Calculate the SPI of choice for the basin
{
  #Extract the precipitation values for the basin
  prec_basin <- extract(as(prec, "SpatRaster"), basins, mean, exact=TRUE)
  
  #Reformat results as data frame with time series for each basin
  drought_ind <- data.table::transpose(prec_basin[,2:dim(prec_basin)[2]])
  names(drought_ind) <- basins$codice
  
  drought_ind[,] <- apply(drought_ind, 2, function(x) spi(ts(x,
                              start = c(year(t[1]),month(t[1])),
                              frequency = 12),
                              3, ##Time scale
                              na.rm = TRUE,
                              verbose = FALSE)$fitted)
  
  #Remove attributes for further calculation
  attr(drought_ind[,], "dimnames") <- NULL
}

#Run analysis on the series, including onset and offset (months below zero before
#and after the period under the threshold). Based on script by Benedetta Rivella
{
  #Initialise a data frame indicating if runs are occurring (T/F)
  runs <- drought_ind
  runs[,] <- FALSE
  
  #Define threshold
  thr <- -1
  
  #For cycle calculating (for all basins) if a run is occurring
  for(i in 1:ncol(runs)){
    for (j in 1:nrow(runs)) {
      #Check if current month is under the -1 threshold
      if (!is.na(drought_ind[j,i]) && drought_ind[j,i] <= thr) {
        runs[j,i] <- TRUE
        
        #Check previous months
        for (k in (j-1):1) {
          if (is.na(drought_ind[k,i]) || drought_ind[k,i] > 0 || runs[k,i]) {
            break
          }
        }
        
        runs[(k+1):j,i] <- TRUE
        
        #Check following months
        for (k in (j+1):nrow(drought_ind)) {
          if (k>nrow(drought_ind) || is.na(drought_ind[k,i]) || 
              drought_ind[k,i] > 0 || drought_ind[k,i] <= -1) {
            break
          }
        }
        
        runs[j:(k-1),i] <- TRUE
      }
    }
  }
}

#Calculate the drought period's characteristics. Based on script by Benedetta Rivella
{
  #Create a list to store all the data frames of drought period's characteristics
  drought_periods <- vector(mode = "list", length = ncol(drought_ind))
  names(drought_periods) <- names(drought_ind)
  
  for(i in 1:ncol(drought_ind)){
    #Initialize a dataframe for the drought periods
    drought_period_loc <-data.frame(
      Start = POSIXct(),
      End = POSIXct(),
      Severity = numeric(),
      Duration = numeric(),
      Intensity = numeric(),
      stringsAsFactors = FALSE
    )
    
    #Calculate the start/end and the duration
    run_ongoing <- FALSE
    
    for (j in 1:nrow(runs)) {
      if (runs[j,i] && !run_ongoing) {
        run_ongoing <- TRUE
        Start <- t[j]
        Severity <- drought_ind[j,i]
        Duration <- 1
      } else if ((!runs[j,i] && run_ongoing) || (j==nrow(runs) && run_ongoing)) {
        run_ongoing <- FALSE
        End <- t[j-1]
        drought_period_loc <- rbind(drought_period_loc,
                                    data.frame(Start = Start,
                                               End = End,
                                               Severity = Severity,
                                               Duration = Duration))
      } else if (runs[j,i] && run_ongoing) {
        Severity <- Severity + drought_ind[j,i]
        Duration <- Duration + 1
      }
    }
    
    drought_period_loc$Intensity <- drought_period_loc$Severity/drought_period_loc$Duration
    
    #Put the dataframe of drought events in the list
    drought_periods[[i]] <- drought_period_loc
  }
}