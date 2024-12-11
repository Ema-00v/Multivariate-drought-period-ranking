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

#Choose the basin of interest
basins <- filter(basins, codice=="BANSA")

#Calculate the SPI/SPEI of choice for the basin
{
  #Extract the precipitation values for the basin
  prec_basin <- extract(as(prec, "SpatRaster"), basins, mean, exact=TRUE)
  
  #Reformat results as data frame with time series for each basin
  drought_ind <- transpose(prec_basins[,2:dim(prec_basins)[2]])
  names(drought_ind) <- basins$codice
  
  drought_ind$BANSA <- spi(ts(drought_ind$BANSA,
                              start = c(year(t[1]),month(t[1])),
                              frequency = 12),
                           3)$fitted
}

#Run analysis on the series

