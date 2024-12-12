# Multivariate-drought-period-ranking
Script to rank different drought periods obtained from normalized indices.

The R script "Index calculations and run analysis" obtains precipitation data from a .nc file and calculates the SPI index for basins defined through a .shp file. From the index, a list of dataframes containing all drought period's characteristics is obtained.

Note: the drought period characteristics are defined not only through a threshold (e.g. -1) but also including all negative values before and after an under-threshold month. For example, see this figure:
![Drought run explanation_alt](https://github.com/user-attachments/assets/83307b00-15d0-4e93-9d14-2047148d56e4)
Drought Severity (DS) is then the cumulative sum of the index in a run, and Drought Intensity (DI) the Severity divided by the Drought Duration (DD).
