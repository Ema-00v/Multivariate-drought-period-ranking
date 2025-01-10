## Multivariate ranking based on severity (cumulative sum of index during runs)
## and duration of runs

pacman::p_load(pacman, readxl, dplyr, tidyverse, ggplot2, scales, plotly)

##No weighting
events <- readxl::read_excel("Test_data.xlsx")

events <- events %>%
  mutate(
    Severity = -Severity,   
    Intensity = -Intensity
  )


# Normalize Duration and Severity between 1 and 10
events <- events %>%
  mutate(
    Intensity_norm = scales::rescale(Intensity, to = c(0, 1)),
    Severity_norm = scales::rescale(Severity, to = c(0, 1)),
    Duration_norm = scales::rescale(Duration, to = c(0, 1))
  )

# Sum of normalized Duration and Severity values to obtain a score
events <- events %>%
  mutate(
    Int_Dur = Intensity_norm + Duration_norm,
    Int_Sev = Intensity_norm + Severity_norm,
    Dur_Sev = Severity_norm + Duration_norm
  )

# Order events based on score (top to bottom) and create ranking
events <- events %>%
  arrange(desc(Int_Dur)) %>%
  mutate(Rank = row_number())

# Convert Dates from strings to Datetime
events$Start_Date <- as.Date(paste(events$Start_Date, "-01", sep=""), format="%Y-%m-%d")
events$End_Date <- as.Date(paste(events$End_Date, "-01", sep=""), format="%Y-%m-%d")

# Create Date interval
events$Date_Range <- paste(format(events$Start_Date, "%b %Y"), 
                           "-", 
                           format(events$End_Date, "%b %Y"))

# Add column for the first 8 events in ranking
events$Top8 <- ifelse(events$Rank <= 8, "Top 8", "Other")

top_8_events_Duration_Intensity <- events %>%
  filter(Rank <= 8)

# top_8_events_Severity_Intensity <- events %>%
#   filter(Rank <= 8)

# top_8_events_Duration_Severity <- events %>%
#   filter(Rank <= 8)

# Create interactive graph
fig <- plot_ly(events, 
               x = ~Intensity, 
               y = ~Duration, 
               type = 'scatter', 
               mode = 'markers', 
               color = ~Top8, 
               colors = c("Top 8" = "red", "Other" = "gray"),
               text = ~paste("Event: ", Event, "<br>Start date: ", Date_Range), # Testo per tooltip
               hoverinfo = 'text',  
               marker = list(size = 8)) %>%
  layout(title = "Interactive Graph: Intensity vs Duration of events",
         xaxis = list(title = "Intensity"),
         yaxis = list(title = "Duration"))

fig

# Save interactive graph
htmlwidgets::saveWidget(fig, "directory/figura.html")
