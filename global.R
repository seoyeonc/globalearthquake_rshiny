library('ggplot2')
library('tidyverse')
library('mapview')
library('svglite')
library('randomForest')

# feature information ----

information_tab <- data.frame(
  Feature = c(
    "magnitude","cdi","mmi","sig","nst","dmin","gap","depth",
    "latitude","longitude","Year","Month","tsunami"
  ),
  Type = c(
    "Float","Integer","Integer","Integer","Integer","Float","Float","Float",
    "Float","Float","Integer","Integer","Binary"
  ),
  Description = c(
    "Earthquake magnitude (Richter scale)",
    "Community Decimal Intensity (felt intensity)",
    "Modified Mercalli Intensity (instrumental)",
    "Event significance score",
    "Number of seismic monitoring stations",
    "Distance to nearest seismic station (degrees)",
    "Azimuthal gap between stations (degrees)",
    "Earthquake focal depth (km)",
    "Epicenter latitude (WGS84)",
    "Epicenter longitude (WGS84)",
    "Year of occurrence",
    "Month of occurrence",
    "Tsunami potential (TARGET)"
  ),
  Range_or_Values = c(
    "6.5 - 9.1",
    "0 - 9",
    "1 - 9",
    "650 - 2910",
    "0 - 934",
    "0.0 - 17.7",
    "0.0 - 239.0",
    "2.7 - 670.8",
    "-61.85° to 71.63°",
    "-179.97° to 179.66°",
    "2001 - 2022",
    "1 - 12",
    "0, 1"
  ),
  Tsunami_Relevance = c(
    "High - Primary tsunami predictor",
    "Medium - Population impact measure",
    "Medium - Structural damage indicator",
    "High - Overall hazard assessment",
    "Low - Data quality indicator",
    "Low - Location precision",
    "Low - Location reliability",
    "High - Shallow = higher tsunami risk",
    "High - Ocean proximity indicator",
    "High - Ocean proximity indicator",
    "Medium - Temporal patterns",
    "Low - Seasonal analysis",
    "TARGET VARIABLE"
  ),
  stringsAsFactors = FALSE
)


df <- read.csv('earthquake_data_tsunami.csv')
df <- df%>%mutate(date=paste(Year,'/',Month))
df$popup_txt <- paste0(
  "<br><b>Magnitude: </b>", df$magnitude,
  "<br><b>Date: </b>", df$date
)

# Map ----


p2 <- mapview(
  df,
  xcol = "longitude",
  ycol = "latitude",
  crs = 4326,
  grid = FALSE,
  zcol = "magnitude",    
  alpha = 0.8,
  cex = 6,
  popup = df$popup_txt,
  legend = TRUE
)@map

# feature and target ----
feature_cols <- c("magnitude","depth","sig","latitude","longitude",
                  "dmin","gap","nst","mmi","cdi")

target_col   <- "magnitude"

# tsunami prediction ----

df$tsunami_factor <- factor(df$tsunami, levels = c(0,1),
                            labels = c("No Tsunami","Tsunami"))

tsu_model <- glm(
  tsunami ~ magnitude + depth + sig + latitude + longitude,
  data = df,
  family = binomial
)


# Hazard Mapping ----

scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}
