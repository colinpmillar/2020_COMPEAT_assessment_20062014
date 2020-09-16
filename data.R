## Preprocess data, write TAF data tables

## Before: AssessmentUnits.csv, UnitGridSize.txt, Indicators.txt, IndicatorUnits.txt, StationSamples.txt
## After: Units.csv, UnitGrids.csv, Indicators.txt, IndicatorUnits.txt, StationSamples.txt

library(icesTAF)
library(sf)
library(data.table)
library(tidyverse)
#library(readxl)

mkdir("data")

unitsFile <- "bootstrap/data/AssessmentUnits/AssessmentUnits.csv"
unitGridSizeFile <- "bootstrap/data/Configs/UnitGridSize.txt"
indicatorsFile <- "bootstrap/data/Configs/Indicators.txt"
indicatorUnitsFile <- "bootstrap/data/Configs/IndicatorUnits.txt"
stationSamplesFile <- "bootstrap/data/StationSamples/StationSamples.txt"

# Units ------------------------------------------------------------------------

# Read units from WKT
units <- st_read(unitsFile) %>%
  st_set_crs(4326)

# Remove unnecessary dimensions and convert data.frame to data.table
units <- as.data.table(st_zm(units))

# Order, Rename and Remove columns 
units <- units[order(ID), .(Code = ID, Description = LongName, GEOM = geometry)] %>%
  st_sf()

# Assign UnitIDs
units$UnitID = 1:nrow(units)

# Identify invalid geometries
st_is_valid(units)

# Write to database
# st_write(
#   units,
#   dsn = "MSSQL:server=SQL09;database=OceanCOMPEAT_20062014_COMP4;trusted_connection=yes;",
#   layer = "AssessmentUnit",
#   layer_options = c("LAUNDER=NO", "GEOM_NAME=GEOM", "FID=ID")
# )

# Transform projection into ETRS_1989_LAEA
units <- st_transform(units, crs = 3035)

# Calculate UnitArea
units$UnitArea <- st_area(units)

# Identify invalid geometries
st_is_valid(units)

# Make geometries valid by doing the buffer of nothing trick
units <- sf::st_buffer(units, 0.0)

# Identify overlapping assessment units
#st_overlaps(units)

# UnitGrids --------------------------------------------------------------------

# Make unit grids
make.unitgrids <- function(units, gridSize) {
  units <- st_transform(units, crs = 3035)
  
  bbox <- st_bbox(units)
  
  xmin <- floor(bbox$xmin / gridSize) * gridSize
  ymin <- floor(bbox$ymin / gridSize) * gridSize
  xmax <- ceiling(bbox$xmax / gridSize) * gridSize
  ymax <- ceiling(bbox$ymax / gridSize) * gridSize
  
  xn <- (xmax - xmin) / gridSize
  yn <- (ymax - ymin) / gridSize
  
  grids <- st_make_grid(units, cellsize = gridSize, c(xmin, ymin), n = c(xn, yn), crs = 3035) %>%
    st_sf()
  
  grids$GridID = gridSize + 1:nrow(grids)
  
  unitgrids <- st_intersection(grids, units)

  unitgrids$Area <- st_area(unitgrids)
  
  return(unitgrids)
}

unitgrids10 <- make.unitgrids(units, 10000)
unitgrids30 <- make.unitgrids(units, 30000)
unitgrids60 <- make.unitgrids(units, 60000)

unitGridSize <-  fread(input = unitGridSizeFile, sep = "\t") %>% setkey(UnitID)

a <- merge(unitGridSize[GridSize == 10000], unitgrids10 %>% select(UnitID, UnitArea, GridID, GridArea = Area))
b <- merge(unitGridSize[GridSize == 30000], unitgrids30 %>% select(UnitID, UnitArea, GridID, GridArea = Area))
c <- merge(unitGridSize[GridSize == 60000], unitgrids60 %>% select(UnitID, UnitArea, GridID, GridArea = Area))
unitgrids <- st_cast(st_as_sf(rbindlist(list(a,b,c))))
rm(a,b,c)

# Plot
#ggplot() + geom_sf(data = units) + coord_sf()
#ggplot() + geom_sf(data = unitgrids10) + coord_sf()
#ggplot() + geom_sf(data = unitgrids30) + coord_sf()
#ggplot() + geom_sf(data = unitgrids60) + coord_sf()
#ggplot() + geom_sf(data = unitgrids) + coord_sf()

# StationSamples ---------------------------------------------------------------

# Read stationSamples
stationSamples <- fread(input = stationSamplesFile, sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE, check.names = TRUE)

# Extract Stations by assigning a StationID by natural key
#stationSamples[, StationID := .GRP, by = .(Cruise, Station, Year, Month, Day, Hour, Minute, Latitude..degrees_north., Longitude..degrees_east.)]
#stationSamples[, .N, .(StationID, Cruise, Station, Year, Month, Day, Hour, Minute, Latitude..degrees_north., Longitude..degrees_east.)]
#stationSamples[, .N, .(StationID.METAVAR.INDEXED_TEXT)]

# Extract Samples by assigning a SampleID ny natural key
#stationSamples[, SampleID := .GRP, by = .(Cruise, Station, Year, Month, Day, Hour, Minute, Latitude..degrees_north., Longitude..degrees_east., Depth..m.db..PRIMARYVAR.DOUBLE)]
#stationSamples[, .N, .(StationID, Cruise, Station, Year, Month, Day, Hour, Minute, Latitude..degrees_north., Longitude..degrees_east., Depth..m.db..PRIMARYVAR.DOUBLE)]
#stationSamples[, .N, .(SampleID.METAVAR.INDEXED_TEXT)]

# Make stations spatial keeping original latitude/longitude
stationSamples <- st_as_sf(stationSamples, coords = c("Longitude..degrees_east.", "Latitude..degrees_north."), remove = FALSE, crs = 4326)

# Transform projection into ETRS_1989_LAEA
stationSamples <- st_transform(stationSamples, crs = 3035)

# Classify stations into unit grids
stationSamples <- st_join(stationSamples, st_cast(unitgrids), join = st_intersects)

# Remove spatial column
stationSamples <- st_set_geometry(stationSamples, NULL)

# Indicators -------------------------------------------------------------------

indicators <- fread(input = indicatorsFile, sep = "\t") %>% setkey(IndicatorID) 

# IndicatorUnits ---------------------------------------------------------------

indicatorUnits <- fread(input = indicatorUnitsFile, sep = "\t") %>% setkey(IndicatorID, UnitID)

# Write data to data directory
st_write(units, "data/units.csv", delete_dsn=TRUE)
st_write(units, "data/units.shp", delete_layer = TRUE)
st_write(unitgrids, "data/unitgrids.csv", delete_dsn=TRUE)
st_write(unitgrids, "data/unitgrids.shp", delete_layer = TRUE)
fwrite(stationSamples, file = "data/stationSamples.txt", sep = "\t")
fwrite(indicators, file = "data/indicators.txt", sep = "\t")
fwrite(indicatorUnits, file = "data/indicatorUnits.txt", sep = "\t")