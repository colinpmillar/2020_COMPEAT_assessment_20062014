## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(sf)
library(data.table)

mkdir("data")

# Read units from WKT
units <- st_read("bootstrap/data/AssessmentUnits/AssessmentUnits.csv") %>%
  st_set_crs(4326) %>%
  st_zm()

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