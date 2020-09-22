## Run analysis, write model results

## Before: Units.csv, UnitGrids.csv, Indicators.txt, IndicatorUnits.txt, StationSamples.txt
## After: Indicator_Annual.csv, Indicator_Assessment.csv, Assessment.csv

library(icesTAF)

mkdir("model")

unitsFile <- "data/Units.csv"
unitGridsFile <- "data/UnitGrids.txt"
indicatorsFile <- "data/Indicators.txt"
indicatorUnitsFile <- "data/IndicatorUnits.txt"
stationSamplesFile <- "data/StationSamples.txt"

# Read indicator configuration files -------------------------------------------
indicators <- fread(input = indicatorsFile, sep = "\t") %>% setkey(IndicatorID) 
indicatorUnits <- fread(input = indicatorUnitsFile, sep = "\t") %>% setkey(IndicatorID, UnitID)

wk1list = list()
wk2list = list()

# Loop indicator units ---------------------------------------------------------
# for (i in 1:nrow(indicatorUnits)) {
#   indicatorID <- indicatorUnits[i, IndicatorID]
# }

# Loop indicators --------------------------------------------------------------
for(i in 1:nrow(indicators)){
  indicatorID <- indicators[i, IndicatorID]
  criteriaID <- indicators[i, CategoryID]
  name <- indicators[i, Name]
  year.min <- indicators[i, YearMin]
  year.max <- indicators[i, YearMax]
  month.min <- indicators[i, MonthMin]
  month.max <- indicators[i, MonthMax]
  depth.min <- indicators[i, DepthMin]
  depth.max <- indicators[i, DepthMax]
  metric <- indicators[i, Metric]
  response <- indicators[i, Response]
  
  # Copy data
  wk <- as.data.table(stationSamples)  
  
  # Create Period
  wk[, Period := ifelse(month.min > month.max & Month >= month.min, Year + 1, Year)]
  
  # Create Indicator
  if (name == 'Dissolved Inorganic Nitrogen') {
    wk$ES <- apply(wk[, list(Nitrate..umol.l., Nitrite..umol.l., Ammonium..umol.l.)], 1, function(x){
      if (all(is.na(x)) | is.na(x[1])) {
        NA
      }
      else {
        sum(x, na.rm = TRUE)
      }
    })
  }
  else if (name == 'Dissolved Inorganic Phosphorus') {
    wk[,ES := Phosphate..umol.l.]
  }
  else if (name == 'Chlorophyll a') {
    wk[, ES := Chlorophyll.a..ug.l.]
  }
  else if (name == 'Oxygen Deficiency') {
    wk[, ES := Oxygen..ml.l. / 0.7] # Convert ml/l to mg/l by factor of 0.7
  }
  
  # Add unit grid size
  wk <- wk[unitGridSize, on="UnitID", nomatch=0]
  
  # Filter stations rows and columns --> UnitID, GridID, GridArea, Period, Month, StationID, Depth, Temperature, Salinity, ES
  if (month.min > month.max) {
    wk0 <- wk[
      (Period >= year.min & Period <= year.max) &
        (Month >= month.min | Month <= month.max) &
        (Depth..m.db..PRIMARYVAR.DOUBLE >= depth.min & Depth..m.db..PRIMARYVAR.DOUBLE <= depth.max) &
        !is.na(ES) & 
        !is.na(UnitID),
      .(IndicatorID = indicatorID, UnitID, GridSize, GridID, GridArea, Period, Month, StationID = StationID.METAVAR.INDEXED_TEXT, Depth = Depth..m.db..PRIMARYVAR.DOUBLE, Temperature = Temperature..degC., Salinity = Salinity..., ES)]
  } else {
    wk0 <- wk[
      (Period >= year.min & Period <= year.max) &
        (Month >= month.min & Month <= month.max) &
        (Depth..m.db..PRIMARYVAR.DOUBLE >= depth.min & Depth..m.db..PRIMARYVAR.DOUBLE <= depth.max) &
        !is.na(ES) & 
        !is.na(UnitID),
      .(IndicatorID = indicatorID, UnitID, GridSize, GridID, GridArea, Period, Month, StationID = StationID.METAVAR.INDEXED_TEXT, Depth = Depth..m.db..PRIMARYVAR.DOUBLE, Temperature = Temperature..degC., Salinity = Salinity..., ES)]
  }
  
  # Salinity Normalisation for Nutrients
  if (name == 'Dissolved Inorganic Nitrogen' || name == 'Dissolved Inorganic Phosphorus') {
    # Get linear regression coefficients on ES~Salinity and Mean Salinity
    wk00 <- wk0[!is.na(Salinity),
                .(N = .N,
                  MeanSalinity = mean(Salinity, na.rm = TRUE),
                  B = coef(lm(ES~Salinity))[1],
                  A = coef(lm(ES~Salinity))[2],
                  P = ifelse(.N >= 2, summary(lm(ES~Salinity))$coef[2, 4], NA_real_),
                  R2 = summary(lm(ES~Salinity))$adj.r.squared),
                keyby = .(IndicatorID, UnitID)]
  }
  
  # Merge data tables
  wk0 <- wk00[wk0]
  
  # Normalise indicator concentration if the indicator has a significant relation to salinity e.g. above the 95% confidence level (p<0.05)
  # ES_normalised = ES_observed + A * (S_reference - S_observed)
  # https://www.ospar.org/site/assets/files/37302/national_common_procedure_report_2016_sweden.pdf
  wk0[, ESS := ifelse(P < 0.05 & !is.na(P) & !is.na(Salinity), ES + A * (MeanSalinity - Salinity), ES)]
  
  # NB! Salinity Normalisation above currently only implemented as test and not taken forward yet!  
  
  if (metric == 'Mean'){
    # Calculate station mean --> UnitID, GridID, GridArea, Period, Month, ES, SD, N
    wk1 <- wk0[, .(ES = mean(ES), SD = sd(ES), N = .N), keyby = .(IndicatorID, UnitID, GridID, GridArea, Period, Month, StationID)]
    
    # Calculate annual mean --> UnitID, Period, ES, SD, N, NM
    wk2 <- wk1[, .(ES = mean(ES), SD = sd(ES), N = .N, NM = uniqueN(Month)), keyby = .(IndicatorID, UnitID, Period)]
  } else if (metric == 'Minimum') {
    # Calculate station minimum --> UnitID, GridID, GridArea, Period, Month, ES, SD, N
    wk1 <- wk0[, .(ES = min(ES), SD = sd(ES), N = .N), keyby = .(IndicatorID, UnitID, GridID, GridArea, Period, Month, StationID)]
    
    # Calculate annual minimum --> UnitID, Period, ES, SD, N, NM
    wk2 <- wk1[, .(ES = min(ES), SD = sd(ES), N = .N, NM = uniqueN(Month)), keyby = .(IndicatorID, UnitID, Period)]
  }
  
  wk1list[[i]] <- wk1
  wk2list[[i]] <- wk2
}

# Combine station and annual indicator results
wk1 <- rbindlist(wk1list)
wk2 <- rbindlist(wk2list)

# Combine with indicator and indicator unit configuration tables
wk3 <- indicators[indicatorUnits[wk2]]

# Calculate Eutrophication Ratio (ER)
wk3[, ER := ifelse(Response == 1, ES / ET, ET / ES)]

# Calculate (BEST)
wk3[, BEST := ifelse(Response == 1, ET / (1 + ACDEV / 100), ET / (1 - ACDEV / 100))]

# Calculate Ecological Quality Ratio (ERQ)
wk3[, EQR := ifelse(Response == 1, ifelse(BEST > ES, 1, BEST / ES), ifelse(ES > BEST, 1, ES / BEST))]

# Calculate Ecological Quality Ratio Boundaries (ERQ_HG/GM/MP/PB)
wk3[, EQR_GM := ifelse(Response == 1, 1 / (1 + ACDEV / 100), 1 - ACDEV / 100)]
wk3[, EQR_HG := 0.5 * 0.95 + 0.5 * EQR_GM]
wk3[, EQR_PB := 2 * EQR_GM - 0.95]
wk3[, EQR_MP := 0.5 * EQR_GM + 0.5 * EQR_PB]

# Calculate Ecological Quality Ratio Scaled (EQRS)
wk3[, EQRS := ifelse(EQR <= EQR_PB, (EQR - 0) * (0.2 - 0) / (EQR_PB - 0) + 0,
                     ifelse(EQR <= EQR_MP, (EQR - EQR_PB) * (0.4 - 0.2) / (EQR_MP - EQR_PB) + 0.2,
                            ifelse(EQR <= EQR_GM, (EQR - EQR_MP) * (0.6 - 0.4) / (EQR_GM - EQR_MP) + 0.4,
                                   ifelse(EQR <= EQR_HG, (EQR - EQR_GM) * (0.8 - 0.6) / (EQR_HG - EQR_GM) + 0.6,
                                          (EQR - EQR_HG) * (1 - 0.8) / (1 - EQR_HG) + 0.8))))]

# Calculate General Temporal Confidence (GTC) - Confidence in number of annual observations
wk3[, GTC := ifelse(N > GTC_HM, 100, ifelse(N < GTC_ML, 0, 50))]

# Calculate Number of Months Potential
wk3[, NMP := ifelse(MonthMin > MonthMax, 12 - MonthMin + 1 + MonthMax, MonthMax - MonthMin + 1)]

# Calculate Specific Temporal Confidence (STC) - Confidence in number of annual missing months
wk3[, STC := ifelse(NMP - NM <= STC_HM, 100, ifelse(NMP - NM >= STC_ML, 0, 50))]

# Calculate General Spatial Confidence (GSC) - Confidence in number of annual observations per number of grids 
wk3 <- wk3[as.data.table(unitgrids)[, .(NG = as.numeric(sum(GridArea) / mean(GridSize^2))), .(UnitID)], on = .(UnitID = UnitID), nomatch=0]
wk3[, GSC := ifelse(N / NG > GSC_HM, 100, ifelse(N / NG < GSC_ML, 0, 50))]

# Calculate Specific Spatial Confidence (SSC) - Confidence in area of sampled grid units as a percentage to the total unit area
a <- wk1[, .N, keyby = .(IndicatorID, UnitID, Period, GridID, GridArea)] # UnitGrids
b <- a[, .(GridArea = sum(as.numeric(GridArea))), keyby = .(IndicatorID, UnitID, Period)] #GridAreas
c <- as.data.table(units)[, .(UnitArea = as.numeric(UnitArea)), keyby = .(UnitID)] # UnitAreas
d <- c[b, on = .(UnitID = UnitID)] # UnitAreas ~ GridAreas
wk3 <- wk3[d[,.(IndicatorID, UnitID, Period, UnitArea, GridArea)], on = .(IndicatorID = IndicatorID, UnitID = UnitID, Period = Period)]
wk3[, SSC := ifelse(GridArea / UnitArea * 100 > SSC_HM, 100, ifelse(GridArea / UnitArea * 100 < SSC_ML, 0, 50))]
rm(a,b,c,d)

# Calculate assessment ES --> UnitID, Period, ES, SD, N, GTC, STC, GSC, SSC
wk4 <- wk3[, .(Period = min(Period) * 10000 + max(Period), ES = mean(ES), SD = sd(ES), N = .N, GTC = mean(GTC), STC = mean(STC), GSC = mean(GSC), SSC = mean(SSC)), .(IndicatorID, UnitID)]

# Add Year Count where STC = 100
wk4 <- wk3[STC == 100, .(NSTC100 = .N), .(IndicatorID, UnitID)][wk4, on = .(IndicatorID, UnitID)]

wk4[, STC := ifelse(!is.na(NSTC100) & NSTC100 >= N/2, 100, STC)]

wk4 <- wk4[, TC := (GTC + STC) / 2]

wk4 <- wk4[, SC := (GSC + SSC) / 2]

wk4 <- wk4[, C := (TC + SC) / 2]

# Combine with indicator and indicator unit configuration tables
wk5 <- indicators[indicatorUnits[wk4]]

# Standard Error
wk5[, SE := SD / sqrt(N)]

# 95 % Confidence Interval
wk5[, CI := qnorm(0.975) * SE]

# Calculate Eutrophication Ratio (ER)
wk5[, ER := ifelse(Response == 1, ES / ET, ET / ES)]

# Calculate (BEST)
wk5[, BEST := ifelse(Response == 1, ET / (1 + ACDEV / 100), ET / (1 - ACDEV / 100))]

# Calculate Ecological Quality Ratio (ERQ)
wk5[, EQR := ifelse(Response == 1, ifelse(BEST > ES, 1, BEST / ES), ifelse(ES > BEST, 1, ES / BEST))]

# Calculate Ecological Quality Ratio Boundaries (ERQ_HG/GM/MP/PB)
wk5[, EQR_GM := ifelse(Response == 1, 1 / (1 + ACDEV / 100), 1 - ACDEV / 100)]
wk5[, EQR_HG := 0.5 * 0.95 + 0.5 * EQR_GM]
wk5[, EQR_PB := 2 * EQR_GM - 0.95]
wk5[, EQR_MP := 0.5 * EQR_GM + 0.5 * EQR_PB]

# Calculate Ecological Quality Ratio Scaled (EQRS)
wk5[, EQRS := ifelse(EQR <= EQR_PB, (EQR - 0) * (0.2 - 0) / (EQR_PB - 0) + 0,
                     ifelse(EQR <= EQR_MP, (EQR - EQR_PB) * (0.4 - 0.2) / (EQR_MP - EQR_PB) + 0.2,
                            ifelse(EQR <= EQR_GM, (EQR - EQR_MP) * (0.6 - 0.4) / (EQR_GM - EQR_MP) + 0.4,
                                   ifelse(EQR <= EQR_HG, (EQR - EQR_GM) * (0.8 - 0.6) / (EQR_HG - EQR_GM) + 0.6,
                                          (EQR - EQR_HG) * (1 - 0.8) / (1 - EQR_HG) + 0.8))))]

# Category ---------------------------------------------------------------------

# Category result as a weighted average of the indicators in each category per unit - CategoryID, UnitID, N, ER, EQR, EQRS, C
wk6 <- wk5[, .(.N, ER = sum(ER * IW / 100), EQR = sum(EQR * IW / 100), EQRS = sum(EQRS * IW / 100), C = sum(C * IW / 100)), .(CategoryID, UnitID)]

wk7 <- dcast(wk6, UnitID ~ CategoryID, value.var = c("N","ER","EQR","EQRS","C"))

# Assessment -------------------------------------------------------------------

# Assessment result - UnitID, N, ER, EQR, EQRS, C
wk8 <- wk6[, .(.N, ER = max(ER), EQR = min(EQR), EQRS = min(EQRS), C = mean(C)), (UnitID)]

wk9 <- wk7[wk8, on = .(UnitID = UnitID), nomatch=0]

# Write results
fwrite(wk3, file = "model/Indicator_Annual.csv")
fwrite(wk5, file = "model/Indicator_Assessment.csv")
fwrite(wk9, file = "model/Assessment.csv")