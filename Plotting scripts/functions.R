#- Load weights--------------------------------------------------------------------------------------------------
loadWeight <- function(dataFolder, type = "stock") {
  if (type == "activity") weight <- fread(file.path(dataFolder, "weights", "weight_million_vehicle_km.csv"))[, c("country", "meanESdemandShare")]
  if (type == "stock") weight <- fread(file.path(dataFolder, "weights", "weight_stock.csv"))[, c("country", "meanStockShare")]
  return(weight)
} 
#- Load ranges---------------------------------------------------------------------------------------------------
loadRanges <- function(dataFolder) {
  pathToRanges <- file.path(dataFolder, "infrastructure", "ranges.csv")
  ranges <- fread(pathToRanges, header = TRUE)[parameter == "Range"]
  ranges[, c("subCategory", "parameter", "note") := NULL]
  ranges <- melt(ranges, id.vars = c("vehicleParameterScenario", "country", "unit", "truckClass", "truckTechnology"), variable.name = "period")
  ranges[, period := as.double(as.character(period))]
  ranges[, value := as.double(value)]
  ranges[, c("country", "unit") := NULL]
  return(ranges)
}
#- Load infrastructure scale up----------------------------------------------------------------------------------
loadInfrastructureBuildUp <- function(dataFolder) {
  pathToInfrastructure <- file.path(dataFolder, "infrastructure", "InfrastructureBuildUp.csv")
  infrastructure <- fread(pathToInfrastructure, header = TRUE)
  infrastructure <- melt(infrastructure, id.vars = c("unit"), variable.name = "period")
  infrastructure[, period := as.double(as.character(period))]
  infrastructure[, c("unit") := NULL]
  return(infrastructure)
}
#- Load CO2 price current polices scenario ------------------------------------------------------------------------------------------------------------------
loadCO2PriceTrajectory <- function(dataFolder) {
  energyCarrierParameters <- fread(file.path(dataFolder, "TCOparameter", "energyCarrierParameters.csv"))
  co2PriceCurrentPol <- energyCarrierParameters[paperScen == "Current policies" & parameter == "Non-ETS CO2"]
  co2PriceCurrentPol <- unique(co2PriceCurrentPol[, c("period", "value")])
  return(co2PriceCurrentPol)
}
#- Load diesel blend shares ------------------------------------------------------------------------------------------------------------------
loadDieselBlendShares <- function(dataFolder) {
  energyCarrierParameters <- fread(file.path(dataFolder, "TCOparameter", "energyCarrierParameters.csv"))
  dieselBlendShares <- energyCarrierParameters[parameter %in% c("Share Synthetic Diesel (E-Fuels)", "Share Biodiesel")]
  return(dieselBlendShares)
}
#- Load and prepare mileage data------------------------------------------------------------------------------------------------------------------
prepareMileageData <- function(dataFolder, bin = TRUE, reduce = TRUE) {
  pathToMilageData <- file.path(dataFolder, "mileage")
  files <- list.files(pathToMilageData, full.names = TRUE)
  
  readMileageFile <- function(path){
    
    region <- gsub(".*\\/", "", path)
    region <- gsub("_.*csv", "", region)
    
    yr <- gsub(".*\\/", "", path)
    yr <- gsub("^.._", "", yr)
    yr <- gsub("_.*csv", "", yr)
    data <- fread(path)[, country := region][, period := as.double(as.character(yr))]
  }
  
  mileage<- rbindlist(lapply(files, readMileageFile))
  # 4173 and 4174 in ES and 4287 4288 4289 in NL are na
  mileage <- mileage[!is.na(jfl)]
  setnames(mileage, c("tfl_av", "tfl_max", "jfl", "type"),
           c("dvktMean", "dvktMax", "avkt", "truckClass"))
  mileage[`truckClass` == "rig", `truckClass` := "Rigid"]
  mileage[`truckClass` == "tt", `truckClass` := "Tractor-trailer"]
  feasColsToDel <- names(mileage)[grepl("^bet.*", names(mileage))]
  mileage[, eval(feasColsToDel) := NULL][, "period" := NULL]
  mileage <- unique(mileage)
  # Weigh the countries based on actual vehicle km traveled
  weightedMileage <- merge(mileage, loadWeight(dataFolder, "activity"))
  weightedMileage <- merge(weightedMileage, loadWeight(dataFolder, "stock"))
  # Calculate total road km 
  weightedMileage[, totRoadkmEUR := sum(avkt)]
  weightedMileage[, totRoadkmCountry := sum(avkt), by = "country"]
  weightedMileage[, totVehEUR := .N]
  weightedMileage[, totVehCountry := .N, by = "country"]
  # Calculate weight: sample deviation to empirical total demand per country
  weightedMileage[, ESdemandShareProfiles := totRoadkmCountry/totRoadkmEUR]
  weightedMileage[, vehShareProfiles := totVehCountry/totVehEUR]
  weightedMileage[, factorDemand := meanESdemandShare/ESdemandShareProfiles]
  weightedMileage[, factorStock := meanStockShare/vehShareProfiles]
  weightedMileage[, avktWeighted := avkt * factorDemand]
  # 1 sample = 1 vehicle
  weightedMileage[, stockWeighted := factorStock]
  weightedMileage[, test1 := abs((sum(avktWeighted)/totRoadkmEUR) - meanESdemandShare) , by = "country"]
  weightedMileage[, test2 := abs((sum(stockWeighted)/totVehEUR) - meanStockShare) , by = "country"]
  if ((sum(weightedMileage$test1) > 1e-6) | (sum(weightedMileage$test2) > 1e-6)) stop("Country weighting did not work properly")
  # Calculate shares in total road km
  weightedMileage[, weightedShareEUR := avktWeighted/totRoadkmEUR]
  weightedMileage[, weightedVehShareEUR := stockWeighted/totVehEUR]
  if ((1 - sum(weightedMileage$weightedShareEUR) > 1e-6) | (1 - sum(weightedMileage$weightedVehShareEUR) > 1e-6)) stop("EUR avkt shares of individual mileage profiles are wrong")
  # Within the Countries no weights need to be applied. The data resembles the mileage distribution within the Countries
  # -> use unweighted avkt and unweighted totRoadkmCountry
  weightedMileage[, shareCountry := avkt/totRoadkmCountry]
  weightedMileage[, shareVehCountry := 1/totVehCountry]
  weightedMileage[, test1 := sum(shareCountry), by = "country"]
  weightedMileage[, test2 := sum(shareVehCountry), by = "country"]
  if (any(abs(weightedMileage$test1 - 1) > 1e-6) | any(abs(weightedMileage$test2 - 1) > 1e-6))
    stop("Country avkt shares of individual mileage profiles are wrong")
  weightedMileage[, c("test1", "test2", "meanESdemandShare", "ESdemandShareProfiles", "vehShareProfiles", "stockWeighted", "avktWeighted",  
                      "totRoadkmCountry", "totVehCountry", "totRoadkmEUR", "totVehEUR", "factorDemand", "factorStock", "meanStockShare") := NULL]
  if (bin == FALSE) {return(weightedMileage)} 
  else if (bin == TRUE) {
    # weightedMileage: Original detailed mileage data weighted by Country regarding the annual vehicle km travelled 
    # so that the share in the total travelled km in the selected markets resembles the actual shares in annual vehicle km travelled these markets have
    # The weighted avkt is used to caluclate the share in total road km. 
    binnedWeightedMileage <- copy(weightedMileage)
    # Split AM data into equally sized bins
    # Define global bin breaks based on the entire dataset
    global_breaks <- seq(min(mileage$avkt, na.rm = TRUE), max(mileage$avkt, na.rm = TRUE), length.out = 31)
    # Apply the same breaks to all groups
    binnedWeightedMileage[, avktBinned := cut(avkt, breaks = global_breaks, include.lowest = TRUE)]
    # Calculate mean annual mileage for each bin
    binnedWeightedMileage[, binMean := mean(avkt), by = c("avktBinned", "truckClass", "country")]
    # Claculate shares per bin 
    binnedWeightedMileage[, binShareCountry := sum(shareCountry), by = c("avktBinned", "truckClass", "country")]
    binnedWeightedMileage[, binVehshareCountry := sum(shareVehCountry), by = c("avktBinned", "truckClass", "country")]
    binnedWeightedMileage[, binWeightedShareEUR := sum(weightedShareEUR), by = c("avktBinned", "truckClass", "country")]
    binnedWeightedMileage[, binWeightedVehShareEUR := sum(weightedVehShareEUR), by = c("avktBinned", "truckClass", "country")]}
    # binnedWeightedMileage: Original detailed mileage data including avkt shares weighted by Country for EUR. Mileage is grouped into bins for TCO calulation
    # Get rid of data within the bins to speed up the process
    if (reduce == FALSE) {return(binnedWeightedMileage)} 
    else if (reduce == TRUE) {
      reducedBinnedWeightedMileage <- copy(binnedWeightedMileage)[, c("V1", "dvktMean", "dvktMax", "wfl", "avkt", "weightedShareEUR", "weightedVehShareEUR", "shareCountry", "shareVehCountry") := NULL]
      reducedBinnedWeightedMileage <- unique(reducedBinnedWeightedMileage)
      reducedBinnedWeightedMileage[, test1 := sum(binWeightedShareEUR)]
      reducedBinnedWeightedMileage[, test2 := sum(binWeightedVehShareEUR)]
      if ((!all(abs(reducedBinnedWeightedMileage$test1 - 1) < 1e-6)) | (!all(abs(reducedBinnedWeightedMileage$test2 - 1) < 1e-6))) {
        stop("Share in tot vehicles of mileage bins are wrong")}
      reducedBinnedWeightedMileage[, c("test1", "test2") := NULL]}
    return(reducedBinnedWeightedMileage)
}

loadTCO <- function(dataFolder) {
  TCO <- fread(file.path(dataFolder, "TCOanalysis", "TCO.csv"))
  TCO[, TCOscenario := paste0(vehicleParameterScenario, "x", energyCarrierParameterScenario)]
  cols <- names(TCO)[!names(TCO)%in% c("value", "country")]
  TCOEUR <- merge(TCO, loadWeight(dataFolder, "stock"), by = "country", allow.cartesian = TRUE)
  TCOEUR <- TCOEUR[, .(value = sum(value * meanStockShare)), by = cols][, country := "EUR"]
  TCO <- rbind(TCO, TCOEUR)
  return(TCO)
}

getDCO <- function(DCOscenarios, TCO, keep = FALSE) {

  dt <- copy(TCO)
  dt[, c("vehicleParameterScenario", "energyCarrierParameterScenario", "energyCarrier") := NULL]
  # get rid of parameter differentiation
  dt <- dt[, .(value = sum(value)), by = c("country", "truckClass", "truckTechnology", "TCOscenario", "paperScen", "unit", "period")]
  
  # merge truck side
  truckSide <- merge(
    DCOscenarios,
    dt,
    by.x = c("truckTech", "truckTCOscenario"), # adjust if needed
    by.y = c("truckTechnology", "TCOscenario"),
    all.x = TRUE,
    all.y = FALSE
  )
  
  # merge countertech side
  counterSide <- merge(
    DCOscenarios,
    dt,
    by.x = c("counterTech", "counterTechTCOscenario"),
    by.y = c("truckTechnology", "TCOscenario"),
    all.x = TRUE,
    all.y = FALSE
  )
  
  mergeCols <- intersect(names(truckSide), names(counterSide))
  mergeCols <- mergeCols[!mergeCols == "value"]
  
  # now combine both sides into one DCO table
  DCO <- merge(
    truckSide,
    counterSide,
    by = eval(mergeCols),
    suffixes = c("_truck", "_counter")
  )
  
  if (keep == FALSE) DCO[, value:=  value_counter - value_truck][, c("value_counter", "value_truck") := NULL]
  if (keep == TRUE) DCO[, dco:=  value_counter - value_truck]
  setnames(DCO, "truckTech", "truckTechnology")
  return(DCO)
}

applyMileage <- function(annualMileage, dt) {
  #merge with annual mileage 
  if (is.data.table(annualMileage) == TRUE) dt <- merge(dt, annualMileage, by = intersect(names(dt), names(annualMileage)), allow.cartesian = TRUE)
  else if (length(annualMileage) == 1)  dt[, annualM := annualMileage]
  
  dt[unit == "EUR/veh yr", value := value/annualM]
  dt[unit == "EUR/veh yr", unit := "EUR/vehkm"]
  #Sum up after unifying the unit
  byCols <- names(dt)
  byCols <- byCols[!byCols == "value"]
  dt <- dt[, .(value = sum(value)), by = eval(byCols)]
}

calculateFreightActivityShares <- function(dt, focus = "EUR") {

  if (focus == "EUR") {
    setorder(dt, truckTechnology, paperScen, DCOscenario, period, -value)
    dt[, cumBinWeightedShareEUR := cumsum(binWeightedShareEUR), by = c("truckTechnology", "paperScen", "DCOscenario", "period")]
    dt[, cumBinWeightedVehShareEUR := cumsum(binWeightedVehShareEUR), by = c("truckTechnology", "paperScen", "DCOscenario", "period")]  
  }
      
  if (focus == "Country") {
    setorder(dt, truckTechnology, paperScen, DCOscenario, period, country, -value)
    dt[, cumBinShareCountry := cumsum(binShareCountry), by = c("truckTechnology", "DCOscenario", "paperScen", "period", "country")]
    dt[, cumBinVehshareCountry := cumsum(binVehshareCountry), by = c("truckTechnology", "DCOscenario", "paperScen", "period", "country")]
  }
  return(dt)
}

#- CO2 price to bridge DCO---------------------------------------------------------------------------------------------------------------------
#Co2 Intensities do not vary by paperScen
calculateSwitchCO2price <- function(DCO, dataFolder) {
  co2Intensities <- fread(file.path(dataFolder, "TCOanalysis", "co2IntensityDieFuel.csv"))[, unit := NULL] # tCO2/km
  setnames(co2Intensities, "value", "co2int")
  if (!unique(DCO$counterTech) == "ICET") stop("You need to transfer DCO with ICET as counter technology")
  DCO[, energyCarrierParameterScenario := gsub("^.*x", "", counterTechTCOscenario)]
  gapCO2Price <- merge(DCO, co2Intensities, by = intersect(names(DCO), names(co2Intensities)), allow.cartesian = TRUE)
  gapCO2Price[, Co2PriceToCloseGap := value/co2int][, unit := "EUR/tCO2"] # EUR/vehkm  / tCO2/vehkm -> [EUR/tCO2]
  gapCO2Price[, energyCarrierParameterScenario := NULL]
  setorder(gapCO2Price, "truckTechnology", "energyCarrier", "period", "paperScen", "DCOscenario", "Co2PriceToCloseGap")
  gapCO2Price[, cumBinWeightedShareEUR := cumsum(binWeightedShareEUR), by = c("truckTechnology", "energyCarrier", "paperScen", "DCOscenario", "period")]
  return(gapCO2Price)
}
#- Annual mileage breakeven ---------------------------------------------------------------------------------------------------------------------

caluclateAnnualMileageBreakeven <- function(DCO) {

  CAPEX <- DCO[unit == "EUR/veh yr"]
  CAPEX[, c("unit", "dco") := NULL]
  setnames(CAPEX, c("value_truck", "value_counter"), c("capex_truck", "capex_counter"))
  
  OPEX <- DCO[!unit == "EUR/veh yr"]
  OPEX[, c("unit", "dco") := NULL]
  setnames(OPEX, c("value_truck", "value_counter"), c("opex_truck", "opex_counter"))
  
  breakeven <- merge(CAPEX, OPEX, by = intersect(names(CAPEX), names(OPEX)))
  breakeven[, AMb := (capex_truck - capex_counter)/(opex_counter - opex_truck)]
  #Negative AMb means no breakeven
  breakeven <- breakeven[AMb >= 0]
  breakeven[, capexDiff := capex_truck - capex_counter]
  breakeven[, opexDiff := opex_counter - opex_truck]
  breakeven <- breakeven[AMb < 300000]
  return(breakeven)
}

caluclateSyntheticalBreakeven <- function() {
  # Step 1: Create a synthetic grid of values
  syntheticalBreakeven <- CJ(
    capexDiff = seq(500, 29500, by = 100),      # Y-axis: delta CAPEX (€/veh yr)
    opexDiff = seq(0.01, 0.6, by = 0.01)     # X-axis: delta OPEX (€/km) - avoid div by 0
  )
  
  # Step 2: Calculate synthetic breakeven mileage
  syntheticalBreakeven[, breakeven := capexDiff / opexDiff]
  syntheticalBreakeven <- syntheticalBreakeven[breakeven < 300000]
  return(syntheticalBreakeven)
}

#- TCO overview data-------------------------------------------------------------------------------------------------------------------------
loadFuelPrices <- function(dataFolder) {
  fuelPricesEURperkwh <- fread(file.path(dataFolder, "TCOanalysis", "fuelPricekWh.csv"))
  return(fuelPricesEURperkwh)
}
loadSalesPrices <- function(dataFolder) {
  salesPrices <- fread(file.path(dataFolder, "TCOanalysis", "SalesPrices.csv"))
  salesPrices <- groupParameters(salesPrices)
  return(salesPrices)
}
loadBatteryAndFCPrices <- function(dataFolder) {
  rawBatFCPrices <- fread(file.path(dataFolder, "TCOparameter", "vehicleParameters.csv"))
  rawBatFCPrices <- rawBatFCPrices[parameter %in% c("Fuel cell system costs", "Battery costs")]
  return(rawBatFCPrices)
}

calulcateTCOperMileage <- function(TCO) {
  dummymileage<- data.table(AM = c(seq(1, 100000, length.out = 201)))
  dummymileage[, dummy := "All"]
  TCOPerMileage <- copy(TCOdata)
  TCOdata <- merge(TCOdata, AnnualMileage, by = c("truckClass"), allow.cartesian = TRUE)
  TCOdata[unit == "EUR/veh yr", value := value/AM][, AM := NULL]
  TCOdata[unit == "EUR/veh yr", unit := "EUR/km"]
  TCOPerMileage[, dummy := "All"]
  TCOPerMileage <- merge(TCOPerMileage, dummymileage, by = "dummy", allow.cartesian = TRUE)[, dummy := NULL]
  TCOPerMileage[unit == "EUR/veh yr", value := value/AM]
  TCOPerMileage[unit == "EUR/veh yr", unit := "EUR/km"]
  return(TCOPerMileage)
}

aggregateCountryData <- function(dt, weightType = "stock") {
 cols <- names(dt)[!names(dt)%in% c("value", "country")]
 dt <- merge(dt, loadWeight(dataFolder, eval(weightType)), by = "country", allow.cartesian = TRUE)
 if (weightType == "stock") {
   dt[, value := value * meanStockShare]
   dt <- dt[, .(value = sum(value)), by = cols]}
 if (weightType == "activity") {
   dt[, value := value * meanESdemandShare]
   dt <- dt[, .(value = sum(value)), by = cols]}
   dt[, country := "EUR"]
 return(dt)
}

checkFeasMileageDistribution <- function(DCOmilageDistribution, binnedWeightedMileage, reducedBinnedWeightedMileage, ranges, infrastructure, focus = "EUR") {

  ranges <- copy(ranges)
  infrastructure <- copy(infrastructure)
  # Ranges of considered BET variants
  setnames(ranges, "value", "drivingRange")
  # Infrastructure build-up
  setnames(infrastructure, "value", "MCSavailability")
  
  # First caluclate feas shares using detailed mileage profiles
  # mileage bins x 3 vehicle parameter scenarios x 2 truck technologies x 5 years
  feasibilityCheck <- merge(binnedWeightedMileage, ranges[period %in% c(2030, 2035, 2040, 2045, 2050)], by = c("truckClass"), allow.cartesian = TRUE)
  feasibilityCheck[, effectiveRangeMCS := 2 * drivingRange]
  feasibilityCheck[, feas := 0]
  # Collect all profiles that are feasible (assuming 100% MCS buildup)
  feasibilityCheck[effectiveRangeMCS > dvktMax, feas := 1]
  feasibilityCheck <- merge(feasibilityCheck, infrastructure[period %in% c(2030, 2035, 2040, 2045, 2050)], by = c("period"), allow.cartesian = TRUE)
  # Calculate total feasible yearly road km for each bin
  feasSums <- copy(feasibilityCheck)
  # Zero share for non-feasible profiles
  feasSums[feas == 0, feasWeightedShareEUR := 0]
  feasSums[feas == 0, feasShareCountry := 0]
  feasSums[feas == 1, feasWeightedShareEUR := weightedShareEUR]
  feasSums[feas == 1, feasShareCountry := shareCountry]
  # Correct share in tot road km if the feasibility relies on MCS charging according to infrastructure availability 
  feasSums[feas == 1 & dvktMax > drivingRange, feasWeightedShareEUR := weightedShareEUR * MCSavailability]
  feasSums[feas == 1 & dvktMax > drivingRange, feasShareCountry := shareCountry * MCSavailability]
  
  # Sum up for whole bin
  feasSums <- feasSums[, .(binFeasWeightedShareEUR = sum(feasWeightedShareEUR),
                           binFeasShareCountry = sum(feasShareCountry)
                           ), by = .(country, truckClass, avktBinned, period, vehicleParameterScenario, truckTechnology)]
  
  # Merge back to reduced Mileage to not lose not feasible profiles
  reducedBinnedWeightedMileageFeasibility <- merge(
     reducedBinnedWeightedMileage, 
     feasSums, 
     by = c("country", "truckClass", "avktBinned"),
     all.x = TRUE
   )
  
  # merge back to DCO
  mergerDCO <- DCOmilageDistribution[truckTechnology %in% c("BET large battery", "BET small battery")][, c("country", "truckClass", "period", "DCOscenario", "truckTCOscenario", "truckTechnology", "avktBinned", "paperScen", "value")]
  mergerDCO[, vehicleParameterScenario := gsub("x.*", "", truckTCOscenario)]
  feasDCO <- merge(reducedBinnedWeightedMileageFeasibility, mergerDCO, by = intersect(names(reducedBinnedWeightedMileageFeasibility), names(mergerDCO)), allow.cartesian = TRUE)
  if (focus == "Country") {
    setorder(feasDCO, truckTechnology, paperScen, DCOscenario, country, period, -value)
    feasDCO[, cumFeasshareCountry := cumsum(binFeasShareCountry) , by = c("truckTechnology", "paperScen", "DCOscenario", "country", "period")]
  } else if (focus == "EUR") {
    setorder(feasDCO, truckTechnology, paperScen, DCOscenario, period, -value)
    feasDCO[, cumFeasWeightedShareEUR := cumsum(binFeasWeightedShareEUR), by = c("truckTechnology", "paperScen", "DCOscenario", "period")]
  }
  return(feasDCO)
}


##- Feasibility for range analysis----------------------------

getRangeAnalysis <- function(weightedMileage, infrastructure) {
  
  setnames(infrastructure, "value", "MCSavailability")
  # When it comes to the real-world mileage profiles solely, the vehicle size they are currently performed with is not relevant
  rangeAnalysis <- weightedMileage[, c("V1", "country", "dvktMax", "weightedShareEUR", "shareCountry")]
  setorder(rangeAnalysis, "dvktMax")
  # get rid of doubly entries for dvktMax
  rangeAnalysis <- rangeAnalysis[, .(weightedShareEUR = sum(weightedShareEUR)), by = .(dvktMax)]
  # sum(rangeAnalysis$weightedShareEUR) equals 1
  # Mileagedistribution stays constant over time 
  rangeAnalysis <- rbindlist(
    lapply(unique(infrastructure$period), function(p) {
      tmp <- copy(rangeAnalysis)
      tmp[, period := p]
      tmp
    })
  )
  infrastructure[, period := as.integer(period)]
  rangeAnalysis <- merge(rangeAnalysis, infrastructure, by = c("period"), all.x = TRUE, allow.cartesian = TRUE)
  setorder(rangeAnalysis, "period", "dvktMax")
  # dummy direct range
  directRange <- data.table(directRange = c(seq(150, 1000, 10)))[, all := "All"]
  dummy <- unique(rangeAnalysis[, c("period")])[, all := "All"]
  directRange <- merge(directRange, dummy, by = "all", allow.cartesian = TRUE)[, all := NULL]
  rangeAnalysis <- merge(rangeAnalysis, directRange, by = c("period"), allow.cartesian = TRUE)
  rangeAnalysis[, MCSrange := 2 * directRange]
  
  # direct feasibility
  rangeAnalysis[directRange > dvktMax, directFeasibility := 1]
  rangeAnalysis[directRange > dvktMax, feasAvktShareEURperMaxdaily := weightedShareEUR]
  # MCS feasibility
  rangeAnalysisNoMCS <- rangeAnalysis[directFeasibility == 1, .(shareFeas = sum(feasAvktShareEURperMaxdaily)), by = c("period", "directRange")]
  rangeAnalysisNoMCS <- rangeAnalysisNoMCS[, c("period") := NULL]
  rangeAnalysisNoMCS <- unique(rangeAnalysisNoMCS)
  rangeAnalysis[directRange < dvktMax & MCSrange > dvktMax, MCSFeasibility := 1]
  rangeAnalysisfullMCS <- rangeAnalysis[(directFeasibility == 1 | MCSFeasibility == 1), .(shareFeas = sum(weightedShareEUR)), by = c("period", "directRange")]
  rangeAnalysisfullMCS <- rangeAnalysisfullMCS[, c("period") := NULL]
  rangeAnalysisfullMCS <- unique(rangeAnalysisfullMCS)
  rangeAnalysis[directRange < dvktMax & MCSrange > dvktMax, feasAvktShareEURperMaxdaily := weightedShareEUR * MCSavailability]
  rangeAnalysis[is.na(feasAvktShareEURperMaxdaily), feasAvktShareEURperMaxdaily := 0]
  
  rangeAnalysis <- rangeAnalysis[, .(shareFeas = sum(feasAvktShareEURperMaxdaily)), by = c("period", "directRange")]
  rangeAnalysis[, cumShareFeas := cumsum(shareFeas), by = c("period", "directRange")]
  rangeAnalysisNoMCS[, cumShareFeas := cumsum(shareFeas), by = c("directRange")]
  rangeAnalysisfullMCS[, cumShareFeas := cumsum(shareFeas), by = c("directRange")]
  return(list(buildUp = rangeAnalysis,
              noMCS = rangeAnalysisNoMCS,
              fullMCS = rangeAnalysisfullMCS)
  )
}

groupParameters <- function(dt){
  dt[parameter %in% 
             c("Glider/Vehicle body invest without drivetrain",
               "Electric motor costs", 
               "Power electronics costs",
               "Emission Aftertreatment",
               "Resale value", 
               "Diesel engine costs (CI engine)"),  
           parameter := "Vehicle body\nincl. engine"]
  
  dt[parameter %in% c( "Wholesale", 
                             "Taxes, Fees, Levies and charges",
                             "Excise Duty", 
                             "Station", 
                             "Transport and distribution", 
                             "Station Overhead, Profit and Mark-Up",
                             "Total Diesel Mark-Up",
                             "Network Costs",
                             "Grid extension",
                             "Network fees savings potential"), 
           parameter := "Fuel cost"]
  
  
  cols <- names(dt)[!names(dt) == "value"]
  dt <- dt[, .(value = sum(as.numeric(value))), 
                       by = cols]
  dt[parameter == "Vehicle Tax", parameter := "Vehicle tax"]
  neworder <- c("CO2 tax", "Toll charge", "Vehicle tax", "Fuel cost", 
                "M&R + Tires", "H2 tank", "Fuel cell system", "Battery", "Vehicle body\nincl. engine")
  valid_neworder <- intersect(neworder, unique(dt$parameter))
  
  droplevels(dt)
  dt[, parameter := factor(parameter, 
                                 levels = valid_neworder)]
  return(dt)
}

