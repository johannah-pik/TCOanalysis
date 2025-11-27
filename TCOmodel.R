rm(list=ls())
library(data.table)

#-Raw data-------------------------------------------------------------------------------------------------------------------------------------------------------------------
vehicleParameters <- fread(file.path("data", "TCOparameter", "vehicleParameters.csv"))
energyCarrierParameters <- fread(file.path("data", "TCOparameter", "energyCarrierParameters.csv"))
# Remove underlying assumptions on bandwith that are not needed for the calculation
energyCarrierParameters <- energyCarrierParameters[!parameter %in% c("Bandwith MCS", "Bandwith Depot")]
energyCarrierParameters[, value := as.numeric(value)]

#-Constants-------------------------------------------------------------------------------------------------------------------------------------------------------------------
Co2IntDie <-  268.20 #gCO2e/kWh
kWhProKgH2 <- 33.33 #kWh/kg

#-CAPEX-------------------------------------------------------------------------------------------------------------------------------------------------------------------
colsToKeep <- c("period", "country", "truckTechnology", "truckClass", "vehicleParameterScenario", "paperScen", "value")
colsToDelete <- names(vehicleParameters)
colsToDelete <- colsToDelete[!colsToDelete %in% colsToKeep]

##-Annuity factor-------------------------------------------------------------------------------------------------------------------------------------------------------------------
InRa <- unique(vehicleParameters[parameter == "Interest Rate"][, eval(colsToDelete) := NULL])
setnames(InRa, "value", "InRa")
SeLi <- unique(vehicleParameters[parameter == "Service Life"][, eval(colsToDelete) := NULL])
setnames(SeLi, "value", "SeLi")
annuityFactor <- merge(InRa, SeLi, by = intersect(names(InRa), names(SeLi)))
annuityFactor[, anF := ((1+as.numeric(InRa))^as.numeric(SeLi) * as.numeric(InRa))/((1+as.numeric(InRa))^as.numeric(SeLi)-1)][, c("InRa", "SeLi") := NULL]

##-Drivetrain-------------------------------------------------------------------------------------------------------------------------------------------------------------------
Drivetrain <- unique(vehicleParameters[parameter == "Glider/Vehicle body invest without drivetrain"][, eval(colsToDelete[!colsToDelete %in% c("parameter", "unit")]) := NULL])

##-Engine-------------------------------------------------------------------------------------------------------------------------------------------------------------------
RatedPower <- unique(vehicleParameters[parameter == "Rated Power"][, eval(colsToDelete) := NULL]) #[kw]
setnames(RatedPower, "value", "RatedPower")
EngineCost <- vehicleParameters[parameter %in% c("Diesel engine costs (CI engine)", "Emission Aftertreatment", "Electric motor costs", "Power electronics costs")] #[EUR/kw]
EngineCost <- unique(EngineCost[, eval(colsToDelete[colsToDelete!="parameter"]) := NULL])
Engine <- merge(EngineCost, RatedPower, by = intersect(names(EngineCost), names(RatedPower)))
Engine[, value := as.numeric(value) * as.numeric(RatedPower)][, RatedPower := NULL][, unit := "EUR/veh"]

##-Battery------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
#Comment: (overdimensioning already included)
BatCap <- unique(vehicleParameters[parameter == "Battery capacity"][, eval(colsToDelete) := NULL])#[kwh]
setnames(BatCap, "value", "BatCap")
BatCost <- unique(vehicleParameters[parameter == "Battery costs"][, eval(colsToDelete) := NULL]) #[EUR/kWh]
Battery <- merge(BatCost, BatCap, by = intersect(names(BatCost), names(BatCap)))
Battery[, value := as.numeric(value)*as.numeric(BatCap)][, BatCap := NULL][, unit := "EUR/veh"][, parameter := "Battery"]

##-Fuel Cell System---------------------------------------------------------------------------------------------------------------------------
FCPower <- unique(vehicleParameters[parameter == "Fuel cell power"][, eval(colsToDelete) := NULL])
setnames(FCPower, "value", "FCPower")
FCCost <- unique(vehicleParameters[parameter == "Fuel cell system costs"][, eval(colsToDelete) := NULL])
FCSystem <- merge(FCCost, FCPower, by = intersect(names(FCCost), names(FCPower)))
FCSystem[, value := as.numeric(value)*as.numeric(FCPower)][, FCPower := NULL][, unit := "EUR/veh"][, parameter := "Fuel cell system"]

##-H2 Tank-------------------------------------------------------------------------------------------------------------------------------------------------------------------
TankCap <- unique(vehicleParameters[parameter == "Hydrogen Tank Capacity"][, eval(colsToDelete) := NULL])
setnames(TankCap, "value", "TankCap")#unit kg H2
TankCost <- vehicleParameters[parameter == "CH2 Hydrogen Tank Cost" & `truckTechnology` == "FCET" & unit == "EUR/kg H2"]
TankCost <- unique(TankCost[, eval(colsToDelete) := NULL])
H2Tank <- merge(TankCost, TankCap, by = intersect(names(TankCost), names(TankCap)))
H2Tank[, value := as.numeric(value)*as.numeric(TankCap)][, TankCap := NULL][, unit := "EUR/veh"][, parameter := "H2 tank"]

##-CAPEX specific markup-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Comment: Markup is given in percentage and is applied on full vehicle costs -> Check with ISI
CAPEX <- rbind(Drivetrain, Engine, Battery, FCSystem, H2Tank)
markUp <- unique(vehicleParameters[parameter == "Mark-Up and Profit"][, eval(colsToDelete) := NULL])
setnames(markUp, "value", "markUp")
CAPEX <- merge(CAPEX, markUp, by = intersect(names(CAPEX), names(markUp)))
CAPEX[!parameter == "Glider/Vehicle body invest without drivetrain", value := as.numeric(value) * as.numeric(markUp)][, markUp := NULL]

##-Resale Value-------------------------------------------------------------------------------------------------------------------------------------------------------------------
ReVa <- unique(vehicleParameters[parameter == "Resale Value"][, eval(colsToDelete) := NULL])
setnames(ReVa, "value", "ReVa")
totCap <- CAPEX[, .(totCap = sum(as.numeric(value))), by = eval(colsToKeep[!colsToKeep == "value"])]
ReVa <- merge(totCap, ReVa, by = intersect(names(totCap), names(ReVa)))
ReVa[, value := as.numeric(totCap)*as.numeric(ReVa)*(-1)][, c("ReVa", "totCap"):= NULL][, unit := "EUR/veh"]
ReVa <- merge(ReVa, InRa, by = intersect(names(ReVa), names(InRa)))
ReVa <- merge(ReVa, SeLi, by = intersect(names(ReVa), names(SeLi)))
#Discount Resale Value
ReVa[, value := as.numeric(value)/(1+as.numeric(InRa))^as.numeric(SeLi)][, c("InRa", "SeLi") := NULL][, parameter := "Resale value"]
CAPEX <- rbind(CAPEX, ReVa)

##-Sales Price and annualized CAPEX-------------------------------------------------------------------------------------------------------------------------------------------------------------------
SalesPrice <- CAPEX[!parameter == "Resale Value"]
#Calc annualized values [EUR/veh]
CAPEX <- merge(CAPEX, annuityFactor, by = intersect(names(CAPEX), names(annuityFactor)))
CAPEX[, value := as.numeric(value) * as.numeric(anF)][, anF := NULL][, unit := "EUR/veh yr"]

#-OPEX---------------------------------------------------------------------------------------------------------------------

##-Vehicle efficiency-------------------------------------------------------------------------------------------------------------------------------------------------------------------
efficiency <- unique(vehicleParameters[parameter == "Specific Energy Consumption"][, eval(colsToDelete[!colsToDelete %in% c("parameter", "unit")]) := NULL])
efficiency[, value := as.numeric(value)*10^-2][, unit := "kWh/km"]

##-BET Charging losses-------------------------------------------------------------------------------------------------------------------------------------------------------------------
ChargingLosses <- unique(vehicleParameters[parameter == "Charging Losses"][, eval(colsToDelete[!colsToDelete %in% c("parameter", "unit")]) := NULL])
ChargingLosses <- ChargingLosses[`truckTechnology` %in% c("BET large battery", "BET small battery")]

##-Maintenance and repair costs-------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Comment: Varied by "Vehicle-Cost-Scenario"
OMOther <- vehicleParameters[parameter == "M&R + Tires specific costs" & !`truckTechnology` %in% c("ICET")]
OMOther <- unique(OMOther[, eval(colsToDelete) := NULL])
OMCostsDie <- vehicleParameters[parameter == "M&R + Tires specific costs" & `truckTechnology` %in% c("ICET")]
OMCostsDie <- unique(OMCostsDie[, eval(colsToDelete) := NULL][, `truckTechnology` := NULL])
setnames(OMCostsDie, "value", "OMDie")
OMOther <- merge(OMOther, OMCostsDie, by = intersect(names(OMOther), names(OMCostsDie)))
OMOther[, value := as.numeric(value)*as.numeric(OMDie)][, OMDie := NULL]
OMCosts <- rbind(OMOther, 
                 unique(vehicleParameters[parameter == "M&R + Tires specific costs" 
                                     & `truckTechnology` %in% c("ICET")][, eval(colsToDelete) := NULL]))
OMCosts[, unit := "EUR/vehkm"][, parameter := "M&R + tires"]

##-Taxes per vehicle per year-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Comment: Varied by country (not by scenarios)
vehTax <- unique(vehicleParameters[parameter == "Vehicle Tax"][, eval(colsToDelete[!colsToDelete %in% c("parameter", "unit")]) := NULL])
vehTax[, unit := "EUR/veh yr"] # [EUR/a]

##-Road Tolls-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Comment: Varied by country (not by scenarios)
Tolls <- unique(vehicleParameters[parameter == "Toll charge"][, eval(colsToDelete[!colsToDelete %in% c("parameter", "unit")]) := NULL])
TollRed <- unique(vehicleParameters[parameter == "Toll Reduction"][, eval(colsToDelete) := NULL])# [%]
setnames(TollRed, "value", "TollRed")
Tolls <- merge(Tolls, TollRed, by = intersect(names(Tolls), names(TollRed)), all.x = TRUE)
Tolls[is.na(TollRed), TollRed := 0]
TollShare <- unique(vehicleParameters[parameter == "Share Road Toll"][, eval(colsToDelete) := NULL])# [%]
setnames(TollShare, "value", "TollShare")
Tolls <- merge(Tolls, TollShare, by = intersect(names(Tolls), names(TollShare)), all.x = TRUE)
Tolls[, value := (as.numeric(value) + as.numeric(value)*as.numeric(TollRed))*as.numeric(TollShare)][, unit := "EUR/vehkm"][, c("TollShare", "TollRed") := NULL]


##-Fuel cost in EUR/kWh-------------------------------------------------------------------------------------------------------------------------------------------------------------------
###-Diesel -------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Comment:Conventional Diesel blend and hypothetical pure fossil/bio/syn diesel ICE truck
  
  # Fuel related parameters are not varied by vehicle-cost-scenario, but by "energyCarrierParameterScenario"
  colsToKeep <- c("period", "country", "energyCarrierParameterScenario", "paperScen", "energyCarrier", "value")
  colsToDelete <- names(energyCarrierParameters)
  colsToDelete <- colsToDelete[!colsToDelete %in% colsToKeep]  

  # Wholesale 
  wholeSaleDieselPrices <- unique(energyCarrierParameters[parameter %in% c("Pt-Diesel Price", "Diesel Price", "Biodiesel Price")][,  eval(colsToDelete[colsToDelete!="parameter"]) := NULL])  
  wholeSaleDieselPrices[parameter == "Biodiesel Price", energyCarrier := "Diesel bio"]
  wholeSaleDieselPrices[parameter == "Pt-Diesel Price", energyCarrier := "Diesel syn"]
  wholeSaleDieselPrices[parameter == "Diesel Price", energyCarrier := "Diesel fossil"][, parameter := NULL]
  dieselBlendparameter <- c("Share Biodiesel", "Share Synthetic Diesel (E-Fuels)") 
  dieselShares <- copy(energyCarrierParameters[parameter %in% dieselBlendparameter])
  dieselShares <- unique(dieselShares[, eval(colsToDelete[colsToDelete!="parameter"]) := NULL])
  dieselShares <- dcast(dieselShares, period + country + `energyCarrierParameterScenario` + paperScen ~ parameter, value.var = "value")
  LiqMixPrice <- unique(energyCarrierParameters[parameter %in% c("Pt-Diesel Price", "Diesel Price", "Biodiesel Price")][,  eval(colsToDelete[colsToDelete!="parameter"]) := NULL])  
  LiqMixPrice <- dcast(LiqMixPrice,  period + country + `energyCarrierParameterScenario` + paperScen ~ parameter, value.var = "value")
  LiqMixPrice <- merge(LiqMixPrice, dieselShares, by = intersect(names(LiqMixPrice), names(dieselShares)))
  LiqMixPrice <- LiqMixPrice[, value := as.numeric(`Pt-Diesel Price`)* as.numeric(`Share Synthetic Diesel (E-Fuels)`) + as.numeric(`Biodiesel Price`)* as.numeric(`Share Biodiesel`)  + as.numeric(`Diesel Price`)* (1-as.numeric(`Share Synthetic Diesel (E-Fuels)`)-as.numeric(`Share Biodiesel`))]
  LiqMixPrice <- LiqMixPrice[, c("country", "energyCarrierParameterScenario", "paperScen", "period", "value")]
  LiqMixPrice[, energyCarrier := "Diesel mix"]
  wholeSaleDieselPrices <- rbind(wholeSaleDieselPrices, LiqMixPrice)[, parameter := "Wholesale"][, unit := "EUR/kWh"]
  
  #CO2 tax 
  Co2IntensityDieFuel <- unique(energyCarrierParameters[parameter %in% c("CO2 emission factor Biodiesel", "CO2 emission factor Pt-Diesel")][,  eval(colsToDelete[colsToDelete!="parameter"]) := NULL])
  Co2IntensityDieFuel <- dcast(Co2IntensityDieFuel, period + country + `energyCarrierParameterScenario`+ paperScen ~ parameter, value.var = "value")
  Co2IntensityDieFuel[, `CO2 Intensity fossil diesel` :=  Co2IntDie * 10^-6]#tCO2e/kWh
  Co2IntensityDieFuel[, `CO2 Intensity Biodiesel` := as.numeric(`CO2 emission factor Biodiesel`)*as.numeric(`CO2 Intensity fossil diesel`)]
  Co2IntensityDieFuel[, `CO2 Intensity Pt-Diesel` := as.numeric(`CO2 emission factor Pt-Diesel`)*as.numeric(`CO2 Intensity fossil diesel`)]
  Co2IntensityDieFuel[, c("CO2 emission factor Biodiesel","CO2 emission factor Pt-Diesel") := NULL][, unit := "EUR/kwh"]
  Co2IntensityDieFuel <- merge(Co2IntensityDieFuel, dieselShares, by = intersect(names(Co2IntensityDieFuel), names(dieselShares)))
  Co2IntensityDieFuel[, `CO2 intensity diesel mix` := as.numeric(`CO2 Intensity Pt-Diesel`)* as.numeric(`Share Synthetic Diesel (E-Fuels)`) 
                  + as.numeric(`CO2 Intensity Biodiesel`)* as.numeric(`Share Biodiesel`)  
                  + as.numeric(`CO2 Intensity fossil diesel`)* (1-as.numeric(`Share Synthetic Diesel (E-Fuels)`)-as.numeric(`Share Biodiesel`))]#tCO2e/kWh
  Co2IntensityDieFuel[, c("Share Synthetic Diesel (E-Fuels)", "Share Biodiesel") := NULL]
  Co2IntensityDieFuel <- melt(Co2IntensityDieFuel, id.vars = c("period", "country", 
                                                               "energyCarrierParameterScenario", "paperScen", "unit"))
  setnames(Co2IntensityDieFuel, "variable", "parameter")
  Co2IntensityDieFuel[parameter == "CO2 intensity diesel mix", energyCarrier := "Diesel mix"]
  Co2IntensityDieFuel[parameter == "CO2 Intensity Biodiesel", energyCarrier := "Diesel bio"]
  Co2IntensityDieFuel[parameter == "CO2 Intensity Pt-Diesel", energyCarrier := "Diesel syn"]
  Co2IntensityDieFuel[parameter == "CO2 Intensity fossil diesel", energyCarrier := "Diesel fossil"][, parameter := "CO2 intensity diesel"][, unit := "tCO2e/kWh"]
  
  NonETSCO2Prices <- unique(energyCarrierParameters[parameter == "Non-ETS CO2"][, c(eval(colsToDelete), "energyCarrier") := NULL])#EUR/tCO2
  setnames(NonETSCO2Prices, "value", "CO2Price")
  CO2Tax <- copy(Co2IntensityDieFuel)#tCO2e/kWh
  CO2Tax <- merge(CO2Tax, NonETSCO2Prices, by = intersect(names(CO2Tax), names(NonETSCO2Prices)))
  CO2Tax[, value := value * CO2Price]
  CO2Tax[, CO2Price := NULL][, unit := "EUR/kWh"][, parameter := "CO2 tax"]
  
  #Diesel Tax
  fossDieselTax <- unique(energyCarrierParameters[parameter %in% c("Excise Duty - Diesel")][, eval(colsToDelete) := NULL]) # EUR/kWH
  fossDieselTaxmerge <- copy(fossDieselTax)
  setnames(fossDieselTaxmerge, "value", "fossDieselTax")
  fossDieselTax[, energyCarrier := "Diesel fossil"]
  
  bioDieselTax <- unique(energyCarrierParameters[parameter %in% c("Excise Duty - Reduction biodiesel")][, eval(colsToDelete) := NULL]) # % of fossil diesel exise duty
  bioDieselTax <- merge(bioDieselTax, fossDieselTaxmerge, by = intersect(names(bioDieselTax), names(fossDieselTaxmerge)))
  bioDieselTax[, value := as.numeric(fossDieselTax) * (1 - as.numeric(value))][, fossDieselTax := NULL]
  bioDieselTax[, energyCarrier := "Diesel bio"]
  
  synDieselTax <- unique(energyCarrierParameters[parameter %in% c("Excise Duty - Reduction Synthetic Diesel (E-Fuels)")][, eval(colsToDelete) := NULL])# % of fossil diesel exise duty
  synDieselTax <- merge(synDieselTax, fossDieselTaxmerge, by = intersect(names(synDieselTax), names(fossDieselTaxmerge)))
  synDieselTax[, value := as.numeric(fossDieselTax) * (1 - as.numeric(value))][, fossDieselTax := NULL]
  synDieselTax[, energyCarrier := "Diesel syn"]
  
  DieselTax <- rbind(fossDieselTax, bioDieselTax, synDieselTax)
  TaxDieMix <- copy(DieselTax)
  TaxDieMix <- dcast(TaxDieMix, period + country + `energyCarrierParameterScenario`+ paperScen ~ energyCarrier, value.var = "value")
  TaxDieMix <- merge(TaxDieMix, dieselShares, by = intersect(names(TaxDieMix), names(dieselShares)))
  TaxDieMix[, value := (1 - as.numeric(`Share Synthetic Diesel (E-Fuels)`) - as.numeric(`Share Biodiesel`)) *as.numeric(`Diesel fossil`)
                        + as.numeric(`Share Biodiesel`) * as.numeric(`Diesel bio`)
                        + as.numeric(`Share Synthetic Diesel (E-Fuels)`) * as.numeric(`Diesel syn`)]
  TaxDieMix[, energyCarrier := "Diesel mix"]
  TaxDieMix <- TaxDieMix[, c("country", "energyCarrier", "energyCarrierParameterScenario", "paperScen", "period", "value")]
  DieselTax <- rbind(DieselTax, TaxDieMix)[, parameter := "Excise Duty"][, unit := "EUR/kWh"]
  
  #Diesel Refueling Station markup (covers refueling station markup, WACC, inner-country transport, storage, admin, distribution, and stock)
  #-> Check ISI: Refueling station cost iself is not considered. Was this done on purpose? (They are all there already?)
  DRSCost <- unique(energyCarrierParameters[parameter == "Total Diesel Mark-Up"][, eval(colsToDelete[!colsToDelete %in% c("truckTechnology", "unit", "parameter")]) := NULL])
  DRSCostDieMix <- DRSCost[, energyCarrier := "Diesel mix"]
  DRSCost <- rbind(DRSCostDieMix, copy(DRSCostDieMix)[, energyCarrier := "Diesel fossil"], copy(DRSCostDieMix)[, energyCarrier := "Diesel bio"], copy(DRSCostDieMix)[, energyCarrier := "Diesel syn"])
  
  dieselAtPumpkWh <- rbind(wholeSaleDieselPrices, CO2Tax, DieselTax, DRSCost)
  
###-Electricity -------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #Wholesale
  elecWholeSalePrice <- unique(energyCarrierParameters[parameter == "Electricity Price"][, eval(colsToDelete[!colsToDelete %in% c("unit")]) := NULL])
  elecWholeSalePrice[, parameter := "Wholesale"]
  MCS <- copy(elecWholeSalePrice)[, energyCarrier := "MCS"]
  depot <- copy(elecWholeSalePrice)[, energyCarrier := "CCS"]
  elecMix <- copy(elecWholeSalePrice)[, energyCarrier := "Mixed charging"]
  
  #Network cost incl increasing grid fees
  elecNetworkCost <- unique(energyCarrierParameters[parameter %in% c("Network Costs - MCS", "Network Costs - Depot")][, eval(colsToDelete[!colsToDelete %in% c("parameter", "unit")]) := NULL])
  #Question to ISI: Is it corect, that Increasing grid fees are applied on Network Costs - MCS, Network Costs - Depot
  increasingGridFees <- unique(energyCarrierParameters[parameter == "Increasing grid fees"][, eval(colsToDelete) := NULL])
  setnames(increasingGridFees, "value", "IncFees")
  elecNetworkCost <- merge(elecNetworkCost, increasingGridFees, by = intersect(names(elecNetworkCost), names(increasingGridFees)))
  elecNetworkCost[, value := as.numeric(value) * (1 + as.numeric(IncFees))][, IncFees := NULL]
  MCSNetwork <- elecNetworkCost[parameter %in% c("Network Costs - MCS")][, parameter := c("Network Costs")]
  depotNetwork <- elecNetworkCost[parameter %in% c("Network Costs - Depot")][, parameter := c("Network Costs")]
  elecMixNetwork <- copy(MCSNetwork)[, c("unit") := NULL]
  setnames(elecMixNetwork, "value", "MCSNet")
  ShareMCSCharging <- unique(energyCarrierParameters[parameter == "Share MCS-Charging"][, eval(colsToDelete) := NULL])#[%]
  setnames(ShareMCSCharging, "value", "shareMCS")
  elecMixNetwork <- merge(elecMixNetwork, ShareMCSCharging, by = intersect(names(elecMixNetwork), names(ShareMCSCharging)))
  elecMixNetwork <- merge(elecMixNetwork, depotNetwork, by = intersect(names(elecMixNetwork), names(depotNetwork)))
  elecMixNetwork[, value := as.numeric(value) * (1 - as.numeric(shareMCS)) + as.numeric(shareMCS) * as.numeric(MCSNet)][, c("shareMCS", "MCSNet") := NULL]
  MCS <- rbind(MCS, MCSNetwork[, energyCarrier := "MCS"])
  depot <- rbind(depot, depotNetwork[, energyCarrier := "CCS"])
  elecMix <- rbind(elecMix, elecMixNetwork[, energyCarrier := "Mixed charging"])
  
  #Taxes, Fees, Levies, Charges  
  MCSTax <- unique(energyCarrierParameters[parameter %in% c("Taxes, Fees, Levies and charges - MCS")][, eval(colsToDelete[!colsToDelete %in% c("parameter", "unit")]) := NULL])
  depotTax <- unique(energyCarrierParameters[parameter %in% c("Taxes, Fees, Levies and charges - Depot")][, eval(colsToDelete[!colsToDelete %in% c("parameter", "unit")]) := NULL])
  elecMixTaxBET <- copy(MCSTax)[, c("parameter", "unit") := NULL]
  setnames(elecMixTaxBET, "value", "MCStax")
  elecMixTaxBET <- merge(elecMixTaxBET, ShareMCSCharging, by = intersect(names(elecMixTaxBET), names(ShareMCSCharging)))
  elecMixTaxBET <- merge(elecMixTaxBET, depotTax, by = intersect(names(elecMixTaxBET), names(depotTax)))
  elecMixTaxBET[, value := as.numeric(value) * (1 - as.numeric(shareMCS)) + as.numeric(shareMCS) * as.numeric(MCStax)]
  elecMixTaxBET[, parameter := "Taxes, Fees, Levies and charges"][, c("shareMCS", "MCStax") := NULL]
  MCS <- rbind(MCS, MCSTax[, energyCarrier := "MCS"])
  depot <- rbind(depot, depotTax[, energyCarrier := "CCS"])
  elecMix <- rbind(elecMix, elecMixTaxBET[, energyCarrier := "Mixed charging"])
  
  #Charger Costs
  MCScharger <- unique(energyCarrierParameters[parameter %in% c("MCS grid extension", "MCS Station (Public Charging) - Incl. Overhead")][, eval(colsToDelete[!colsToDelete %in% c("parameter", "unit")]) := NULL])
  MCScharger[parameter == "MCS grid extension", parameter := "Grid extension"]
  MCScharger[parameter == "MCS Station (Public Charging) - Incl. Overhead", parameter := "Station"]
  depotcharger <- unique(energyCarrierParameters[parameter %in% c("CCS grid extension", "CCS Station (Depot) - Incl. Overhead")][, eval(colsToDelete[!colsToDelete %in% c("parameter", "unit")]) := NULL])
  depotcharger[parameter == "CCS grid extension", parameter := "Grid extension"]
  depotcharger[parameter == "CCS Station (Depot) - Incl. Overhead", parameter := "Station"]
  elecMixcharger <- copy(MCScharger)
  setnames(elecMixcharger, "value", "Mixcharger")
  elecMixcharger <- merge(elecMixcharger, ShareMCSCharging, by = intersect(names(elecMixcharger), names(ShareMCSCharging)), allow.cartesian = TRUE)
  elecMixcharger <- merge(elecMixcharger, depotcharger, by = intersect(names(elecMixcharger), names(depotcharger)))
  elecMixcharger[, value := as.numeric(value) * (1 - as.numeric(shareMCS)) + as.numeric(shareMCS) * as.numeric(Mixcharger)][, c("shareMCS", "Mixcharger") := NULL]
  MCS <- rbind(MCS, MCScharger[, energyCarrier := "MCS"])
  depot <- rbind(depot, depotcharger[, energyCarrier := "CCS"])
  elecMix <- rbind(elecMix, elecMixcharger[, energyCarrier := "Mixed charging"])
  
  #Markup only applied on MCS charging
  MCStot <- MCS[, .(MCStot = sum(as.numeric(value))), by = c("country", "period", "paperScen", "energyCarrierParameterScenario", "unit")]
  MCSmarkup <- unique(energyCarrierParameters[parameter == "MCS Profit and Mark-Up"][, eval(colsToDelete) := NULL])
  MCSmarkup <- merge(MCSmarkup, MCStot, by = intersect(names(MCSmarkup), names(MCStot)))
  MCSmarkup[, value := as.numeric(value) * as.numeric(MCStot)][, parameter := "Station Overhead, Profit and Mark-Up"][, c("MCStot") := NULL]
  elecMixmarkup <- copy(MCSmarkup)
  elecMixmarkup <- merge(elecMixmarkup, ShareMCSCharging, by = intersect(names(elecMixmarkup), names(ShareMCSCharging)))
  elecMixmarkup[, value := as.numeric(value) * as.numeric(shareMCS)][, c("shareMCS") := NULL]
  MCS <- rbind(MCS, MCSmarkup[, energyCarrier := "MCS"])
  elecMix <- rbind(elecMix, elecMixmarkup[, energyCarrier := "Mixed charging"])
  
  #Network fees savings potential for depot charging
  depotSaving <- unique(energyCarrierParameters[parameter %in% c("Depot - Network fees savings potential")][, eval(colsToDelete[!colsToDelete %in% c("parameter", "unit")]) := NULL])
  depotSaving[parameter == "Depot - Network fees savings potential", parameter := "Network fees savings potential"]
  elecMixSaving <- copy(depotSaving)
  elecMixSaving <- merge(elecMixSaving, ShareMCSCharging, by = intersect(names(elecMixSaving), names(ShareMCSCharging)))
  elecMixSaving[, value := as.numeric(value) * (1 - as.numeric(shareMCS))][, c("shareMCS") := NULL]
  depot <- rbind(depot, depotSaving[, energyCarrier := "CCS"])
  elecMix <- rbind(elecMix, elecMixSaving[, energyCarrier := "Mixed charging"])
  
  elecAtPumpkWh <- rbind(MCS, depot, elecMix)

###-Hydrogen -------------------------------------------------------------------------------------------------------------------------------------------------------------------

  #Wholesale
  h2WholeSalePrice <- unique(energyCarrierParameters[parameter == "Hydrogen Price"][, eval(colsToDelete[!colsToDelete %in% c("parameter", "unit")]) := NULL])
  h2WholeSalePrice[, parameter := "Wholesale"][, energyCarrier := "Hydrogen"]

  #Hydrogen Refueling Station Costs
  # Comment: No differentiation between LH2 and CH2 infrastructure
  HRSCost <- unique(energyCarrierParameters[subCategory %in% c("H2 Infrastructure")][, eval(colsToDelete[!colsToDelete %in% c("parameter", "unit")]) := NULL])
  HRSCost[, energyCarrier := "Hydrogen"]
  HRSCost[parameter == "HRS costs", parameter := "Station"]
  HRSCost[parameter == "H2 transport and distribution", parameter := "Transport and distribution"]
  HRSCost[parameter == "HRS Overhead, Profit and Mark-Up", parameter := "Station Overhead, Profit and Mark-Up"]
  #Convert unit
  HRSCost[unit == "EUR/kg H2", `kWh/kg H2` := kWhProKgH2]
  HRSCost[unit == "EUR/kg H2", value := as.numeric(value)/`kWh/kg H2`][unit == "EUR/kg H2", unit := "EUR/kWh"][, `kWh/kg H2` := NULL]

  #Taxes, Fees, Levies, Charges  
  H2Tax <- unique(energyCarrierParameters[parameter %in% c("Taxes, Fees, Levies and charges - H2")][, eval(colsToDelete[!colsToDelete %in% c("parameter", "unit")]) := NULL])
  #Tax reduction H2 -> Check with ISI
  H2TaxReduction <- unique(energyCarrierParameters[parameter %in% c("Tax reduction")][, eval(colsToDelete) := NULL])# [negative %]
  setnames(H2TaxReduction, "value", "TaxRed")
  H2Tax <- merge(H2Tax, H2TaxReduction, by = intersect(names(H2Tax), names(H2TaxReduction)))
  H2Tax[, value := as.numeric(value) + as.numeric(value) * as.numeric(TaxRed)][, TaxRed := NULL]
  H2Tax[parameter == "Taxes, Fees, Levies and charges - H2", parameter := "Taxes, Fees, Levies and charges"][, energyCarrier := "Hydrogen"]

  H2AtPumpkWh <- rbind(h2WholeSalePrice, HRSCost, H2Tax)


vehicleParameters <- rbind(CAPEX, efficiency, ChargingLosses, OMCosts, vehTax, Tolls)      
energyCarrierParameters <- unique(rbind(dieselAtPumpkWh, elecAtPumpkWh, H2AtPumpkWh))

# check files---------------------------------------------------------
if (anyDuplicated(energyCarrierParameters) | anyNA(energyCarrierParameters)) stop("energyCarrierParameters contains NAs or duplicates")
if (anyDuplicated(vehicleParameters) | anyNA(vehicleParameters)) stop("vehicleParameters contains NAs or duplicates")
if (anyDuplicated(Co2IntensityDieFuel) | anyNA(Co2IntensityDieFuel)) stop("Co2IntensityDieFuel contains NAs or duplicates")
if (anyDuplicated(SalesPrice) | anyNA(SalesPrice)) stop("SalesPrice contains NAs or duplicates")

#-Combine data---------------------------------------------------------------------------------------------------------------------
EleTechs <- c("BET large battery", "BET small battery")
H2Techs <- c("FCET")
DieselTechs <- c("ICET")
allFuelTypes <- c(EleTechs, H2Techs, DieselTechs)
fuelMap <- data.table(`truckTechnology` = allFuelTypes)
fuelMap[`truckTechnology` %in% EleTechs, energyCarrier := "Mixed charging"]
fuelMap[`truckTechnology` %in% H2Techs, energyCarrier := "Hydrogen"]
fuelMap[`truckTechnology` == "ICET", energyCarrier := "Diesel mix"]

vehParameterScenarioMap <- data.table(`vehicleParameterScenario` = unique(vehicleParameters$`vehicleParameterScenario`))
vehParameterScenarioMap[, map := "All"]
energyParameterScenarioMap <- data.table(`energyCarrierParameterScenario` = unique(energyCarrierParameters$`energyCarrierParameterScenario`))
energyParameterScenarioMap[, map := "All"]
truckClassMap <- data.table(truckClass = unique(vehicleParameters$truckClass))
truckClassMap[, map := "All"] 

vehicleParameters[, map := "All"]
vehicleParameters <- merge(vehicleParameters, energyParameterScenarioMap, by = "map", allow.cartesian = TRUE, all = TRUE)[, map := NULL]
efficiency <- vehicleParameters[parameter == "Specific Energy Consumption"][, c("parameter", "unit") := NULL] # [kWh/km]
setnames(efficiency, "value", "eff")
chargingLosses <- vehicleParameters[parameter == "Charging Losses"][, c("parameter", "unit")  := NULL] # [%]
setnames(chargingLosses, "value", "losses")
vehicleParameters <- vehicleParameters[!parameter %in% c("Specific Energy Consumption", "Charging Losses")]
vehicleParameters <- merge(vehicleParameters, fuelMap, by = "truckTechnology", allow.cartesian = TRUE, all.y = TRUE)

fuelCost <- merge(energyCarrierParameters, fuelMap, by = "energyCarrier", allow.cartesian = TRUE, all.y = TRUE)[, map := "All"]
fuelCost <- merge(fuelCost, truckClassMap, by = "map", all = TRUE, allow.cartesian = TRUE)
fuelCost <- merge(fuelCost, vehParameterScenarioMap, by = "map", all.x = TRUE, allow.cartesian = TRUE)[, map := NULL]
fuelCost <- merge(fuelCost, efficiency, by = intersect(names(efficiency), names(fuelCost)), allow.cartesian = TRUE, all = TRUE)
fuelCost[, value := value * eff][, unit := "EUR/vehkm"][, eff := NULL]
fuelCost <- merge(fuelCost, chargingLosses, by = intersect(names(chargingLosses), names(fuelCost)), allow.cartesian = TRUE, all.x = TRUE)
fuelCost[`truckTechnology` %in% EleTechs, value := value * losses][, losses := NULL]


TCO <- rbind(vehicleParameters, fuelCost)

# save CO2 intensity in tCO2/vehkm for CO2 price plots (making use of vehicleParameterScenario dependant efficiency ---------------------
effDie <- efficiency[truckTechnology == "ICET"][, truckTechnology := NULL] # [kWh/vehkm]
Co2IntensityDieICE <- copy(Co2IntensityDieFuel)# [tCO2e/kWh]
Co2IntensityDieICE <- merge(Co2IntensityDieICE, effDie, by = intersect(names(Co2IntensityDieICE), names(effDie)), allow.cartesian = TRUE)
Co2IntensityDieICE[, value := value * eff][, unit := "tCO2/vehkm"] #tCO2/vehkm

#-Check completeness---------------------------------------------------------------------------------------------------------------------

BET <- c(                                              
  "Glider/Vehicle body invest without drivetrain",    
  "Electric motor costs",                             
  "Power electronics costs",                          
  "Battery",                                    
  "Resale value",                                     
  "Vehicle Tax",
  "Toll charge",
  "M&R + tires",
  "Wholesale", 
  "Network Costs",                              
  "Taxes, Fees, Levies and charges",            
  "Station",                                     
  "Grid extension",
  "Station Overhead, Profit and Mark-Up",
  "Network fees savings potential"
) 

FCET <- c(                                              
  "Glider/Vehicle body invest without drivetrain",    
  "Electric motor costs",                             
  "Power electronics costs",                          
  "Battery",
  "Fuel cell system", 
  "H2 tank",
  "Resale value",                                     
  "Vehicle Tax",
  "Toll charge",
  "M&R + tires",
  "Wholesale", 
  "Transport and distribution",
  "Taxes, Fees, Levies and charges",            
  "Station", 
  "Station Overhead, Profit and Mark-Up"
) 

Diesel <- c(                                              
  "Glider/Vehicle body invest without drivetrain",    
  "Diesel engine costs (CI engine)",                             
  "Emission Aftertreatment",                          
  "Resale value",                                     
  "Vehicle Tax",
  "Toll charge",
  "M&R + tires",
  "Wholesale", 
  "CO2 tax",                              
  "Excise Duty",
  "Total Diesel Mark-Up"
) 

if (length((intersect(unique(TCO[`truckTechnology` == "ICET"]$parameter), Diesel))) < length(Diesel)) 
  stop(paste0("The following parameters are missing in the Diesel TCO: ", Diesel[!Diesel %in% unique(TCO[`truckTechnology` == "ICET"]$parameter)]))
if (length((intersect(unique(TCO[`truckTechnology` == "BET large battery"]$parameter), BET))) < length(BET)) 
  stop(paste0("The following parameters are missing in the BET LB TCO: ", BET[!BET %in% unique(TCO[`truckTechnology` == "BET large battery"]$parameter)]))
if (length((intersect(unique(TCO[`truckTechnology` == "BET small battery"]$parameter), BET))) < length(BET)) 
  stop(paste0("The following parameters are missing in the BET SB TCO: ", BET[!BET %in% unique(TCO[`truckTechnology` == "BET small battery"]$parameter)]))
if (length((intersect(unique(TCO[`truckTechnology` == "FCET"]$parameter), FCET))) < length(FCET)) 
  stop(paste0("The following parameters are missing in the FCET TCO: ", FCET[!FCET %in% unique(TCO[`truckTechnology` == "FCET"]$parameter)]))

# Save files---------------------------------------------------------
write.csv(TCO, file.path("data", "TCOanalysis", "TCO.csv"), row.names = FALSE)
write.csv(SalesPrice, file.path("data", "TCOanalysis", "salesPrices.csv"), row.names = FALSE)
write.csv(Co2IntensityDieFuel, file.path("data", "TCOanalysis", "co2IntensityDieFuel.csv"), row.names = FALSE)
write.csv(energyCarrierParameters, file.path("data", "TCOanalysis", "fuelPricekWh.csv"), row.names = FALSE)

rm(list=ls())

