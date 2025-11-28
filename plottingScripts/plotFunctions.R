scenOverview <- function(baseZise, paperColors, baseTextSize) {
  tileWidth   <- baseTextSize * 2   
  tileHeight  <- baseTextSize * 0.5   
  tileSpacing <- baseTextSize * 0.01
  rowSpacing  <- baseTextSize * 0.01
  cornerRadius <- 0.15
  
  nRows <- 6
  stepVert <- tileHeight + rowSpacing
  rowPositions <- seq(from = nRows, by = -stepVert, length.out = nRows)
  topHeaderY <- rowPositions[1] + stepVert
  colPositions <- cumsum(c(0, rep(tileWidth + tileSpacing, 3)))
  
  colLabels <- c(
    "BET/FCET",
    "ICET\ncounterpart",
    "DCO scenario"
  )
  rowLabels <- c(
    "Vehicle\nparameters", "Energy carrier\nparameters",
    "Vehicle\nparameters", "Energy carrier\nparameters",
    "Vehicle\nparameters", "Energy carrier\nparameters"
  )
  
  combos <- data.frame(
    zet = c("LC_HTM", "PROG",
            "MC_MTM", "BAU",
            "HC_LTM", "BAU"),
    diesel = c("HC_LTM", "BAU",
               "MC_MTM", "BAU",
               "LC_HTM", "PROG"),
    stringsAsFactors = FALSE
  )
  
  leftDf <- data.frame(
    colX = rep(colPositions[2:3], each = nrow(combos)),
    scenarios = c(combos$zet, combos$diesel),
    y = rep(rowPositions, times = 2),
    stringsAsFactors = FALSE
  )
  
  rowHeaderDf <- data.frame(
    colX = colPositions[1],
    label = rowLabels,
    y = rowPositions,
    stringsAsFactors = FALSE
  )
  
  topHeaderDf <- data.frame(
    colX = colPositions[2:4],             
    label = colLabels,                     
    y = rep(topHeaderY, length(colLabels)),
    stringsAsFactors = FALSE
  )
  
  # DCO column
  pairs <- matrix(c(1,2, 3,4, 5,6), ncol = 2, byrow = TRUE)
  dcoYCenter <- apply(pairs, 1, function(p) mean(rowPositions[p]))
  dcoYMin <- apply(pairs, 1, function(p) min(rowPositions[p]) - tileHeight/2)
  dcoYMax <- apply(pairs, 1, function(p) max(rowPositions[p]) + tileHeight/2)
  dcoCol <- c("Optimistic", "Medium", "Pessimistic")
  
  dcoDf <- data.frame(
    dco = dcoCol,
    yCenter = dcoYCenter,
    yMin = dcoYMin,
    yMax = dcoYMax,
    xCenter = colPositions[4],
    stringsAsFactors = FALSE
  )
  
  xMargin <- tileWidth * 0.5
  xLimits <- c(min(colPositions) - tileWidth/2 - xMargin,
               max(colPositions) + tileWidth/2 + xMargin)
  
  yMarginTop <- tileHeight * 0.5
  yMarginBottom <- tileHeight * 0.5
  yLimits <- c(min(rowPositions) - tileHeight/2 - yMarginBottom,
               topHeaderY + tileHeight/2 + yMarginTop)
  
  scenariosPlot <- ggplot() +
    
    # Top Header
    funkyheatmap::geom_rounded_rect(
      data = topHeaderDf,
      aes(xmin = colX - tileWidth/2, xmax = colX + tileWidth/2,
          ymin = y - tileHeight/2, ymax = y + tileHeight/2),
      fill = "grey80", color = "white", radius = cornerRadius
    ) +
    geom_text(
      data = topHeaderDf,
      aes(x = colX, y = y, label = label),
      fontface = "bold", size = relSize(1, baseTextSize), lineheight = 0.9
    ) +
    
    # Row Header
    funkyheatmap::geom_rounded_rect(
      data = rowHeaderDf,
      aes(xmin = colX - tileWidth/2, xmax = colX + tileWidth/2,
          ymin = y - tileHeight/2, ymax = y + tileHeight/2),
      fill = "grey80", color = "white", radius = cornerRadius
    ) +
    geom_text(
      data = rowHeaderDf,
      aes(x = colX, y = y, label = label),
      fontface = "bold", size = relSize(1, baseTextSize), lineheight = 0.9
    ) +
    
    # Left two columns
    funkyheatmap::geom_rounded_rect(
      data = leftDf,
      aes(xmin = colX - tileWidth/2, xmax = colX + tileWidth/2,
          ymin = y - tileHeight/2, ymax = y + tileHeight/2,
          fill = scenarios),
      color = "white", radius = cornerRadius
    ) +
    geom_text(
      data = leftDf,
      aes(x = colX, y = y, label = scenarios),
      size = relSize(1.0, baseTextSize), lineheight = 1
    ) +
    
    # DCO column
    funkyheatmap::geom_rounded_rect(
      data = dcoDf,
      aes(xmin = xCenter - tileWidth/2, xmax = xCenter + tileWidth/2,
          ymin = yMin, ymax = yMax, fill = dco),
      color = "white", radius = cornerRadius
    ) +
    geom_text(
      data = dcoDf,
      aes(x = xCenter, y = yCenter, label = dco),
      color = "white", fontface = "bold", size = relSize(1.0, baseTextSize)
    ) +
    
    scale_x_continuous(breaks = colPositions, labels = NULL, limits = xLimits) +
    scale_y_continuous(breaks = NULL, limits = yLimits) +
    scale_fill_manual(values = paperColors) +
    coord_cartesian() +
    theme_minimal(base_size = baseTextSize) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.position = "none"
    )
  
  return(scenariosPlot)
}

examplaryUtilisation <- function(country, reducedBinnedWeightedMileage, baseTextSize) {

  exampleCountry <- country
  widthSize <- 0.7 
  heightSize <- 0.8
  
  #stack effect
  nShadow <- 6   
  offsetX <- 0.02 * widthSize 
  offsetY <- 0.02 * heightSize
  
  # ----- Plot ----- 
  foregroundData <- copy(reducedBinnedWeightedMileage[binMean < 500000 & country == exampleCountry]) 
  
  xLevels <- levels(foregroundData$avktBinned)
  xLimPlot <- c(0.5, length(xLevels) + 0.5)
  yLimPlot <- range(foregroundData$binWeightedShareEUR)
  
  fullPlot <- gTree(children = gList())
  
  # Foreground plot
  xBreaks <- xLevels[seq(1, length(xLevels), by = 3)]
  
  numRanges <- lapply(xBreaks, function(lbl) {
    as.numeric(unlist(strsplit(gsub("\\[|\\]|\\(|\\)", "", lbl), ",")))
  })
  
  # Build rounded labels in thousands
  xLabels <- sapply(numRanges, function(r) {
    paste0(round(r[1] / 1000), "–", round(r[2] / 1000), "k")
  })
  
  mileageDensity <- ggplot(foregroundData, aes(x = avktBinned, y = binWeightedShareEUR, fill = truckClass)) +
    geom_bar(stat = "identity", position = "identity", alpha = 0.6, color = "white") +
    facet_wrap(~country, scales = "free_y", ncol = 1) +
    scale_x_discrete(breaks = xBreaks, labels = xLabels) +
    labs(
      x = "Annual mileage [km/yr]",
      y = "Weighted share",
      fill = NULL
    ) +
    plotTheme(baseTextSize) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = rel(1) * 0.8),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(lineheight = 0.9),
      axis.title.x = element_text(margin = margin(t = 4)),
      axis.title.y = element_text(margin = margin(r = 4)),
      plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "pt"),
      panel.background = element_rect(fill = NA, color = NA),
      plot.background  = element_rect(fill = NA, color = NA))
  
  # Background stacked plots
  for (i in seq(nShadow, 1)) {
    alphaVal <- ifelse(i == 1, 1, 0.3)
    
    shadow <- rectGrob(
      x = 0.5 + i * offsetX,
      y = 0.5 - i * offsetY,
      width = widthSize,
      height = heightSize,
      gp = gpar(fill = "grey95", col = "grey85", alpha = alphaVal)
    )
    
    fullPlot <- addGrob(fullPlot, shadow)
  }
  
  # Transfer to grob for assembly
  foregroundGrob <- ggplotGrob(mileageDensity)
  foregroundGrob$vp <- viewport(
    x = 0.5, y = 0.5,
    width = widthSize * 0.8,
    height = heightSize * 0.9
  )
  
  fullPlot <- addGrob(fullPlot, foregroundGrob)
  return(fullPlot)
}

dcoBarPlot <- function(TCO, DCOscenarios, yr, vehSize, exampleCountry, paperScenario, baseTextSize) {
  # - Prepare data -------------------------------------------------------------
  barPlotData <- copy(TCO)[period == yr & truckClass == vehSize & country == exampleCountry & paperScen == paperScenario]
  # filter DCO scenarios
  truckSide <- merge(
    barPlotData,
    unique(DCOscenarios[, .(truckTech, truckTCOscenario, DCOscenario)]),
    by.x = c("truckTechnology", "TCOscenario"),
    by.y = c("truckTech", "truckTCOscenario"),
    all = FALSE
  )
  counterSide <- merge(
    barPlotData,
    unique(DCOscenarios[, .(counterTech, counterTechTCOscenario, DCOscenario)]),
    by.x = c("truckTechnology", "TCOscenario"),
    by.y = c("counterTech", "counterTechTCOscenario"),
    all = FALSE
  )
  barPlotData <- rbind(truckSide, counterSide)
  # Rename for slim legend and axis labelling
  barPlotData[parameter == "Vehicle body\nincl. engine", parameter := "Vehicle body incl. engine"]
  newOrder <- c("CO2 tax", "Toll charge", "Vehicle tax", "Fuel cost", 
                "M&R + tires", "H2 tank", "Fuel cell system", "Battery", "Vehicle body incl. engine")
  validNewOrder <- intersect(newOrder, unique(barPlotData$parameter))
  
  droplevels(barPlotData)
  barPlotData[, parameter := factor(parameter, 
                                 levels = validNewOrder)]
  barPlotData$TCOscenario <- gsub("x", "\nx\n", barPlotData$TCOscenario)
  barPlotData[truckTechnology == "BET large battery", truckTechnology := "BET\nlarge\nbattery"]
  barPlotData[truckTechnology == "BET small battery", truckTechnology := "BET\nsmall\nbattery"]
  barPlotData[, `truckTechnology` := factor(truckTechnology, levels = c("ICET", "FCET", "BET\nlarge\nbattery", "BET\nsmall\nbattery"))]
  # Step 1: Compute sum per truckTechnology
  TCOSum <- barPlotData[, .(value = sum(value)), by = c("truckTechnology", "TCOscenario", "DCOscenario")]
  yMax <- max(TCOSum$value)* 1.01
  # Step 2: Compute differences to ICET
  TCOSum[, diff := value - value[`truckTechnology` == "ICET"], by = "DCOscenario"]
  dieselVal <- TCOSum[`truckTechnology` == "ICET"]
  # Step 3: Create segment data for arrows (exclude ICET)
  arrowData <- TCOSum[`truckTechnology` != "ICET"]
  arrowData[, x := TCOscenario]
  arrowData[, xend := TCOscenario]
  arrowData[, y := value, by = "DCOscenario"]
  arrowData[, yend := value - diff]
  
  # - Plot -------------------------------------------------------------
  # Create one plot per facet, then stitch them back together
  facetLevels <- unique(barPlotData$DCOscenario)
  facetLevels <- facetLevels[c(2,3,1)]
  plotList <- lapply(seq_along(facetLevels), function(i) {
    
    facetLevel <- facetLevels[i]
    
    subBarPlotData <- subset(barPlotData, DCOscenario == facetLevel)
    subArrowData <- subset(arrowData, DCOscenario == facetLevel)
    subDieselVal <- subset(dieselVal, DCOscenario == facetLevel)
    
    TCObarPlots <- ggplot(subBarPlotData, aes(x = `TCOscenario`)) +
      geom_bar(aes(y = value, fill = parameter), position = "stack", stat = "identity", width = 0.6) +
      scale_fill_manual(values = paperColors) +
      geom_hline(yintercept = subDieselVal$value, linetype = "dashed", color = "black", linewidth = 0.7) +
      geom_segment(
        data = subArrowData,
        aes(x = x, xend = xend, y = y, yend = yend),
        arrow = arrow(
          length = unit(0.1, "cm"),  
          type = "closed",            
          ends = "last",
          angle = 25                 
        ),
        color = "black",
        linewidth = baseLineWidth              
      ) +
      geom_label(data = subArrowData,
                 aes(x = xend, y = (y + yend) / 2, label = "DCO"),
                 position = position_nudge(x = 0.05),
                 hjust = -0.1,
                 vjust = 0.5,
                 size = relSize(0.8, baseTextSize),
                 color = "black",           
                 fill = "white",          
                 linewidth = 0,       
                 label.r = unit(0.15, "lines"),  
                 fontface = "bold") +
      facet_wrap(~ `truckTechnology`, nrow = 1, scales = "free_x") +
      coord_cartesian(ylim = c(0, yMax)) +
      labs(
        x = if (i == 2) paste0("Vehicle and energy carrier parameter scenario") else NULL,
        y = if (i == 1) "TCO [EUR/km]" else NULL,
        title = as.character(facetLevel),
        fill = NULL
      ) +  
      plotTheme(baseTextSize) +
      theme(
        plot.margin   =  margin(t = 3, r = , b = 3, l = 1),
      )
  })
  TCOtoDCOexample <- wrap_plots(
    plotList,
    ncol = 3,
    guides = "collect"
  ) &
    guides(
      fill = guide_legend(
        nrow = 2,
      )
    ) &
    theme(
      legend.position   = "bottom",
      legend.key.size   = unit(1, "lines"),  # global scaling
      legend.text       = element_text(size = rel(1)),
      legend.title      = element_text(size = rel(1)),
      panel.background  = element_rect(fill = "transparent", colour = NA),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA)
    )
  #Create and draw grop
  TCOtoDCO_grob <- patchworkGrob(TCOtoDCOexample)
  grid.newpage()
  grid.draw(TCOtoDCO_grob)
  
  widthSize <- 0.9 
  heightSize <- 0.9
  
  # - Shadow plots 
  nShadow <- 10    # Amount of background plots
  offsetX <- 0.005 * widthSize 
  offsetY <- 0.005 * heightSize
  
  shadows <- gTree(children=gList())
  for (i in seq(nShadow, 1)) {
    a <- ifelse(i == 1, 1, 0.3)
    
    shadow <- rectGrob(
      x = 0.5 + i * offsetX,
      y = 0.5 - i * offsetY,
      width  = widthSize,
      height = heightSize,
      gp = gpar(fill = "grey95", col = "grey85", alpha = a)
    )
    
    shadows <- addGrob(shadows, shadow)
  }
  
  TCOtoDCO_grob$vp <- viewport(x = 0.497, y = 0.45, width = widthSize, height = heightSize*0.95)
  TCOtoDCO <- gTree(children = gList(
    shadows,          # background
    TCOtoDCO_grob     # foreground
  ))

  grid.newpage()
  grid.draw(TCOtoDCO)
  
  ellipsis <- textGrob(
    "...",
    x = unit(0.98, "npc"),  
    y = unit(0.02, "npc"),  
    just = c("right", "bottom"),
    gp = gpar(col = "black", fontsize = baseTextSize*1.8, fontface = "bold", alpha = 0.5)
  )
  
  titleGrob <- textGrob(
    paste("Utilisation 160000 km/yr |", vehSize, " | ", yr, " | Germany"),
    x = 0.5,                # centered horizontally
    y = 0.87,               # slightly above the top of the plot
    just = c("center", "bottom"),
    gp = gpar(fontsize = baseTextSize *1.2 , fontface = "bold", col = "black")
  )
  
  # Combine your TCOtoDCO grob with title and ellipsis
  TCOtoDCOAnnotated <- grobTree(
    TCOtoDCO,
    titleGrob,
    ellipsis
  )
  return(TCOtoDCOAnnotated)
}

amDistributionBarPlot <- function(DCOmileageDistributionShares, yr, paperScenario, xAxis = "activity", feas = FALSE) {

  data <- copy(DCOmileageDistributionShares)[period %in% yr & paperScen == paperScenario]

  if (xAxis == "activity") {
     if (feas == FALSE) setnames(data, "cumBinWeightedShareEUR", "cumShare") else setnames(data, "cumBinFeasWeightedShareEUR", "cumShare")
    xAxisTitle <- "Activity [%]" 
    } else if (xAxis == "vehicles") {
      if (feas == FALSE) setnames(data, "cumBinWeightedVehShareEUR", "cumShare") else setnames(data, "cumBinFeasWeightedVehShareEUR", "cumShare")
      xAxisTitle <- "Vehicles [%]"}
  # witdth of mileage bins
  data[, cumWidth := 0.999 * (cumShare - shift(cumShare, type = "lag")),
       by = c("paperScen", "DCOscenario", "truckTechnology")]
  data[is.na(cumWidth), cumWidth := (cumShare),
       by = c("paperScen", "DCOscenario", "truckTechnology")]
  # ensure that DCO scenarios are handled in the right order
  data[, DCOscenario := factor(
    DCOscenario,
    levels = c("Optimistic", "Medium", "Pessimistic")
  )]
  # find cost benefitial percentage
  findZeroPoints <- data[, .SD[which.min(abs(value))],
                         by = .(period, truckTechnology, paperScen, DCOscenario)]
  findZeroPoints[, truckTechnology := factor(truckTechnology,
                                             levels = c("BET small battery", "BET large battery", "FCET"))]
  setorder(findZeroPoints, paperScen, DCOscenario, truckTechnology, -cumShare)
  findZeroPoints[, arrowY := seq(-0.2, -0.5, length.out = .N),
                 by = c("paperScen", "DCOscenario")]
  middleY <- -0.35
  
  # Split data in positive and negative DCO for better visualisation
  dataPositive <- data[value > 0]
  dataNegative <- data[value < 0]
  dataPositive[, truckTechnology := factor(truckTechnology,
                                           levels = c("BET small battery", "BET large battery", "FCET"))]
  dataNegative[, truckTechnology := factor(truckTechnology,
                                           levels = c("FCET", "BET large battery", "BET small battery"))]
  setorder(dataPositive, truckTechnology)
  setorder(dataNegative, truckTechnology)
  
  facetLevels <- levels(data$DCOscenario)
  facetLevels <- facetLevels[facetLevels %in% unique(data$DCOscenario)]
  xBreaks <- c(0, 25, 50, 75, 100)
  # function to find equivalent veh Share for 2n x axis (only for xAxis == activity)
  secLabels <- sapply(xBreaks, function(x) {
    # Find the nearest cumBinWeightedShareEUR
    idx <- which.min(abs(data$cumShare * 100 - x))
    # Return corresponding cumBinWeightedVehShare value
    round(data$cumBinWeightedVehShareEUR[idx] * 100)
  })
  if (feas) {annotation <- "cost beneficial\n and feasible"
             xAnnotation = 50} else {
    annotation <- "of road km\ncost beneficial"
    xAnnotation = 42}
  if (length(facetLevels) > 2) {xAxisLabel <- length(facetLevels)} else {
    xAxisLabel <- seq(1:length(facetLevels))}
  if (length(facetLevels) > 2) {yAxisLabel <- ceiling(length(facetLevels)/2)} else {
    yAxisLabel <- seq(1:length(facetLevels))}
  
  secLabels <- sapply(xBreaks, function(x) {
    idx <- which.min(abs(data$cumShare * 100 - x))
    round(data$cumBinWeightedVehShareEUR[idx] * 100)
  })
  
  plotList <- lapply(seq_along(facetLevels), function(i) {
    facetLevel <- facetLevels[i]
    subDataPos <- subset(dataPositive, DCOscenario == facetLevel)
    subDataNeg <- subset(dataNegative, DCOscenario == facetLevel)
    subZero <- subset(findZeroPoints, DCOscenario == facetLevel)
    
    extraLabel <- if (i %in% xAxisLabel) {
      annotate("label", x = xAnnotation, y = middleY, label = annotation,
               color = "black", fill = "white", fontface = "bold", size = relSize(0.9),
               hjust = 0, vjust = 0.5, linewidth = 0)
    } else NULL
    
    scales <- if (i %in% xAxisLabel) {
      list(scale_color_manual(values = paperColors, name = NULL,  guide = guide_legend(nrow = 1)),
           scale_fill_manual(values = paperColors, name = NULL, guide = guide_legend(nrow = 1))
      )
    } else {
      list(scale_color_manual(values = paperColors, guide = "none"),
           scale_fill_manual(values = paperColors, guide = "none"))
    }
    
    p <- ggplot(subDataPos) +
      geom_rect(
        aes(xmin = cumShare * 100 - cumWidth * 100,
            xmax = cumShare * 100, ymin = 0, ymax = value,
            fill = truckTechnology),
        color = NA, alpha = 0.5, show.legend = FALSE
      ) +
      geom_rect(
        aes(xmin = cumShare * 100 - cumWidth * 100,
            xmax = cumShare * 100, ymin = 0, ymax = value,
            color = truckTechnology),
        fill = NA, linewidth = 0.002 * baseLineWidth, alpha = 0.005, show.legend = FALSE
      ) +
      geom_rect(
        data = subDataNeg,
        aes(xmin = cumShare * 100 - cumWidth * 100,
            xmax = cumShare * 100, ymin = 0, ymax = value,
            fill = truckTechnology),
        color = NA, alpha = 0.5
      ) +
      geom_rect(
        data = subDataNeg,
        aes(xmin = cumShare * 100 - cumWidth * 100,
            xmax = cumShare * 100, ymin = 0, ymax = value,
            color = truckTechnology),
        fill = NA, linewidth = 0.002 * baseLineWidth, show.legend = FALSE, alpha = 0.005
      ) +
      geom_point(
        data = subZero,
        aes(x = cumShare * 100, y = 0, color = truckTechnology),
        shape = 1, size = 3, stroke = 0.8, fill = NA
      ) +
      geom_vline(data = subZero,
                 aes(xintercept = cumShare * 100, color = truckTechnology),
                 linetype = "dashed", linewidth = baseLineWidth, show.legend = FALSE) +
      geom_segment(data = subZero,
                   aes(x = 0, xend = cumShare * 100,
                       y = arrowY, yend = arrowY, color = truckTechnology),
                   arrow = arrow(length = unit(0.2, "cm"), type = "open"),
                   linewidth = baseLineWidth) +
      geom_label(data = subZero,
                 aes(x = 40, y = arrowY,
                     label = paste("~", round(cumShare * 100), "%")),
                 color = "black", fill = "white",
                 size = relSize(0.9), fontface = "bold",
                 label.padding = unit(0.1, "cm"),
                 linewidth = 0, hjust = 1, vjust = 0.5) +
      extraLabel +
      geom_hline(yintercept = 0, color = "black", linewidth = baseLineWidth) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 101), breaks = xBreaks) +
      coord_cartesian(ylim = c(-0.6, 0.6)) +
      scales +
      labs(
        x = if (i %in% xAxisLabel) xAxisTitle else NULL,
        y = if (i %in% yAxisLabel) paste0("DCO in ", yr, " [EUR/km]") else NULL
      ) +
      plotTheme(baseTextSize) 
    
    return(p)
    
  })
  
  
  combined <- wrap_plots(plotList, ncol = 1, guides = "collect") +
    plot_annotation(
      theme = theme(
        legend.position = "bottom",
        legend.margin = margin(t = 20),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1))
      )
    )
  
  return(combined)
}


metaPlot <- function(DCOmilageDistributionShares, paperScenario, horizontal = FALSE) {
  
  data <- copy(DCOmilageDistributionShares)[paperScen %in% paperScenario]
  findZeroPoints <- data[ , .SD[which.min(abs(value))], by = .(period, truckTechnology, paperScen, DCOscenario)][
    , .(period, `truckTechnology`, paperScen, DCOscenario, cumBinWeightedShareEUR)
  ]
  findZeroPoints[, DCOscenario := factor(DCOscenario, levels = c("Optimistic", "Medium", "Pessimistic"))]
  
  p1 <- ggplot(findZeroPoints[DCOscenario == "Optimistic"], 
               aes(x = period, y = cumBinWeightedShareEUR * 100, color = truckTechnology, linetype = paperScen)) +
    geom_line(linewidth = baseLineWidth) +
    scale_color_manual(values = paperColors, guide = "none") +  # legend removed
    scale_linetype_manual(values = paperLines, guide = "none") +
    geom_vline(xintercept = 2030, linetype = "dashed", color = "darkgrey", linewidth = baseLineWidth) +
    labs(y = if (horizontal == TRUE) "Economically\nviable activity [%]" else NULL, x = NULL) +
    scale_x_continuous(breaks = seq(min(findZeroPoints$period),
                                    max(findZeroPoints$period), by = 5)) +
    scale_y_continuous(limits = c(0, 102)) +
    plotTheme(baseTextSize)
  
  p2 <- ggplot(findZeroPoints[DCOscenario == "Medium"], 
               aes(x = period, y = cumBinWeightedShareEUR * 100, color = truckTechnology, linetype = paperScen)) +
    geom_line(linewidth = baseLineWidth) +
    scale_color_manual(values = paperColors, guide = "none") +  # legend removed
    scale_linetype_manual(values = paperLines, guide = "none") +
    geom_vline(xintercept = 2030, linetype = "dashed", color = "darkgrey", linewidth = baseLineWidth) +
    labs(y = if (horizontal == FALSE) "Economically viable activity [%]" else NULL, 
         x = if (horizontal == FALSE) NULL else "Year") +
    scale_x_continuous(breaks = seq(min(findZeroPoints$period),
                                    max(findZeroPoints$period), by = 5)) +
    scale_y_continuous(limits = c(0, 102)) +
    plotTheme(baseTextSize)
  
  p3 <- ggplot(findZeroPoints[DCOscenario == "Pessimistic"], 
               aes(x = period, y = cumBinWeightedShareEUR * 100, color = truckTechnology, linetype = paperScen)) +
    geom_line(linewidth = baseLineWidth) +
    scale_color_manual(values = paperColors, guide = "none") +  # legend removed
    scale_linetype_manual(values = paperLines) +
    geom_vline(xintercept = 2030, linetype = "dashed", color = "darkgrey", linewidth = baseLineWidth) +
    labs(x = if (horizontal == FALSE) "Year" else NULL, y = NULL) +
    scale_x_continuous(breaks = seq(min(findZeroPoints$period),
                                    max(findZeroPoints$period), by = 5)) +
    scale_y_continuous(limits = c(0, 102)) +
    plotTheme(baseTextSize)
  
  # Combine plots
  if (horizontal) columns = 3 else columns = 1
  combined <- wrap_plots(p1, p2, p3, ncol = columns, heights = c(1, 1, 1)) &
    theme(
      legend.position   = "bottom")
  return(combined)
}

plotRangeAnalysis <- function(rangeAnalysis, ranges, infrastructure) {

  ranges <- ranges[truckClass == "Tractor-trailer" &
                      `truckTechnology` %in% c("BET small battery", "BET large battery") &
                      `vehicleParameterScenario` == "MC_MTM" &
                      period %in% c(2030, 2040)]
  setnames(ranges, "value", "directRange")
  rangeComparison <- merge(
    ranges,
    rangeAnalysis$buildUp,
    by = c("directRange", "period"),
    all.x = TRUE
  )
  rangeComparison[`truckTechnology` == "BET large battery", `truckTechnology` := "Large\nbattery"]
  rangeComparison[`truckTechnology` == "BET small battery", `truckTechnology` := "Small\nbattery"]
  rangeComparison[, `truckTechnology` := factor(`truckTechnology`, levels = unique(rangeComparison$`truckTechnology`))]
  rangeComparison[, period := factor(period, levels = unique(rangeComparison$period))]
  
  p <- ggplot() +
    
    # direct driving range over freight activity for different periods and MCS coverage
    geom_line(data = rangeAnalysis$buildUp,
              aes(x = cumShareFeas * 100, y = directRange, 
                  group = period), color = "darkgrey", linewidth = 0.5 * baseLineWidth) +
    # direct driving range over freight activity for different periods and MCS coverage
    # Markers for 2030 and 2035
    geom_line(data = rangeAnalysis$buildUp[period %in% c(2030, 2035)],
              aes(x = cumShareFeas * 100, y = directRange, 
                  group = period), color = "black", linewidth = 0.5 * baseLineWidth, alpha = 0.5) +
    # Border no MCS: direct driving range over freight activity for different periods
    geom_line(data = rangeAnalysis$noMCS,  aes(x = cumShareFeas * 100, y = directRange), color = "black", linewidth = 0.5 * baseLineWidth, alpha = 0.5) +
    geom_text(
      data = data.frame(x = 58, y = 950, label = "Fast-charging\ninfrastructure\navailability"),
      aes(x = x, y = y, label = label),
      color = "black",       
      size = relSize(0.7),
      fontface = "bold"
    ) +
    # Label border no MCS
    geom_text(
      data = data.frame(x = 78, y = 849, label = "0 %"),
      aes(x = x, y = y, label = label),
      color = "black",       
      size = relSize(0.5),
      angle = 20,
      fontface = "bold"
    ) +
    # Border full MCS: direct driving range over freight activity for different periods
    geom_line(data = rangeAnalysis$fullMCS,  aes(x = cumShareFeas * 100, y = directRange), color = "black", linewidth = 0.5 * baseLineWidth, alpha = 0.5) +
    # Label border full MCS
    geom_text(
      data = data.frame(x = 91, y = 511, label = "100 %"),
      aes(x = x, y = y, label = label),
      color = "black",       
      size = relSize(0.5),
      angle = 41,
      fontface = "bold"
    ) +
    # Label 2030
    geom_text(
      data = data.frame(x = 75, y = 735, label = "2030"),
      aes(x = x, y = y, label = label),
      color = "black",       # Black text
      size = relSize(0.5),
      angle = 15,
      fontface = "bold"
    ) +
    # Label 2035
    geom_text(
      data = data.frame(x = 80, y = 515, label = "2035"),
      aes(x = x, y = y, label = label),
      color = "black",       # Black text
      size = relSize(0.5),
      angle = 15,
      fontface = "bold"
    ) +
    # Add arrows from considered variants
    # Horizontal line
    geom_segment(
      data = rangeComparison,
      aes(x = 0, xend = cumShareFeas * 100,
          y = directRange, yend = directRange,
          color = `truckTechnology`, linetype = period),
      linewidth = baseLineWidth
    ) +
    # Vertical line
    geom_segment(
      data = rangeComparison,
      aes(x = cumShareFeas * 100, xend = cumShareFeas * 100,
          y = directRange, yend = 140,
          color = `truckTechnology`, linetype = period),
      linewidth = baseLineWidth
    ) +
    # Horizontal arrow head
    geom_segment(
      data = rangeComparison,
      aes(x = (cumShareFeas * 100) - 0.1, xend = cumShareFeas * 100,
          y = directRange, yend = directRange,
          color = `truckTechnology`),
      arrow = arrow(length = unit(0.1, "cm"), type = "closed")
    ) +
    # Vertical arrow head
    geom_segment(
      data = rangeComparison,
      aes(x = cumShareFeas * 100, xend = cumShareFeas * 100,
          y = 140 + 0.1, yend = 140,
          color = `truckTechnology`),
      arrow = arrow(length = unit(0.1, "cm"), type = "closed")
    ) +
    labs(
      x = "Feasible activity at a given\ndirect driving range [%]",
      y = "Direct driving\nrange [km]",
      color = "BET\nvariant",
      linetype = "Year"
    ) +
    coord_cartesian(xlim = c(0, 100), ylim = c(150, 1100)) +
    scale_x_continuous(limits = c(0, 100),
                       expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(limits = c(140, 1010),
                       expand = expansion(mult = c(0, 0))) +
    scale_color_manual(values = paperColors) +
    plotTheme(baseTextSize)
  
  p <- p +
    guides(
      linetype = guide_legend(
        override.aes = list(arrow = NULL)  # no arrow in legend
      ))

  return(p)
}

plotMileageDensity <- function(weightedMileage) {

  
 t <-  ggplot(weightedMileage[dvktMax < 1100], aes(x = dvktMax)) +
    stat_ecdf(geom = "line", color = "darkgrey", linewidth = baseLineWidth * 1.1) +
   coord_flip() + 
   scale_x_continuous(
     limits = c(0, 1100),
     expand = expansion(mult = c(0, 0))
   ) +
   geom_hline(yintercept = 1) +
   scale_y_continuous(
     limits = c(0, 1.1),
     breaks = c(0, 0.5, 1),  
     expand = expansion(mult = c(0,0))
   ) +
    labs(
      x = "Maximum daily\nvehicle km travelled",
      y = "Cumulative\nfraction"
    ) +
    plotTheme(baseTextSize) 
  
  return(t)
}


capexVsOpexOverviewPlot <- function(DCO, breakeven, syntheticalBreakeven, scenario, vehType, region) {

  yrs <- c(2030, 2040, 2050)
  DCO <- copy(DCO)[, share := value_truck / value_counter * 100]
  plotCapex <- DCO[unit == "EUR/veh yr" & paperScen == scenario & truckClass == vehType & country == region & period %in% yrs]
  plotCapex <- dcast(
    plotCapex[, c("truckTechnology", "period", "DCOscenario", "share")],
    truckTechnology + period ~ DCOscenario,
    value.var = "share"
  )
  plotOpex <- DCO[!unit == "EUR/veh yr" & paperScen == scenario & truckClass == vehType & country == region & period %in% yrs]
  plotOpex <- dcast(
    plotOpex[, c("truckTechnology", "period", "DCOscenario", "share")],
    truckTechnology + period ~ DCOscenario,
    value.var = "share"
  )
  
  plotBreakeven <- breakeven[
    paperScen == scenario &
      truckClass == vehType &
      period %in% yrs &
      country == region
  ]

  syntheticalBreakeven[, breakevenBin := cut(
    breakeven,
    breaks = c(0, 10000, 25000, 50000, 100000, 200000, 300000),
    labels = c("<10k", "10–25k", "25–50k", "50–100k", "100–200k", "200–300k"),
    include.lowest = TRUE,
    right = FALSE
  )]
  
  colorMap <- colorspace::sequential_hcl(6, palette = "Sunset", power = 1.3, rev = TRUE)

  dodgeWidth <- 0.7
  periods <- sort(unique(plotCapex$period))
  
  capexShare <- ggplot(plotCapex, aes(x = factor(period),
                                      color = truckTechnology,
                                      group = truckTechnology)) +
    # period boxes
    geom_rect(
      data = data.frame(period = periods),
      aes(
        xmin = as.numeric(factor(period)) - 0.5,
        xmax = as.numeric(factor(period)) + 0.5,
        ymin = -Inf,
        ymax = Inf
      ),
      inherit.aes = FALSE,
      color = "grey95",   
      fill = NA,         
      linewidth = 0.5 * baseLineWidth
    ) +
    # bars
    geom_linerange(
      aes(ymin = Pessimistic, ymax = Optimistic),
      linewidth = 3.5, alpha = 0.3,
      position = position_dodge(width = dodgeWidth)
    ) +
    # medium scenario markers
    geom_point(
      aes(y = Medium),
      shape = 15,             
      size = 2,             
      stroke = 0.1,           
      position = position_dodge(width = dodgeWidth)
    ) +
    # ICET bottom line
    geom_hline(yintercept = 100, color = "grey50", linewidth = 1.6 * baseLineWidth) +
    annotate(
      "text",
      x = 3,
      y = 103,
      label = "ICET",
      hjust = 0.5,
      vjust = 0,
      color = "grey50",
      size = relSize(0.8)
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_color_manual(values = paperColors, guide = guide_legend(title.position = "top")) +
    labs(
      x = "Period",
      y = "CAPEX [% rel. to ICET]",
      color = "Truck\ntechnology",
      title = ""
    ) +
    plotTheme(baseTextSize) +
    theme(
      legend.position = "bottom",
      legend.box.just = "left",  
      legend.box = "horizontal",
      legend.direction = "vertical", 
      legend.title = element_text(hjust = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 1.6 * baseLineWidth),
      panel.grid.minor.y = element_line(color = "grey95", linewidth = 0.5 * baseLineWidth)
    )

  opexShare <- ggplot(plotOpex, aes(x = factor(period))) +
    # period boxes
    geom_rect(
      data = data.frame(period = periods),
      aes(
        xmin = as.numeric(factor(period)) - 0.5,
        xmax = as.numeric(factor(period)) + 0.5,
        ymin = -Inf,
        ymax = Inf
      ),
      inherit.aes = FALSE,
      color = "grey95",   
      fill = NA,          
      linewidth = 0.5 * baseLineWidth
    ) +
    # bars
    geom_linerange(
      aes(ymin = Pessimistic, ymax = Optimistic, color = truckTechnology,
                         group = truckTechnology),
      linewidth = 7 * baseLineWidth, alpha = 0.3,
      position = position_dodge(width = dodgeWidth),
      show.legend = FALSE
    ) +
    # medium scenario markers
    geom_point(
      aes(y = Medium, color = truckTechnology,
          group = truckTechnology),
      shape = 15,            
      size = 2,             
      stroke = 0.1,          
      position = position_dodge(width = dodgeWidth),
      show.legend = FALSE
    ) +
    # ICET bottom line
    geom_hline(yintercept = 100, color = "grey50", linewidth = 1.6 * baseLineWidth) +
    annotate(
      "text",
      x = 3,
      y = 103,
      label = "ICET",
      hjust = 0.5,
      vjust = 0,
      color = "grey50",
      size = relSize(0.8)
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_color_manual(values = paperColors) +
    labs(
      x = "Period",
      y = "OPEX [% rel. to ICET]",
      title = ""
    ) +
    plotTheme(baseTextSize) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 1.6 * baseLineWidth),
      panel.grid.minor.y = element_line(color = "grey95", linewidth = 0.5 * baseLineWidth)
    ) 
  
  breakevenHeatMap <- ggplot() +
    geom_tile(data = syntheticalBreakeven, aes(x = capexDiff, y = opexDiff, fill = breakevenBin)) +
    scale_fill_manual(
      values =  colorMap,
      name = "Breakeven mileage\n[vehkm/yr]",
      drop = FALSE,
      guide = guide_legend(title.position = "top", ncol = 2)
    ) +
    scale_x_reverse(expand = c(0, 0), breaks = seq(5000, 30000, by = 5000),
                       labels = function(x) paste0(x / 1000, "k")) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(xlim = c(500, 29500), ylim = c(0.1, 0.6)) +
    geom_line(
      data = plotBreakeven,
      aes(
        x = capexDiff,
        y = opexDiff,
        linetype = DCOscenario,
        color = truckTechnology
      ),
      linewidth = baseLineWidth,
      inherit.aes = FALSE,
      show.legend = c(color = FALSE) 
    ) +
    geom_point(
      data = plotBreakeven,
      aes(
        x = capexDiff,
        y = opexDiff,
        color = truckTechnology,
        shape = factor(period)
      ),
      size = 3,
      inherit.aes = FALSE,
      show.legend = c(color = FALSE, shape = TRUE) 
    ) +
    scale_color_manual(values = paperColors) +
    scale_linetype_manual(values = c("solid", "dotted", "longdash"), guide = guide_legend(title.position = "top")) +
    scale_shape_manual(values = c(16, 17, 18), guide = guide_legend(title.position = "top")
    ) +
    labs(
      x = "CAPEX disadvantage\n(alt – ICET) [€/veh yr]",
      y = "OPEX advantage\n(ICET – alt) [€/km]",
      shape = "Period",
      linetype = "DCO\nScenario"
    ) +
    plotTheme(baseTextSize) +
    theme(legend.position = "bottom",
          legend.box.just = "left",  
          legend.box = "horizontal",
          legend.direction = "vertical", 
          legend.title = element_text(hjust = 0.5))
  
  combinedCapexOpexAnalysis <- (
    (capexShare | opexShare | breakevenHeatMap) +
      plot_layout(widths = c(1, 1, 1), guides = "collect") &
      theme(
        legend.position = "bottom"
      ) &
      guides(
        color = guide_legend(nrow = 3, order = 1),
        shape = guide_legend(nrow = 3, order = 2),
        linetype = guide_legend(nrow = 3, order = 3)
      )
  ) + 
    plot_annotation(tag_levels = "a") &
    theme(
      plot.tag = element_text(size = baseTextSize*1.2, face = "bold")  # adjust size here
    )
  
  return(combinedCapexOpexAnalysis)
}



