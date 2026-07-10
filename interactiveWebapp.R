library(shiny)
library(ggplot2)
library(data.table)
library(patchwork)
library(cowplot)
library(grid)
library(tibble)

# ==============================================================================
# 1. GLOBAL DATA & MAPPINGS (KEEPING ALL DETAILS)
# ==============================================================================
mainFolder <- this.path::this.dir()
dataFolder <- file.path(mainFolder, "data")

# Source files
source(file.path(mainFolder, "PlottingScripts", "functions.R"))
source(file.path(mainFolder, "PlottingScripts", "plotFunctions.R"))
source(file.path(mainFolder, "PlottingScripts", "plotSettings.R"))

countryMapping <- as.data.table(tribble(
  ~country, ~fullName,
  "DE", "Germany", "ES", "Spain", "FR", "France",
  "IT", "Italy", "NL", "Netherlands", "PL", "Poland", "UK", "United Kingdom"
))

scenMapVeh <- c(
  "Low cost and high technical maturity" = "LC_HTM",
  "Medium cost and medium technical maturity" = "MC_MTM",
  "High cost and low technical maturity" = "HC_LTM"
)
scenMapEn <- c(
  "Business as usual" = "BAU",
  "Progressive" = "PROG"
)

# ==============================================================================
# 2. UI DESIGN (KEEPING ALL ORCID LINKS & HEADER DETAILS)
# ==============================================================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body, html { background-color: #e5e5e5; }
      .left-panel {
        width: 510px; position: fixed; top: 0; bottom: 0; left: 0;
        padding: 20px; background-color: #fafafa; overflow-y: auto;
        border-right: 1px solid #ddd; z-index: 100;
      }
      .left-header-box {
        background-color: #f0f0ff; padding: 18px; border-radius: 6px;
        margin-bottom: 25px; border: 1px solid #b6b6d8;
      }
      .PIK-left-header { font-size: 20px; font-weight: 700; margin-bottom: 10px; color: #E37222; }
      .left-header-title { font-size: 20px; font-weight: 700; margin-bottom: 10px; color: #333366; }
      .left-header-text { font-size: 12px; font-weight: 500; margin-bottom: 8px; color: #444; }
      .left-header-authors { font-size: 12px; line-height: 1.4; color: #333; }
      
      .main-panel { margin-left: 520px; padding: 20px; }
      
      .figure-box {
        background-color: white; padding: 20px; margin-bottom: 40px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.12); width: fit-content;
        max-width: 1400px; border-radius: 4px;
      }
      
      .figure-caption {
        font-size: 13px; line-height: 1.6; color: #333;
        margin-top: 15px; padding-top: 15px; border-top: 1px solid #eee;
      }
      .figure-caption b { color: #000; font-weight: 700; }
    "))
  ),
  
  div(class = "left-panel",
      div(class = "left-header-box",
          div(class = "PIK-left-header", "PIK Interactive Webapp"),
          div(class = "left-header-title", "Cost competitiveness of alternative heavy-duty truck technologies under real-world utilisation profiles"),
          div(class = "left-header-authors",
              HTML("
                Johanna Hoppe&nbsp;<a href='https://orcid.org/0009-0004-6753-5090' target='_blank'><img src='https://orcid.org/sites/default/files/images/orcid_24x24.png' style='width:16px; height:16px;'/></a>
                Falko Ueckerdt&nbsp;<a href='#'><img src='https://orcid.org/sites/default/files/images/orcid_24x24.png' style='width:16px; height:16px;'/></a>
                Patrick Plötz&nbsp;<a href='#'><img src='https://orcid.org/sites/default/files/images/orcid_24x24.png' style='width:16px; height:16px;'/></a>
                Steffen Link&nbsp;<a href='#'><img src='https://orcid.org/sites/default/files/images/orcid_24x24.png' style='width:16px; height:16px;'/></a>
                Daniel Speth&nbsp;<a href='#'><img src='https://orcid.org/sites/default/files/images/orcid_24x24.png' style='width:16px; height:16px;'/></a>
                Bastian Weißenburger&nbsp;<a href='#'><img src='https://orcid.org/sites/default/files/images/orcid_24x24.png' style='width:16px; height:16px;'/></a>
                Pei Zhao&nbsp;<a href='#'><img src='https://orcid.org/sites/default/files/images/orcid_24x24.png' style='width:16px; height:16px;'/></a>
                Robert Pietzcker&nbsp;<a href='#'><img src='https://orcid.org/sites/default/files/images/orcid_24x24.png' style='width:16px; height:16px;'/></a>
              ")),
          div(class = "left-header-text", "Accompanying: Hoppe et al., Cost competitiveness of alternative heavy-duty truck technologies under real-world utilisation profiles.")
      ),
      
      tags$h3("Choose parameters"),
      selectInput("yr", "Year of analysis:", choices = c("2030", "2035", "2040")),
      selectInput("DCOscen", "DCO scenario:", choices = c("Medium", "Optimistic", "Pessimistic", "Custom")),
      
      tags$h3("Show exemplary DCO for"),
      selectInput("vehSize", "Truck type:", choices = c("Rigid", "Tractor-trailer")),
      selectInput("countryChoice", "Country:", choices = countryMapping$fullName),
      textInput("annualMileage", "Utilisation [km/yr]:", value = "160000"),
      
      tags$h3("Optional: Choose a custom DCO scenario below (you must first select it above)."),
      tags$h4("Battery electric truck (BET)"),
      selectInput("BETveh", "Vehicle parameter scenario:", choices = names(scenMapVeh), selected = "Medium cost and medium technical maturity"),
      selectInput("BETen", "Energy carrier scenario:", choices = names(scenMapEn), selected = "Business as usual"),
      
      tags$h4("Fuel cell electric truck (FCET)"),
      selectInput("FCETveh", "Vehicle parameter scenario:", choices = names(scenMapVeh), selected = "Medium cost and medium technical maturity"),
      selectInput("FCETen", "Energy carrier scenario:", choices = names(scenMapEn), selected = "Business as usual"),
      
      tags$h4("Diesel-ICET"),
      selectInput("Dveh", "Vehicle parameter scenario:", choices = names(scenMapVeh), selected = "Medium cost and medium technical maturity"),
      selectInput("Den", "Energy carrier scenario:", choices = names(scenMapEn), selected = "Business as usual")
  ),
  
  div(class = "main-panel",
      # --- FIGURE 1 BOX (Intro Panel) ---
      div(class = "figure-box",
          plotOutput("plot1", height = "auto"),
          div(class = "figure-caption", 
              HTML("<b>Fig.1 | Exemplary Differential cost of Ownership (DCO) of zero emission trucks (ZET) compared to their diesel ICET counterparts across utilisation profiles and vehicle-parameter/energy-carrier scenarios. a,</b> Share in road freight activity over annual mileage bin for considered truck types and markets. <b>b,</b> Scenario-based approach for DCO. Regarding performance and capital costs, we distinguish a low-cost & high technical maturity (LC_HTM), a medium cost & medium technical maturity (MC_MTM) and a high-cost & low technical maturity case (HC_LTM). Regarding energy carrier prices, we differentiate between a progressive (PROG) and a business-as-usual (BAU) end-use price pathway for the associated energy carrier of a truck technology, e.g., hydrogen for a fuel cell electric truck (FCET), electricity for a battery electric truck (BET) and diesel blend for an internal combustion engine truck (ICET). To incorporate our scenario-based approach in the DCO metric, we explore a medium case and two edge cases: The most optimistic and the most pessimistic combination for the BET/FCET under consideration. <b>c,</b> Exemplary DCO evaluation."))
           ),
      
      # --- FIGURE 2 BOX (Activity Distribution) ---
      div(class = "figure-box",
          plotOutput("plot2", height = "auto"),
          div(class = "figure-caption", 
              HTML("<b>Fig.2 |Economically viable road freight activity for the selected differential cost of ownership (DCO) scenario. a-c</b>, Heterogeneous DCO in 2030 in descending order and their cumulative share in total annual road freight activity in the considered markets. The total share of economically viable road freight activity for one-to-one replacement of internal combustion engine trucks (ICETs) with battery- (BETs) or fuel cell electric trucks (FCETs) is marked with arrows for each alternative. <b>d-f</b>, Evolution of the cumulative share of economically viable road freight activity over time for the respective DCO scenarios and alternative truck technologies."))
      )
  )
)

# ==============================================================================
# 3. SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  getDCOScenarios <- reactive({
    betStr  <- paste0(scenMapVeh[input$BETveh], "x", scenMapEn[input$BETen])
    fcetStr <- paste0(scenMapVeh[input$FCETveh], "x", scenMapEn[input$FCETen])
    diceStr <- paste0(scenMapVeh[input$Dveh], "x", scenMapEn[input$Den])
    scenarioSelection <- input$DCOscen
    scens <- as.data.table(tribble(
        ~truckTech,          ~truckTCOscenario, ~counterTech, ~counterTechTCOscenario, ~DCOscenario,
        "BET small battery", betStr,           "ICET",        diceStr,                 "Custom",
        "BET large battery", betStr,           "ICET",        diceStr,                 "Custom",
        "FCET",              fcetStr,          "ICET",        diceStr,                 "Custom",
        "BET small battery", "MC_MTMxBAU",     "ICET",        "MC_MTMxBAU",            "Medium",
        "BET large battery", "MC_MTMxBAU",     "ICET",        "MC_MTMxBAU",            "Medium",
        "FCET",              "MC_MTMxBAU",     "ICET",        "MC_MTMxBAU",            "Medium",
        "BET small battery", "LC_HTMxPROG",    "ICET",        "HC_LTMxBAU",            "Optimistic",
        "BET large battery", "LC_HTMxPROG",    "ICET",        "HC_LTMxBAU",            "Optimistic",
        "FCET",              "LC_HTMxPROG",    "ICET",        "HC_LTMxBAU",            "Optimistic",
        "BET small battery", "HC_LTMxBAU",     "ICET",        "LC_HTMxPROG",           "Pessimistic",
        "BET large battery", "HC_LTMxBAU",     "ICET",        "LC_HTMxPROG",           "Pessimistic",
        "FCET",              "HC_LTMxBAU",     "ICET",        "LC_HTMxPROG",           "Pessimistic"
      ))
    return(scens[DCOscenario == scenarioSelection])
  })

  output$plot1 <- renderPlot({
    baseTextSize <- 4
    countryCode <- countryMapping[fullName == input$countryChoice, country]
    
    TCOdata <- loadTCO(dataFolder)
    TCO <- applyMileage(as.numeric(input$annualMileage), TCOdata)
    TCO <- groupParameters(TCO)
    reducedBinnedMileage <- prepareMileageData(dataFolder, bin = TRUE, reduce = TRUE)
    
    # Using your global plotSettings
    scenariosPlot <- scenOverview(baseSize, paperColors, baseTextSize)
    utilizationExample <- examplaryUtilisation(countryCode, reducedBinnedMileage, baseTextSize)

    TCOtoDCOexample <- dcoBarPlot(TCO, getDCOScenarios(), as.numeric(input$yr), 
                                  input$vehSize, countryCode, input$annualMileage, 
                                  "Current policies", baseTextSize)
    
    # 1. Row 1: Activity + Scenarios
    row1 <- plot_grid(utilizationExample, ggplotGrob(scenariosPlot), 
                      ncol = 2, rel_widths = c(3, 3.5))
    
    # 2. Consistent white spacer
    whiteSpace <- ggdraw() + 
      theme_void() + 
      theme(plot.background = element_rect(fill = "white", color = NA))
    
    # 3. Vertical Stack (Exact proportions kept)
    introPanel <- plot_grid(
      whiteSpace, 
      row1, 
      whiteSpace, 
      TCOtoDCOexample, 
      whiteSpace, 
      ncol = 1, 
      rel_heights = c(0.16, 1.9, 0.16, 3, 0.05)
    )
    
    # 4. Final Draw with Labels
    ggdraw(introPanel) +
      theme(plot.background = element_rect(fill = "white", color = NA)) +
      draw_label("×", x = 0.485, y = 0.77, size = 15, fontface = "bold", alpha = 0.3) +
      draw_plot_label(
        label = c("a", "b", "c"), 
        x = c(0.01, 0.46, 0.01), 
        y = c(0.98, 0.98, 0.59), 
        size = baseTextSize * 1.2, 
        fontface = "bold"
      )
    
  }, width = 4 * 300, height = 3.9 * 300, res = 300, bg = "white")
  
  output$plot2 <- renderPlot({
    baseTextSize <- 6
    TCOdata <- loadTCO(dataFolder)
    mileageDistribution <- prepareMileageData(dataFolder)
    DCOdata <- getDCO(getDCOScenarios(), TCOdata)
    setnames(mileageDistribution, "binMean", "annualM")
    
    DCOmilageDistribution <- applyMileage(mileageDistribution, DCOdata)
    DCOmilageDistributionShares <- calculateFreightActivityShares(DCOmilageDistribution, focus = "EUR")
    
    # Pass variables explicitly to functions
    TCOxAnnualMileage <- amDistributionBarPlot(DCOmilageDistributionShares, as.numeric(input$yr), "Current policies", baseTextSize = 6)
    
    # Meta plot logic
    dataMeta <- copy(DCOmilageDistributionShares)[period < 2041 & paperScen %in% "Current policies"]
    findZeroPoints <- dataMeta[ , .SD[which.min(abs(value))], by = .(period, truckTechnology, paperScen, DCOscenario)][
      , .(period, truckTechnology, paperScen, DCOscenario, cumBinWeightedShareEUR)
    ]
    
    MetaTCOxAnnualMileage <- ggplot(findZeroPoints, 
                                    aes(x = period, y = cumBinWeightedShareEUR * 100, color = truckTechnology, linetype = paperScen)) +
      geom_line(linewidth = baseLineWidth) +
      scale_color_manual(values = paperColors, guide = "none") + 
      scale_linetype_manual(values = paperLines, guide = "none") +
      geom_vline(xintercept = 2030, linetype = "dashed", color = "darkgrey", linewidth = baseLineWidth) +
      labs(y = "Economically viable\nactivity [%]", x = "Year") +
      scale_x_continuous(breaks = seq(min(findZeroPoints$period), max(findZeroPoints$period), by = 5)) +
      scale_y_continuous(limits = c(0, 102)) +
      plotTheme(baseTextSize)
    
    legend <- get_legend(TCOxAnnualMileage + theme(legend.position = "bottom", legend.justification = "left"))
    
    # Combined subplots
    combined <- plot_grid(
      TCOxAnnualMileage + theme(legend.position = "none"),
      MetaTCOxAnnualMileage + theme(legend.position = "none"),
      ncol = 2, rel_widths = c(1.2, 0.8), align = "v", labels = c("a", "b"), label_size = baseTextSize
    ) + theme(plot.margin = margin(t = 8, r = 3, b = 3, l = 15)) # Increased left margin for scenario label
    
    # Main layout
    finalPlot <- plot_grid(combined, 
                           plot_grid(NULL, legend, ncol = 2, rel_widths = c(0.05, 0.3)), 
                           ncol = 1, rel_heights = c(1, 0.1))
    
    # Drawing the canvas
    canvas <- ggdraw(finalPlot) +
      # Centered label on the y-axis
      draw_label(input$DCOscen, x = 0.02, y = 0.58, angle = 90, 
                 fontface = "bold", size = baseTextSize * 1.1, color = "black", hjust = 0.5)
    
    return(canvas)
    
  }, width = 4.5 * 300, height = 2.2 * 300, res = 300, bg = "transparent")

}

shinyApp(ui, server)