library(shiny)

ui <- fluidPage(
  tags$head(
      tags$style(HTML("
      /* app background */
      body, html {
      background-color: #e5e5e5;  /* light grey background for app */
      }
      .left-panel {
        width: 700px;
        position: fixed;
        top: 0;
        bottom: 0;
        left: 0;
        padding: 20px;
        background-color: #fafafa;
        overflow-y: auto;
        border-right: 1px solid #ddd;
      }

      .left-header-box {
        background-color: #f0f0ff;
        padding: 18px;
        border-radius: 6px;
        margin-bottom: 25px;
        border: 1px solid #b6b6d8;
      }
      
      .PIK-left-header {
        font-size: 20px;
        font-weight: 700;
        margin-bottom: 10px;
        color: #E37222;
      }

      .left-header-title {
        font-size: 20px;
        font-weight: 700;
        margin-bottom: 10px;
        color: #333366;
      }

      .left-header-text {
        font-size: 12px;
        font-weight: 500;
        margin-bottom: 8px;
        color: #444;
      }

      .left-header-authors {
        font-size: 12px;
        line-height: 1.4;
        color: #333;
      }
     .main-panel {
      margin-left: 720px;
      padding: 20px;
      }
      .plot-row {
      display: flex;
      flex-direction: row;
      gap: 20px;
      overflow-x: auto;    /* THIS makes it scrollable */
      overflow-y: hidden;
      }

      /* Prevent the plots from shrinking */
      .plot-row > * {
      flex: 0 0 auto;
      }
    "))
  ),
  
  div(class = "left-panel",
      
      # --- HEADER BLOCK ---
      div(class = "left-header-box",
          
          # Title
          div(class = "PIK-left-header",
              "PIK Interactive Webapp"
          ),
          div(class = "left-header-title",
              "Cost competitiveness of alternative heavy-duty truck technologies under real-world utilisation profiles"
          ),
          
          # Add multiple lines of authors, hyperlinks, etc.
          div(class = "left-header-authors",
              HTML("
                    Johanna Hoppe&nbsp;
                    <a href='https://orcid.org/0009-0004-6753-5090' target='_blank' aria-label='ORCID iD'>
                      <img src='https://orcid.org/sites/default/files/images/orcid_24x24.png' alt='ORCID iD' style='vertical-align:middle; width:16px; height:16px;'/> </a>
                       Falko Ueckerdt&nbsp;
                    <a href='https://orcid.org/0009-0004-6753-5090' target='_blank' aria-label='ORCID iD'>
                      <img src='https://orcid.org/sites/default/files/images/orcid_24x24.png' alt='ORCID iD' style='vertical-align:middle; width:16px; height:16px;'/> </a>
                      Patrick Plötz&nbsp;
                    <a href='https://orcid.org/0009-0004-6753-5090' target='_blank' aria-label='ORCID iD'>
                      <img src='https://orcid.org/sites/default/files/images/orcid_24x24.png' alt='ORCID iD' style='vertical-align:middle; width:16px; height:16px;'/> </a>
                      Steffen Link&nbsp;
                    <a href='https://orcid.org/0009-0004-6753-5090' target='_blank' aria-label='ORCID iD'>
                      <img src='https://orcid.org/sites/default/files/images/orcid_24x24.png' alt='ORCID iD' style='vertical-align:middle; width:16px; height:16px;'/> </a>
                     Daniel Speth&nbsp;
                    <a href='https://orcid.org/0009-0004-6753-5090' target='_blank' aria-label='ORCID iD'>
                      <img src='https://orcid.org/sites/default/files/images/orcid_24x24.png' alt='ORCID iD' style='vertical-align:middle; width:16px; height:16px;'/> </a>
                      Bastian Weißenburger&nbsp;
                    <a href='https://orcid.org/0009-0004-6753-5090' target='_blank' aria-label='ORCID iD'>
                      <img src='https://orcid.org/sites/default/files/images/orcid_24x24.png' alt='ORCID iD' style='vertical-align:middle; width:16px; height:16px;'/> </a>
                      Pei Zhao&nbsp;
                    <a href='https://orcid.org/0009-0004-6753-5090' target='_blank' aria-label='ORCID iD'>
                      <img src='https://orcid.org/sites/default/files/images/orcid_24x24.png' alt='ORCID iD' style='vertical-align:middle; width:16px; height:16px;'/> </a>
                      Robert Pietzcker&nbsp;
                    <a href='https://orcid.org/0009-0004-6753-5090' target='_blank' aria-label='ORCID iD'>
                      <img src='https://orcid.org/sites/default/files/images/orcid_24x24.png' alt='ORCID iD' style='vertical-align:middle; width:16px; height:16px;'/> </a>
                    </a>
            ")
          ),
          div(class = "left-header-text",
              "Accompanying: Hoppe et al., Cost competitiveness of alternative heavy-duty truck technologies under real-world utilisation profiles."
          ),
      ),
      
      # --- INPUTS BELOW THE HEADER ---
      tags$h4("Choose parameters"),
      selectInput("yr", "Year of analysis:",
                  choices = c("2030", "2040", "2050")),
      tags$h4("Show exemplary DCO for"),
      selectInput("vehSize", "Truck type:",
                  choices = c("Rigid", "Tractor-trailer")),
      selectInput("countryChoice", "Country:",
                  choices = c("Germany", "Spain", "France", "Italy", "Netherlands", "Poland", "United Kingdom")),
      textInput(
        inputId = "annualMileage",
        label = "Utilisation [km/yr]:",
        value = "160000"   # optional default text
      ),
      tags$h3("Optional: Specify own sensitivity analysis"),
      
      tags$h4("Battery electric truck (BET)"),
      selectInput("BETveh", "Vehicle parameter scenario:",
                  choices = c("Low cost and high technical maturity",
                              "Medium cost and medium technical maturity",
                              "High cost and low technical maturity")),
      selectInput("BETen", "Energy carrier scenario:",
                  choices = c("Business as usual", "Progressive")),
      
      tags$h4("Fuel cell electric truck (FCET)"),
      selectInput("FCETveh", "Vehicle parameter scenario:",
                  choices = c("Low cost and high technical maturity",
                              "Medium cost and medium technical maturity",
                              "High cost and low technical maturity")),
      selectInput("FCETen", "Energy carrier scenario:",
                  choices = c("Business as usual", "Progressive")),
      
      tags$h4("Diesel-ICET"),
      selectInput("Dveh", "Vehicle parameter scenario:",
                  choices = c("Low cost and high technical maturity",
                              "Medium cost and medium technical maturity",
                              "High cost and low technical maturity")),
      selectInput("Den", "Energy carrier scenario:",
                  choices = c("Business as usual", "Progressive"))
  ),
  div(
    class = "main-panel",
    
    # Add a row container
    div(
      class = "plot-row",
      plotOutput("plot1", height = "1030px", width = "800px"),
      plotOutput("plot2", height = "1050px", width = "2000px")   # match height for alignment
    )
  )
)

server <- function(input, output, session) {
  
  library(ggplot2)
  library(data.table)
  library(RColorBrewer)
  library(ggtext)
  library(ggh4x)
  library(ggforce)
  library(patchwork)
  library(scales)
  library(colorspace)
  library(viridisLite)
  library(cowplot)
  library(funkyheatmap)
  library(stringr)
  library(grid)
  library(tibble)
  
  mainFolder <- this.path::this.dir()
  dataFolder <-  file.path(mainFolder, "data")
  plotFolder <- file.path(mainFolder, "plots", "main")
  source(file.path(mainFolder, "PlottingScripts", "functions.R"))
  source(file.path(mainFolder, "PlottingScripts", "plotFunctions.R"))
  source(file.path(mainFolder, "PlottingScripts", "plotSettings.R"))

  baseLineWidth <- 1
  
  DCOscenarios <- tribble(
    ~truckTech,         ~truckTCOscenario, ~counterTech, ~counterTechTCOscenario, ~DCOscenario,
    "BET small battery", "LC_HTMxPROG",    "ICET",       "HC_LTMxBAU",            "Optimistic",
    "BET large battery", "LC_HTMxPROG",    "ICET",       "HC_LTMxBAU",            "Optimistic",
    "FCET",              "LC_HTMxPROG",    "ICET",       "HC_LTMxBAU",            "Optimistic",
    "BET small battery", "MC_MTMxBAU",     "ICET",       "MC_MTMxBAU",            "Medium",
    "BET large battery", "MC_MTMxBAU",     "ICET",       "MC_MTMxBAU",            "Medium",
    "FCET",              "MC_MTMxBAU",     "ICET",       "MC_MTMxBAU",            "Medium",
    "BET small battery", "HC_LTMxBAU",     "ICET",       "LC_HTMxPROG",           "Pessimistic",
    "BET large battery", "HC_LTMxBAU",     "ICET",       "LC_HTMxPROG",           "Pessimistic",
    "FCET",              "HC_LTMxBAU",     "ICET",       "LC_HTMxPROG",           "Pessimistic"
  )
  setDT(DCOscenarios)
  
  countryMapping <- tribble(
    ~ country, ~ fullName,
    "DE",       "Germany",
    "ES",       "Spain",
    "FR",       "France",
    "IT",       "Italy",
    "NL",       "Netherlands",
    "PL",       "Poland",
    "UK",       "United Kingdom"
  )
  setDT(countryMapping)
  paperScenario <- "Current policies"  

  output$plot1 <- renderPlot({
    TCOdata <- loadTCO(dataFolder)
    mileageDistribution <- prepareMileageData(dataFolder)
    DCOdata <- getDCO(DCOscenarios, TCOdata)
    setnames(mileageDistribution, "binMean", "annualM")
    DCOmilageDistribution <- applyMileage(mileageDistribution, DCOdata)
    DCOmilageDistributionShares <- calculateFreightActivityShares(DCOmilageDistribution, focus = "EUR")

    TCOxAnnualMileage <- amDistributionBarPlot(DCOmilageDistributionShares, input$yr, paperScenario) +
             theme(plot.margin = margin(t = 10, r = 70, b = 120, l = 50))
    
    whiteSpace <- ggdraw() + theme_void() + 
      theme(plot.background = element_rect(fill = "white", color = NA))
    
    TCOxAnnualMileage <- plot_grid(
      whiteSpace,
      TCOxAnnualMileage,
      whiteSpace,
      ncol = 1,
      rel_heights = c(0.02, 1, 0.02)
    )
    
    ggdraw() +
      draw_plot(TCOxAnnualMileage) + 
      draw_label(
        "Optimistic", x = 0.04, y = 0.76, angle = 90,
        fontface = "bold",
        fontfamily = "sans",
        size = 16 * 1.2,
        hjust = 0
      ) +
      draw_label(
        "Medium", x = 0.04, y = 0.5, angle = 90,
        fontface = "bold",
        fontfamily = "sans",
        size = 16 * 1.2,
        hjust = 0
      ) +
      draw_label(
        "Pessimistic", x = 0.04, y = 0.185, angle = 90,
        fontface = "bold",
        fontfamily = "sans",
        size = 16 * 1.2,
        hjust = 0
      ) +
      draw_grob(
        gridtext::richtext_grob(
        "<b>Fig.1|Economically viable HDV road freight activity,</b><br>
         Heterogeneous differential cost of Owndership (DCO) in descending order and their cumulative share<br>
         in total annual road freight activity in the considered markets. The total share of economically<br> 
         viable road freight activity for one-to-one replacement of ICETs by BETs/FCETs is marked with<br> 
         arrows for each alternative.", 
        x = 0.04, 
        y = 0.07, 
        hjust = 0, 
        gp = gpar(fontsize = 14, fontfamily = "sans", col = "black")
        )
      )
    
  })

  output$plot2 <- renderPlot({
    TCOdata <- loadTCO(dataFolder)
    TCO <- applyMileage(as.numeric(input$annualMileage), TCOdata)
    TCO <- groupParameters(TCO)
    countryCode <- countryMapping[fullName == input$countryChoice]$country
    scenariosPlot <- scenOverview(baseZise, paperColors, 14) 
    reducedBinnedWeightedMileage <- prepareMileageData(dataFolder, bin = TRUE, reduce = TRUE)
    utilizationExample <- examplaryUtilisation(countryCode, reducedBinnedWeightedMileage, 16)
    TCOtoDCOexample <- dcoBarPlot(TCO, DCOscenarios, input$yr, input$vehSize,countryCode, paperScenario, 14)
    
    gScenarios <- ggplotGrob(scenariosPlot)
    
    row1 <- plot_grid(
      utilizationExample, gScenarios,
      ncol = 2,
      rel_widths = c(3, 3.5)
    )
    
    row2 <- plot_grid(
      TCOtoDCOexample,
      ncol = 1
    )
    
    whiteSpace <- ggdraw() + theme_void() + 
      theme(plot.background = element_rect(fill = "white", color = NA))
    
    introPanel <- plot_grid(
      whiteSpace,
      row1,
      whiteSpace,
      row2,
      whiteSpace,
      ncol = 1,
      rel_heights = c(0.2, 1.5, 0.2, 2.7, 0.2)
    )
    xMiddle <- 0.485
    triangleWidth  <- 0.06      
    triangleHeight <- 0.017      
    triangleY <- 8.3 / 13
    
    # Create the triangle grob
    triangleGrob <- polygonGrob(
      x = c(xMiddle - triangleWidth / 2, xMiddle + triangleWidth / 2, xMiddle),
      y = c(triangleY, triangleY, triangleY - triangleHeight),
      gp = gpar(fill = "black", col = NA, alpha = 0.3)
    )
    
    ggdraw(introPanel) +
      draw_label(
        "×",
        x = xMiddle,
        y = 10 / 13,
        size = 20,
        fontface = "bold",
        color = "black",
        alpha = 0.3
      ) +
      draw_grob(triangleGrob) +
      draw_plot_label(
        label = c("a", "b", "c"),
        x = c(0, xMiddle - 0.025, 0),       
        y = c(1, 1, 0.57 + 0.005),           
        hjust = 0, vjust = 1,
        size = 16,
        fontface = "bold",
        color = "black"
      ) +
      draw_plot_label(
        label = c(
          "Country- and truck type specific\nutilisaton profiles",
          "Scenario based approach",
          "Country- and truck type specific differential cost of ownership (DCO)"
        ),
        x = c(0.03, xMiddle, 0.03),
        y = c(0.995, 0.995, 0.57),
        hjust = 0, vjust = 1,
        size = 16 * 1.2,
        fontface = "plain",
        color = "black"
      ) +
      draw_grob(
        gridtext::richtext_grob(
          "<b>Fig.2|Exemplary DCO across utilisation profiles and vehicle-parameter/energy-carrier scenarios.,</b><br>
          <b>a</b>, Share in road freight activity over annual mileage bin for considered truck types selected market.<br> 
          <b>b</b>, Scenario-based approach for DCO. c, Exemplary DCO evaluation", 
          x = 0.02, 
          y = 0.05, 
          hjust = 0, 
          gp = gpar(fontsize = 14, fontfamily = "sans", col = "black")
        )
      )
    
    
    
    
    
  })


  
}

shinyApp(ui, server)