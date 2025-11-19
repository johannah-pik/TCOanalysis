paperColors <- c(
  "CO2 tax" = "#2E8B57", 
  "Toll charge" = '#66C26F', 
  "Vehicle tax" = '#889c4e', 
  "Fuel cost" = '#A8E6A1', 
  "M&R + tires" = '#14532D',  
  "H2 tank" = '#c2d7e1', 
  "Fuel cell\nsystem" = '#858ca7', 
  "Battery" =  '#96a8f2', 
  "Vehicle\nbody incl.\nengine" = '#040f3e',
  "Vehicle body incl. engine" = '#040f3e',
  "MCS" = "#2A0068",      
  "CCS" = "#AD85E8", 
  "Hydrogen" = "#C94A64",  
  "Mix" = "#077001",
  "Bio" = "#0ee302", 
  "Syn" = "#968d21",  
  "Fossil" = "black", 
  "BET small battery" = "#17BECF", 
  "Small\nbattery" = "#17BECF",
  "BET" = "#17BECF", 
  "BET large battery" = "#9467BD", 
  "Large\nbattery" = "#9467BD", 
  "FCET" = "#E65C00",
  "Battery costs" = "#17BECF",
  "Fuel cell system costs" = "#E65C00",
  "Diesel" = "black",
  "Rigid" = "#008080",         
  "Tractor-trailer" = "#FF6F61",
  "LC_HTM" = "#6baed6", 
  "MC_MTM" = "#4292c6",  
  "HC_LTM" = "#2171b5",  
  "PROG"   = "#74c476",  
  "BAU"    = "#31a354",
  "Optimistic" = "#607d8b",
  "Medium"     = "#607d8b",
  "Pessimistic"= "#607d8b"
)

baseTextSize <- 7
baseLineWidth <- 0.5
themePanel <- theme_minimal(base_size = baseTextSize) +
  theme(
    axis.title.x  = element_text(size = baseTextSize, lineheight = 0.8),
    axis.title.y  = element_text(size = baseTextSize, lineheight = 0.8),
    axis.text     = element_text(size = baseTextSize * 0.8),
    axis.ticks.y = element_line(),
    axis.ticks.x = element_line(),                   
    axis.line.y = element_line(),
    axis.line.x = element_line(),                   
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    plot.title    = element_text(size = baseTextSize, face = "bold", hjust = 0.5),
    strip.text    = element_text(size = baseTextSize, face = "bold"),
    legend.key.width = unit(0.3, "cm"),       
    legend.key.height = unit(0.2, "cm"),
    legend.title  = element_text(size = baseTextSize),
    legend.text   = element_text(size = baseTextSize),
    plot.margin   =  margin(t = 3, r = 3, b = 3, l = 3)
  )

  

# convert baseTextSize in pt to mm (mult is scaling factor)
relSize <- function(mult = 1) {
  baseTextSize * mult / .pt   # .pt = 2.845 (in ggplot2 definiert)
}

