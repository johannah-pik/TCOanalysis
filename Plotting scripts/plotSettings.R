paperColors <- c(
  "CO2 tax" = "#2E8B57", 
  "Toll charge" = '#66C26F', 
  "Vehicle tax" = '#889c4e', 
  "Fuel cost" = '#A8E6A1', 
  "M&R + tires" = '#14532D',  
  "H2 tank" = '#c2d7e1', 
  "Fuel cell system" = '#858ca7', 
  "Battery" =  '#96a8f2', 
  "Vehicle body\nincl. engine" = '#040f3e',
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
  "Battery\nsystem\nprices" = "#17BECF",
  "Fuel cell\nsystem\nprices" = "#E65C00",
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
themePanel <- theme_minimal(base_size = baseTextSize) +
  theme(
    axis.text.y = element_text(size = rel(0.9)),
    axis.text.x = element_text(size = rel(0.9)),       
    axis.ticks.y = element_line(),
    axis.ticks.x = element_line(),                   
    axis.line.y = element_line(),
    axis.line.x = element_line(),                   
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    plot.title = element_text(size = rel(1), face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = rel(1)), # , margin = margin(r = 12)
    axis.title.x = element_text(size = rel(1)),#, margin = margin(t = 12)
    strip.text = element_text(face = "bold"),
    legend.key.width = unit(0.2, "cm"),       
    legend.key.height = unit(0.2, "cm"),
    axis.text = element_text(size = rel(1)),
    legend.text  = element_text(size = rel(1)),   
    legend.title = element_text(size = rel(1))
  )

  

# convert baseTextSize in pt to mm (mult is scaling factor)
relSize <- function(mult = 1) {
  baseTextSize * mult / .pt   # .pt = 2.845 (in ggplot2 definiert)
}

