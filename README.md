# Research software for analysing the total cost of ownership (TCO) of alternative technologies for heavy-duty trucks under real-world utilisation profiles

## Summary
This repository make source codes and input data publicly available that were used in the research article "Cost competitiveness of alternative heavy-duty
truck technologies under real-world utilisation profiles".

## How  to cite this work
Hoppe, Johanna; Ueckerdt, Falko; Plötz, Patrick; Link, Steffen; Speth, Daniel; Weißenburger, Bastian; Zhao, Pei; Pietzcker, Robert (2025): Research software for analysing the total cost of ownership (TCO) of alternative technologies for heavy-duty trucks under real-world utilisation profiles.

## How to use this software
- Clone repository and setup R environment:
    1. Open project in your preferred IDE (RStudio, VS Code, etc.).
    2. To save disk space, enable global cache: renv::settings$use.cache(TRUE)
    3. Run renv::restore() to install required package versions.
- TCOmodel.R reads in parameter raw data from data/TCOparameter
	and saves calculated output in data/TCOanalysis.
- The figures and analyses in the research article can be reproduced with the help of plottingScripts/mainFigures.RMD and plottingScripts/extendedAndSupplementaryFigures.Rmd
- Resulting figures are stored in plots/main or plots/supplementary
- Running the interactiveWebapp.R allows users to combine different vehicle parameter and energy carrier
  scenarios to visualize the DCO and explore results for specific countries and utilisation
  profiles.

## System requirements: 
R (version 4.3.2). All package dependencies are managed via renv and listed in renv.lock. 
No non-standard hardware required; tested on a standard x64-based desktop environment (e.g., Intel Core i7, 16GB RAM) running on Windows 11 Pro.
