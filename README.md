# Research software for Total Cost of Ownership (TCO) analysis of alternative heavy-duty truck technologies under real-world utilisation profiles

## Summary
This repository make source codes and input data publicly available that were used in the research article "Cost competitiveness of alternative heavy-duty
truck technologies under real-world utilisation
profiles".


## How  to cite this work
Hoppe, Johanna; Ueckerdt, Falko; Plötz, Patrick; Link, Steffen; Speth, Daniel; Weißenburger, Bastian; Zhao, Pei; Pietzcker, Robert (2025): Research software for Total Cost of Ownership (TCO) analysis of alternative heavy-duty
truck technologies under real-world utilisation profiles 

## How to use this software
- Clone repository and setup R environment using renv::restore()
- TCOmodel.R reads in parameter raw data from data/TCOparameter
	and saves calculated output in data/TCOanalysis.
- The figures and analyses in the research article can be reproduced with the help of plottingScripts/mainFigures.RMD and plottingScripts/extendedAndSupplementaryFigures.Rmd
- Resulting figures are stored in plots/main or plots/supplementary