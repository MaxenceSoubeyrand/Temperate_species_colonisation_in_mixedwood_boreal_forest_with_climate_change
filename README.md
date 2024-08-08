# Effect of climate and competition on future northward migration of temperate tree species into mixedwood boreal forest

This repository contains the R code and data to retrieve the results and graphs from the "DOI of the article when available" study. We aimed to determine if future climate and competition are factors that prevent the colonization of temperate species, i.e., sugar maple, red maple, and yellow birch, from becoming established just above their continuous northern limit of distribution. 

## Citation 

Soubeyrand M., Gennaretti F., Brice M.H., Bergeron Y., Grondin P. and Marchand P. (submitted) Increase in young forests, more than climate change may accelerate future colonization of temperate tree species in mixedwood boreal stands . *Ecological Modelling*

Please cite this article for any work that re-uses this code.

## Data availability 

Initial conditions from Lake Duparquet Research and Teaching Forest (FERLD):

- Initial conditions for unharvested stands: Maleki, K., Marchand, P., Charron, D., Lafleur, B., & Bergeron, Y. (2021). A 249‐yr chronosequence of forest plots from eight successive fires in the Eastern Canada boreal mixedwoods. Ecology, 102(5). https://doi.org/10.1002/ecy.3306

- Initial conditions for harvested stands: Brais, S., Work, T., Robert, É., O’Connor, C., Strukelj, M., Bose, A., Celentano, D., & Harvey, B. (2013). Ecosystem Responses to Partial Harvesting in Eastern Boreal Mixedwood Stands. Forests, 4(2), 364–385. https://doi.org/10.3390/f4020364

## R packages required

- The following [tidyverse](https://www.tidyverse.org) packages: 
*dplyr*, *tidyr*, *stringr* and *ggplot*;
- the *sp*, *rgdal*, *maps*, *ggnewscale*, *ggspatial* and *raster* packages to manipulate and display spatial objects;
- the *brms*, *quantreg*, *cmdstanr* and *loo* packages to fit Bayesian model and perform model selections;
- the *xml2* package to manipulate SORTIE-ND parameter files;
- the *factoextra* and *FactoMineR* packages to compute PCA; and
- the *ggpubr*, *grid*, *ggplotify*, *gtable* and *RColorBrewer* packages working with *ggplot* package using to display results. 

## Acknowledgement

We thank the Natural Science and Engineering Research Council of Canada to support financially the project. This research was enabled in part by computer resources provided by [Digital Research Alliance of Canada](https://www.alliancecan.ca/en). We thank anonymous reviewers from Corrige-moi UQAT students’ group for critical revision and suggestions on the manuscript. 

## Notes 
We used the computing resources of the Digital Research Alliance of Canada to execute the simulations with SORTIE-ND.
