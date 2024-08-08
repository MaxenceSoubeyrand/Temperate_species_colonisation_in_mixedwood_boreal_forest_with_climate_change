rm(list = ls())


library(tidyverse)
library(rsortie)
library(R.utils)


simulations <- str_subset(list.files(path = "../tree_maps/"), "tree_map_edit.R", negate = TRUE)

for(i in simulations){
  print(paste0("sbatch -J ", i, " --export=vars='", i,"' submit_one_simulation.sh"))
  system(paste0("sbatch -J ", i, " --export=vars='", i,"' submit_one_simulation.sh"))
}
