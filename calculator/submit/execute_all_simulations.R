#Script that launches all timelines
rm(list = ls())


library(tidyverse)

sim <- str_remove(dir("../tree_maps/"), "_1991.txt")

for(j in sim){ #j="1760_ERS_low_rcp26"
  system(paste0("sbatch -J ", j, " --export=vars='", j,"' submit_one_set_simulation.sh")) 
} #OK

#Run an sbatch that waits for everything to be finished and then runs the output conversion file
system(paste0("sbatch submit_wait_convert.sh"))