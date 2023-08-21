#Script that creates the tree_maps of the simulations. 
rm(list = ls())

library(tidyverse)
library(RODBC)

setwd("~/Temperate_species_colonisation_in_mixedwood_boreal_forest_with_climate_change")

t_files_dir <- dir("~/initial_conditions/tree_maps", full.names=T)

t_files <- str_remove(t_files_dir, "initial_conditions/tree_maps/")
t_files <- str_remove(t_files, ".txt")

for(i in 1:length(t_files_dir)){ 
  tree_map_name <- c(paste0("~/initial_conditions/tree_maps/", t_files[i], "_noCC_1991.txt"),
                     paste0("~/initial_conditions/tree_maps/", t_files[i], "_rcp26_1991.txt"),
                     paste0("~/initial_conditions/tree_maps/", t_files[i], "_rcp45_1991.txt"), 
                     paste0("~/initial_conditions/tree_maps/", t_files[i], "_rcp85_1991.txt"))
  file.copy(t_files_dir[i], tree_map_name)
}


#Position of the trees in the central part of the simulation, data come from the RESEF dataset under licence.
big_patch <- readRDS(file="~/initial_conditions/big_patch.rds")


#Functions to create patch of different size

create_patch <- function(patch, size){
  patch %>% 
    mutate(d=(X-22.5)^2+(Y - 18.75)^2) %>% 
    filter(d<size/pi) %>% 
    dplyr::select(-d) %>% 
    mutate(X=X+50-22.5 ,
           Y=Y+50-18.75)
}

#remove the patch for control simulations
remove_patch_control <-  function(control, size){
  control %>% 
    mutate(d=(X-50)^2+(Y - 50)^2) %>% 
    filter(d>size/pi) %>% 
    dplyr::select(-d)
}

files <- dir("~/initial_conditions/tree_maps/", full.names=F)

init_patch_size <- c(10, 30, 80, 150, 250, 400)
for(i in files){ #i=files[1]
  control <- read.table(paste0("~/initial_conditions/tree_maps/",i), h=T)
  
  for(patch_size in init_patch_size){
    patch <- create_patch(patch=big_patch, size=patch_size)
    control_without_patch <- remove_patch_control(control=control, size=patch_size)
    
    #ERS
    ers <- bind_rows(control_without_patch, patch)
    name_ers <- str_replace(i, "control_NA", paste0("ERS_", patch_size))
    write.table(x = ers, file = paste0("~/initial_conditions/tree_maps/", name_ers), sep="\t", row.names=F, quote=F)
    
    #ERR
    err <- ers %>% #We take the ers patch and we make as if they were err to have the same initial conditions. 
      mutate(Species=ifelse(Species=="Sugar_Maple", "Red_Maple",Species))
    name_err <- str_replace(i, "control_NA", paste0("ERR_", patch_size))
    write.table(x = err, file = paste0("~/initial_conditions/tree_maps/", name_err), sep="\t", row.names=F, quote=F)
    
    #ERR
    boj <- ers %>% 
      mutate(Species=ifelse(Species=="Sugar_Maple", "Yellow_Birch",Species))
    name_boj <- str_replace(i, "control_NA", paste0("BOJ_", patch_size))
    write.table(x = boj, file = paste0("~/initial_conditions/tree_maps/", name_boj), sep="\t", row.names=F, quote=F)
  }
}
