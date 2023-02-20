#Script that creates the tree_maps of the simulations for the sensitivity analysis of MaxpotGrowth. 
rm(list = ls())

library(tidyverse)
library(RODBC)

setwd("~/PhD/chap3/submit")

options(scipen = 999)

t_files_dir <- dir("~/sensitivity_analysis/tree_maps", full.names=T)
t_files_dir <- t_files_dir[str_detect(string = t_files_dir, pattern = "1964")]
t_files_dir <- t_files_dir[str_detect(string = t_files_dir, pattern = "_150_rcp45_")]

t_files <- str_remove(t_files_dir, "initial_conditions/tree_maps/")
t_files <- str_remove(t_files, "_1991.txt")

percent <- c(0.1, 0.2, 0.4, 0.6, 0.8, 1.2, 1.4, 1.6, 1.8, 2)

parameter_df <- data.frame(species=c(rep("ERS", 10),rep("ERR", 10),rep("BOJ", 10)),
                           parameter=rep("maxpotgrowth", 30),
                           sens_value=c(percent,percent,percent),
                           crossing="no") %>% 
  rbind(data.frame(species=c(rep("ERS", 2),rep("ERR", 2),rep("BOJ", 2)),
                   parameter=rep("maxpotgrowth", 6),
                   sens_value=c("ERR", "BOJ","ERS", "BOJ", "ERS", "ERR"),
                   crossing=c("ERR", "BOJ","ERS", "BOJ", "ERS", "ERR"))) 

saveRDS(parameter_df, file="/sensitivity_analysis/sensitivity_analysis_design_percent_maxptogrowth.rds")

sp <- c("BOJ", "ERR", "ERS")
for(i in 1:length(t_files_dir)){ #i=1
  parameter_df_sp <- filter(parameter_df, species==sp[i])
  for(j in 1:nrow(parameter_df_sp)){ #j=1
    tree_map_name <- paste0("sensitivity_analysis/tree_maps_sensitivity/", 
                            t_files[i], "_",
                            parameter_df_sp[j,2], "_", parameter_df_sp[j,3],  
                            "_1991.txt")
    file.copy(t_files_dir[i], tree_map_name)
  }
}
