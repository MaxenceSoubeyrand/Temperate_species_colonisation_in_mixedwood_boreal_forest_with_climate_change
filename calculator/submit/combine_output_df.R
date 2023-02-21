rm(list=ls())

library(tidyverse)


files_adult <- dir("../output_df", pattern="adult", full.names = T)
files_sapling <- dir("../output_df", pattern="sapling", full.names = T)

f <- function(x) get(load(x))

res_detailed_adult <- files_adult %>% map(f) %>% reduce(rbind) %>% 
  separate(plot, c("fire_year", "species_test", "density", "rcp", "period", "stage")) %>%
  mutate(perturbation=ifelse(str_detect(fire_year, "Perturb"), "Perturb", "No_perturb")) %>% 
  mutate(fire_year=str_remove(fire_year, "Perturb"))
  
res_detailed_adult %>% filter(species %in% c("Yellow_Birch", "Sugar_Maple", "Red_Maple")) %>% 
  saveRDS("../result_simulations/res_detailed_adult_temperate_species.rds")

res_detailed_adult %>% filter(!species %in% c("Yellow_Birch", "Sugar_Maple", "Red_Maple")) %>% 
  saveRDS("../result_simulations/res_detailed_adult_boreal_species.rds")

res_detailed_sapling <- files_sapling %>% map(f) %>% reduce(rbind) %>% 
  separate(plot, c("fire_year", "species_test", "density", "rcp", "period", "stage")) %>%
  mutate(perturbation=ifelse(str_detect(fire_year, "Perturb"), "Perturb", "No_perturb")) %>% 
  mutate(fire_year=str_remove(fire_year, "Perturb"))

res_detailed_sapling %>% filter(species %in% c("Yellow_Birch", "Sugar_Maple", "Red_Maple")) %>% 
  saveRDS("../result_simulations/res_detailed_sapling_temperate_species.rds")

res_detailed_sapling %>% filter(!species %in% c("Yellow_Birch", "Sugar_Maple", "Red_Maple")) %>% 
  saveRDS("../result_simulations/res_detailed_sapling_boreal_species.rds")