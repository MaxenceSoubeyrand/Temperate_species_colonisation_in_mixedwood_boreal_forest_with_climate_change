rm(list=ls())

library(tidyverse)

files_adult <- dir("../output_df", pattern="_adult", full.names = T)
files_sapling <- dir("../output_df", pattern="_sapling", full.names = T)

f <- function(x) readRDS(x) %>% filter(timestep=="110")

res_detailed_adult <- files_adult %>% map(f) %>% reduce(rbind) %>% 
  mutate(perturbation=ifelse(str_detect(fire_year, "Perturb"), "Perturb", "No_perturb")) %>% 
  mutate(fire_year=str_remove(fire_year, "Perturb"))
saveRDS(res_detailed_adult, file="../result_simulations/res_detailed_adult.rds")

res_detailed_sapling <- files_sapling %>% map(f) %>% reduce(rbind) %>% 
  mutate(perturbation=ifelse(str_detect(fire_year, "Perturb"), "Perturb", "No_perturb")) %>% 
  mutate(fire_year=str_remove(fire_year, "Perturb"))
saveRDS(res_detailed_sapling, file="../result_simulations/res_detailed_sapling.rds")
