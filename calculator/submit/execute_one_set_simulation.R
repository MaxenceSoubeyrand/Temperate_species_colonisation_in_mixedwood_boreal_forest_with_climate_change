#Script that runs a single simulation timeline based on stand, test, density and rcp
rm(list = ls())

library(tidyverse)
library(xml2)

#parameter_file="1760_BOJ_10_noCC"
parameter_file <- Sys.getenv("vars")
print(parameter_file)
parameter_file <- str_split(parameter_file, "_")[[1]]
fire_year <- parameter_file[1]; species_test <- parameter_file[2]; density <- parameter_file[3]; rcp <- parameter_file[4]

source("edit_param_functions.R")
source("process_output_functions.R")
source("submit_files_function.R")



ferld_growth <- readRDS("../growth/ferld_growth_rcp_period.rds")

submit_chrono(fire_year, species_test, density, rcp)

parameter_file <- paste(parameter_file, collapse="_")

file <- dir("../out", pattern=parameter_file, full.names = T)
if(length(file)==5) print("Pas de bug de l'azimuth: no_azimuth")