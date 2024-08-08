rm(list = ls())

library(tidyverse)
library(rsortie)
library(R.utils)


source("create_parameter_files_function.R")
source("process_output_functions.R")

parameter_file <- "1964_BOJ_150_ssp245_a_0.34_1991.txt"
parameter_file <- Sys.getenv("vars")
print(parameter_file)

#Recover simulation conditions
parameter_file <- str_split(parameter_file, "_")[[1]]
fire_year <- parameter_file[1]; species_test_f <- parameter_file[2]; 
patch_size <- parameter_file[3]; ssp_f <- parameter_file[4]
parameter <- parameter_file[5]; par_value <- parameter_file[6]



#Create parameter file 
create_parameter_file(fire_year = fire_year, ssp_f = ssp_f,
                      patch_size= patch_size, species_test_f = species_test_f,
                      parameter, par_value)


#Start simulation 
simulation_name <- paste(fire_year, species_test_f, patch_size, ssp_f, parameter, par_value, sep="_")
parameter_file_name <- paste0("../parameter_files/", simulation_name, ".xml")

#An output/simulation folder is created for the output of each simulation, which is not in the same file. dir.create(paste0("../output/", simulation_name))

system(paste("/project/6071506/msoubeyr/chap3_sensitivity_new/submit/runsortie.sh", parameter_file_name))

#Execute functions for summary output
out_dir_summary <- paste0("../output/", simulation_name, "/", simulation_name, ".out")
res <- read_summary_output(out_dir_summary)

res <- res %>% 
  dplyr::select(-Subplot) %>% 
  rename(Abs.Den=`Abs Den`, Abs.BA=`Abs BA`, Rel.BA=`Rel BA`, Rel.Den=`Rel Den`) %>%
  mutate(year=Step+1991) %>% 
  mutate_if(is.character,as.factor) %>% 
  filter(Abs.Den !=0)

saveRDS(res, paste0("../output_df/", simulation_name,"_summary.rds"))

#Execute functions for detailed output
#Second we took more detailed informations: adult growth, regeneration.

t_files <- dir(paste0("../output/", simulation_name), pattern = paste0("^", simulation_name, "_"), full.names = TRUE)
step <- str_split(t_files, "_", simplify = TRUE)[,12]
step <- as.numeric(str_remove(step, ".xml"))
step <- unique(c(subset(step, step %in% seq(0,200,5)), max(step)))
t_files <- paste0("../output/", simulation_name,"/", simulation_name,"_",step,".xml")

#adults


# Produce one data frame per file (treemap; mature trees only stored in a list
t <- map(t_files, ~ treemap_from_file(.)$adult)
names(t) <- step

t2 <- compact(t)

t <- t2

#Changes the name of the 5th column of tables in t.
for(j in names(t)) {
  colnames(t[[j]])[5] <- paste0("DBH", j)
  colnames(t[[j]])[6] <- paste0("Growth", j)
  colnames(t[[j]])[7] <- paste0("Light", j)
}

#Merge the data into a single table with a column for the DBH of each timestep (this will make it possible to calculate mortalit?).t <- t %>%
  reduce(full_join, by=c("species", "X", "Y")) %>%
  dplyr::select(-contains("id"))

  #Reverse the picture to make it tidy
t <- t %>% pivot_longer(cols=4:ncol(t), names_to = c(".value", "timestep"), names_pattern = "([a-zA-Z]+)(\\d+)") %>% 
  mutate(plot= paste0(simulation_name, "_adult")) %>% 
  separate(plot, into=c("fire_year", "species_test", "patch_size", "climate_scenario", "parameter", "parameter_value", "stage") , sep="_")

saveRDS(t, file=paste0("../output_df/", simulation_name, "_adult.rds"))


#saplings

t <- map(t_files, ~ treemap_from_file(.)$sapling)
names(t) <- step

t2 <- compact(t)

t <- t2

for(j in names(t)) {
  colnames(t[[j]])[5] <- paste0("DBH", j)
  colnames(t[[j]])[6] <- paste0("Growth", j)
  colnames(t[[j]])[7] <- paste0("Light", j)
}

t <- t %>%
  reduce(full_join, by=c("species", "X", "Y")) %>%
  dplyr::select(-contains("id"))

t <- t %>% pivot_longer(cols=4:ncol(t), names_to = c(".value", "timestep"), names_pattern = "([a-zA-Z]+)(\\d+)") %>% 
  mutate(plot= paste0(simulation_name, "_sapling")) %>% 
  separate(plot, into=c("fire_year", "species_test", "patch_size", "climate_scenario", "parameter", "parameter_value", "stage") , sep="_")

saveRDS(t, file=paste0("../output_df/", simulation_name, "_sapling.rds"))