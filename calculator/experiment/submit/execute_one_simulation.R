rm(list = ls())

# devtools::install_github("aclason/rsortie", build_vignettes = TRUE)
library(tidyverse)
library(rsortie)
library(R.utils)


source("create_parameter_files_function.R")
source("process_output_functions.R")

parameter_file <- "1760_BOJ_10_ssp126_1991.txt"
parameter_file <- Sys.getenv("vars")
print(parameter_file)

#Recover simulation conditions
parameter_file <- str_split(parameter_file, "_")[[1]]
fire_year <- parameter_file[1]; species_test_f <- parameter_file[2]; patch_size <- parameter_file[3]; ssp_f <- parameter_file[4]


#Create parameter file 
create_parameter_file(fire_year = fire_year, ssp_f = ssp_f,
                      patch_size= patch_size, species_test_f = species_test_f)


#Start simulation
simulation_name <- paste(fire_year, species_test_f, patch_size, ssp_f, sep="_")
parameter_file_name <- paste0("../parameter_files/", simulation_name, ".xml")

file.exists(parameter_file_name)

#An output folder is created for the output of each simulation, which is not in the same file. 
dir.create(paste0("../output/", simulation_name))

print("OK SORTIE se lance maintenant")

system(paste("submit/runsortie.sh", parameter_file_name))


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
step <- str_split(t_files, "_", simplify = TRUE)[,8]
step <- as.numeric(str_remove(step, ".xml"))
step <- unique(c(subset(step, step %in% seq(0,200,5)), max(step)))
t_files <- paste0("../output/", simulation_name,"/", simulation_name,"_",step,".xml")

#adults


# Produire un data frame par fichier (treemap; arbres adultes seulement) stock?s ans une liste
t <- map(t_files, ~ treemap_from_file(.)$adult)
names(t) <- step

t2 <- compact(t)

t <- t2

#Change le nom de la 5i?me colonne des tableaux dans t.
for(j in names(t)) {
  colnames(t[[j]])[5] <- paste0("DBH", j)
  colnames(t[[j]])[6] <- paste0("Growth", j)
  colnames(t[[j]])[7] <- paste0("Light", j)
}

#Merge les datas dans un seul tableau avec une colonne pour le DBH de chaque timestep (permettra de faire la mortalit?)
t <- t %>%
  reduce(full_join, by=c("species", "X", "Y")) %>%
  dplyr::select(-contains("id"))

#Renverse le tableau pour qu'il soit tidy
t <- t %>% pivot_longer(cols=4:ncol(t), names_to = c(".value", "timestep"), names_pattern = "([a-zA-Z]+)(\\d+)") %>% 
  mutate(plot= paste0(simulation_name, "_adult")) %>% 
  separate(plot, into=c("fire_year", "species_test", "patch_size", "climate_scenario", "stage") , sep="_")

saveRDS(t, file=paste0("../output_df/", simulation_name, "_adult.rds"))


#saplings

# Produire un data frame par fichier (treemap; arbres adultes seulement) stock?s ans une liste
t <- map(t_files, ~ treemap_from_file(.)$sapling)
names(t) <- step

t2 <- compact(t)

t <- t2

#Change le nom de la 5i?me colonne des tableaux dans t.
for(j in names(t)) {
  colnames(t[[j]])[5] <- paste0("DBH", j)
  colnames(t[[j]])[6] <- paste0("Growth", j)
  colnames(t[[j]])[7] <- paste0("Light", j)
}

#Merge les datas dans un seul tableau avec une colonne pour le DBH de chaque timestep (permettra de faire la mortalit?)
t <- t %>%
  reduce(full_join, by=c("species", "X", "Y")) %>%
  dplyr::select(-contains("id"))

#Renverse le tableau pour qu'il soit tidy
t <- t %>% pivot_longer(cols=4:ncol(t), names_to = c(".value", "timestep"), names_pattern = "([a-zA-Z]+)(\\d+)") %>% 
  mutate(plot= paste0(simulation_name, "_sapling")) %>% 
  separate(plot, into=c("fire_year", "species_test", "patch_size", "climate_scenario", "stage") , sep="_")

saveRDS(t, file=paste0("../output_df/", simulation_name, "_sapling.rds"))