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

#Here I will retrieve temperate hardwood stands from the RESEF data
con <- odbcConnectAccess2007("initial_conditions/RESEF_30-novembre-2020_UQAT.accdb") #One year embargo
data_tables <- sqlTables(con ,TABVLE_TYPE=='TABLE')$TABLE_NAME
dendro <- sqlFetch(con, "t_Dendrometrie")

# Calculation of X-Y coordinates of trees per plot
# I select for the RHS because there are pure stands and I put the same for the other two temperate hardwoods, the results will then be comparable. 
dendro2 <- mutate(dendro, PX = PEP %/% 10, PY = PEP %% 10,
                 X = round(PX*10 + AXEX/100, 2), 
                 Y = round(PY*10 + AXEY/100, 2)) %>% 
  select(SERIE, PLACE, PEP, ESS, DHP, X, Y) %>% 
  group_by(SERIE, PLACE, PEP) %>% 
  summarize(n=n(),
            pers=sum(ESS=="ERS")/n,
            ners=sum(ESS=="ERS")) %>% 
  arrange(desc(pers))

#I take SERIES B, PLACE 802 and PEP 4 because it is a pure sugar maple stand

patch <- mutate(dendro, PX = PEP %/% 10, PY = PEP %% 10,
                     X = round(PX*10 + AXEX/100, 2)-90, 
                     Y = round(PY*10 + AXEY/100, 2)) %>% 
  filter(SERIE=="A", PLACE=="103", PEP == "90") %>% 
  mutate(Type=ifelse(DHP<100, "Sapling","Adult"),
         Height=0, 
         Species="Sugar_Maple",
         Diam=DHP/10) %>%
  dplyr::select(X, Y, Species, Type, Diam, Height)

big_patch <- NULL
for(xi in 0:3){
  for(yj in 0:3){
    big_patch <- rbind(big_patch, mutate(patch, X=X+xi*10, Y=Y+yj*10))
  }
}
  
#The plot of the resef is selected, we must now create the plots that we will put in the simulations 
#One of 10m2 and another of 400m2

files <- dir("~/initial_conditions/tree_maps/", full.names=F)

create_patch <- function(patch, size){
  patch %>% 
    mutate(d=(X-22.5)^2+(Y - 18.75)^2) %>% 
    filter(d<size/pi) %>% 
    dplyr::select(-d) %>% 
    mutate(X=X+50-22.5 ,
           Y=Y+50-18.75)
}

remove_patch_control <-  function(control, size){
  control %>% 
    mutate(d=(X-50)^2+(Y - 50)^2) %>% 
    filter(d>size/pi) %>% 
    dplyr::select(-d)
}

create_patch(big_patch, 10)


init_patch_size <- c(10, 30, 80, 150, 250, 400)
for(i in files){ #i=files[5]
  control <- read.table(paste0("~/initial_conditions/tree_maps/",i), h=T)
  
  for(patch_size in init_patch_size){
    patch <- create_patch(patch=big_patch, size=patch_size)
    control_without_patch <- remove_patch_control(control=control, size=patch_size)
    
    #ERS
    ers <- bind_rows(control_without_patch, patch)
    name_ers <- str_replace(i, "control_NA", paste0("ERS_", patch_size))
    write.table(x = ers, file = paste0("/tree_maps/", name_ers), sep="\t", row.names=F, quote=F)
    
    #ERR
    err <- ers %>% #We take the ers patch and we make as if they were err to have the same initial conditions. 
      mutate(Species=ifelse(Species=="Sugar_Maple", "Red_Maple",Species))
    name_err <- str_replace(i, "control_NA", paste0("ERR_", patch_size))
    write.table(x = err, file = paste0("/tree_maps/", name_err), sep="\t", row.names=F, quote=F)
    
    #ERR
    boj <- ers %>% 
      mutate(Species=ifelse(Species=="Sugar_Maple", "Yellow_Birch",Species))
    name_boj <- str_replace(i, "control_NA", paste0("BOJ_", patch_size))
    write.table(x = boj, file = paste0("/tree_maps/", name_boj), sep="\t", row.names=F, quote=F)
  }
}
