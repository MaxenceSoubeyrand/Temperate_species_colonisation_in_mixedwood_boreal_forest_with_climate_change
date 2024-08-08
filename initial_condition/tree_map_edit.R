#Script that creates tree_maps for simulations. 
rm(list = ls())

library(tidyverse)
library(RODBC)


t_files_dir <- dir("tree_maps_origine", full.names=T)

t_files <- str_remove(t_files_dir, "tree_maps_origine/")
t_files <- str_remove(t_files, ".txt")

for(i in 1:length(t_files_dir)){
  tree_map_name <- c(paste0("tree_maps/", t_files[i], "_noCC_1991.txt"),
                     paste0("tree_maps/", t_files[i], "_ssp126_1991.txt"),
                     paste0("tree_maps/", t_files[i], "_ssp245_1991.txt"), 
                     paste0("tree_maps/", t_files[i], "_ssp585_1991.txt"))
  file.copy(t_files_dir[i], tree_map_name)
}

big_patch <- readRDS("big_patch.rds")

ggplot(big_patch, aes(x=X, y=Y, col=Species, size=Diam)) + geom_point() +
  annotate("path",
           x=2.5+20+sqrt(400/pi)*cos(seq(0,2*pi,length.out=100)),
           y=8.75+10+sqrt(400/pi)*sin(seq(0,2*pi,length.out=100)))
  
#The resef plot has been selected, now it's time to create the plots we'll use in the simulations. 
#One of 10m2 and another of 400m2

files <- dir("tree_maps/", full.names=F)

create_patch <- function(patch, size){
  patch %>% 
    mutate(d=(X-22.5)^2+(Y - 18.75)^2) %>% 
    filter(d<size/pi) %>% 
    dplyr::select(-d) %>% 
    mutate(X=X+50-22.5 ,
           Y=Y+50-18.75)#not sqrt because it's the root distance calculated 
}

remove_patch_control <-  function(control, size){
  control %>% 
    mutate(d=(X-50)^2+(Y - 50)^2) %>% 
    filter(d>size/pi) %>% 
    dplyr::select(-d)
}

init_patch_size <- c(10, 30, 80, 150, 250, 400)
for(i in files){
  control <- read.table(paste0("tree_maps/",i), h=T)
  
  for(patch_size in init_patch_size){
    patch <- create_patch(patch=big_patch, size=patch_size)
    control_without_patch <- remove_patch_control(control=control, size=patch_size)
    
    #Sugar maple
    ers <- bind_rows(control_without_patch, patch)
    name_ers <- str_replace(i, "control_NA", paste0("ERS_", patch_size))
    write.table(x = ers, file = paste0("tree_maps/", name_ers), sep="\t", row.names=F, quote=F)
    
    #Red maple
    err <- ers %>% 
      mutate(Species=ifelse(Species=="Sugar_Maple", "Red_Maple",Species))
    name_err <- str_replace(i, "control_NA", paste0("ERR_", patch_size))
    write.table(x = err, file = paste0("tree_maps/", name_err), sep="\t", row.names=F, quote=F)
    
    #Yellow birch
    boj <- ers %>% 
      mutate(Species=ifelse(Species=="Sugar_Maple", "Yellow_Birch",Species))
    name_boj <- str_replace(i, "control_NA", paste0("BOJ_", patch_size))
    write.table(x = boj, file = paste0("tree_maps/", name_boj), sep="\t", row.names=F, quote=F)
  }
}

