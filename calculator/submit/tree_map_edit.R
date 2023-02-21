#Script qui crée les tree_maps des simulations. 
rm(list = ls())

library(tidyverse)
library(RODBC)

setwd("~/PhD/chap3/submit")


t_files_dir <- dir("~/PhD/Chap1/SORTIE/SORTIE_serveur/FERLD/tree_maps", full.names=T)
t_files_dir <- t_files_dir[str_detect(string = t_files_dir, pattern = "control")]
t_files_dir <- t_files_dir[str_detect(string = t_files_dir, pattern = "1760_|1964_|1823_|Perturb1923_")]

t_files <- str_remove(t_files_dir, "C:/Users/Ordinateur/OneDrive - UQAT/Documents/PhD/Chap1/SORTIE/SORTIE_serveur/FERLD/tree_maps/")
t_files <- str_remove(t_files, ".txt")

for(i in 1:length(t_files_dir)){ #i=25
  tree_map_name <- c(paste0("~/PhD/chap3/tree_maps/", t_files[i], "_noCC_1991.txt"),
                     paste0("~/PhD/chap3/tree_maps/", t_files[i], "_rcp26_1991.txt"),
                     paste0("~/PhD/chap3/tree_maps/", t_files[i], "_rcp45_1991.txt"), 
                     paste0("~/PhD/chap3/tree_maps/", t_files[i], "_rcp85_1991.txt"))
  file.copy(t_files_dir[i], tree_map_name)
}

#Ici je vais récupérer des peuplements de feuillus tempérés dans les données du RESEF
con <- odbcConnectAccess2007("../../Chap1/SORTIE/Parametre_SORTIE/lambda/data/RESEF_30-novembre-2020_UQAT.accdb")
data_tables <- sqlTables(con ,TABVLE_TYPE=='TABLE')$TABLE_NAME
dendro <- sqlFetch(con, "t_Dendrometrie")

# Calcul des coordonnees X-Y des arbres par placette
# je sélectionne pour l'ERS car il y a des peuplements purs et je mets la même pour les deux autres feuillus teméprés, les résultats seront alors comparables. 
dendro2 <- mutate(dendro, PX = PEP %/% 10, PY = PEP %% 10,
                 X = round(PX*10 + AXEX/100, 2), 
                 Y = round(PY*10 + AXEY/100, 2)) %>% 
  select(SERIE, PLACE, PEP, ESS, DHP, X, Y) %>% 
  group_by(SERIE, PLACE, PEP) %>% 
  summarize(n=n(),
            pers=sum(ESS=="ERS")/n,
            ners=sum(ESS=="ERS")) %>% 
  arrange(desc(pers))

#Je prend la SERIE B, PLACE 802 et PEP 4 car peuplement pur d'érable à sucre

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

ggplot(big_patch, aes(x=X, y=Y, col=Species, size=Diam)) + geom_point() +
  annotate("path",
           x=2.5+20+sqrt(400/pi)*cos(seq(0,2*pi,length.out=100)),
           y=8.75+10+sqrt(400/pi)*sin(seq(0,2*pi,length.out=100)))
  
#La placette du resef est sélectionnée, il faut maintenant créer les placettes qu'on va mettre dans les simulations 
#Une de 10m2 et une autre de 400m2

files <- dir("~/PhD/chap3/tree_maps/", full.names=F)

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

create_patch(big_patch, 10)

# create_patch(patch=big_patch, size=10)
# remove_patch_control(control=control, size=10)

init_patch_size <- c(10, 30, 80, 150, 250, 400)
for(i in files){ #i=files[5]
  control <- read.table(paste0("~/PhD/chap3/tree_maps/",i), h=T)
  
  for(patch_size in init_patch_size){
    patch <- create_patch(patch=big_patch, size=patch_size)
    control_without_patch <- remove_patch_control(control=control, size=patch_size)
    
    #ERS
    ers <- bind_rows(control_without_patch, patch)
    name_ers <- str_replace(i, "control_NA", paste0("ERS_", patch_size))
    write.table(x = ers, file = paste0("../tree_maps/", name_ers), sep="\t", row.names=F, quote=F)
    
    #ERR
    err <- ers %>% #On prend le patch d'ers et on fait comme si c'était des err pour avoir le mêmes conditions initiales. 
      mutate(Species=ifelse(Species=="Sugar_Maple", "Red_Maple",Species))
    name_err <- str_replace(i, "control_NA", paste0("ERR_", patch_size))
    write.table(x = err, file = paste0("../tree_maps/", name_err), sep="\t", row.names=F, quote=F)
    
    #ERR
    boj <- ers %>% 
      mutate(Species=ifelse(Species=="Sugar_Maple", "Yellow_Birch",Species))
    name_boj <- str_replace(i, "control_NA", paste0("BOJ_", patch_size))
    write.table(x = boj, file = paste0("../tree_maps/", name_boj), sep="\t", row.names=F, quote=F)
  }
}

#test
a=read.table("../tree_maps/Perturb1923_ERR_150_noCC_1991.txt", h=T)

ggplot(a, aes(x=X, y=Y, col=Species, size=Diam)) + geom_point() +
  annotate("path",
           x=50+sqrt(150/pi)*cos(seq(0,2*pi,length.out=100)),
           y=50+sqrt(150/pi)*sin(seq(0,2*pi,length.out=100)))
