
library(ggplot2)
library(tidyverse)
theme_set(theme_bw())


setwd("~/Temperate_species_colonisation_in_mixedwood_boreal_forest_with_climate_change")


sap_bor <- readRDS(file="result_simulations/res_detailed_sapling_boreal_species.rds")

gc()

sap_bor <- sap_bor %>% 
  select(-perturbation, -subplot) 



gc()

sap_bor <- sap_bor %>%
  filter(rcp=="rcp45",
         density=="400",
         !is.na(DBH),
         !(timestep=="0" & period %in% c("2025", "2055", "2085"))) #I do this otherwise it takes into account the tfinal of each period + the tinitial of the next period.

gc()

sap_bor <- sap_bor %>% 
  mutate(year=as.numeric(timestep)+as.numeric(period))

gc()


saveRDS(sap_bor, file="result_simulations/sap_bor_video.rds")

gc()

sap_bor <- readRDS(file="result_simulations/sap_bor_video.rds")

#The same for other species and other stages
sap_temp <- readRDS(file="result_simulations/res_detailed_sapling_temperate_species.rds")
ad_bor <- readRDS(file="result_simulations/res_detailed_adult_boreal_species.rds")
ad_temp <- readRDS(file="result_simulations/res_detailed_adult_temperate_species.rds")

res <- bind_rows(sap_temp, ad_bor, ad_temp) %>% #sap_bor is bind_row() later
  select(-perturbation, -subplot) %>% 
  filter(rcp=="rcp45",
         density=="400",
         !is.na(DBH),
         !(timestep=="0" & period %in% c("2025", "2055", "2085"))) %>% #Je fais ça sinon il prend en compte le tfinal de chaque période + le tinitial de la période suivante.
  mutate(year=as.numeric(timestep)+as.numeric(period))

res <- bind_rows(res, sap_bor)

res$species <- factor(res$species, 
                      levels = c("Balsam_Fir", "Jack_Pine", "Paper_Birch",
                                 "Trembling_Aspen", "White_Cedar", "White_Spruce",
                                 "Red_Maple", "Sugar_Maple", "Yellow_Birch"), 
                      labels = c("Balsam fir", "Jack pine", "Paper birch",
                                 "Trembling aspen", "White cedar", "White spruce",
                                 "Red Maple", "Sugar Maple", "Yellow Birch"))

res$species_test <- factor(res$species_test, 
                      levels = c("ERR", "ERS", "BOJ"), 
                      labels = c("Red maple", "Sugar maple", "Yellow birch"))

res$fire_year <- factor(res$fire_year, 
                         levels = c("1760", "1823", "1964", "1923"), 
                         labels = c("1760", "1823", "1964", "1923 - Harvested"))

colors <- c("#0077BB", "#33BBEE", "#009988","#EE7733", "#CC3311", "#EE3377", "#882255", "#E6AB02", "#66A61E")
sq <- scales::trans_new("squared", function(x) x^2, sqrt)

res <- res %>% 
  mutate(`DBH class`=ifelse(DBH<10, "<10","\u2265 10"))

make_plot <- function(){
  for(i in sort(unique(res$year))){
    p <- ggplot(filter(res, year==i, !species %in% c("Red Maple", "Sugar Maple", "Yellow Birch")), 
                aes(x=X, y=Y, color=species, size=`DBH class`)) +
      geom_point() +
      geom_point(data=filter(res, year==i, species %in% c("Red Maple", "Sugar Maple", "Yellow Birch")), 
                 aes(x=X, y=Y, size=`DBH class`), color="black") +
      scale_size_manual(values=c(0.1, 1)) +
      scale_color_manual("Species", values=colors) +
      facet_grid(species_test~fire_year) +
      annotate("path",
               color="red", size=0.7,
               x=50+sqrt(400/pi)*cos(seq(0,2*pi,length.out=100)),
               y=50+sqrt(400/pi)*sin(seq(0,2*pi,length.out=100)))+
      xlim(0,100) + ylim(0,100) +
      ggtitle(paste0(i))
    
    ggsave(p, filename=paste0("video/", i,".png"), width=20, height=13, unit="cm")
  }
}

make_plot() 
#video editor



