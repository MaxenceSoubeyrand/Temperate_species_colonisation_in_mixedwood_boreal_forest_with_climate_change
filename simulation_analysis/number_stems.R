rm(list=ls())

setwd("~/Temperate_species_colonisation_in_mixedwood_boreal_forest_with_climate_change")

library(tidyverse)
theme_set(theme_bw())
library(ggh4x)

res_ad <- readRDS(file="result_simulations/res_detailed_adult_temperate_species.rds")

res_sap <- readRDS(file="result_simulations/res_detailed_sapling_temperate_species.rds")

res <- bind_rows(res_ad, res_sap)%>% 
  filter(timestep != 0) %>% 
  filter(!is.na(DBH), density != "NA") %>% 
  mutate(year=as.numeric(timestep)+as.numeric(as.character(period))) %>% 
  dplyr::select(-perturbation, -subplot, -timestep) %>% 
  filter(rcp=="rcp45") 

#J'enlève le patch du centre, et je fait 8 réplicas 
resd <- res %>% 
  mutate(d=(X-50)^2+(Y - 50)^2) %>% 
  mutate(density=ifelse(density=="NA", "0", density)) %>% 
  mutate(d2=as.numeric(density)/pi) %>% 
  filter(d2<d) %>% 
  mutate(angle=acos(abs(X-50)/sqrt(d))) %>% 
  mutate(subplot=case_when(X<=50 & Y > 50 & angle>pi/4 ~ "1",
                           X<=50 & Y > 50 & angle<=pi/4 ~ "2",
                           X>50 & Y > 50 & angle>pi/4 ~ "3",
                           X>50 & Y > 50 & angle<=pi/4 ~ "4",
                           X>50 & Y <= 50 & angle>pi/4 ~ "5",
                           X>50 & Y <= 50 & angle<=pi/4 ~ "6",
                           X<=50 & Y <= 50 & angle>pi/4 ~ "7",
                           X<=50 & Y <= 50 & angle<=pi/4 ~ "8"))


  
res2 <- resd %>% 
  group_by(fire_year, species_test, density, species, year) %>% 
  count()


#Add zeros when absent
des <- expand(unique(res2[,-c(5,6)]), rbind(data.frame(year=c(1991)), unique(res2[,c(5)])))

res3 <- right_join(res2, des)

sum(is.na(res3))

res3[is.na(res3)] <- 0

res3$fire_year <- factor(res3$fire_year, 
                         levels = c("1760", "1823", "1964", "1923"), 
                         labels = c("1760", "1823", "1964", "1923 - Harvested"))

res3$species <- factor(res3$species, 
                       levels = c("Balsam_Fir", "Jack_Pine", "Paper_Birch", 
                                  "Red_Maple", "Sugar_Maple", "Trembling_Aspen",
                                  "White_Cedar", "White_Spruce", "Yellow_Birch"), 
                       labels = c("Balsam Fir", "Jack Pine", "Paper Birch", 
                                  "Red Maple", "Sugar Maple", "Trembling Aspen",
                                  "White Cedar", "White Spruce", "Yellow Birch"))


pdf(file = "figures/number_stems.pdf", width = 8, height =10)
ggplot(res3, aes(x=year, y=n, color=species, group=species)) +
  geom_line(size=1) +
  facet_nested(as.numeric(density)~fire_year, scale="free_y") + 
  ylab("Number of stems (juveniles + adults)") + xlab("Year") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 16)) +
  theme(legend.position="bottom") 
dev.off()
  #scale_y_continuous(trans = log2_trans())
