rm(list=ls())

setwd("~/Temperate_species_colonisation_in_mixedwood_boreal_forest_with_climate_change")

library(tidyverse)
theme_set(theme_bw())

res_ad <- readRDS(file="result_simulations/res_detailed_adult_temperate_species.rds")

res_sap <- readRDS(file="result_simulations/res_detailed_sapling_temperate_species.rds")

res <- bind_rows(res_ad, res_sap) %>% 
  filter(!is.na(DBH), density != "NA") %>% 
  mutate(year=as.numeric(timestep)+as.numeric(as.character(period))) %>% 
  filter(year=="2100") %>% 
  dplyr::select(-perturbation, -subplot, -timestep)

#I remove the center patch, and make 8 replicas 
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

test <- resd %>% 
  filter(fire_year=="1923", species_test=="ERS", rcp=="noCC", density=="400")

ggplot(test, aes(x=X, y=Y, col=subplot, size=DBH))+ geom_point()+
  annotate("path",
           x=50+sqrt(400/pi)*cos(seq(0,2*pi,length.out=100)),
           y=50+sqrt(400/pi)*sin(seq(0,2*pi,length.out=100)))+ xlim(0,100)+ylim(0,100)

res2 <- resd %>% 
  group_by(fire_year, species_test, density, rcp, species, subplot) %>% 
  summarize(BA=sum(((DBH*0.01)/2)^2*pi, na.rm=T)) %>% 
  mutate(BA=BA*(1-as.numeric(density)/10000)*8) %>% #Here I put back to the hectare by removing the surface of the circle and I multiply by 8 since I have 8 subplots
  filter(species %in% c("Sugar_Maple", "Red_Maple", 'Yellow_Birch')) %>% 
  ungroup %>% dplyr::select(-species_test) %>% 
  complete(fire_year, density, rcp, species, subplot, fill=list(BA=0)) %>% 
  group_by(fire_year, density, rcp, species) %>% 
  summarize(med_BA=median(BA),
            min_BA=quantile(BA,0.1),
            max_BA=quantile(BA,0.9))

res2$fire_year <- factor(res2$fire_year, 
                         levels = c("1760", "1823", "1964", "1923"), 
                         labels = c("1760", "1823", "1964", "Harvested"))

res2$species <- factor(res2$species, 
                       levels = c("Balsam_Fir", "Jack_Pine", "Paper_Birch", 
                                  "Red_Maple", "Sugar_Maple", "Trembling_Aspen",
                                  "White_Cedar", "White_Spruce", "Yellow_Birch"), 
                       labels = c("Balsam Fir", "Jack Pine", "Paper Birch", 
                                  "Red Maple", "Sugar Maple", "Trembling Aspen",
                                  "White Cedar", "White Spruce", "Yellow Birch"))

res2$rcp <- factor(res2$rcp, levels = c("noCC", "rcp26", "rcp45", "rcp85"),
                   labels=c("Current climate", "RCP 2.6", "RCP 4.5", "RCP 8.5"))

pdf(file = "~/PhD/Chap3/article/figures/experimental_design/BA_temperate_1_1.pdf", width = 8, height = 6)
ggplot(res2, aes(x=as.numeric(density), y=med_BA, ymin=min_BA, ymax=max_BA, color=rcp, group=rcp))+
  geom_errorbar(position=position_dodge(10), width=0) +
  geom_point(position=position_dodge(10)) +
  geom_line(position=position_dodge(10)) +
  facet_grid(species~fire_year, scales="free_y") +
  ylab(expression("Adult and sapling basal area at time 110 years "~~(m^2/h))) +
  xlab("Central plot size (in m²)") +
  scale_color_discrete("Climate scenarios") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 16),
        legend.position="bottom")
dev.off()

