#The goal is to decompose the growth and to look at the impact of the different 
#effects modeled according to the scenarios tested, we will be able to know what 
#limits the growth the most. 

#We only have the growth and the light from Sortie output, but with the growth
#equation, we are able to find DBH effect and shadow effect. With the maximum
#potential growth, we are able to find the crowding effect. 

rm(list=ls())
gc()

setwd("~/Temperate_species_colonisation_in_mixedwood_boreal_forest_with_climate_change")

library(tidyverse)
theme_set(theme_bw())
library(viridis)
library(ggh4x)

#Loading the data
res_ad <- readRDS(file="result_simulations/res_detailed_adult_temperate_species.rds")
head(res_ad)

#Calculate the timestep
res <- res_ad %>% 
  filter(!is.na(DBH)) %>% 
  mutate(year=as.numeric(period)+as.numeric(timestep),
         Growth=Growth/5) %>%  
  mutate(d=(X-50)^2+(Y - 50)^2) %>% 
  mutate(density=ifelse(density=="NA", "0", density)) %>% 
  mutate(d2=as.numeric(density)/pi) %>% 
  filter(d2<d) %>% 
  select(-perturbation, -stage, -X, -Y, -d2, -d) %>% 
  filter(Growth != 0, Light<1, species != "Jack_Pine")

#Get MaxPotGrowth
load("growth/ferld_growth_rcp_period.Rdata")
colnames(ferld_growth)[4] <- "MaxPotGrowth"

#Get parameters (X0, Xb and m)
par_growth <- data.frame(species = c("Sugar_Maple", "Red_Maple", "Yellow_Birch", "Paper_Birch", "Trembling_Aspen", "White_Cedar", "White_Spruce", "Balsam_Fir"),
           X0 = c(21.654829,	20.189625,	16.182373, 15.866908, 45.282974, 103.21548, 16.386118, 13.686995),
           Xb = c(1.1890368,	1.8364733,	1.0655644, 0.90537345, 1.0310016, 2.8542862, 0.8052472, 0.7543218),
           m = c(1.16,	0.564334,	0.8021385, 0.72, 0.84, 0.42, 0.83, 0.7))

par_growth <- right_join(par_growth, ferld_growth)
gc()
res <- right_join(res, par_growth)

res_effect <- res %>% 
  mutate(DBH_effect=exp(-0.5*((log(DBH)-log(X0))/Xb)^2),
         shading_effect=exp(-m*Light),
         crowding_effect=Growth/(MaxPotGrowth*DBH_effect*shading_effect), 
         DBH_effect_growth=MaxPotGrowth*DBH_effect,
         DBH_shading_effect_growth=MaxPotGrowth*DBH_effect*shading_effect,
         realized_growth=MaxPotGrowth*DBH_effect*shading_effect*crowding_effect)

res_effect_ex <- res_effect %>% 
  filter(species %in% c("Yellow_Birch", "Red_Maple", "Sugar_Maple"),
         species_test != "all") %>% 
  group_by(fire_year, species, rcp, year, density) %>% 
  summarise(MaxPotGrowth=mean(MaxPotGrowth),
            DBH_effect=mean(DBH_effect),
            shading_effect = mean(shading_effect),
            crowding_effect = mean(crowding_effect),
            DBH_effect_growth_cum = mean(DBH_effect_growth),
            DBH_shading_effect_growth_cum =mean(DBH_shading_effect_growth),
            realized_growth = mean(realized_growth)) %>%
  mutate(DBH_effect2=MaxPotGrowth-DBH_effect_growth_cum,
         shading_effect2=DBH_effect_growth_cum-DBH_shading_effect_growth_cum,
         crowding_effect2=DBH_shading_effect_growth_cum-realized_growth) %>% 
  pivot_longer(cols = `MaxPotGrowth`:`crowding_effect2`, names_to = "Effects", values_to="Growth") %>% 
  filter(Effects %in% c("crowding_effect2", "DBH_effect2", "shading_effect2", "realized_growth")) 

#Change the names of the occurences
res_effect_ex$Effects <- factor(res_effect_ex$Effects, levels = c('DBH_effect2', 'shading_effect2', 'crowding_effect2', 'realized_growth'),
                                labels=c('Size effect', 'Shading effect', 'Crowding effect', 'Realized growth'))

res_effect_ex$species <- factor(res_effect_ex$species, levels = c("Red_Maple", "Sugar_Maple", "Yellow_Birch"),
                                labels=c("Red Maple",  "Sugar Maple", "Yellow Birch"))

res_effect_ex$fire_year <- factor(res_effect_ex$fire_year, levels = c("1760", "1823", "1964", "1923"),
                                labels=c("1760", "1823", "1964", "Harvested"))

res_effect_ex$rcp <- factor(res_effect_ex$rcp, levels = c("noCC", "rcp26", "rcp45", "rcp85"),
                                  labels=c("Current\nclimate", "RCP 2.6", "RCP 4.5", "RCP 8.5"))

pdf(file = "figure/growth_decomposition.pdf", width = 15.3, height = 8)
ggplot(filter(res_effect_ex, density=="400"), aes(x=year, y=Growth, group=Effects, fill=Effects)) +
  geom_area() + scale_fill_viridis("", discrete = T) +
  geom_line(position="stack") +
  facet_nested(fire_year~species+rcp) + 
  ylim(0,NA) +
  ylab(expression("Tree diametrical growth"~~(cm.year^-1))) + 
  xlab("Year") +
  theme(axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        strip.text = element_text(size = 16),
        plot.title = element_text(size = 20),
        legend.position="bottom") + 
  guides(x = guide_axis(angle = 90))
dev.off()
