rm(list=ls())

library(tidyverse)
theme_set(theme_bw())
library(viridis)

res_ad <- readRDS(file="../result_simulations/res_detailed_adult.rds") %>% 
  filter(!is.na(DBH), patch_size != "NA") %>% 
  mutate(year=as.numeric(timestep)+1990) %>% 
  filter(year=="2100")

res_sap <- readRDS(file="../result_simulations/res_detailed_sapling.rds") %>% 
  filter(!is.na(DBH), patch_size != "NA") %>% 
  mutate(year=as.numeric(timestep)+1990) %>% 
  filter(year=="2100")

res <- bind_rows(res_ad, res_sap) %>%  
  dplyr::select(-perturbation, -timestep)

#I remove the center patch, and make 8 replicas
resd <- res %>% 
  mutate(d=(X-50)^2+(Y - 50)^2) %>% 
  mutate(patch_size=ifelse(patch_size=="NA", "0", patch_size)) %>% 
  mutate(d2=as.numeric(patch_size)/pi) %>% 
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
  group_by(fire_year, species_test, patch_size, climate_scenario, species, subplot) %>% 
  summarize(BA=sum(((DBH*0.01)/2)^2*pi, na.rm=T)) %>% 
  mutate(BA=BA*(1-as.numeric(patch_size)/10000)*8) %>% #Here, I'm resetting to the hectare by subtracting the area of the circle and multiplying by 8, since I have 8 subplots.
  filter(species %in% c("Sugar_Maple", "Red_Maple", 'Yellow_Birch')) %>% 
  ungroup %>% dplyr::select(-species_test) %>% 
  complete(fire_year, patch_size, climate_scenario, species, subplot, fill=list(BA=0)) %>% 
  group_by(fire_year, patch_size, climate_scenario, species) %>% 
  summarize(med_BA=median(BA),
            min_BA=min(BA),
            max_BA=max(BA)) 


res2$fire_year <- factor(res2$fire_year, 
                         levels = c("1760", "1823", "1964", "1923"), 
                         labels = c("1760", "1823", "1964", "Harvested"))

res2$species <- factor(res2$species, 
                       levels = c("Balsam_Fir", "Jack_Pine", "Paper_Birch", 
                                  "Red_Maple", "Sugar_Maple", "Trembling_Aspen",
                                  "White_Cedar", "White_Spruce", "Yellow_Birch"), 
                       labels = c("Balsam fir", "Jack pine", "Paper birch", 
                                  "Red maple", "Sugar maple", "Trembling aspen",
                                  "White cedar", "White spruce", "Yellow birch"))

res2$climate_scenario <- factor(res2$climate_scenario, levels = c("noCC", "ssp126", "ssp245", "ssp585"),
                   labels=c("Current climate", "SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5"))

plot <- ggplot(res2, aes(x=as.numeric(patch_size), y=med_BA, ymin=min_BA, ymax=max_BA, color=climate_scenario, group=climate_scenario))+
  scale_color_viridis("Climate scenarios", discrete = T) +
  geom_errorbar(position=position_dodge(10), width=0) +
  geom_point(position=position_dodge(10)) +
  geom_line(position=position_dodge(10)) +
  facet_grid(species~fire_year, scales="free_y") +
  ylab(expression("Adult and sapling basal area at time 110 years "~~(m^2/h))) +
  xlab("Central plot size (in m²)") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 16),
        legend.position="bottom")

pdf(file = "../figures/BA_temperate_1_1.pdf", width = 8, height = 6)
plot
dev.off()

png(file = "../figures/BA_temperate_1_1.png", width = 8, height = 6, unit="in", res= 300)
plot
dev.off()

#Pour présentation en anglais
res2 <- filter(res2, patch_size=="400")

ggplot(res2, aes(x=fire_year, y=med_BA, ymin=min_BA, ymax=max_BA, color=climate_scenario))+
  geom_errorbar(position=position_dodge(0.4), width=0, size=1) +
  geom_point(position=position_dodge(0.4), size=2) +
  facet_wrap(~species, scales="free_y") +
  ylab(expression("Adult and sapling basal area at time 110 years "~~(m^2/h))) +
  xlab("Stand age") +
  scale_color_discrete("Climate scenarios") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 16),
        legend.position="bottom")

res2 <- resd %>% 
  group_by(fire_year, species_test, patch_size, climate_scenario, species, subplot) %>% 
  summarize(BA=sum(((DBH*0.01)/2)^2*pi, na.rm=T)) %>% 
  mutate(BA=BA*(1-as.numeric(patch_size)/10000)*8) %>% #Ici je remet ? l'hectare en enlevant la surface du cercle et je multipli? par 8 puisque j'ai 8 subplots
  filter(species %in% c("Sugar_Maple", "Red_Maple", 'Yellow_Birch')) %>% 
  ungroup %>% dplyr::select(-species_test) %>% 
  complete(fire_year, patch_size, climate_scenario, species, subplot, fill=list(BA=0)) %>% 
  group_by(fire_year, patch_size, climate_scenario, species) %>% 
  mutate(med_BA=median(BA),
            min_BA=quantile(BA,0.1),
            max_BA=quantile(BA,0.9))

res2$fire_year <- factor(res2$fire_year, 
                         levels = c("1760", "1823", "1964", "1923"), 
                         labels = c("1760", "1823", "1964", "1923 - Harvested"))

res2$species <- factor(res2$species, 
                       levels = c("Balsam_Fir", "Jack_Pine", "Paper_Birch", 
                                  "Red_Maple", "Sugar_Maple", "Trembling_Aspen",
                                  "White_Cedar", "White_Spruce", "Yellow_Birch"), 
                       labels = c("Balsam Fir", "Jack Pine", "Paper Birch", 
                                  "Red Maple", "Sugar Maple", "Trembling Aspen",
                                  "White Cedar", "White Spruce", "Yellow Birch"))

res2$climate_scenario <- factor(res2$climate_scenario, levels = c("noCC", "ssp126", "ssp245", "ssp585"),
                                labels=c("Climat de 1990", "SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5"))

res2_fr <- res2
res2_fr$species <- factor(res2_fr$species, 
                       levels = c("Balsam Fir", "Jack Pine", "Paper Birch", 
                                  "Red Maple", "Sugar Maple", "Trembling Aspen",
                                  "White Cedar", "White Spruce", "Yellow Birch"), 
                       labels = c("Balsam Fir", "Jack Pine", "Paper Birch", 
                                  "Erable rouge", "Erable à sucre", "Trembling Aspen",
                                  "White Cedar", "White Spruce", "Bouleau jaune"))

res2_fr$fire_year <- factor(res2_fr$fire_year, 
                         levels = c("1760", "1823", "1964", "1923 - Harvested"), 
                         labels = c("1760", "1823", "1964", "1923\nRécolté"))

res2_fr <- filter(res2_fr, patch_size=="400")

#Graphe en fran?ais
ggplot(res2_fr, aes(x=fire_year, y=BA, ymin=min_BA, ymax=max_BA, color=climate_scenario))+
  geom_errorbar(position=position_dodge(0.4), width=0, size=1) +
  geom_point(aes(x=fire_year, y=med_BA), position=position_dodge(0.4), size=2) +
  facet_wrap(~species, scales="free_y") +
  ylab(expression("Surface terrière des adultes et des gaulis en 2100 "~~(m^2/h))) +
  xlab("Âge du peuplement") +
  scale_color_discrete("Scénarios climatiques") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 16),
        legend.position="bottom")

a=res2 %>% 
  filter(fire_year=="1964", climate_scenario=="SSP 2-4.5", patch_size=="400") %>% 
  group_by(species) %>% 
  summarize(mean_BA=mean(med_BA))

summary(aov(med_BA~fire_year+rcp+density+species, data=res2))

