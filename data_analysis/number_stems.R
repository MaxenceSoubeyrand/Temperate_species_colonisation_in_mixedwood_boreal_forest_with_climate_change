rm(list=ls())

library(tidyverse)
theme_set(theme_bw())
library(ggh4x)

res_ad <- readRDS("../result_simulations/res_detailed_adult.rds")%>% 
  filter(species %in% c("Red_Maple", "Sugar_Maple", "Yellow_Birch"))
res_sap <- readRDS("../result_simulations/res_detailed_sapling.rds")%>% 
  filter(species %in% c("Red_Maple", "Sugar_Maple", "Yellow_Birch"))

res <- bind_rows(res_ad, res_sap)%>% 
  filter(!is.na(DBH), patch_size != "NA") %>% 
  mutate(year=as.numeric(timestep)+1991) %>% 
  dplyr::select(-perturbation, -timestep) %>% 
  filter(climate_scenario=="ssp245") 

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
  group_by(fire_year, species_test, patch_size, species, year) %>% 
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
                       levels = c("Red_Maple", "Sugar_Maple", "Yellow_Birch"), 
                       labels = c("Red Maple", "Sugar Maple", "Yellow Birch"))


pdf(file = "../figures/number_stems.pdf", width = 8, height =10)
ggplot(res3, aes(x=year, y=n, color=species, group=species)) +
  geom_line(linewidth=1) +
  facet_nested(as.numeric(patch_size)~fire_year, scale="free_y") + 
  ylab("Number of stems (juveniles + adults)") + xlab("Year") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 16)) +
  theme(legend.position="bottom") 
dev.off()

png(filename = "../figures/number_stems.png", width = 8, height =10, units="in", res=1000)
ggplot(res3, aes(x=year, y=n, color=species, group=species)) +
  geom_line(linewidth=1) +
  facet_nested(as.numeric(patch_size)~fire_year, scale="free_y") + 
  ylab("Number of stems (juveniles + adults)") + xlab("Year") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 16)) +
  theme(legend.position="bottom") 
dev.off()
