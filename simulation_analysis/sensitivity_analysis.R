rm(list = ls())

library(tidyverse)
theme_set(theme_bw())
library(RODBC)
library(ggh4x)
library(scales)

setwd("~/Temperate_species_colonisation_in_mixedwood_boreal_forest_with_climate_change")

####################################################################
bug <- readRDS(file="result_simulations/res_bug_sensitivity.rds") #Because some of results has been compiled after because of unknown in SORTIE

sen <- readRDS(file="result_simulations/sensitivity/res_summary_sensitivity.rds") %>%
  mutate(year=as.numeric(as.character(period))+as.numeric(as.character(step))) %>%
  filter(species %in% c("Sugar_Maple", "Red_Maple", "Yellow_Birch"), stage=="Adult", year==2100)


exp <- readRDS(file="result_simulations/res_detailed_adult_temperate_species.rds") %>%
  bind_rows(readRDS(file="result_simulations/res_detailed_sapling_temperate_species.rds")) %>% 
  filter(rcp=="rcp45", density=="150", fire_year=="1964") %>% 
  mutate(year=as.numeric(as.character(period))+as.numeric(as.character(timestep))) %>% 
  filter(year==2100) %>% 
  mutate(crossing=species_test) %>% 
  select(-rcp, -year, -fire_year, -period, -stage, -species_test, -timestep, -subplot, -perturbation, -Growth, -Light) %>% 
  filter(!is.na(DBH))

sen <- readRDS(file="result_simulations/sensitivity/res_detailed_adult_temperate_species_sensitivity.rds") %>%
  bind_rows(readRDS(file="result_simulations/sensitivity/res_detailed_sapling_temperate_species_sensitivity.rds")) %>%
  bind_rows(readRDS(file="result_simulations/sensitivity/mortality/res_detailed_adult_temperate_species_sensitivity_mort.rds")) %>%
  bind_rows(readRDS(file="result_simulations/sensitivity/mortality/res_detailed_sapling_temperate_species_sensitivity_mort.rds")) %>% 
  filter(!(parameter=="beta" & species=="Sugar_Maple")) %>%
  bind_rows(filter(bug, species %in% c("Sugar_Maple"))) %>%
  mutate(year=as.numeric(as.character(period))+as.numeric(as.character(timestep))) %>%
  filter(year==2100) %>% 
  mutate(sens_value=as.character(as.numeric(value))) %>% 
  select(-rcp, -year, -fire_year, -period, -stage, -value, -species_test, -timestep, -Growth, -Light) %>% 
  filter(!is.na(DBH))


submodel_par <- data.frame(parameter=c("STR", "d", "x0", "xb", "m", "c", "alpha", "beta", "a", "s", "z", "max", "randadult", "randjuv", "M2", "maxpotgrowth"),
                           submodel=c("Dispersion", "Dispersion",
                                      "Adult growth:\nSize Effect", "Adult growth:\nSize Effect",
                                      "Adult growth:\nShading Effect",
                                      "Adult growth:\nCrowding Effect",
                                      "Adult growth:\nCrowding Effect",
                                      "Adult growth:\nCrowding Effect",
                                      "Juvenile growth",
                                      "Juvenile growth", 
                                      "Adult mortality", 
                                      "Adult mortality",
                                      "Adult mortality",
                                      "Juvenile mortality",
                                      "Juvenile mortality",
                                      "Adult growth"))

design_sen <- readRDS(file="../result_simulations/sensitivity/sensitivity_analysis_design_percent.rds") 
design_sen$species <- factor(design_sen$species, 
                             levels = c("ERR", "ERS", "BOJ"), 
                             labels = c("Red_Maple", "Sugar_Maple", "Yellow_Birch"))


design_sen2 <- unique(select(design_sen, species, parameter, par_value))
exp_f <- NULL
for(i in unique(design_sen2$parameter)){ #i="STR"
  des <- filter(design_sen2, parameter==i)
  exp_sub <- right_join(exp, des) %>% 
    rename(sens_value=par_value)
  exp_f <- bind_rows(exp_f, exp_sub)
}

design_sen <-  design_sen %>% 
  mutate(sens_value=as.character(sens_value)) %>% 
  left_join(submodel_par)

exp_f <- exp_f %>% 
  left_join(unique(select(design_sen, parameter, submodel, par_value))) %>% 
  mutate(sens_value=as.character(sens_value))

sen <- sen %>% 
  left_join(design_sen) %>% 
  bind_rows(exp_f) %>% 
  mutate(fac_value=as.numeric(sens_value)/par_value)


#Adding maxpotgrowth to sen
mpg <- readRDS("../result_simulations/sensitivity/maxpotgrowth/res_detailed_adult_temperate_species_maxpotgrowth.rds") %>% 
  bind_rows(readRDS("../result_simulations/sensitivity/maxpotgrowth/res_detailed_sapling_temperate_species_maxpotgrowth.rds"),
            readRDS("../result_simulations/sensitivity/maxpotgrowth/res_detailed_adult_temperate_species_maxpotgrowth_12.rds"),
            readRDS("../result_simulations/sensitivity/maxpotgrowth/res_detailed_sapling_temperate_species_maxpotgrowth_12.rds")) %>% 
    mutate(year=as.numeric(as.character(period))+as.numeric(as.character(timestep))) %>%
  filter(year==2100) %>% 
  select(species, X, Y, DBH, density, parameter, fac_value=value) %>% 
  left_join(submodel_par) %>% 
  filter(!is.na(DBH)) %>% 
  mutate(crossing="no") %>% 
  bind_rows(mutate(exp, fac_value="1", parameter="maxpotgrowth", submodel="Adult growth"))


mpg_cross <- mpg %>% 
  filter(fac_value %in% c("ERR", "BOJ", "ERS"))

mpg <- mpg %>% 
  filter(!fac_value %in% c("ERR", "BOJ", "ERS")) %>% 
  mutate(fac_value=as.numeric(fac_value))

sen <- bind_rows(sen, mpg)

sen <- sen %>% 
  mutate(d=(X-50)^2+(Y - 50)^2) %>%
  mutate(density=ifelse(density=="NA", "0", density)) %>%
  mutate(d2=as.numeric(density)/pi) %>%
  filter(d2<d) %>%
  group_by(species,parameter, sens_value, density, crossing, fac_value, submodel) %>% 
  summarize(BA=sum(((DBH*0.01)/2)^2*pi, na.rm=T)) %>% 
  mutate(BA=BA*(1-as.numeric(density)/10000)) %>% 
  ungroup() %>% 
  select(-density) %>% 
  full_join(design_sen) 

sen <- rbind(sen,
             data.frame(species="Red_Maple", parameter="maxpotgrowth", sens_value=NA, crossing="no", fac_value=0.01, submodel="Adult growth", BA=0, par_value=NA),
             data.frame(species="Sugar_Maple", parameter="maxpotgrowth", sens_value=NA, crossing="no", fac_value=0.01, submodel="Adult growth", BA=0, par_value=NA),
             data.frame(species="Yellow_Birch", parameter="maxpotgrowth", sens_value=NA, crossing="no", fac_value=0.01, submodel="Adult growth", BA=0, par_value=NA))

sen$BA[is.na(sen$BA)] <- 0
sen$fac_value[is.na(sen$fac_value)] <- as.numeric(sen$sens_value[is.na(sen$fac_value)])/sen$par_value[is.na(sen$fac_value)]

options( "digits"=7, "scipen"=0) 

sen$species <- factor(sen$species, 
                       levels = c("Red_Maple", "Sugar_Maple", "Yellow_Birch"), 
                       labels = c("Red~Maple", "Sugar~Maple", "Yellow~Birch"))

sen$submodel <- factor(sen$submodel, 
                      levels = c(
                        "Juvenile growth",
                        "Adult growth",
                        "Adult growth:\nSize Effect",
                        "Adult growth:\nShading Effect",
                        "Adult growth:\nCrowding Effect",
                        "Dispersion", 
                        "Adult mortality", 
                        "Juvenile mortality"),
                      labels = c(
                        "Juvenile~growth", 
                        "Adult~growth",
                        expression(atop("Adult growth:","Size Effect")),
                        expression(atop("Adult growth:","Shading Effect")),
                        expression(atop("Adult growth:","Crowding Effect")),
                        "Dispersion", 
                        "Adult~mortality", 
                        "Juvenile~mortality"))



sen$parameter <- factor(sen$parameter, 
                       levels = c("STR", "d", "x0", "xb", "m", "c", "alpha", "beta", "a", "s","z", "max", "randadult", "randjuv", "M2", "maxpotgrowth"),
                       labels = c("STR/n", "d", expression(X[0]), expression(X[b]), "m", "c" , expression(alpha), expression(beta), "A", "S","Z", "max", expression(c[a]), expression(c[j]), expression(M[2]), "MaxPotGrowth"))

sen <- rename(sen, `Parameter value from`=crossing)

sen$`Parameter value from` <- factor(sen$`Parameter value from`, 
                      levels = c("BOJ", "ERR", "ERS", "no"), 
                      labels = c("Yellow Birch", "Red Maple", "Sugar Maple", "Sensivity analysis"))

sen <- sen %>% 
  select(-par_value)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols <-  c(gg_color_hue(3), "black")
sen$BA[is.na(sen$BA)] <- 0

sen_prop <- bind_rows(filter(sen, species == "Red~Maple" & `Parameter value from` == "Red Maple"),
          filter(sen, species == "Sugar~Maple" & `Parameter value from` == "Sugar Maple"),
          filter(sen, species == "Yellow~Birch" & `Parameter value from` == "Yellow Birch")) %>% 
  mutate(BA_rel=BA, 
         par_value_rel=sens_value) %>% 
  select(species, parameter, BA_rel, par_value_rel) %>% 
  right_join(sen) %>% arrange(species, parameter) %>% 
  mutate(BA_prop=BA/BA_rel)

equal_breaks <- function(n = 3, s = 0.05, ...){
  function(x){
    d <- s * diff(range(x)) / (1+2*s)
    seq(min(x)+(d), max(x)-(d), length=n)
  }
}

pdf(file = "figures/sensitivity_analysis.pdf", width = 15, height = 7.5)
ggplot(filter(sen_prop,
              submodel %in% c("Juvenile~growth",
                              expression(atop("Adult growth:","Crowding Effect")),
                              "Dispersion",
                              "Juvenile~mortality", 
                              "Adult~growth")),
       aes(x=fac_value, y=BA_prop)) + 
  geom_line() +
  geom_point(aes(color=`Parameter value from`), size=2)+ 
  facet_nested(species~submodel+parameter, scale="free", 
               labeller = labeller(.default=label_parsed, .multi_line=T)) +
  xlab("Multiplicative change in parameter value") +
  ylab("Multiplicative change in final basal area") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 16),
        legend.position="bottom") +
  scale_color_manual(values=cols) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(trans = log2_trans(), breaks=c(0.00025, 0.008, 0.25, 8))+
  scale_x_continuous(trans = log2_trans(), breaks=c(0.03, 0.25,2,16))
dev.off()


pdf(file = "figures/sensitivity_analysis_supp_info.pdf", width = 10, height = 7.5)
ggplot(filter(sen_prop,
              !submodel %in% c("Juvenile~growth",
                              expression(atop("Adult growth:","Crowding Effect")),
                              "Dispersion",
                              "Juvenile~mortality", 
                              "Adult~growth")),
       aes(x=fac_value, y=BA_prop)) + 
  geom_line() +
  geom_point(aes(color=`Parameter value from`), size=2)+ 
  facet_nested(species~submodel+parameter, scale="free", 
               labeller = labeller(.default=label_parsed, .multi_line=T)) +
  xlab("Multiplicative change in parameter value") +
  ylab("Multiplicative change in final basal area") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 16),
        legend.position="bottom") +
  scale_color_manual(values=cols) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(trans = log2_trans())+
  scale_x_continuous(trans = log2_trans(), breaks=c(0.03, 0.25,2))
dev.off()
