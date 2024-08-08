rm(list = ls())

library(tidyverse)
theme_set(theme_bw())
library(RODBC)
library(ggh4x)
library(scales)


sen <- readRDS("../result_simulations/sensitivity_analysis/res_detailed_adult.rds") %>%
  bind_rows(readRDS("../result_simulations/sensitivity_analysis/res_detailed_sapling.rds")) %>% 
  mutate(year=1990+as.numeric(as.character(timestep))) %>%
  mutate(sens_value=as.character(as.numeric(parameter_value))) %>% 
  select(-climate_scenario, -year, -fire_year, -stage, -parameter_value , patch_size, -species_test, -timestep, -Growth, -Light, -perturbation) %>% 
  filter(!is.na(DBH), 
         species%in% c("Sugar_Maple", "Red_Maple", "Yellow_Birch"))

#creation of sub-models
submodel_par <- data.frame(parameter=c("STR", "d", "x0", "xb", "m", "c", "alpha", 
                                       "beta", "a", "s", "z", "max", "randadult", 
                                       "randjuv", "M2", "MaxPotGrowth",
                                       "Atemp", "Ctemp", "Aprec", "Cprec"),
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
                                      "Adult growth",
                                      "Adult growth:\nTemperature Effect",
                                      "Adult growth:\nTemperature Effect",
                                      "Adult growth:\nPrecipitation Effect",
                                      "Adult growth:\nPrecipitation Effect"))


design_sen <- readRDS("../calculator/sensitivity_analysis/data/sensitivity_analysis_design_percent.rds") %>% 
  mutate(sens_value=as.character(sens_value))

design_sen$species <- factor(design_sen$species, 
                      levels = c("ERR", "ERS", "BOJ"), 
                      labels = c("Red_Maple", "Sugar_Maple", "Yellow_Birch"))


#I need to attach the sensitivity analysis design table and the results table
sen <- sen %>% 
  left_join(design_sen) %>% 
  left_join(submodel_par) %>% 
  mutate(fac_value=as.numeric(sens_value)/par_value)

sen <- sen %>% 
  mutate(d=(X-50)^2+(Y - 50)^2) %>%
  mutate(patch_size=ifelse(patch_size=="NA", "0", patch_size)) %>%
  mutate(d2=as.numeric(patch_size)/pi) %>%
  filter(d2<d) %>%
  group_by(species,parameter, sens_value,par_value, patch_size, crossing, fac_value, submodel) %>% 
  summarize(BA=sum(((DBH*0.01)/2)^2*pi, na.rm=T)) %>% 
  mutate(BA=BA*(1-as.numeric(patch_size)/10000)) %>% 
  ungroup() %>% 
  select(-patch_size)

exp <- readRDS("../result_simulations/sensitivity_analysis/res_prin_sens_analysis.rds") %>% 
  mutate(d=(X-50)^2+(Y - 50)^2) %>%
  mutate(patch_size=ifelse(patch_size=="NA", "0", patch_size)) %>%
  mutate(d2=as.numeric(patch_size)/pi) %>%
  filter(d2<d) %>%
  group_by(species, patch_size, crossing) %>% 
  summarize(BA=sum(((DBH*0.01)/2)^2*pi, na.rm=T)) %>% 
  mutate(BA=BA*(1-as.numeric(patch_size)/10000)) %>% 
  ungroup() %>% 
  select(-patch_size) %>% 
  mutate(fac_value=1, 
         par_value=NA,
         sens_value=NA) %>%
  slice(rep(1:n(), 20)) %>% 
  mutate(parameter=sort(rep(unique(sen$parameter), 3))) %>%
  left_join(submodel_par)
  
sen <- sen %>% 
  bind_rows(exp) %>% 
  arrange(species, parameter, fac_value)

options( "digits"=7, "scipen"=0) 

sen$species <- factor(sen$species, 
                       levels = c("Red_Maple", "Sugar_Maple", "Yellow_Birch"), 
                       labels = c("Red~maple", "Sugar~maple", "Yellow~birch"))

sen$submodel <- factor(sen$submodel, 
                      levels = c(
                        "Juvenile growth",
                        "Adult growth",
                        "Adult growth:\nSize Effect",
                        "Adult growth:\nShading Effect",
                        "Adult growth:\nCrowding Effect",
                        "Adult growth:\nTemperature Effect",
                        "Adult growth:\nPrecipitation Effect",
                        "Dispersion", 
                        "Adult mortality", 
                        "Juvenile mortality"),
                      labels = c(
                        "Juvenile~growth", 
                        "Adult~growth",
                        expression(atop("Adult growth:","Size effect")),
                        expression(atop("Adult growth:","Shading effect")),
                        expression(atop("Adult growth:","Crowding effect")),
                        expression(atop("Adult growth:","Temperature effect")),
                        expression(atop("Adult growth:","Precipitation effect")),
                        "Dispersion", 
                        "Adult~mortality", 
                        "Juvenile~mortality"))


sen$parameter <- factor(sen$parameter, 
                       levels = c("STR", "d", "x0", "xb",
                                  "m", "c", "alpha", "beta",
                                  "a", "s","z", "max", "randadult",
                                  "randjuv", "M2", "MaxPotGrowth",
                                  "Atemp", "Ctemp", 
                                  "Aprec", "Cprec"),
                       labels = c("STR", "d", expression(X[0]), expression(X[b]),
                                  "m", "c" , expression(alpha), expression(beta),
                                  "A", "S","Z", "max", expression(c[a]), 
                                  expression(c[j]), expression(M[2]), "MaxPotGrowth",
                                  expression(A[t]), expression(C[t]), 
                                  expression(A[p]), expression(C[p])))

sen <- rename(sen, `Parameter value from`=crossing)

sen$`Parameter value from` <- factor(sen$`Parameter value from`, 
                      levels = c("BOJ", "ERR", "ERS", "no"), 
                      labels = c("Yellow birch", "Red maple", "Sugar maple", "Sensivity analysis"))

sen <- sen %>% 
  select(-par_value)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols <-  c(gg_color_hue(3), "black")
sen$BA[is.na(sen$BA)] <- 0


sen_prop <- bind_rows(filter(sen, species == "Red~maple" & `Parameter value from` == "Red maple"),
          filter(sen, species == "Sugar~maple" & `Parameter value from` == "Sugar maple"),
          filter(sen, species == "Yellow~birch" & `Parameter value from` == "Yellow birch")) %>% 
  mutate(BA_rel=BA, 
         par_value_rel=sens_value) %>% 
  select(species, parameter, BA_rel, par_value_rel) %>% 
  right_join(sen) %>% arrange(species, parameter) %>% 
  mutate(BA_prop=BA/BA_rel)

equal_breaks <- function(n = 3, s = 0.05, ...){
  function(x){
    # rescaling
    d <- s * diff(range(x)) / (1+2*s)
    seq(min(x)+(d), max(x)-(d), length=n)
  }
}


p_article <- ggplot(filter(sen_prop,
                           submodel %in% c("Juvenile~growth",
                                           "Adult~growth",
                                           "Dispersion",
                                           "Juvenile~mortality", 
                                           "Adult~growth")),
                    aes(x=fac_value, y=BA_prop)) + 
  geom_line() +
  geom_vline(xintercept = 1, color="grey40", linetype=2) +
  geom_point(aes(color=`Parameter value from`), size=2)+ 
  ggh4x::facet_nested(species~submodel+parameter, scale="free", 
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


#In the article: Juvenile growth, MaxPotGrowth, Dispersion, Juvenile mortality
pdf(file = "../figures/sensitivity_analysis.pdf", width = 15, height = 7.5)
p_article
dev.off()

png(filename = "../figures/sensitivity_analysis.png", width = 15, height = 7.5, unit="in", res=1000)
p_article
dev.off()

p_SI <- ggplot(filter(sen_prop,
                      !submodel %in% c("Juvenile~growth",
                                       "Adult~growth",
                                       "Dispersion",
                                       "Juvenile~mortality", 
                                       "Adult~growth")),
               aes(x=fac_value, y=BA_prop)) + 
  geom_line() +
  geom_vline(xintercept = 1, color="grey40", linetype=2) +
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
  scale_y_continuous(trans = log2_trans())+
  scale_x_continuous(trans = log2_trans(), breaks=c(0.03, 0.25,2))


pdf(file = "../figures/sensitivity_analysis_SI.pdf", width = 15, height = 7.5)
p_SI
dev.off()

png(filename = "../figures/sensitivity_analysis_SI.png", width = 20, height = 7.5, unit="in", res=1000)
p_SI
dev.off()

