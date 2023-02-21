rm(list=ls())

library(leaflet)
library(tidyverse)
library(ggthemes)
library(mapview)
library(rgdal)
library(htmlwidgets)
library(leaflegend)
library(sf)
library(maps)
library(ggspatial)
library(ggnewscale)
library(rgeos)
library(grid)
library(ggplotify)


FERLD <- read.csv("~/PhD/Chap1/SORTIE/Change_par_lancer_SORTIE/Data/FERLD_PEP_climateNA/FERLD_climateNA.csv") %>% 
  mutate(plot_name="")

dom_bio <- readOGR(dsn = "~/PhD/Data/ShapeFiles/Quebec/Sous_domaine_bioclimatique/sous_dom_bioclim.shp",
                   layer = "sous_dom_bioclim")

lim_ers <- readOGR(dsn = "~/PhD/Data/ShapeFiles/Limite_distribution_espece/ERS_northern_boundary.shp",
                   layer = "ERS_northern_boundary")
lim_ers <- spTransform(lim_ers, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

lim_err <- readOGR(dsn = "~/PhD/Data/ShapeFiles/Limite_distribution_espece/ERR_northern_boundary.shp",
                   layer = "ERR_northern_boundary")
lim_err <- spTransform(lim_err, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

lim_boj <- readOGR(dsn = "~/PhD/Data/ShapeFiles/Limite_distribution_espece/BOJ_northern_boundary.shp",
                   layer = "BOJ_northern_boundary")
lim_boj <- spTransform(lim_boj, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

lims <- raster::bind(lim_ers, lim_err, lim_boj)

PEP <- PEP %>% mutate(ID="Validation plots") %>% select(-ID1, -ID2)
FERLD <- FERLD %>% mutate(ID="FERLD") %>% select(-ï..ID1, -ID2)
data <- rbind(PEP, FERLD)

dom_bio_map <- subset(dom_bio, dom_bio$DOM_BIO %in% c("4", "5"))


dom_bio_map <- spTransform(dom_bio_map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))


canada <- sf::st_as_sf(map('world', regions="canada", plot = FALSE, fill = TRUE))
world <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))

lim_bio <- dom_bio_map@bbox

dom_bio_map <- st_as_sf(dom_bio_map) 

dom_bio_map  = st_sf(
  aggregate(
    dom_bio_map,
    by=list(DOM_BIO=dom_bio_map$DOM_BIO),
    FUN=function(vals){vals[1]}))

lims <- st_as_sf(lims)
lims$Species <- c("Sugar maple", "Red maple", "Yellow birch")

color_blind <- c('black','#EE7733', '#0077BB', "#009E73", "#F0E442", '#33BBEE', '#EE3377', '#CC3311', '#009988', '#AA4499') #https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible
                       
# color_blind <- unname(palette.colors(palette = "Okabe-Ito"))
# unname(palette.colors(palette = "Okabe-Ito"))
# "#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7" "#999999"

data <- filter(data, ID=="FERLD")

main <- ggplot()+ geom_sf(data = canada) +
  geom_sf(data=dom_bio_map, aes(fill=DOM_BIO.1), alpha=0.6) +
  scale_fill_manual(values=c(color_blind[5], color_blind[4]),
                    breaks = c("4", "5"),
                    labels = c("Balsam fir -\nyellow birch", "Balsam fir -\npaper birch")) +
  labs(fill="Bioclimatic domain") +
  geom_sf(data=lims, aes(color=Species), size=1) +
  labs(color="Continuous northern\ndistribution limit") +
  scale_color_manual(values=c(color_blind[2], color_blind[3], color_blind[10])) + 
  new_scale_color() + 
  geom_point(data=data, aes(x=long, y=lat, color=ID), size=5)+
  scale_color_manual(values=c(color_blind[1], color_blind[2])) +
  # geom_text(data=data, aes(x=long, y=lat, label=plot_name)) +
  labs(color="Study area")+
  coord_sf(xlim = c(lim_bio[1,1]+0.45, lim_bio[1,2]+lim_bio[1,2]/7), ylim = c(lim_bio[2,1], lim_bio[2,2]-0.4), expand=T) +
  theme_bw() +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.15, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.35, height = unit(0.1, "cm")) +
  xlab("Latitude") +
  ylab("Longitude")+
  theme(legend.title.align = 0.5,
    panel.background = element_rect(fill = "dodgerblue3"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 15),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15),
    strip.text = element_text(size = 12),
    plot.title = element_text(size = 16)
  )

emprise <- ggplot(data=world) +
  geom_sf() +
  #???coord_sf(xlim = c(lim_bio[1,1], lim_bio[1,2]+lim_bio[1,2]/7), ylim = c(lim_bio[2,1], lim_bio[2,2]), expand = FALSE) +
  coord_sf(xlim = c(-99, -51), ylim = c(30, 62), expand = FALSE) +
  theme_void() +
  annotate("rect", xmin = lim_bio[1,1]+0.45, xmax = lim_bio[1,2]+lim_bio[1,2]/7, ymin = lim_bio[2,1], ymax = lim_bio[2,2]-0.4,
           alpha = .6, fill = "grey50", color="black",size=0.5) +
  theme(
    panel.background = element_rect(fill = "dodgerblue3"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1))

library(cowplot)
library(ggpubr)

map <-
  ggdraw() +
  draw_plot(main) +
  draw_plot(emprise, x=0.83, y=0.7, width=.15, height=.15)

# climate <- readRDS(file="~/PhD/chap3/article/figures/map/climate_ferld.rds") %>% 
#   pivot_longer(cols = CMI_JJA:MAT, names_to="climate", values_to = "values")
# 
# clim_plot <- ggplot(climate, aes(x=as.numeric(period), y=values, group=rcp, col=rcp)) + geom_point() + geom_line() +
#   facet_wrap(~climate, scale="free", nrow=1) + 
#   ylab("Climate values at LDRTF") + xlab("Time (in year)") + 
#   labs(color="Climate scenario")+ theme(legend.position="bottom")

pdf(file="~/PhD/chap3/article/figures/map/map_ggplot.pdf")
  map
dev.off()




