library(openxlsx)
library(ggplot2)
library(sf)
library(dplyr)   
library(tidyverse)
library(MetBrewer)

rm(list=ls())
gc()

code_path <- "~/public_repo/Scripts"
data_path <- "~public_repo/Data"

# ------------------------------------------------------------------------------
#  Reproduce Figure 6
# ------------------------------------------------------------------------------
grid_10km <- st_as_sf(st_read(paste0(data_path, "/grid_10km_shp/grid_10km_wgs84.shp"))) %>%
  mutate(grid_id_10km=ID)
states_map <- map_data("state")

grid_10km_point <- grid_10km %>% st_transform(crs="WGS84") %>% st_centroid()
grid_10km_point$COORDX <- st_coordinates(grid_10km_point)[,1]
grid_10km_point$COORDY <- st_coordinates(grid_10km_point)[,2]

col_smoke<- c("#CCFFFF","#6EB6F1", "#699E1D", "#fff179","#EA8A39","#A3340D","#470F27")
col_model <- c("purple3", "orange", "steelblue2")

#---------------------------------------------
# Figure 6b: map of the predicted smoke data
#---------------------------------------------
calibrated_smoke <- readRDS(paste0(data_path, "/calibrated_smoekPM.rds"))

date_range <- seq(as.Date("2020-02-01"), as.Date("2020-10-30"), by="days")
ndays <- length(date_range) 
pred_annual <- calibrated_smoke %>% filter(date %in% date_range) %>%
  group_by(grid_id_10km, region) %>%
  summarise(smokePM_pred=sum(smokePM_pred)/ndays) %>% ungroup()

### west US
cut_smoke <- c(0, 1, 3, 5, 10, 15, 25, 50, 100)
max_smoke <- 100

annual_con_sf <- right_join(grid_10km, pred_annual) %>% filter(region=="west")
annual_con_sf[annual_con_sf$smokePM_pred>max_smoke, "smokePM_pred"] <- max_smoke
annual_con_sf$bins <- cut(annual_con_sf$smokePM_pred , cut_smoke)

ggplot() +
  geom_sf(data=annual_con_sf, aes(colour=bins, fill=bins), linewidth=0.0001) +
  geom_polygon(data=states_map,aes(long, lat, group = group),
               fill=NA,color ="black",size=0.3) +
  scale_colour_manual(values = col_smoke, na.value = "white") +
  scale_fill_manual(values = col_smoke, na.value = "white") +
  theme_void() + theme(text = element_text(size=12)) +
  coord_sf(xlim=c(-125, -100), expand=F)
ggsave("Fig6B_west.png", width = 8, height = 4)

### eastern US
cut_smoke <- c(0.1, 0.25, 0.5, 0.75, 1, 2)
max_smoke <- 2

annual_con_sf <- right_join(grid_10km, pred_annual) %>% filter(region=="east")
annual_con_sf[annual_con_sf$smokePM_pred>max_smoke, "smokePM_pred"] <- max_smoke
annual_con_sf$bins <- cut(annual_con_sf$smokePM_pred , cut_smoke)

ggplot() +
  geom_sf(data=annual_con_sf, aes(colour=bins, fill=bins), linewidth=0.0001) +
  geom_polygon(data=states_map,aes(long, lat, group = group),
               fill=NA,color ="black",size=0.3) +
  scale_colour_manual(values = col_smoke, na.value = "white") +
  scale_fill_manual(values = col_smoke, na.value = "white") +
  theme_void() + theme(text = element_text(size=12)) +
  coord_sf(xlim=c(-100, -60), expand = F)
ggsave("Fig6B_east.png", width = 8, height = 4)


