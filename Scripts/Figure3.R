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

grid_10km <- st_as_sf(st_read(paste0(data_path, "/grid_10km_shp/grid_10km_wgs84.shp"))) %>%
  mutate(grid_id_10km=ID)
states_map <- map_data("state")

col_smoke <- c("#CCFFFF","#6EB6F1", "#699E1D", "#fff179","#EA8A39","#A3340D","#470F27")

grid_10km_point <- grid_10km %>%
  st_transform(crs="WGS84") %>% st_centroid()
grid_10km_point$COORDX <- st_coordinates(grid_10km_point)[,1]
grid_10km_point$COORDY <- st_coordinates(grid_10km_point)[,2]
# ------------------------------------------------------------------------------
#  Reproduce Figure 3
# ------------------------------------------------------------------------------

#----------------------------------------------------------------
###### Figure 3A-C average smoke estimates over west in September
#----------------------------------------------------------------
date_range <- seq(as.Date("2020-09-06"), as.Date("2020-09-20"), by="days") ## west US
ndays <- length(date_range)
  cut_smoke <- c(5, 10, 25, 50, 100, 200, 500, 1000)
  max_smoke <- 1000
  xlim <- c(-125, -118)
  ylim <- c(36, 46)

  gc <- readRDS(paste0(data_path, "/GC_results_10km.rds")) %>%
    filter(date %in% date_range) %>%
    rename(gc_smokePM = smoke_PM25,
           gc_nonsmokePM = nonsmoke_PM25,
           gc_totalPM = total_PM25)
  
  cmaq <- readRDS(paste0(data_path, "/CMAQ_results_10km.rds")) %>%
    filter(date %in% date_range) %>%
    rename(cmaq_smokePM = smoke_PM25,
           cmaq_nonsmokePM = nonsmoke_PM25,
           cmaq_totalPM = total_PM25)
  
  ml <- readRDS(paste0(data_path, "/ML_results_10km.rds")) %>%
    filter(date %in% date_range) %>%
    rename(ml_smokePM = smokePM_pred)
  
combined_conc <- full_join(gc, cmaq) %>% full_join(ml) %>% 
  mutate(ml_smokePM=replace(ml_smokePM, is.na(ml_smokePM), 0))  

########### calculate average over the smoke period
conc_mean <- combined_conc %>% group_by(grid_id_10km) %>% 
  summarise_at(c("gc_smokePM","gc_totalPM", "gc_nonsmokePM",
                 "cmaq_smokePM","cmaq_totalPM", "cmaq_nonsmokePM",
                 "ml_smokePM"),
                mean, na.rm = T) %>% ungroup()

mean_sf <- right_join(grid_10km, conc_mean)

mean_sf[mean_sf$gc_smokePM>max_smoke, "gc_smokePM"] <- max_smoke
mean_sf[mean_sf$cmaq_smokePM>max_smoke, "cmaq_smokePM"] <- max_smoke
mean_sf[mean_sf$ml_smokePM>max_smoke, "ml_smokePM"] <- max_smoke

mean_sf$gc_bins <- cut(mean_sf$gc_smokePM, cut_smoke)
mean_sf$cmaq_bins <- cut(mean_sf$cmaq_smokePM, cut_smoke)
mean_sf$ml_bins <- cut(mean_sf$ml_smokePM, cut_smoke)

mean_sf <- mean_sf %>% pivot_longer(cols=c("gc_bins","cmaq_bins","ml_bins"))
mean_sf$name <- factor(mean_sf$name, levels = c("gc_bins","cmaq_bins","ml_bins"))

ggplot() +
  geom_sf(data=mean_sf,
          aes(colour=value, fill=value), linewidth=0.0001) +
  geom_polygon(data=states_map, aes(long, lat, group = group),
               fill=NA,color ="black",size=0.3) +
  scale_colour_manual(values = col_smoke, na.value = "white") +
  scale_fill_manual(values = col_smoke, na.value = "white") +
  theme_void() + theme(text = element_text(size=12),
                       panel.spacing = unit(2, "lines")) +
  coord_sf(xlim=xlim, ylim=ylim) +
  facet_wrap(~name)
ggsave("Figure3_ABC.png", width = 10, height = 4)


