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
#  Reproduce Figure 5
# ------------------------------------------------------------------------------
grid_10km <- st_as_sf(st_read(paste0(data_path, "/grid_10km_shp/grid_10km_wgs84.shp"))) %>%
  mutate(grid_id_10km=ID)
states_map <- map_data("state")

grid_10km_point <- grid_10km %>% st_transform(crs="WGS84") %>% st_centroid()
grid_10km_point$COORDX <- st_coordinates(grid_10km_point)[,1]
grid_10km_point$COORDY <- st_coordinates(grid_10km_point)[,2]

#---------------------------------------------
# Figure 5A: which monitor performs the best
#---------------------------------------------
pred_data <- readRDS(paste0(data_path, "/station_model_totalPM_eval.rds"))

### evaluate over all days
rmse_grid <-  pred_data %>%
  group_by(grid_id_10km, lat, lon) %>%
  summarise(rmse_gc=sqrt(mean((station_totalPM-gc_total_pred)^2)),
            rmse_cmaq=sqrt(mean((station_totalPM-cmaq_total_pred)^2)),
            rmse_ml=sqrt(mean((station_totalPM-ml_total_pred)^2)),
            station_smokePM=mean(station_calib_smokePM,na.rm=T),
            n=length(date)) %>%
  ungroup() %>%
  mutate(rmse_best=pmin(rmse_gc, rmse_cmaq, rmse_ml),
         best=case_when(rmse_gc==rmse_best ~ "GEOS-Chem",
                        rmse_cmaq==rmse_best ~ "CMAQ",
                        rmse_ml==rmse_best ~ "ML"))  %>%
  filter(n>30, rmse_best > 0)

rmse_grid_sf <- right_join(grid_10km_point %>% rename(lat=COORDY, lon=COORDX),
                           rmse_grid %>% select(-lon, -lat))
rmse_grid_sf$best <- factor(rmse_grid_sf$best, levels=c("GEOS-Chem","CMAQ","ML")) 

ggplot() +
  geom_sf(data=rmse_grid_sf,
          aes(x=lon, y=lat, colour=best), size=1, alpha=0.7) +
  geom_polygon(data=states_map, aes(long, lat, group = group),
               fill=NA,color ="black",size=0.3) +
  scale_colour_manual(values = col_model, na.value = "white") +
  theme_void() + theme(text = element_text(size=12),
                       panel.spacing = unit(2, "lines")) 
ggsave("Figure5A.png", width = 6, height = 4)

#---------------------------------------------
# Figure 5B: difference against the best method
#---------------------------------------------
rmse_grid_long <- rmse_grid %>% 
  pivot_longer(cols=rmse_gc:rmse_ml, values_to = "rmse",
               names_to = "model") %>%
  mutate(rmse_diff=(rmse-rmse_best)/rmse_best)

rmse_grid_long <- rmse_grid_long %>%
  group_by(model,best) %>%
  summarise(mean=mean(rmse_diff, na.rm=T),
            p50=quantile(rmse_diff, 0.5, na.rm=T),
            p01=quantile(rmse_diff, 0.01, na.rm=T),
            p05=quantile(rmse_diff, 0.05, na.rm=T),
            p10=quantile(rmse_diff, 0.1, na.rm=T),
            p25=quantile(rmse_diff, 0.25, na.rm=T),
            p99=quantile(rmse_diff, 0.99, na.rm=T),
            p95=quantile(rmse_diff, 0.95, na.rm=T),
            p90=quantile(rmse_diff, 0.9, na.rm=T),
            p75=quantile(rmse_diff, 0.75, na.rm=T) ) %>% ungroup()

rmse_grid_long$model <- factor(rmse_grid_long$model, 
                               levels=c("rmse_gc", "rmse_cmaq", "rmse_ml"),
                               labels=c("GEOS-Chem", "CMAQ", "ML"))
rmse_grid_long$best <- factor(rmse_grid_long$best, 
                               levels=c("GEOS-Chem", "CMAQ", "ML"))

ggplot(rmse_grid_long %>% filter(model!=best),
       aes(x=model, colour=model)) +
  geom_errorbar(aes(ymin=p25, ymax=p75), size=5, width=0.0001) +
  geom_errorbar(aes(ymin=p10, ymax=p90), width=0.0001) +
  geom_errorbar(aes(ymin=p50, ymax=p50),width=0.45) +
  scale_colour_manual(values=col_model) +
  facet_wrap(~best, scale="free") +
  theme_classic() +
  theme(text = element_text(size=18),
        axis.text = element_text(size=18),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  labs(x="", y="RMSE diff", colour="") +
  scale_y_continuous(labels = scales::percent)
ggsave("Figure5B.pdf",width=9, height=2.7)
