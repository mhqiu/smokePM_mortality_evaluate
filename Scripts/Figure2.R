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
col_model <- c("purple3", "orange", "steelblue2")

# ------------------------------------------------------------------------------
#  Reproduce figure 2
# ------------------------------------------------------------------------------
date_range <- seq(as.Date("2020-02-01"), as.Date("2020-10-30"), by="days") ### average over Feb-Oct the fire active months
ndays <- length(date_range)

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

#----------------------------------------------------------------
######  Figure 2D: annual average of GC, CMAQ, and ML estimates
#----------------------------------------------------------------
gc_mean <- gc %>% group_by(grid_id_10km) %>% 
  summarise_at(c("gc_smokePM","gc_totalPM", "gc_nonsmokePM"), mean, na.rm = T) %>% ungroup()

cmaq_mean <- cmaq %>% group_by(grid_id_10km) %>% 
  summarise_at(c("cmaq_smokePM","cmaq_totalPM", "cmaq_nonsmokePM"), mean, na.rm = T) %>% ungroup()

ml_mean <- ml %>% group_by(grid_id_10km) %>% 
  summarise(ml_smokePM=sum(ml_smokePM, na.rm = T)/ndays) %>% ungroup()

annual_conc <- full_join(gc_mean, cmaq_mean) %>% full_join(ml_mean) %>% filter(!is.na(ml_smokePM))             
### for visualization
annual_con_sf <- right_join(grid_10km, annual_conc)
cut_smoke <- c(0.5, 1, 5, 10, 15, 25, 50, 100)
max_smoke <- 100

annual_con_sf[annual_con_sf$gc_smokePM>max_smoke, "gc_smokePM"] <- max_smoke
annual_con_sf[annual_con_sf$cmaq_smokePM>max_smoke, "cmaq_smokePM"] <- max_smoke
annual_con_sf[annual_con_sf$ml_smokePM>max_smoke, "ml_smokePM"] <- max_smoke

annual_con_sf$gc_bins <- cut(annual_con_sf$gc_smokePM, cut_smoke)
annual_con_sf$cmaq_bins <- cut(annual_con_sf$cmaq_smokePM, cut_smoke)
annual_con_sf$ml_bins <- cut(annual_con_sf$ml_smokePM, cut_smoke)

ggplot() +
  geom_sf(data=annual_con_sf, aes(colour=gc_bins, fill=gc_bins), linewidth=0.0001) +
  geom_polygon(data=states_map,aes(long, lat, group = group),
               fill=NA,color ="black",size=0.3) +
  scale_colour_manual(values = col_smoke, na.value = "white") +
  scale_fill_manual(values = col_smoke, na.value = "white") +
  theme_void() + theme(text = element_text(size=12))
ggsave("Fig2A.png", width = 8, height = 4)

ggplot() +
  geom_sf(data=annual_con_sf, aes(colour=cmaq_bins, fill=cmaq_bins), linewidth=0.0001) +
  geom_polygon(data=states_map,aes(long, lat, group = group),
               fill=NA,color ="black",size=0.3) +
  scale_colour_manual(values = col_smoke, na.value = "white") +
  scale_fill_manual(values = col_smoke, na.value = "white") +
  theme_void() + theme(text = element_text(size=12))
ggsave("Fig2B.png", width = 8, height = 4)

ggplot() +
  geom_sf(data=annual_con_sf, aes(colour=ml_bins, fill=ml_bins), linewidth=0.0001) +
  geom_polygon(data=states_map,aes(long, lat, group = group),
               fill=NA,color ="black",size=0.3) +
  scale_colour_manual(values = col_smoke, na.value = "white") +
  scale_fill_manual(values = col_smoke, na.value = "white") +
  theme_void() + theme(text = element_text(size=12))
ggsave("Fig2C.png", width = 8, height = 4)

#----------------------------------------------------------------
######  Figure 2D: population-weighted smoke PM at the state-level
#----------------------------------------------------------------
pop_df <- readRDS(paste0(data_path, "/population_10km_grid.rds"))
state_list <- c("California", "Georgia")
pop_state <- filter(pop_df, state%in%state_list)
grid_id_list <- unique(pop_state$grid_id_10km)

state_grid <- gc %>% filter(grid_id_10km %in% grid_id_list) %>%
  full_join(cmaq %>% filter(grid_id_10km %in% grid_id_list)) %>%
  full_join(ml %>% filter(grid_id_10km %in% grid_id_list)) %>%
  mutate(ml_smokePM=replace(ml_smokePM, is.na(ml_smokePM), 0))

state_grid <- left_join(state_grid, pop_state)

var_list <- c("gc_smokePM", "cmaq_smokePM", "ml_smokePM")
state_summ <- state_grid %>% group_by(state, date) %>%
  summarise_at(var_list,  funs(weighted.mean(., w=pop_2020))) %>%
  ungroup() %>%
  pivot_longer(cols=var_list)

#### calculate US pop-weighted smoke
us_grid <- gc %>% 
  full_join(cmaq) %>%
  full_join(ml) %>%
  mutate(ml_smokePM=replace(ml_smokePM, is.na(ml_smokePM), 0))

us_summ <- left_join(us_grid, pop_df) %>% group_by(date) %>%
  summarise_at(var_list,  funs(weighted.mean(., w=pop_2020))) %>%
  ungroup() %>%
  pivot_longer(cols=var_list) %>% mutate(state="US")
state_summ <- bind_rows(state_summ, us_summ)

state_summ$state <- factor(state_summ$state, levels=c("US","California", "Georgia"))
state_summ$name <- factor(state_summ$name, levels=var_list)

ggplot(state_summ, aes(x=date, y=value, colour=name)) +
  geom_line(size=1.5, alpha=0.7) +
  theme_classic() +
  theme(text = element_text(size=22)) +
  labs(x="", y="smoke PM (ug m-3)", colour="") +
  scale_colour_manual(values = col_model) +
  facet_wrap(~state, scales = "free", ncol=1)
ggsave("Figure2D.pdf", height = 8, width=8)
