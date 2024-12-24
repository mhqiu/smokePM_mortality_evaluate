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

col_count = c("grey95",met.brewer(name="Tam", n=12, type="continuous"))
# ------------------------------------------------------------------------------
#  Reproduce Figure 4
# ------------------------------------------------------------------------------

#----------------------------------------
#####  Figure 4a: scatter plot
#----------------------------------------
station_model_full <- readRDS(paste0(data_path, "/station_model_conc_combined_10km.rds"))
smoke_condition <- unique(station_model_full$smoke_condition)

for (ss in smoke_condition){
  if (ss == "no smoke"){
    axis_lim = c(0,105)
    axis_break = c(1,5,10,25,50,100)
    max_smoke = 100
  } else if (ss == "low smoke"){
    axis_lim = c(0,105)
    axis_break = c(1,5,10,25,50,100)
    max_smoke = 100
  } else if (ss == "medium and high"){
    axis_lim = c(4,1500)
    axis_break = c(5,25,200,1000)
    max_smoke = 1000 
  }
  
  station_model <- station_model_full %>% filter(smoke_condition==ss)
  station_model[station_model$gc_totalPM>max_smoke, "gc_totalPM"] <- max_smoke
  station_model[station_model$cmaq_totalPM>max_smoke, "cmaq_totalPM"] <- max_smoke
  
  ggplot(station_model, aes(x=station_totalPM, y=cmaq_totalPM)) +
    geom_bin2d() +
    scale_x_continuous(trans = "pseudo_log", limits=axis_lim, breaks = axis_break) +
    scale_y_continuous(trans = "pseudo_log", limits=axis_lim, breaks = axis_break) +
    geom_abline(,0,1,linetype="dashed",size=1) +
    #geom_smooth(method="lm", formula = "y~0+x") +
    scale_fill_gradientn(colors=col_count) +
    theme_classic() + labs(x="",y="") +
    theme(text = element_text(size=24), axis.text = element_text(size=32),
          strip.text = element_text(size=33)) +
    coord_cartesian(expand = F) +
    facet_wrap(~smoke_condition)
  ggsave(paste0("Fig4_totalPM_CAMQ_scatter_",ss,".pdf"), width=8, height=6.4)
}


##----------------------------------------
##### Fig4B: mean over all monitors
#----------------------------------------
station_model_long <- station_model_full %>%
  pivot_longer(cols=c("gc_totalPM", "cmaq_totalPM", "station_totalPM"),
               values_to = "totalPM",
               names_to = "model") 

station_model_date <- station_model_long %>% 
  group_by(date, model) %>%
  summarise(totalPM=mean(totalPM),
            n=length(model),
            frac_smoke=sum(smoke_condition=="medium and high")/n) %>%
  ungroup() %>%
  mutate(smoke_group=if_else(frac_smoke>0.02,1,0))

station_model_date$model <- factor(station_model_date$model, 
                                   levels=c("gc_totalPM", "cmaq_totalPM", "station_totalPM"))

ggplot(station_model_date, 
       aes(x=date,y=totalPM,colour=model)) +
  annotate("rect",xmin=as.Date("2020-08-01"),
                xmax=as.Date("2020-10-30"),
                ymin=2, ymax=240, alpha=0.5, fill="orangered4",  colour=NA) +
  geom_line(size=1, alpha=1) +
  theme_classic() +
  theme(text = element_text(size=22)) +
  labs(x="", y="smoke PM (ug m-3)", colour="") +
  scale_colour_manual(values=c("purple3", "orange","black"))  +
  scale_x_continuous(breaks=as.Date(c("2020-01-01","2020-04-01", "2020-07-01", "2020-10-01")),
                     labels=c("Jan","Apr","Jul","Oct"),
                     limits=as.Date(c("2019-12-15","2020-12-15"))) + 
  scale_y_continuous( trans = "pseudo_log", breaks=c(5,15,50,100,200)) +
  coord_cartesian(expand=F)
ggsave("Figure4B",width=10.5, height=3.5)



