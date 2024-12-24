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

col_model <- c("purple3", "orange", "steelblue2")
# ------------------------------------------------------------------------------
# Reproduce Figure 7
# ------------------------------------------------------------------------------

#---------------------------------------------
# Figure 7A: dose-response function
#---------------------------------------------
coef <- read.xlsx(paste0(data_path,"/crf_different_smoke_allcause.xlsx"))
coef$model <- factor(coef$model, levels= c("smokePM_pred","gc_smokePM", "cmaq_smokePM", "ml_smokePM"),
                                           labels= c("Calibrated","GEOS-Chem", "CMAQ", "ML"))

ggplot(coef,
       aes(x=bins, y=mean, colour=model, group=model)) +
  geom_point(size=3,
             position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=p025,
                    ymax=p975), width=0.001, linewidth=1,alpha=0.8,
                    position=position_dodge(width=0.5)) +
  geom_hline(aes(yintercept=0), linetype="dashed") +
  theme_classic() +
  theme(text = element_text(size=21),
        legend.position = c(.25,.29), legend.background = element_blank()) +
  scale_y_continuous(labels = scales::percent, breaks=seq(-0.06,0.09,0.03),
                     limits = c(-0.1, 0.12)) +
  scale_colour_manual(values=c("darkred",col_model)) +
  labs(x="", y="death rate change", colour="") 
ggsave("Fig7A.pdf", width=7.5, height=5.5)

