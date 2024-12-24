# README

This repo includes supporting material for Qiu et al., Environmental Science and Technology, 2024 "Evaluating Chemical Transport and Machine Learning Models for Wildfire Smoke PM2.5: Implications for Assessment of Health Impacts". 
Publisher version accessed at: https://doi.org/10.1021/acs.est.4c05922


## Supplementary data can be downloaded at https://www.dropbox.com/scl/fo/lcfulvl4mhym0lanb1n8g/AG_0M2T5mBMAu-JxhydMW38?rlkey=nembvraooos85bn4u3o9w1ico&dl=0
(Data size: 1.8 gb)

- grid_10km_shp: shapefile for the 10km grid used in the paper.

- GC_results_10km.rds: daily GEOS-Chem outputs regridded at 10km grid

- CMAQ_results_10km.rds: daily CMAQ outputs regridded at 10km grid

- ML_results_10km.rds: daily ML outputs regridded at 10km grid

- population_10km_grid.rds: population at 10km grid

- station_model_conc_combined_10km.rds: smoke estimates and station observations regridded at 10km

- station_model_totalPM_eval.rds: dataset used to perform evaluation based on totalPM.

“nonsmokePM_pred” is the common predicted nonsmoke_PM, "gc_total_pred", ”cmaq_total_pred", “ml_total_pred" are derived by adding associated smoke estimates to the common nonsmoke_PM estimates

- calibrated_smoekPM.rds: calibrated smoke PM estimates

- crf_different_smoke_allcause.xlsx: derived dose-response functions


### downloaded data needs to be put in /Data for the code to run.

### /Scripts contains R code that can be used to replicate main text figures 





