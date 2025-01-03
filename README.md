# turbidity-forecasting
Developing models and forecasting turbidity in Esopus Creek, NY

## Overview
In this repo, we share several codes for transforming flow forecasts (from NOAA River Forecast Centers and the U.S. National Water Model) into forecasts of turbidity. This intended as a proof-of-concept for building water quality forecasts from high-resolution monitoring data and a streamflow forecast source of your choice. The testbed for this approach is Esopus Creek, NY, a highly instrumented watershed in the Catskill Mountains of southeastern New York, USA. 

## Specifc files
00_functions: this is where all the functions needed live
01_data_discovery: this finds data from gages upstream of a single gage
02_observational_data_download_and_clean: this downloads discharge and turbidity data from the Esopus Creek gages
03_observational_data_prep: This cleans & formats observational data
04_event_delineation: this delineates storm events from observed discharge
05_model_development: this performs variable selection and trains quantile regression and LightGBM models
06_forecast_data_download: this downloads archived National Water Model and Northeast River Forecast Center streamflow forecasts
07_forecast_data_prep: this formats forecast data and calculates various antecedent parameters needed to make turbidity forecasts
08_make_forecasts: this generates turbidity forecasts 
09_evalulate_performance: this evaluates the performance of benchmark turbidity predictions and turbidity forecasts
