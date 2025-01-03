# turbidity-forecasting
Developing models and forecasting turbidity in Esopus Creek, NY

## Overview
In this repo, we share several codes for transforming flow forecasts (from NOAA River Forecast Centers and the U.S. National Water Model) into forecasts of turbidity. This intended as a proof-of-concept for building water quality forecasts from high-resolution monitoring data and a streamflow forecast source of your choice. The testbed for this approach is Esopus Creek, NY, a highly instrumented watershed in the Catskill Mountains of southeastern New York, USA. 

## Specifc files

* [**00_functions**](https://github.com/jtkemper/turbidity-forecasting/blob/main/all-scripts/00_functions.md): this is where all the functions live\n

* [**01_data_discovery**](https://github.com/jtkemper/turbidity-forecasting/blob/main/all-scripts/01_data_discovery.md): this finds data from gages upstream of a single gage\n

* [**02_observational_data_download_and_clean**](https://github.com/jtkemper/turbidity-forecasting/blob/main/all-scripts/02_observational_data_download_and_clean.md): this downloads discharge and turbidity data from the Esopus Creek gages\n

* [**03_observational_data_prep**](https://github.com/jtkemper/turbidity-forecasting/blob/main/all-scripts/03_observational_data_prep.md): This cleans & formats observational data\n

* [**04_event_delineation**](https://github.com/jtkemper/turbidity-forecasting/blob/main/all-scripts/04_event_delineation.md): this delineates storm events from observed discharge\n

* [**05_model_development**](https://github.com/jtkemper/turbidity-forecasting/blob/main/all-scripts/05_model_development.md): this performs variable selection and trains quantile regression and LightGBM models\n

* [**06_forecast_data_download**](https://github.com/jtkemper/turbidity-forecasting/blob/main/all-scripts/06_forecast_data_download.md): this downloads archived National Water Model and Northeast River Forecast Center streamflow forecasts\n

* [**07_forecast_data_prep**](https://github.com/jtkemper/turbidity-forecasting/blob/main/all-scripts/07_forecast_data_prep.md): this formats forecast data and calculates various antecedent parameters needed to make turbidity forecasts\n

* [**08_make_forecasts**](https://github.com/jtkemper/turbidity-forecasting/blob/main/all-scripts/08_make_forecasts.md): this generates turbidity forecasts\n

* [**09_evaluate_performance**](https://github.com/jtkemper/turbidity-forecasting/blob/main/all-scripts/09_evaluate_performance.md): this evaluates the performance of benchmark turbidity predictions and turbidity forecasts\n
