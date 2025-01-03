08_make_forecasts
================
JTK
2025-01-02

# 

################################################################################ 

This script transforms (post-processes) NWM and NERFC forecasts to make
turbidity forecasts
\################################################################################

# Housekeeping

### Packages

``` r
### Data mgmt
require(tidyverse)
require(here)

### Modeling
require(hydroGOF)
require(quantreg)
require(lightgbm)
```

### Load prior scripts

``` r
source(knitr::purl(here("Rmd-files/02_observational_data_download_and_clean.Rmd"), 
                   quiet=TRUE))

source(knitr::purl(here("Rmd-files/03_observational_data_prep.Rmd"), 
                   quiet=TRUE))

source(knitr::purl(here("Rmd-files/04_event_delineation.Rmd"), 
                   quiet=TRUE))

source(knitr::purl(here("Rmd-files/05_model_development.Rmd"), 
                   quiet=TRUE))

source(knitr::purl(here("Rmd-files/06_forecast_data_download.Rmd"), 
                   quiet=TRUE))

source(knitr::purl(here("Rmd-files/07_forecast_data_prep.Rmd"), 
                   quiet=TRUE))
```

# Forecast with NWM

### Using Quantile-Coldbrook

``` r
#### Make the forecasts

qqm_nwm_median <- quantile_predictor(nwm_forecasts_mt_trim %>%
                                        filter(tributary == "ESOPUSCREEKATCOLDBROOKNY") %>%
                                        mutate(init_date = as_date(init_date)) %>%
                                        rename(modeled_flow_cms = mean_modeled_flow) %>%
                                        dplyr::select(!c(init_datetime, 
                                                         predict_water_year, comid)),
                                     "forecast") %>%
  rename(model = tau) %>%
  filter(model == 0.5) %>%
  tidyr::unnest(data) %>%
  unnest(data)

#### Clean up the dataframe

qqm_nwm_median <- qqm_nwm_median %>%
  mutate(lead_time = as.numeric(lead_time)) %>%
  inner_join(., coldbrook_turbidity_plus_drivers_clean %>%
               dplyr::select(dateTime, 
                             fw_mean_observed_turbidity,
                             log_observed_turbidity),
             join_by(predict_dateTime == dateTime)) %>%
  mutate(forecasted_turbidity = 10^log_predicted_turbidity*D_qqm) %>%
  inner_join(., cold_test %>%
               dplyr::select(dateTime, log_observed_flow),
               join_by(predict_dateTime == dateTime)) %>%
  inner_join(., lead_groups,
             by = "lead_time")
```

### Using LGBM-Coldbrook

``` r
here("r_images", "final_tuned_lgbm_for_turb_with_nerfc.txt")


#### First, subset the testing data to only include
#### variables chosen by the backwards selection

nwm_cold_only_forecast <- all_predictors_cold_only %>%
  dplyr::ungroup() %>%
  dplyr::select(nerfc_chosen_model$Feature)

### Declare the test data

forecast_tuned_lgbm_co <- data.matrix(nwm_cold_only_forecast)

### Load in the tuned LGBM model

final_tuned_lgbm_for_turb_with_nerfc <- lgb.load(here("r_images", 
                                                      "final_tuned_lgbm_for_turb_with_nerfc.txt"))

### Forecast with the model and bind to observations

nwm_turb_forecast_co <- predict(final_tuned_lgbm_for_turb_with_nerfc, 
                                       data = forecast_tuned_lgbm_co) %>% 
  as_tibble() %>% 
  rename(log_forecasted_turbidity = 1) %>%
  bind_cols(., all_predictors_cold_only %>%
              rename_with(~paste0(., "_forc"), where(is.numeric))) %>%
  rename(log_modeled_flow = log_observed_flow_forc,
         lead_time = lead_time_forc) %>%
  inner_join(., cold_test %>%
               dplyr::select(dateTime, 
                             fw_mean_observed_turbidity,
                             log_observed_turbidity, 
                             nerfc_chosen_model$Feature),
             join_by(predict_dateTime == dateTime)) 

### Finalize dataframe by adding in smearing factor

nwm_turb_forecast_co <- nwm_turb_forecast_co %>%
  mutate(forecasted_turbidity = 10^log_forecasted_turbidity*d_lgbm_cold_only) %>%
  mutate(raw_error = forecasted_turbidity - fw_mean_observed_turbidity,
         sqrerr = (raw_error)^2,
         abs_error = abs(raw_error),
         abs_pct_error = abs_error/fw_mean_observed_turbidity)
```

### Using LGBM-Network

``` r
### Now subset the predictor dataframe to the variables selected
### in the model development process

esopus_cold_forecast <- all_predictors %>%
  dplyr::ungroup() %>%
  dplyr::select(chosen_vars$Feature)
  
### Declare the test data

forecast_tuned_lgbm <- data.matrix(esopus_cold_forecast)
        
### Load in the tuned LGBM model

final_tuned_lgbm_for_turb_with_nerfc <- lgb.load(here("r_images", 
                                                      "final_tuned_lgbm_for_turb_with_nwm.txt"))
        
### Forecast with the model, bind to observations

cold_turb_forecast <- predict(final_tuned_lgbm_for_turb_with_nwm, 
                                       data = forecast_tuned_lgbm) %>%
          as_tibble() %>% 
          rename(log_forecasted_turbidity = 1) %>%
          bind_cols(., all_predictors) %>%
          rename(log_modeled_flow = 
                      log_observed_flow,
                 log_modeled_flow_BEAVERKILLATMOUNTTREMPERNY= 
                      log_observed_flow_BEAVERKILLATMOUNTTREMPERNY,
                 log_modeled_flow_WARNERCREEKNEARCHICHESTERNY =
                  log_observed_flow_WARNERCREEKNEARCHICHESTERNY) %>%
          rename(log_limb_forc = log_limb,
                 max_prior_monthly_log_q_forc = max_prior_monthly_log_q,
                 max_prior_threemonth_log_q_forc = max_prior_threemonth_log_q,
                 mean_prior_weekly_log_q_BEAVERKILLATMOUNTTREMPERNY_forc = 
                    mean_prior_weekly_log_q_BEAVERKILLATMOUNTTREMPERNY,
                 time_since_annual_peak_ESOPUSCREEKATALLABENNY_forc = 
                    time_since_annual_peak_ESOPUSCREEKATALLABENNY) %>%
          inner_join(., cold_test %>%
                   dplyr::select(dateTime, 
                                 fw_mean_observed_turbidity,
                                 log_observed_turbidity, 
                                 chosen_vars$Feature),
                   join_by(predict_dateTime == dateTime)) 
        

        

#### Calculate final turbidity value using smearing factor 

cold_turb_forecast <- cold_turb_forecast %>%
  mutate(forecasted_turbidity = 10^log_forecasted_turbidity*d_lgbm_network)
```

# Forecast with NERFC

### Using Quantile-Coldbrook

``` r
#### Make the forecasts

qqm_nerfc_median <- quantile_predictor(discharge_nerfc_esopus_once_daily %>%
                                        filter(tributary == "ESOPUSCREEKATCOLDBROOKNY") %>%
                                        mutate(init_date = as_date(init_date)) %>%
                                        dplyr::select(!c(init_dateTime
                                                         )),
                                     "forecast") %>%
  rename(model = tau) %>%
  filter(model == 0.5) %>%
  unnest(data) %>%
  unnest(data) %>%
  dplyr::select(!lead_time)

#### Clean up the dataframe

qqm_nerfc_median <- qqm_nerfc_median %>%
  inner_join(., coldbrook_turbidity_plus_drivers_clean %>%
               dplyr::select(dateTime, 
                             fw_mean_observed_turbidity,
                             log_observed_turbidity),
             join_by(predict_dateTime == dateTime)) %>%
  mutate(forecasted_turbidity = 10^log_predicted_turbidity*D_qqm) %>%
  mutate(lead_days = case_when(lead_group == "6-24" ~ 1,
                                   lead_group == "30-48" ~ 2,
                                   lead_group == "54-72" ~ 3)) %>%
  mutate(raw_error = forecasted_turbidity - fw_mean_observed_turbidity,
         sqrerr = raw_error^2,
         abs_error = abs(raw_error),
         abs_pct_error = abs_error/fw_mean_observed_turbidity) %>%
  inner_join(., cold_test %>%
               dplyr::select(dateTime, log_observed_flow),
               join_by(predict_dateTime == dateTime)
  )
```

### Using LGBM-Coldbrook

``` r
### Trim the prepared forecast dataframe to include only the features selected
### in the model development process

data_for_nerfc_forecast <- all_predictors_nerfc %>%
  dplyr::select(nerfc_chosen_model$Feature)

### Declare the test data

nerfc_forecasting_data <- data.matrix(data_for_nerfc_forecast)
        

### Load in the tuned LGBM model

final_tuned_lgbm_for_turb_with_nerfc <- lgb.load(here("r_images", 
                                                      "final_tuned_lgbm_for_turb_with_nerfc.txt"))
        

### Forecast with the model, bind to observations, make final prediction with smearing factor

nerfc_turb_forecast <- predict(final_tuned_lgbm_for_turb_with_nerfc, 
                               data = nerfc_forecasting_data) %>%
  as_tibble() %>% 
  rename(log_forecasted_turbidity = 1) %>%
  bind_cols(., all_predictors_nerfc %>%
              rename_with(~paste0(., "_forc"), where(is.numeric))) %>%
  rename(log_modeled_flow = 
           log_observed_flow_forc) %>%
  rename(lead_time = lead_time_forc) %>%
  inner_join(., cold_test %>%
               dplyr::select(dateTime, 
                             fw_mean_observed_turbidity,
                             log_observed_turbidity, 
                             nerfc_chosen_model$Feature),
             join_by(predict_dateTime == dateTime)) %>%
  mutate(exp_log_model_residuals = 10^(log_observed_turbidity -
                                         log_forecasted_turbidity)) %>%
  mutate(forecasted_turbidity = 10^log_forecasted_turbidity*d_lgbm_cold_only) %>%
  mutate(lead_days = case_when(lead_group == "6-24" ~ 1,
                               lead_group == "30-48" ~ 2,
                               lead_group == "54-72" ~ 3))
```
