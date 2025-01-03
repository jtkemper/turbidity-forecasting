09_evaluate_performance
================
JTK
2025-01-03

# Housekeeping

### Packages

``` r
### Data mgmt
require(tidyverse)
require(here)

### Evaluation
require(hydroGOF)
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

source(knitr::purl(here("Rmd-files/08_make_forecasts.Rmd"), 
                   quiet=TRUE))
```

# Evaluate benchmark performance

### Combine performance dataframes

``` r
benchmark_performance_all <- bind_rows(qqm_bench_summary_stats %>%
                                         dplyr::select(rmse, pbias, nse, kge, 
                                                       r, variability, bias, 
                                                       model),
                                       summary_stats_final_nerfc %>%
                                         dplyr::select(rmse, pbias, nse, kge, 
                                                       r, variability, bias, 
                                                       model),
                                       summary_stats_final_model %>%
                                         dplyr::select(rmse, pbias, nse, kge, 
                                                       r, variability, bias, 
                                                       model))
```

### Calculate event-based error

``` r
#### First, combine all benchmark timeseries 

all_models_benchmark <- nerfc_pred_obs_ts %>%
  dplyr::select(dateTime, fw_mean_observed_turbidity, predicted_turbidity) %>%
  rename(predicted_turbidity_nerfc = predicted_turbidity) %>%
  dplyr::group_by(dateTime) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  full_join(., nwm_pred_obs_ts %>%
              dplyr::group_by(dateTime) %>%
              dplyr::slice(1) %>%
              dplyr::ungroup() %>%
              dplyr::select(dateTime, 
                           fw_mean_observed_turbidity, 
                           predicted_turbidity) %>%
              rename(predicted_turbidity_nwm = predicted_turbidity),
            by = c("dateTime", "fw_mean_observed_turbidity")) %>%
  full_join(., qqm_benchmark_median %>%
              dplyr::select(c(dateTime,
                              fw_mean_observed_turbidity,
                              predicted_turbidity)) %>%
              rename(predicted_turbidity_qqm = predicted_turbidity),
            by = c("dateTime", "fw_mean_observed_turbidity")) %>%
  pivot_longer(cols = predicted_turbidity_nerfc:predicted_turbidity_qqm,
               values_to = "predicted_turbidity", names_to = "model") %>%
  mutate(model = case_when(str_detect(model, "qqm") ~ "Quantile-Coldbrook",
                                str_detect(model, "nerfc") ~ "LGBM-Coldbrook",
                                str_detect(model, "nwm") ~ "LGBM-Network"))

#### Then calculate error of storm turbidity "load" for each storm and each model

all_models_event_flux <- all_models_benchmark %>% 
  inner_join(., hourly_discharge_coldbrook %>%
               dplyr::select(dateTime, mean_observed_flow_cms),
             by = "dateTime") %>%
  dplyr::group_by(model) %>%
  nest() %>%
  mutate(storm_error = map(data,
                           .f = load_error_calculator,
                           obs_storm_df = storms_2023,
                           forecast_source = "NWM",
                           type = "Benchmark")) %>%
  dplyr::select(!data) %>%
  unnest(storm_error)
```

# Evaluate forecast performance

## Calculate error metrics for each forecast product

These could really be streamlined by writing a function, but this is old
code and I simply never got around to it

### For NWM-forced forecasts

``` r
#### For forecasts made with Quantile-Coldbrook

qqm_nwm_median_errors <- qqm_nwm_median %>%
    mutate(raw_error_qqm = fw_mean_observed_turbidity - forecasted_turbidity,
         abs_error_qqm = abs(raw_error_qqm),
         sqr_error_qqm = raw_error_qqm^2,
         pct_raw_error_qqm = raw_error_qqm/fw_mean_observed_turbidity,
         pct_abs_error_qqm = abs_error_qqm/fw_mean_observed_turbidity) %>%
  group_by(lead_days 
           ) %>%
  summarise(rmse = sqrt(mean(sqr_error_qqm)),
            mae = mean(abs_error_qqm),
            nse = hydroGOF::NSE(forecasted_turbidity, 
                                        fw_mean_observed_turbidity),
            logNSE = hydroGOF::NSE(log10(forecasted_turbidity), 
                                        log10(fw_mean_observed_turbidity)),
            kge = hydroGOF::KGE(forecasted_turbidity, 
                                        fw_mean_observed_turbidity),
            mape = mean(pct_abs_error_qqm),
            pbias = hydroGOF::pbias(forecasted_turbidity,
                                            fw_mean_observed_turbidity),
            corr = cor(forecasted_turbidity, fw_mean_observed_turbidity),
             r_sq = hydroGOF::R2(forecasted_turbidity,
                            fw_mean_observed_turbidity),
             decompose_kge(forecasted_turbidity,
                              fw_mean_observed_turbidity))

#### For forecasts made with LGBM-Coldbrook

errors_by_leadtime_nwm_cold_only <- nwm_turb_forecast_co %>%
  inner_join(., lead_groups,
             by = "lead_time")  %>%
    group_by(lead_days) %>%
  summarise(rmse = sqrt(mean(sqrerr)),
                mae = mean(abs_error),
                NSE = hydroGOF::NSE(forecasted_turbidity, 
                                    fw_mean_observed_turbidity),
                      logNSE = hydroGOF::NSE(log_forecasted_turbidity, 
                                    log_observed_turbidity),
                KGE = hydroGOF::KGE(forecasted_turbidity, 
                                    fw_mean_observed_turbidity),
                mape = mean(abs_pct_error),
                pbias = hydroGOF::pbias(forecasted_turbidity,
                                        fw_mean_observed_turbidity),
                corr = cor(forecasted_turbidity, fw_mean_observed_turbidity),
            br2 = hydroGOF::br2(forecasted_turbidity, fw_mean_observed_turbidity),
            r_sq = hydroGOF::R2(forecasted_turbidity,
                            fw_mean_observed_turbidity),
            decompose_kge(forecasted_turbidity,
                              fw_mean_observed_turbidity)
                ) 
  

#### For forecasts made with LGBM-Network

errors_by_lead_time <- cold_turb_forecast%>%
  mutate(
             forecasted_turb_flux = 10^log_modeled_flow*forecasted_turbidity,
             observed_turb_flux = 10^log_observed_flow*fw_mean_observed_turbidity,
             forecasted_turb_inst_load = forecasted_turb_flux*60*60,
             observed_turb_inst_load = observed_turb_flux*60*60,
             sqrerr = (fw_mean_observed_turbidity - forecasted_turbidity)^2,
             abs_error = abs((fw_mean_observed_turbidity - forecasted_turbidity)),
             abs_pct_error = abs_error/fw_mean_observed_turbidity) %>%
  full_join(., storms_2023 %>%
               dplyr::select(dateTime, storm),
             join_by(predict_dateTime == dateTime)) %>%
  drop_na(log_forecasted_turbidity) %>%
  mutate(storm_or_base = ifelse(is.na(storm), "Baseflow", "Storm")) %>%
  inner_join(., lead_groups,
             by = "lead_time") %>%
  group_by(lead_days) %>%
  summarise(rmse = sqrt(mean(sqrerr)),
                mae = mean(abs_error),
                NSE = hydroGOF::NSE(forecasted_turbidity, 
                                    fw_mean_observed_turbidity),
                      logNSE = hydroGOF::NSE(log_forecasted_turbidity, 
                                    log_observed_turbidity),
                KGE = hydroGOF::KGE(forecasted_turbidity, 
                                    fw_mean_observed_turbidity),
                mape = mean(abs_pct_error),
                pbias = hydroGOF::pbias(forecasted_turbidity,
                                        fw_mean_observed_turbidity),
                corr = cor(forecasted_turbidity, fw_mean_observed_turbidity),
                br2 = hydroGOF::br2(forecasted_turbidity, fw_mean_observed_turbidity),
            r_sq = hydroGOF::R2(forecasted_turbidity,
                            fw_mean_observed_turbidity),
            decompose_kge(forecasted_turbidity,
                              fw_mean_observed_turbidity)
                ) 
```

### For NERFC-forced forecasts

``` r
##### For forecasts made using Quantile-Coldbrook
errors_by_lead_time_qqm_nerfc <- qqm_nerfc_median %>%
  group_by(lead_days) %>%
  summarise(rmse = sqrt(mean(sqrerr)),
                mae = mean(abs_error),
                NSE = hydroGOF::NSE(forecasted_turbidity, 
                                    fw_mean_observed_turbidity),
                      logNSE = hydroGOF::NSE(log10(forecasted_turbidity), 
                                    log10(fw_mean_observed_turbidity)),
                KGE = hydroGOF::KGE(forecasted_turbidity, 
                                    fw_mean_observed_turbidity),
                mape = mean(abs_pct_error),
                pbias = hydroGOF::pbias(forecasted_turbidity,
                                        fw_mean_observed_turbidity),
                corr = cor(forecasted_turbidity, fw_mean_observed_turbidity),
            r_sq = hydroGOF::R2(forecasted_turbidity, 
                                    fw_mean_observed_turbidity),
                decompose_kge(forecasted_turbidity,
                              fw_mean_observed_turbidity))




##### For forecasts made using LGBM-Coldbrook

errors_by_lead_time_nerfc <- nerfc_turb_forecast %>%
        mutate( raw_error = forecasted_turbidity - fw_mean_observed_turbidity,
             sqrerr = (fw_mean_observed_turbidity - forecasted_turbidity)^2,
             abs_error = abs((fw_mean_observed_turbidity - forecasted_turbidity)),
             raw_pct_error = raw_error/fw_mean_observed_turbidity,
             abs_pct_error = abs_error/fw_mean_observed_turbidity) %>%
  full_join(., storms_2023 %>%
               dplyr::select(dateTime, storm),
             join_by(predict_dateTime == dateTime)) %>%
  drop_na(log_forecasted_turbidity) %>%
  mutate(storm_or_base = ifelse(is.na(storm), "Baseflow", "Storm")) %>%
  group_by(lead_days) %>%
  summarise(rmse = sqrt(mean(sqrerr)),
                mae = mean(abs_error),
                NSE = hydroGOF::NSE(forecasted_turbidity, 
                                    fw_mean_observed_turbidity),
                      logNSE = hydroGOF::NSE(log_forecasted_turbidity, 
                                    log_observed_turbidity),
                KGE = hydroGOF::KGE(forecasted_turbidity, 
                                    fw_mean_observed_turbidity),
                mape = mean(abs_pct_error),
                pbias = hydroGOF::pbias(forecasted_turbidity,
                                        fw_mean_observed_turbidity),
                corr = cor(forecasted_turbidity, fw_mean_observed_turbidity),
                r_sq = hydroGOF::R2(forecasted_turbidity, 
                                    fw_mean_observed_turbidity),
                decompose_kge(forecasted_turbidity,
                              fw_mean_observed_turbidity)
                ) %>%
    mutate(model = "nerfc_lgbm_cold_only")
```

## Calculate event-based load error for each forecast product

### For NWM-forced forecasts

``` r
#### Quantile-Coldbrook & NWM 

storm_loads_and_peaks_qqm_nwm <- load_error_calculator(qqm_nwm_median,
                                              storms_2023,
                                              "NWM",
                                              "QQM") 


#### LGBM-Coldbrook & NWM 

storm_loads_and_peaks_co_nwm <- load_error_calculator(nwm_turb_forecast_co,
                                              storms_2023,
                                              "NWM",
                                              "LGBM - Cold Only") 

#### LGBM-Network & NWM 

storm_loads_and_peaks_network_nwm <- load_error_calculator(final_cold_forecast_observed %>%
                                                dplyr::select(!storm),
                                              storms_2023,
                                              "NWM",
                                              "LGBM - Network") 
```

### For NERFC-forced forecasts

``` r
#### Quantile-Coldbrook & NERFC

storm_loads_and_peaks_nerfc_qqm <- load_error_calculator(qqm_nerfc_median,
                                                     storms_2023,
                                                     "NERFC",
                                                     "QQM",
                                                     "Model")

### LGBM-Coldbrook & NERFC 
storm_loads_and_peaks_nerfc <- load_error_calculator(nerfc_turb_forecast,
                                                     storms_2023,
                                                     "NERFC",
                                                     "LGBM - Cold Only", 
                                                     "Model")
```
