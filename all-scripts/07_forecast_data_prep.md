07_forecast_data_prep
================
JTK
2025-01-02

################################################################################ 

This script prepares various dataframes to make turbidity predictions
from NWM and NERFC forecasts. The most difficult task with regards to
this preperation is the creation of antecedent condition dataframes,
which account for antecedent daily, weekly, monthly, etc. streamflow.
Once these are prepped, we can make the actual forecasts
\################################################################################

# Housekeeping

### Packages

``` r
# Data mgmt

require(tidyverse)
```

    ## Loading required package: tidyverse

    ## Warning: package 'ggplot2' was built under R version 4.3.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
# Parallel computing

require(future)
```

    ## Loading required package: future

``` r
require(furrr)
```

    ## Loading required package: furrr

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
```

# Calculate antecedents for NWM

### Prepare lookup tables

``` r
#### Create a lookup table to determine which timesteps 
#### For which we need antecedent values 

timesteps_needed <- nwm_forecasts_mt_trim %>%
      filter(predict_water_year == 2023) %>%
      ungroup() %>%
      mutate(lead_time = as.numeric(lead_time)) %>%
      mutate(hours_needed_from_gage_weekly_ant = 168 - (lead_time-1)) %>%
      mutate(hours_needed_from_gage_weekly_ant = 
               ifelse(hours_needed_from_gage_weekly_ant < 0, 0, 
                    hours_needed_from_gage_weekly_ant)) %>%
      mutate(hours_needed_from_nwm_weekly_ant = 168 - 
               hours_needed_from_gage_weekly_ant) %>%
      mutate(hours_needed_from_gage_monthly_ant = (24*30) - (lead_time-1)) %>%
      mutate(hours_needed_from_gage_monthly_ant = 
               ifelse(hours_needed_from_gage_monthly_ant < 0, 0, 
                    hours_needed_from_gage_monthly_ant)) %>%
      mutate(hours_needed_from_nwm_monthly_ant = (24*30) - 
               hours_needed_from_gage_monthly_ant) %>% # Year below
      mutate(hours_needed_from_gage_yearly = (24*365) - (lead_time-1)) %>%
      mutate(hours_needed_from_gage_yearly = 
               ifelse(hours_needed_from_gage_yearly < 0, 0, 
                    hours_needed_from_gage_yearly)) %>%
      mutate(hours_needed_from_nwm_yearly = (24*365) - 
               hours_needed_from_gage_yearly) %>%
      dplyr::select(predict_dateTime, 
                      hours_needed_from_gage_weekly_ant, hours_needed_from_nwm_weekly_ant,
                      hours_needed_from_gage_monthly_ant, hours_needed_from_nwm_monthly_ant,
                      hours_needed_from_gage_yearly, hours_needed_from_nwm_yearly,
                      tributary, lead_time, init_date) 


#### Trim down to just the tributary watersheds we need

timesteps_needed_beaverkill <- timesteps_needed %>%
  filter(tributary == "BEAVERKILLATMOUNTTREMPERNY") 

timesteps_needed_cold <- timesteps_needed %>%
  filter(tributary == "ESOPUSCREEKATCOLDBROOKNY") 

timesteps_needed_woodland <- timesteps_needed %>%
  filter(tributary == "WARNERCREEKNEARCHICHESTERNY") 

timesteps_needed_allaben <- timesteps_needed %>%
  filter(tributary == "ESOPUSCREEKATALLABENNY")
```

### Calculate mean and max antecedent conditions

``` r
#### We run these individually for each station because it takes a long time to do so
#### And we want to avoid breaking anything
#### The user can change which station they want manually

############################# CALCULATE ANTECENDENT MAXES AND MEANS ###########

#### Set up parallel computing 
plan(multisession, workers = 8)

################################################################################

#### Calculate antecedent monthly and weekly mean discharge at Beaver Kill

beaverkill_ants <- furrr::future_pmap_dfr(list(timesteps_needed_beaverkill$predict_dateTime,
                   timesteps_needed_beaverkill$hours_needed_from_gage_weekly_ant,
                   timesteps_needed_beaverkill$hours_needed_from_nwm_weekly_ant,
                   timesteps_needed_beaverkill$hours_needed_from_gage_monthly_ant,
                   timesteps_needed_beaverkill$hours_needed_from_nwm_monthly_ant,
                   timesteps_needed_beaverkill$init_date,
                   timesteps_needed_beaverkill$tributary,
                   time_span = "week_month",
                   metric = "mean"
                   ),
                   .f = antecedent_calculator,
                   obs_df = hourly_discharge_all_sites %>% 
                     filter(station == "BEAVERKILLATMOUNTTREMPERNY") %>%
                     dplyr::ungroup(),
                   model_df = nwm_forecasts_mt_trim %>%
                     filter(tributary == "BEAVERKILLATMOUNTTREMPERNY") %>%
                     dplyr::ungroup(),
                   forecast_model = "NWM",
                   .progress = TRUE)

### Rename variables to match the names of predictors from LightGBM model

beaverkill_ants <- beaverkill_ants %>%
  dplyr::rename(mean_prior_monthly_log_q_BEAVERKILLATMOUNTTREMPERNY = mean_prior_monthly_log_q,
                mean_prior_weekly_log_q_BEAVERKILLATMOUNTTREMPERNY = mean_prior_weekly_log_q) %>%
  dplyr::select(mean_prior_weekly_log_q_BEAVERKILLATMOUNTTREMPERNY)

################################################################################

#### calculate antecedent monthly and weekly mean at Coldbrook 
cold_month_week_mean <- furrr::future_pmap_dfr(list(timesteps_needed_cold$predict_dateTime,
                   timesteps_needed_cold$hours_needed_from_gage_weekly_ant,
                   timesteps_needed_cold$hours_needed_from_nwm_weekly_ant,
                   timesteps_needed_cold$hours_needed_from_gage_monthly_ant,
                   timesteps_needed_cold$hours_needed_from_nwm_monthly_ant,
                   timesteps_needed_cold$init_date,
                   timesteps_needed_cold$tributary,
                   time_span = "week_month",
                   metric = "mean"
                   ),
              .f = antecedent_calculator,
              obs_df = hourly_discharge_all_sites %>% 
                     filter(station == "ESOPUSCREEKATCOLDBROOKNY") %>%
                dplyr::ungroup(),
            model_df = nwm_forecasts_mt_trim %>%
                     filter(tributary == "ESOPUSCREEKATCOLDBROOKNY") %>%
              dplyr::ungroup(),
            forecast_model = "NWM",
              .progress = TRUE)

################################################################################

#### Calculate antecedent 3 monthly max Q at coldbrook
cold_3month_max <- furrr::future_pmap_dfr(list(timesteps_needed_cold$predict_dateTime,
                   timesteps_needed_cold$hours_needed_from_gage_weekly_ant,
                   timesteps_needed_cold$hours_needed_from_nwm_weekly_ant,
                   timesteps_needed_cold$hours_needed_from_gage_monthly_ant,
                   timesteps_needed_cold$hours_needed_from_nwm_monthly_ant,
                   timesteps_needed_cold$init_date,
                   timesteps_needed_cold$tributary,
                   time_span = "3month",
                   metric = "max"
                   ),
              .f = antecedent_calculator,
              obs_df = hourly_discharge_all_sites %>% 
                     filter(station == "ESOPUSCREEKATCOLDBROOKNY") %>%
                dplyr::ungroup(),
            model_df = nwm_forecasts_mt_trim %>%
                     filter(tributary == "ESOPUSCREEKATCOLDBROOKNY") %>%
              dplyr::ungroup(),
            forecast_model = "NWM",
              .progress = TRUE)

################################################################################
```

### Calculate antecedent peaks and times since those peaks

``` r
#### Get annual peaks at the Allaben station

######################## ALLABEN ###############################################

plan(multisession, workers = 8)

#### Annual antecedent conditions at Allaben
max_annual_ant_allaben <- furrr::future_pmap_dfr(list(timesteps_needed_allaben$predict_dateTime,
                   timesteps_needed_allaben$hours_needed_from_gage_yearly,
                   timesteps_needed_allaben$hours_needed_from_nwm_yearly,
                   timesteps_needed_allaben$init_date,
                   timesteps_needed_allaben$tributary,
                   length = "annual",
                   return_value = TRUE
                   ),
                   .f = peak_time_getter,
                   obs_df = hourly_discharge_all_sites %>% 
                     filter(station == "ESOPUSCREEKATALLABENNY") %>%
                     dplyr::ungroup(),
                   model_df = nwm_forecasts_mt_trim %>%
                     filter(tributary == "ESOPUSCREEKATALLABENNY") %>%
                     dplyr::ungroup(),
                   forecast_model = "NWM",
                   .progress = TRUE)


#### Get the time since that discharge occurred for each predict timestep
time_since_annual_peak_allaben <- peak_time_calculator(timesteps_needed_allaben, 
                                               max_annual_ant_allaben,
                                               length = "annual") %>%
  rename(time_since_annual_peak_ESOPUSCREEKATALLABENNY = 1)


################################################################################

#### Get annual and six-month peaks for Coldbrook 

#################### COLDBROOK #################################################

#### Annual peak

max_annual_ant_cold_nwm <- furrr::future_pmap_dfr(list(timesteps_needed_cold$predict_dateTime,
                   timesteps_needed_cold$hours_needed_from_gage_yearly,
                   timesteps_needed_cold$hours_needed_from_nwm_yearly,
                   timesteps_needed_cold$init_date,
                   timesteps_needed_cold$tributary,
                   length = "annual",
                   return_value = TRUE
                   ),
                   .f = peak_time_getter,
                   obs_df = hourly_discharge_all_sites %>% 
                     filter(station == "ESOPUSCREEKATCOLDBROOKNY") %>%
                     dplyr::ungroup(),
                   model_df = nwm_forecasts_mt_trim %>%
                     filter(tributary == "ESOPUSCREEKATCOLDBROOKNY") %>%
                     dplyr::ungroup(),
                   forecast_model = "NWM",
                   .progress = TRUE)

##### Get the time since that discharge occurred for each predict timestep

time_since_annual_peak_cold <- peak_time_calculator(timesteps_needed_cold, 
                                               max_annual_ant_cold_nwm,
                                               length = "annual") %>%
  rename(time_since_annual_peak = 1)

#### Six-month peak

max_sixmonth_ant_cold_nwm <- furrr::future_pmap_dfr(list(timesteps_needed_cold$predict_dateTime,
                   timesteps_needed_cold$hours_needed_from_gage_yearly,
                   timesteps_needed_cold$hours_needed_from_nwm_yearly,
                   timesteps_needed_cold$init_date,
                   timesteps_needed_cold$tributary,
                   length = "annual",
                   return_value = TRUE
                   ),
              .f = peak_time_getter,
             obs_df = hourly_discharge_all_sites %>% 
                     filter(station == "ESOPUSCREEKATCOLDBROOKNY") %>%
               dplyr::ungroup(),
            model_df = nwm_forecasts_mt_trim %>%
                     filter(tributary == "ESOPUSCREEKATCOLDBROOKNY") %>%
              dplyr::ungroup(),
            forecast_model = "NWM",
              .progress = TRUE)

##### Rename the field to more accurately reflect what it is 

sixmonth_peak_q_cold_nwm <- max_sixmonth_ant_cold_nwm %>%
  dplyr::select(log_flow) %>%
  rename(max_prior_sixmonth_log_q = 1)

##### Get the time since that six-month peak for each timestep

time_since_6month_peak_cold <- peak_time_calculator(timesteps_needed_cold,
                                               max_sixmonth_ant_cold_nwm,
                                               length = "annual") %>%
  rename(time_since_6month_peak = 1)




################################################################################
```

### Calculate the change in discharge from one timestep to the next

This is, effectively, the hydrograph “limb” for each timestep, with
negative representing the falling limb, positive the rising

``` r
#### Calculate the limb at coldbrook

cold_limb <- limb_getter(nwm_forecasts_mt_trim %>%
                           rename(init_dateTime = init_datetime)) %>%
  dplyr::select(log_limb)
```

### Prepare final antecedent dataframes

``` r
#### For the LGBM-Network model

#### LGBM-Network ###########################################################
all_antecedents <- bind_cols(cold_limb, 
                             time_since_annual_peak_allaben,
                             beaverkill_ants,
                             cold_3month_max, 
                             cold_month_max)


##########################################################################

#### For the LGBM-Coldbrook Model 

######## NWM & LGBM-Coldbrook ###################################################

all_antecedents_cold_only <- bind_cols(cold_limb, 
                             cold_3month_max, 
                             cold_month_max,
                             time_since_annual_peak_cold,
                             time_since_6month_peak_cold,
                             sixmonth_peak_q_cold_nwm,
                             cold_month_week_mean)




################################################################################
```

# Calculate antecedents for NERFC

### Prepare the lookup table

``` r
### Prepare lookup dataframe
### Note that here, when referring to "hours needed from nerfc",
### It's not really hours needed, but timesteps needed
### So 3 "hours" need from NERFC is 3 actually timesteps, 
### So since NERFC outputs forecasts at 6 hourly timesteps
### It's really 18 hours needed

timesteps_needed_nerfc <- discharge_nerfc_esopus_once_daily %>%
  dplyr::ungroup() %>%
  mutate(hours_needed_from_gage_daily_ant = (24 - (lead_time-3))) %>%
  mutate(hours_needed_from_gage_daily_ant = 
           ifelse(hours_needed_from_gage_daily_ant < 0, 0, 
                  hours_needed_from_gage_daily_ant)) %>%
  mutate(hours_needed_from_gage_daily_ant = 
           ifelse(hours_needed_from_gage_daily_ant %% 6 != 0,
                  (hours_needed_from_gage_daily_ant - 1),
                  hours_needed_from_gage_daily_ant)) %>%
  mutate(hours_needed_from_model_daily_ant = (24 - hours_needed_from_gage_daily_ant)/6) %>%
  mutate(hours_needed_from_model_daily_ant = round(hours_needed_from_model_daily_ant, 0)) %>%
  mutate(hours_needed_from_gage_weekly_ant = (168 - (lead_time-3))) %>%
  mutate(hours_needed_from_gage_weekly_ant = 
           ifelse(hours_needed_from_gage_weekly_ant < 0, 0, 
                  hours_needed_from_gage_weekly_ant)) %>%
  mutate(hours_needed_from_gage_weekly_ant = 
           ifelse(hours_needed_from_gage_weekly_ant %% 6 != 0,
                  (hours_needed_from_gage_weekly_ant - 1),
                  hours_needed_from_gage_weekly_ant)) %>%
  mutate(hours_needed_from_model_weekly_ant = (168 - hours_needed_from_gage_weekly_ant)/6) %>%
  mutate(hours_needed_from_model_weekly_ant = round(hours_needed_from_model_weekly_ant, 0)) %>%
  mutate(hours_needed_from_gage_monthly_ant = (24*30) - (lead_time-3)) %>%
  mutate(hours_needed_from_gage_monthly_ant = 
           ifelse(hours_needed_from_gage_monthly_ant < 0, 0, 
                  hours_needed_from_gage_monthly_ant)) %>%
  mutate(hours_needed_from_gage_monthly_ant = 
           ifelse(hours_needed_from_gage_monthly_ant %% 6 != 0,
                  (hours_needed_from_gage_monthly_ant - 1),
                  hours_needed_from_gage_monthly_ant)) %>%
  mutate(hours_needed_from_model_monthly_ant = ((24*30) - hours_needed_from_gage_monthly_ant)/6) %>% 
  mutate(hours_needed_from_model_monthly_ant = round(hours_needed_from_model_monthly_ant, 0)) %>%
  mutate(hours_needed_from_gage_yearly = (24*365) - (lead_time-3)) %>%
  mutate(hours_needed_from_gage_yearly = 
           ifelse(hours_needed_from_gage_yearly < 0, 0,
                  hours_needed_from_gage_yearly)) %>%
  mutate(hours_needed_from_gage_yearly = 
           ifelse(hours_needed_from_gage_yearly %% 6 != 0,
                  (hours_needed_from_gage_yearly - 1),
                  hours_needed_from_gage_yearly)) %>%
  mutate(hours_needed_from_model_yearly = ((24*365) - hours_needed_from_gage_yearly)/6) %>%
  mutate(hours_needed_from_model_yearly = round(hours_needed_from_model_yearly, 0)) %>%
  dplyr::select(predict_dateTime, 
                hours_needed_from_gage_daily_ant, hours_needed_from_model_daily_ant,
                hours_needed_from_gage_weekly_ant, hours_needed_from_model_weekly_ant,
                hours_needed_from_gage_monthly_ant, hours_needed_from_model_monthly_ant,
                hours_needed_from_gage_yearly, hours_needed_from_model_yearly,
                tributary, lead_time, init_date) %>%
  filter(hours_needed_from_model_daily_ant >= 0)
```

### Calculate daily antecedent discharge

``` r
### Set up some parallel workers

plan(multisession, workers = 8)

### Calculate daily antecedent discharge

daily_ant_mean <- future_pmap_dfr(list(timesteps_needed_nerfc$predict_dateTime,
                    timesteps_needed_nerfc$hours_needed_from_gage_daily_ant,
                    timesteps_needed_nerfc$hours_needed_from_model_daily_ant,
                    timesteps_needed_nerfc$init_date,
                    timesteps_needed_nerfc$tributary
                   ),
              .f = daily_mean_calculator,
              obs_df = hourly_discharge_all_sites %>% 
                     filter(station == "ESOPUSCREEKATCOLDBROOKNY"),
            model_df = discharge_nerfc_esopus_once_daily %>%
                     filter(tributary == "ESOPUSCREEKATCOLDBROOKNY"),
            forecast_model = "NERFC",
              .progress = TRUE)
```

### Calculate weekly & monthly mean flow antecedents

``` r
mean_weekly_monthly_ant_nerfc <- furrr::future_pmap_dfr(list(timesteps_needed_nerfc$predict_dateTime,
                   timesteps_needed_nerfc$hours_needed_from_gage_weekly_ant,
                   timesteps_needed_nerfc$hours_needed_from_model_weekly_ant,
                   timesteps_needed_nerfc$hours_needed_from_gage_monthly_ant,
                   timesteps_needed_nerfc$hours_needed_from_model_monthly_ant,
                   timesteps_needed_nerfc$init_date,
                   timesteps_needed_nerfc$tributary,
                   time_span = "week_month",
                   metric = "mean"
                   ),
              .f = antecedent_calculator,
             obs_df = hourly_discharge_all_sites %>% 
                      dplyr::ungroup() %>%
                     filter(station == "ESOPUSCREEKATCOLDBROOKNY"),
            model_df = discharge_nerfc_esopus_once_daily %>%
              dplyr::ungroup() %>%
                     filter(tributary == "ESOPUSCREEKATCOLDBROOKNY"),
            forecast_model = "NERFC",
              .progress = TRUE)

mean_weekly_monthly_ant_nerfc
```

### Calculate peak flow antecedents (annual, six-month, etc.)

``` r
#### Annual antecedent conditions

max_annual_ant_nerfc <- furrr::future_pmap_dfr(list(timesteps_needed_nerfc$predict_dateTime,
                   timesteps_needed_nerfc$hours_needed_from_gage_yearly,
                   timesteps_needed_nerfc$hours_needed_from_model_yearly,
                   timesteps_needed_nerfc$init_date,
                   timesteps_needed_nerfc$tributary,
                   length = "annual",
                   return_value = TRUE
                   ),
              .f = peak_time_getter,
             obs_df = hourly_discharge_all_sites %>% 
                     filter(station == "ESOPUSCREEKATCOLDBROOKNY"),
            model_df = discharge_nerfc_esopus_once_daily %>%
                     filter(tributary == "ESOPUSCREEKATCOLDBROOKNY"),
            forecast_model = "NERFC",
              .progress = TRUE)

#### Pull out the maximum annual discharge magnitude 

max_prior_annual_q <- max_annual_ant_nerfc %>%
  rename(max_prior_annual_q = log_flow) %>%
  dplyr::select(max_prior_annual_q)

#### Get the time since that discharge occured for each predict timestep

time_since_annual_peak <- peak_time_calculator(timesteps_needed_nerfc, 
                                               max_annual_ant_nerfc,
                                               length = "annual")

##############################################

### Three month antecedent conditions

max_threemonth_ant_nerfc <- furrr::future_pmap_dfr(list(timesteps_needed_nerfc$predict_dateTime,
                   timesteps_needed_nerfc$hours_needed_from_gage_yearly,
                   timesteps_needed_nerfc$hours_needed_from_model_yearly,
                   timesteps_needed_nerfc$init_date,
                   timesteps_needed_nerfc$tributary,
                   length = "3month",
                   return_value = TRUE
                   ),
              .f = peak_time_getter,
             obs_df = hourly_discharge_all_sites %>% 
               dplyr::ungroup() %>%
               filter(station == "ESOPUSCREEKATCOLDBROOKNY"),
            model_df = discharge_nerfc_esopus_once_daily %>%
              dplyr::ungroup() %>%
              filter(tributary == "ESOPUSCREEKATCOLDBROOKNY"),
            forecast_model = "NERFC",
              .progress = TRUE)


#### Pull out the maximum three month discharge magnitude 
max_prior_threemonth_log_q <- max_threemonth_ant_nerfc %>%
  rename(max_prior_threemonth_log_q = log_flow) %>%
  dplyr::select(max_prior_threemonth_log_q)

#### Get the time since that discharge occured for each predict timestep
time_since_3month_peak <- peak_time_calculator(timesteps_needed_nerfc, 
                                               max_threemonth_ant_nerfc,
                                               length = "3month")

#########################################

### Monthly antecedents
max_monthly_ant_nerfc <- furrr::future_pmap_dfr(list(timesteps_needed_nerfc$predict_dateTime,
                   timesteps_needed_nerfc$hours_needed_from_gage_weekly_ant,
                   timesteps_needed_nerfc$hours_needed_from_model_weekly_ant,
                   timesteps_needed_nerfc$hours_needed_from_gage_monthly_ant,
                   timesteps_needed_nerfc$hours_needed_from_model_monthly_ant,
                   timesteps_needed_nerfc$init_date,
                   timesteps_needed_nerfc$tributary,
                   time_span = "week_month",
                   metric = "max"
                   ),
              .f = antecedent_calculator,
             obs_df = hourly_discharge_all_sites %>% 
                      dplyr::ungroup() %>%
                     filter(station == "ESOPUSCREEKATCOLDBROOKNY"),
            model_df = discharge_nerfc_esopus_once_daily %>%
                          dplyr::ungroup() %>%
                          filter(tributary == "ESOPUSCREEKATCOLDBROOKNY"),
            forecast_model = "NERFC",
              .progress = TRUE)


#############################################


### Time since six month peak
max_6month_ant_nerfc <- furrr::future_pmap_dfr(list(timesteps_needed_nerfc$predict_dateTime,
                   timesteps_needed_nerfc$hours_needed_from_gage_yearly,
                   timesteps_needed_nerfc$hours_needed_from_model_yearly,
                   timesteps_needed_nerfc$init_date,
                   timesteps_needed_nerfc$tributary,
                   length = "6month",
                   return_value = TRUE
                   ),
              .f = peak_time_getter,
             obs_df = hourly_discharge_all_sites %>% 
                     filter(station == "ESOPUSCREEKATCOLDBROOKNY"),
            model_df = discharge_nerfc_esopus_once_daily %>%
                     filter(tributary == "ESOPUSCREEKATCOLDBROOKNY"),
            forecast_model = "NERFC",
              .progress = TRUE)


#### Pull out the maximum three month discharge magnitude 
max_prior_sixmonth_log_q <- max_6month_ant_nerfc %>%
  rename(max_prior_sixmonth_log_q = log_flow) %>%
  dplyr::select(max_prior_sixmonth_log_q)


#### Get the time since peak discharge occured for each predict timestep
time_since_6month_peak <- peak_time_calculator(timesteps_needed_nerfc, 
                                               max_6month_ant_nerfc,
                                               length = "6month")
```

### Calculate the change in discharge from one timestep to the next

Same as above, this is effectively the hydrograph “limb”

``` r
#### Calculate the limb at coldbrook
nerfc_limb <- limb_getter(discharge_nerfc_esopus_once_daily) 
```

### Combine all antecedents into one dataframe

``` r
#### Cobble together all the antecedents conditions

all_antecedents_nerfc <- bind_cols(timesteps_needed_nerfc %>%
                                     dplyr::select(predict_dateTime, init_date),
                                   #daily_ant_mean, 
                                   max_monthly_ant_nerfc,
                                   max_prior_sixmonth_log_q,
                                   max_prior_threemonth_log_q, 
                                   mean_weekly_monthly_ant_nerfc,
                                   time_since_annual_peak, 
                                   time_since_6month_peak
                                   ) %>%
  inner_join(., nerfc_limb, by = c("predict_dateTime", "init_date"))
```

# Final dataframe for prediction w/ NWM

``` r
#### Now add in the observed flow from the three stations we need
all_predictors <- bind_cols(
                            nwm_forecasts_mt_trim %>%
                                filter(predict_water_year == 2023) %>%
                                filter(tributary == "ESOPUSCREEKATCOLDBROOKNY") %>%
                                dplyr::select(init_date, predict_dateTime, lead_time,
                                              log_modeled_flow),
                             nwm_forecasts_mt_trim %>%
                                filter(predict_water_year == 2023) %>%
                                filter(tributary == "BEAVERKILLATMOUNTTREMPERNY") %>%
                                dplyr::select(log_modeled_flow) %>%
                                rename(log_modeled_flow_BEAVERKILLATMOUNTTREMPERNY = 
                                         log_modeled_flow),
                            nwm_forecasts_mt_trim %>%
                                filter(predict_water_year == 2023) %>%
                                filter(tributary == "WARNERCREEKNEARCHICHESTERNY") %>%
                                dplyr::select(log_modeled_flow) %>%
                                rename(log_modeled_flow_WARNERCREEKNEARCHICHESTERNY = 
                                         log_modeled_flow),
                            all_antecedents)

### Rename to reflect the predictor variable names in the LightGBM model
### Note that this involves "pretending" modeled flows are observed flows
all_predictors <- all_predictors %>%
  rename(log_observed_flow = 
            log_modeled_flow,
         log_observed_flow_BEAVERKILLATMOUNTTREMPERNY = 
            log_modeled_flow_BEAVERKILLATMOUNTTREMPERNY,
         log_observed_flow_WARNERCREEKNEARCHICHESTERNY = 
           log_modeled_flow_WARNERCREEKNEARCHICHESTERNY)

#names(all_predictors)



##################### FOR LGBM COLD ONLY #######################################
all_predictors_cold_only <- bind_cols(
                            nwm_forecasts_mt_trim %>%
                                filter(predict_water_year == 2023) %>%
                                filter(tributary == "ESOPUSCREEKATCOLDBROOKNY") %>%
                                dplyr::select(init_date, predict_dateTime, lead_time,
                                              log_modeled_flow),
                            all_antecedents_cold_only)

all_predictors_cold_only <- all_predictors_cold_only %>%
  rename(log_observed_flow = 
            log_modeled_flow)

################################################################################
```

# Final dataframe for prediction w/ NERFC

``` r
#### Joined with the modeled flow and produce a final
#### dataframe for predicting turbidity with NERFC forecasts


all_predictors_nerfc <- inner_join(all_antecedents_nerfc,
                            discharge_nerfc_esopus_once_daily %>%
                                dplyr::select(log_modeled_flow, predict_dateTime, init_date, lead_time,
                                              lead_group),
                            by = c("predict_dateTime", "init_date")) %>%
  rename(log_observed_flow = log_modeled_flow)


################################################################################
```

# Lead groups dataframe for evaluation

``` r
lead_groups <- tibble(group = paste0("group", rep(1:8, times = 24))) %>%
  mutate(group = str_sort(group, numeric = TRUE)) %>%
  mutate(lead_time = seq(1,192,1))%>%
  group_by(group) %>%
  mutate(time_of_day = lead_time - 24*(cur_group_id()-1))%>%
  dplyr::ungroup() %>%
  group_by(group) %>%
  mutate(max = max(lead_time),
         min = min(lead_time)) %>%
  mutate(lead_time_group = paste0(min, "-", max)) %>%
  mutate(lead_days = round((max)/24,1)) %>%
  dplyr::ungroup() %>%
  dplyr::select(lead_days, lead_time_group, lead_time)
```
