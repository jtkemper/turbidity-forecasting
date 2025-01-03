03_data_prep
================
JTK
2024-12-20

# HOUSEKEEPING

``` r
require(tidyverse)
require(here)
```

### Load prior scripts

``` r
source(knitr::purl(here("Rmd-files/02_observational_data_download_and_clean.Rmd"), 
                   quiet=TRUE))
```

# DATA MANIPULATION

### Seperate out data from upstream stations and from Coldbrook

``` r
#### This is necessary to build the final dataframe for model development
#### Later we'll widen the data from the tribs, but keep the Coldbrook data
#### in long format

##### First, get it for the antecedent data that is monthly or less
expanded_discharge_tribs <- expanded_discharge_all_sites %>%
  mutate(station = str_remove_all(station, " ")) %>%
  filter(site_no != "01362500")

expanded_discharge_coldbrook <- expanded_discharge_all_sites %>%
  mutate(station = str_remove_all(station, " ")) %>%
  filter(site_no == "01362500")

##### Then for the longer-term antecedent conditions (3month, 6month, year)

longer_term_expanded_discharge_tribs <- longer_term_expanded_discharge_all_sites %>%
  mutate(station = str_remove_all(station, " ")) %>%
  filter(site_no != "01362500")

longer_term_expanded_discharge_coldbrook <- longer_term_expanded_discharge_all_sites %>%
  mutate(station = str_remove_all(station, " ")) %>%
  filter(site_no == "01362500")
```

### Now, transform the antecedent discharge data from the tributaries (i.e., all stations but Coldbrook) into wide format

``` r
#### This is essential to do because now we can use the values at each timestep
#### from each upstream site
#### as predictors for turbidity at the Coldbrook station

##### Do it for the shorter-term antecedent data
expanded_discharge_tribs_wide <- expanded_discharge_tribs %>%
  as_tibble() %>%
  ungroup() %>%
  mutate(station = str_remove(station, " ")) %>%
  dplyr::select(!c(site_no, date, hour)) %>%
  pivot_wider(names_from = "station", 
              names_sep = "_",
              values_from = c("mean_observed_flow_cms", "log_observed_flow",
                              "log_squared_observed_flow",
                              "mean_prior_daily_log_q",
                              "mean_prior_weekly_log_q", "mean_prior_monthly_log_q", 
                              "max_prior_monthly_log_q",
                              "six_hour_lag_log_q", 
                              "log_limb",
                              "dq_q")) 

##### And now the longer term
longer_term_expanded_discharge_tribs_wide <- longer_term_expanded_discharge_tribs %>%
  as_tibble() %>%
  ungroup() %>%
  mutate(station = str_remove(station, " ")) %>%
  dplyr::select(!c(site_no, date)) %>%
    pivot_wider(names_from = "station", 
              names_sep = "_",
              values_from = c(
                              "max_prior_threemonth_log_q", 
                              "max_prior_sixmonth_log_q",
                              "max_prior_annual_q",
                              "time_since_3month_peak",
                              "time_since_6month_peak", 
                              "time_since_annual_peak")) 
```

### Build a final dataframe for models

``` r
#### First, we put together all the turbidity data and hydrology data
#### To build a full predictor dataset that ultimately comprises
#### hydrology at Coldbrook and upstream as predictors of turbidity 
#### at Coldbrook
#### Then, we add in tunnel flow in order to remove those flows
#### Where the tunnel is at least 20% of the Coldbrook discharge
#### And where turbidity is high
#### This removes those anamolous turbidity excursions caused by the
#### transfer of turbid water through the tunnel
#### These would not be predictable with the National Water Model

coldbrook_turbidity_plus_drivers <- expanded_discharge_coldbrook %>%
  inner_join(., hourly_turbidity_coldbrook %>%
               dplyr::select(dateTime, log_observed_turbidity,
                             fw_mean_observed_turbidity),
             by = "dateTime") %>%
  inner_join(., longer_term_expanded_discharge_coldbrook %>%
               dplyr::select(!c(station, site_no,
                             date)),
             by = "dateTime") %>%
   inner_join(., expanded_discharge_tribs_wide, by = "dateTime") %>%
     inner_join(., longer_term_expanded_discharge_tribs_wide, by = "dateTime") %>%
  inner_join(., hourly_discharge_tunnel %>%
                                 dplyr::select(dateTime,
                                               mean_observed_flow_cms) %>%
                                 dplyr::rename(tunnel_flow_cms =mean_observed_flow_cms),
                               by = "dateTime")%>%
  mutate(tunnel_percent_of_cold = tunnel_flow_cms/mean_observed_flow_cms) %>%
  mutate(turb_flux = fw_mean_observed_turbidity*mean_observed_flow_cms) %>%
  mutate(day = day(dateTime))%>%
  mutate(water_year = add_waterYear(dateTime))


#### Then remove those flows where the tunnel is making up much of the discharge
#### And this is our final model dataframe, with all potential drivers included
coldbrook_turbidity_plus_drivers_clean <- coldbrook_turbidity_plus_drivers %>%
  filter(!(tunnel_percent_of_cold > 0.2 & fw_mean_observed_turbidity >= 400)) %>%
  #filter(dateTime %in% low_tunnel_days$dateTime) %>%
  drop_na() %>%
  ungroup() %>%
  dplyr::select(!c(tunnel_percent_of_cold, turb_flux,
                   contains("tunnel")))
```
