04_event_delineation
================
JTK
2024-12-21

################################################################################ 

This script extracts storms at the Coldbrook gaging station in 2023. It
relies primarily on the “hydroEvents” package (Wasko and Guo, 2022,
doi.org/10.1002/hyp.14563). Importantly, we try and mitigate “events”
driven by tunnel releases by subtracting the discharge recorded at the
Shandakan Tunnel from that recorded at Coldbrook. We will also calculate
some statistics for each storm.
\################################################################################

# HOUSEKEEPING

### Packages

``` r
require(tidyverse)
require(here)
require(hydroEvents)
require(plotly)
```

### Load prior scripts

``` r
source(knitr::purl(here("Rmd-files/02_observational_data_download_and_clean.Rmd"), 
                   quiet=TRUE))

source(knitr::purl(here("Rmd-files/03_observational_data_prep.Rmd"), 
                   quiet=TRUE))
```

# Event Delineation

## ID the storms

### Subtract tunnel discharge from Coldbrook

``` r
#### Releases from the Shandakan tunnel can cause a hydrograph response at
#### Coldbrook that is similar to a precipitation-driven event
#### However, these "events" are not what we want to capture, as they are 
#### rather disparate in terms of cause (and behavior) from precip events
#### To remove these from the record, we simply subtract discharge at the tunnel
#### at time t from discharge at Coldbrook at time t
#### Though there are more involved (and potentially more "correct") ways to do 
#### this, this approach is good enough for our purposes (i.e., removal of 
#### "tunnel-driven" hydrograph peaks)

##### Subtract tunnel discharge from Coldbrook discharge
hourly_discharge_coldbrook_minus_tunnel <- hourly_discharge_coldbrook %>%
  inner_join(., hourly_discharge_tunnel %>%
              mutate(tunnel_observed_flow_cms = mean_observed_flow_cms) %>%
              dplyr::select(dateTime, tunnel_observed_flow_cms),
            by = "dateTime") %>%
  mutate(observed_flow_minus_tunnel = mean_observed_flow_cms - tunnel_observed_flow_cms) %>%
  mutate(observed_flow_minus_tunnel = ifelse(observed_flow_minus_tunnel < 0, 0,
                                             observed_flow_minus_tunnel)) %>%
  drop_na(observed_flow_minus_tunnel)
```

### Declare variables and extract baseflow

``` r
#### Declare the hourly (tunnel-subtracted) discharge as a numeric
hourly_observed_discharge <- hourly_discharge_coldbrook_minus_tunnel$observed_flow_minus_tunnel

#### Extract the baseflow
#### Here we use hydroEvent's built-in baseflow identification functions
bf_coldbrook <- baseflowB(hourly_observed_discharge,
                          alpha = 0.95, pass = 3, r = 300)

#### Subtract baseflow from discharge to identify quickflow component
qf_coldbrook <- hourly_observed_discharge - bf_coldbrook$bf
```

### Visualize baseflow seperation

``` r
#### Combine total discharge, baseflow, and quickflow into one dataframe
q_bf_qf_coldbrook <- hourly_discharge_coldbrook_minus_tunnel %>%
  bind_cols(bf = bf_coldbrook$bf) %>%
  bind_cols(qf = qf_coldbrook)

#### Visualize
plotly::plot_ly(q_bf_qf_coldbrook, x = ~dateTime, y = ~observed_flow_minus_tunnel,
                color = I("grey44"), mode = "lines", type = "scatter", 
                name = "observed flow") %>%
  add_trace(y = ~qf, color = I("lightblue"), 
            mode = "lines", name = "quickflow") %>%
  add_trace(y = ~bf, color = I("darkblue"), 
            mode = "lines", name = "baseflow")
```

### Identify the events

``` r
#### This relies on the hydroEvents functions to identify storm events
#### There are a variety of functions for this, but here we utilize the 
#### eventMinima approach
#### See help(eventMinima) and package documentation for further explanation
#### Of this method

##### Pick out the events 
past_events_min <- eventMinima(qf_coldbrook, delta.x = 12, delta.y = 0.5,
                                           threshold = 0.2)

##### Then further filter out events by thresholding max discharge
##### This removes some very small events that may be more anamolous excursions
##### (perhaps driven by a tunnel releases)
##### And also further chops off some long-tail behavior of big events
storms_index <- past_events_min %>% 
  filter(max > 5)
```

## Extract the storms

### Pull out all storm data from overall flow data

``` r
#### The above code identifies the index of the start and end timestamps for 
#### each storm in the qf_coldbrook dataframe
#### Now, we need to actually extract all the timestamps (rows) that fall within
#### each storm event

##### Do the extraction
all_storms <- list()

for(i in 1:nrow(storms_index)) {
  
  print(paste("storm", i))
  
  storm_hr <- hourly_discharge_coldbrook_minus_tunnel %>% 
    dplyr::slice(storms_index$srt[i]:storms_index$end[i]) %>%
    mutate(storm = paste0("storm", i))
  
  all_storms[[i]] <- storm_hr
  
  
}

##### Bind together
coldbrook_all_storms <- bind_rows(all_storms)

##### Extract just 2023 storms
storms_2023 <-  coldbrook_all_storms %>%
  mutate(water_year = add_waterYear(dateTime)) %>%
  filter(water_year == 2023)
```

### Plot extracted storms

``` r
#### Combine identified storms with the discharge, baseflow, quickflow
#### dataframe from above
#### This allows us to layer identified storms over the observed hydrograph
#### And inspect our selection
q_bf_qf_storms_coldbrook <- q_bf_qf_coldbrook %>%
  full_join(., storms_2023 %>%
              dplyr::select(dateTime, observed_flow_minus_tunnel, water_year) %>%
              rename(storm_observed_flow_cms = observed_flow_minus_tunnel),
            by = "dateTime")

#### And plot
plotly::plot_ly(q_bf_qf_storms_coldbrook %>%
                  filter(water_year == 2023), 
                x = ~dateTime, y = ~observed_flow_minus_tunnel,
                color = I("grey44"), mode = "lines", type = "scatter", 
                name = "observed flow") %>%
  add_trace(y = ~storm_observed_flow_cms, color = I("darkred"), 
            mode = "lines", name = "stormz") %>%
  add_trace(y = ~qf, color = I("lightblue"), mode = "lines", name = "quickflow")
```

## Clean storm data

``` r
#### We want to drop the storms that have missing data because this complicates
#### prediction
#### (remember, we have already filled gaps of 24 hr or less)
storms_2023_clean <- storms_2023 %>%
  ungroup() %>%
  dplyr::select(!c("tunnel_observed_flow_cms", "observed_flow_minus_tunnel")) %>%
  group_by(storm) %>%
  drop_na() %>%
  ungroup()
```

## Calculate storm stats

``` r
#### Get the start and end date of each storm
storm_start_end <- storms_2023_clean %>%
  group_by(storm) %>%
  summarise(storm_start = first(date),
            storm_end = last(date))

#### Rank them by peak discharge
storms_ranked_by_peak <- storms_2023_clean %>%
  group_by(storm) %>%
  summarise(peak_q = max(mean_observed_flow_cms)) %>%
  arrange(desc(peak_q))


#### Calculate turbidity "load" for each storm
storm_turbidity_load <- storms_2023_clean %>%
  inner_join(., hourly_turbidity_coldbrook %>%
               dplyr::select(site_no, dateTime, fw_mean_observed_turbidity),
             by = c("dateTime", "site_no")) %>%
  mutate(turbidity_flux = mean_observed_flow_cms*fw_mean_observed_turbidity) %>%
  mutate(turbidity_load = turbidity_flux*60*60) %>%
  group_by(storm) %>%
  summarise(observed_storm_turbidity_load = sum(turbidity_load),
            observed_peak_turbidity_flux = max(turbidity_flux))
```
