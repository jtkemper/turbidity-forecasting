02_observational_data_download_and_clean
================
JTK
2024-12-20

# HOUSEKEEPING

``` r
knitr::opts_chunk$set(echo = TRUE,
                      fig.path = "figures/"
                      )


require(here)
require(tidyverse)
require(ggthemes)
require(nwmTools)
require(dataRetrieval)
require(tsibble)
require(ggpubr)
require(zoo)
require(slider)
require(sbtools)
```

# Load prior variables and functions

``` r
source(knitr::purl(here("Rmd-files/01_data_discovery.Rmd"), quiet=TRUE))

source(knitr::purl(here("Rmd-files/00_functions.Rmd"), quiet=TRUE))
```

# GET GAGE METADATA

``` r
#### These are the gages within Esopus Creek that have paired turbidity & flow data
#### Dating back to at least 2016
#### Additionally, we further pared down the gage dataset by selecting only
#### the downstream-most gage when multiple gages were located in a single COMID reach
#### For further details, see 01_data_discovery.md
#### Note that this script must be run first

#### Trim the gage meta data to remove extraneous fields

selected_gages_trim <- selected_gages %>%
  dplyr::select(site_no, station_nm, 
                drain_area_va, nhdpv2_comid,
                dec_lat_va, dec_long_va) %>%
  rename(tributary = station_nm,
         comid = nhdpv2_comid,
         lat = dec_lat_va,
         long = dec_long_va,
         drain_area_mi2 = drain_area_va) %>%
  mutate(comid = as.character(comid))

#### Check the gage IDs that we identified in the prior script

print(selected_gages_nwis_ids)
```

# DOWNLOAD OBSERVATIONAL DATA

## Flow Data

### Download data

``` r
#### Do the download
#### The USGS parameter code for discharge
#### is "00060"
#### Note that this does take awhile

inst_discharge_all_sites <- readNWISuv(siteNumbers = selected_gages_nwis_ids$nwis_id,
                                  parameterCd = "00060",
                                  startDate = "2015-10-01",
                                  endDate = "2023-09-30",
                                  tz = "America/New_York") %>%
  renameNWISColumns() %>%
  addWaterYear()

#### Add in site names to discharge file

inst_discharge_all_sites <- inst_discharge_all_sites %>%
  inner_join(., selected_gages_trim,
             by = "site_no") %>%
  rename(station = tributary,
         Flow = Flow_Inst)


#### And download the discharge data from the Shandaken tunnel
#### This will be necessary later when we identify storm events

inst_discharge_tunnel <- readNWISdata( sites = "01362230", 
                                           service = "iv",
                                           parameterCd = "00060",
                                           startDate = "2016-10-01", 
                          endDate = "2023-09-19",
                                           tz = "America/New_York") %>% 
  renameNWISColumns() %>%
  as_tsibble() %>%
  fill_gaps() %>%
  mutate(Flow_Inst = 
           zoo::na.approx(Flow_Inst, maxgap = 24)) %>% ## Fill in six hr gaps
  as_tibble() %>%
  mutate(station = "Tunnel") %>%
  rename(Flow = Flow_Inst)
```

### Subset the data

``` r
########### Grab the discharge from the Coldbrook gage only ###################

#### Pull out just the data for the Coldbrook gage
#### We will need this later

inst_discharge_coldbrook <- inst_discharge_all_sites %>%
  filter(str_detect(station, "COLD")) 

###############################################################################

########### Extract stations that have flow records extending to 2015 ##########

#### First check when the flow record starts and ends

start_ends <- inst_discharge_all_sites %>%
  group_by(station, site_no) %>%
  summarise(start_date = first(dateTime),
            end_date = last(dateTime)) %>%
  mutate(longer = ifelse(start_date < "2016-10-01", TRUE, FALSE))

### See which ones extend beyond 2016

long_stations <- start_ends %>%
  filter(longer == TRUE)

### Extract the ones with longer discharge records 

longer_record_q <- inst_discharge_all_sites %>%
  filter(station %in% long_stations$station)

################################################################################
```

## Turbidity Data

### Download turbidity at Coldbrook

``` r
#### Here we are going to download the turbidity data for the Coldbrook gage
#### Because this is where we want to forecast turbidity
#### Note that downstream_site_id is inherited from 01_data_discovery
#### But here we repeat it just to make this script more independent
#### It can be uncommented if 01 has not been run
#### Note that 63680 is the USGS code for instaneously-measured in-stream turbidity

#downstream_site_id <- "USGS-01362500"

#### This will get us the turbidity data that is available on NWIS

inst_turbidity_coldbrook <- readNWISdata(siteNumbers = "01362500",
                                    service = "iv",
                                   parameterCd = "63680",
                                   startDate = "2016-10-01",
                                   endDate = "2023-09-30",
                                 tz = "America/New_York") %>%
  renameNWISColumns() %>%
  addWaterYear()

#### This will get us the turbidity data that is only available on ScienceBase.
#### Here we use the sbtools package to get data directly from ScienceBase
#### See https://journal.r-project.org/archive/2016-1/winslow-chamberlain-appling-etal.pdf
#### For documentation

#### Get the item

sb_item <- sbtools::item_get("65ff1c43d34e64ff1548df16")

#### Download the file to a tempdir()

turb_sb_files <- item_file_download(sb_item, dest_dir = tempdir())
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |=========                                                             |  14%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |===================                                                   |  28%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |=====================                                                 |  31%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  39%  |                                                                              |============================                                          |  40%  |                                                                              |============================                                          |  41%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  56%  |                                                                              |========================================                              |  57%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |=================================================                     |  71%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================|  99%  |                                                                              |======================================================================| 100%
    ##   |                                                                              |                                                                      |   0%  |                                                                              |======================================================                |  78%  |                                                                              |======================================================================| 100%

``` r
#### Read in the turbidity data

turb_from_sb <- read_csv(turb_sb_files[1], skip = 1)
```

### Download turb at 01362370 & 01362497 (Stony Clove at Chicester and Little Beaver Kill)

``` r
#### We will need this to make a correction between the DTS-12 probe and the Analite probe
#### Because there is a period where the Analite probe was the only probe at Coldbrook
#### And it is not a 1:1 conversion 

#### These are the sites where USGS has deployed DTS-12 (old probes) and analites (new probes) simulatanously. We are downloading to develop a regression between new probe and old probe. Analites went in the stream on 2022-11-10 at Little Beaver Kill site and 2022-11-15 at the Stony Clove at Chichester site.

### Stony at Chichester

inst_turbidity_chichester <- readNWISdata( sites = "01362370",
                               service = "iv",
                               parameterCd = "63680",
                               startDate = "2022-11-15",
                               endDate = "2024-01-31",
                                           tz = "America/New_York") %>%
  renameNWISColumns() %>%
  rename(code = contains("_cd")) %>%
  rename(dts_12 = contains("Forest"),
         analite = contains("Analite")) %>%
  drop_na(analite) %>%
  drop_na(dts_12) 

### Little Beaver

inst_turbidity_little_beaver <- readNWISdata( sites = "01362497",
                               service = "iv",
                               parameterCd = "63680",
                               startDate = "2022-11-10",
                               endDate = "2024-01-31",
                                           tz = "America/New_York") %>%
  renameNWISColumns() %>%
  rename(code = contains("_cd")) %>%
  rename(dts_12 = contains("dts"),
         analite = contains("Analite")) %>%
  drop_na(analite) %>%
  drop_na(dts_12)
```

# CLEAN & TRANSFORM DATA

## Flow Data

### Transform to hourly

``` r
#### Here we are going to transform the flow data into hourly data
#### Note that this uses the hourly_q_maker custom function
#### So 00_functions must be run

##### For all sites 

hourly_discharge_all_sites <- hourly_Q_maker(inst_discharge_all_sites) %>%
  mutate(station = str_remove_all(station, " "))

##### For the tunnel

hourly_discharge_tunnel <- hourly_Q_maker(inst_discharge_tunnel)

##### Then extract hourly discharge data at Coldbrook
##### We will need this later

hourly_discharge_coldbrook <- hourly_discharge_all_sites %>%
  filter(site_no == "01362500")
```

## Turbidity data

### Convert Analite data

#### Build the regression

``` r
#### The first thing we need to do is build a regression between
#### Turbidity as measured by the DTS-12 probe and the Analite probe
#### USGS has both probes at Little Beaver and Stony Clove at Chichester
#### Measuring turbidity concomittantly
#### We will take this 15-minute data and calculate a linear regression line
#### Then we will use it to convert the Coldbrook data
#### This way, all turbidity data is essentially from the same instrument
#### And we can make apples-to-apples comparisons

#### First, filter out the provisional data from Little Beaver & Stony Clove

inst_turbidity_little_beaver_clean <- inst_turbidity_little_beaver %>%
    filter(code2 != "P" & code3 != "P")

inst_turbidity_chichester_clean <- inst_turbidity_chichester %>%
  filter(code2 != "P" & code3 != "P")

##### Combine into one dataframe

inst_turb_chi_lb <- bind_rows(inst_turbidity_little_beaver_clean,
                              inst_turbidity_chichester_clean)

###############################################################################
############# PROBE TO PROBE REGRESSION #######################################

#### Probe-to-probe regression


###### Do the calculations with a simple linear regression. Currently set up to develop regression equations that can modify Analite (new probe) data "into" DTS-12 (old probe) data by making DTS-12 turbidity the dependent variable. We can change this if that's not the best approach (probably as we start to get more and more Analite data). Note that we are forcing the intercept through zero



probe_to_probe_calcs <- inst_turb_chi_lb %>%
  nest() %>%
  dplyr::mutate(fit = map(data,
                          ~lm(dts_12 ~ analite, .)),
                rsquare = map_dbl(fit, ~summary(.)$r.squared),
                m = map_dbl(fit, ~summary(.)$coefficients[2]),
                b = 0,
                pval = map_dbl(fit, ~summary(.)$coefficients[4])
                )


#### See the equations, R2, and pvalues

probe_to_probe_eqns <- probe_to_probe_calcs %>%
  dplyr::select(!data)

probe_to_probe_eqns
```

    ## # A tibble: 1 × 5
    ##   fit    rsquare     m     b    pval
    ##   <list>   <dbl> <dbl> <dbl>   <dbl>
    ## 1 <lm>     0.916 0.769     0 0.00100

#### Plot regression

``` r
#### Do it

inst_turb_chi_lb %>%
  ggplot() +
    geom_point(aes(x = analite, y = dts_12),
               color = "goldenrod4",
               shape = 0) +
    geom_smooth(aes(x = analite, y = dts_12),
                method = "lm",
                color = "goldenrod",
                formula = y ~ 0 + x,
                se = TRUE) +
    annotate(x = 280, y = 1250,
             geom = "text", label = paste0("y = ", 
                                           round(probe_to_probe_calcs$m, 2),
                                           "x + ",
                                           probe_to_probe_calcs$b),
             color = "goldenrod",
             size = 5) + 
    annotate(x = 1100, y = 80,
             geom = "text", label = paste0("R^2 == ", 
                                           round(probe_to_probe_calcs$rsquare,
                                                 2)),
             parse = TRUE,
             color = "goldenrod",
             size = 4) +
    annotate(x = 1100, y = 0,
             geom = "text", label = paste0("p = 0.001"),
             parse = FALSE,
             color = "goldenrod",
             fontface = "italic",
             size = 3) +
    scale_shape_manual(values = c(0,1,2),
                       name = "USGS Site ID") + 
    scale_y_continuous(breaks = seq(0,1300,200),
                       minor_breaks = seq(0,1300,100)) +
    scale_x_continuous(breaks = seq(0,1300,200),
                       minor_breaks = seq(0,1300,100)) + 
    labs(y = "DTS-12", x = "Analite") + 
    guides(shape = guide_legend(nrow = 2)) + 
    theme_few() +
    theme(panel.grid = element_line(color = "gray90"),
          legend.position = "bottom")
```

![](figures/unnamed-chunk-9-1.png)<!-- -->

#### Convert the Coldbrook data

``` r
#### Here we are going to take the equation we just built 
#### And use it to "convert" the Analite-measured turbidity to
#### DTS-12 turbidity 
#### Once we do this, then we coalesce the DTS-12 column and Analite column
#### And get one continuous record of turbidity that is more-or-less
#### Apples-to-apples 

##### Do this for the data from NWIS

inst_turbidity_coldbrook_clean <- inst_turbidity_coldbrook %>%
  as_tibble() %>%
  select(!contains("_cd")) %>%
  dplyr::rename(analite = contains("Analite")) %>%
  dplyr::rename(dts_12 = contains("Forest")) %>%
  mutate(analite = probe_to_probe_eqns$m*analite + probe_to_probe_eqns$b) %>%
  mutate(observed_turbidity = coalesce(dts_12, analite)) %>%
  dplyr::select(site_no, dateTime, observed_turbidity)


##### And for the data from ScienceBase

inst_turbidity_from_sb_clean <- turb_from_sb %>%
  mutate(transformed_analite_turbidity = probe_to_probe_eqns$m*Analite_turbidity_FNU  + 
           probe_to_probe_eqns$b) %>%
  rename(observed_turbidity = transformed_analite_turbidity) %>%
  dplyr::select(site_no, dateTime, observed_turbidity)

##### And then combine the above two dataframes to get an overall record of 
##### Turbidity at coldbrook

inst_turbidity_coldbrook_clean_all <- bind_rows(inst_turbidity_coldbrook_clean,
                                                inst_turbidity_from_sb_clean) %>%
  arrange(dateTime)
```

### Transform to hourly

``` r
################################################################################
#### The transformation to hourly data for turbidity is more complicated
#### Because we do not want to simply take a arithmetic mean of turbidity
#### But instead take a "flow-weighted" average
#### Meaning we have to convert turbidity measures to "flux" using discharge
#### Intergrate under the flux curve over each hour
#### And then divide by the water "load" for that hour 
#### (Which we get by intergrating under the discharge curve)
#### This gives us a mean turbidity that is more reflective of the central 
#### tendency of turbidity in the real world than the arithmetic mean
#### Which might be overly skewed by high or low values
################################################################################

#### To do this, we must join the instantaneous discharge data from Coldbrook
#### to the instantaneous turbidity data from Coldbrook
#### Multiply together to get a flux
#### Multiply the flux by the unit of time reflected in each measurement 
#### (Importantly, this is one hour if there is one measure per hour,
#### 30 minutes if there are two per hour
#### 20 minutes if there are three per hour
#### and 15 minutes if there are four per hour)
#### We do the same for discharge to get water load
#### Then we divide turbidity "load" by water "load"
#### And finally fill in gaps that are 24 hours are less
#### With linear interpolation

hourly_turbidity_coldbrook <- inst_discharge_coldbrook %>%
  inner_join(., 
             inst_turbidity_coldbrook_clean_all, 
             by = c("site_no", "dateTime")) %>%
  mutate(observed_flow_cms = Flow*0.0283168) %>%
  dplyr::select(!Flow) %>%
  mutate(date = date(dateTime),
         hour = hour(dateTime)) %>%
  dplyr::group_by(date, hour, site_no) %>%
  mutate(obs_per_hour = as.numeric(n())) %>%
  ungroup() %>%
  mutate(observed_turbidity_flux = observed_turbidity*observed_flow_cms) %>%
  mutate(observed_turbidity_load = case_when(obs_per_hour == 4 ~ observed_turbidity_flux*60*15,
                                 obs_per_hour == 2  ~ observed_turbidity_flux*60*30,
                                 obs_per_hour == 1 ~ observed_turbidity_flux*60*60,
                                 obs_per_hour == 3 ~ observed_turbidity_flux*60*20)) %>%
  mutate(observed_water_load = case_when(obs_per_hour == 4 ~ observed_flow_cms*60*15,
                                 obs_per_hour == 2  ~ observed_flow_cms*60*30,
                                 obs_per_hour == 1 ~ observed_flow_cms*60*60,
                                 obs_per_hour == 3 ~ observed_flow_cms*60*20)) %>%
  dplyr::group_by(station, site_no, date, hour) %>%
  summarise(dateTime = floor_date(dateTime[1], unit = "hour"),
            fw_mean_observed_turbidity = sum(observed_turbidity_load)/
              sum(observed_water_load)) %>%
  dplyr::ungroup() %>%
  as_tsibble(key = c(station, site_no), index = "dateTime") %>%
  fill_gaps(.full = FALSE) %>%
  mutate(fw_mean_observed_turbidity = 
           zoo::na.approx(fw_mean_observed_turbidity, maxgap = 24)) %>%
  as_tibble() %>%
  mutate(log_observed_turbidity = log10(fw_mean_observed_turbidity)) %>%
  dplyr::ungroup()
```

# CALCULATE ANTECEDENT FLOW VALUES

``` r
#### Finally, here we are going to calculate a variety of antecedent discharge
#### Values for each station
#### These are antecedent 6-hr, daily, weekly, and monthly mean discharge
#### These simply the arithmetic mean discharge over that time period
#### As well as the maximum Q over the past month, three months, six months
#### And year
#### And the time since those peaks 
#### As well as the difference in discharge between time t and time t-1
#### Both raw and normalized by the discharge at time t


##### Here we calculate the various antecedent mean discharege terms
##### That are a month or shorter
##### We do the longer term stuff seperately, because that involves a dataset
##### That is longer in time, which is not available for all sites
expanded_discharge_all_sites <- hourly_discharge_all_sites %>%
  dplyr::group_by(station) %>%
  arrange(dateTime, .by_group = TRUE) %>%
  mutate(mean_prior_daily_log_q = rollapply(log_observed_flow, 
                                        width = list(-(24:1)), mean,
                                  align = "right", fill = NA),
         mean_prior_weekly_log_q = rollapply(log_observed_flow, 
                                         width = list(-((24*7):1)), mean,
                                  align = "right", fill = NA),
         mean_prior_monthly_log_q = rollapply(log_observed_flow, 
                                          width = list(-((24*30):1)), mean,
                                  align = "right", fill = NA),
         max_prior_monthly_log_q = rollapply(log_observed_flow, 
                                          width = list(-((24*30):1)), max,
                                  align = "right", fill = NA),
        six_hour_lag_log_q = rollapply(log_observed_flow, 
                                          width = list(-((6):1)), mean,
                                  align = "right", fill = NA),
        log_limb = (log_observed_flow) - lag(log_observed_flow)) %>%
    mutate(dq_q = log_limb/log_observed_flow) %>%
  dplyr::ungroup()


##### Now we calculate some antecedent discharge conditions from the longer-term
##### sites (where discharge goes back to at least 2015-10-01)
##### This is maximum discharge  over the past 3 months and 6 months
##### Note that this takes a decent amount of time to run
longer_record_expanded_discharge <- hourly_discharge_all_sites %>%
  dplyr::group_by(station) %>%
  arrange(dateTime, .by_group = TRUE) %>%
  mutate(max_prior_threemonth_log_q = rollapply(log_observed_flow, 
                                          width = list(-((24*30*3):1)), max,
                                  align = "right", fill = NA),
         max_prior_sixmonth_log_q = rollapply(log_observed_flow, 
                                          width = list(-((24*30*6):1)), max,
                                  align = "right", fill = NA)) %>%
  mutate(date_3month_max = slider::slide2(log_observed_flow, 
                                    dateTime,
                                    .f = ~as_datetime(.y[which.max(.x)]),
                                    .before = (24*30*3),
                             .complete = FALSE)) %>%
  unnest(cols = date_3month_max) %>%
  mutate(date_6month_max = slider::slide2(log_observed_flow, 
                                    dateTime,
                                    .f = ~as_datetime(.y[which.max(.x)]),
                                    .before = (24*30*6),
                             .complete = FALSE)) %>%
  unnest(cols = date_6month_max)


##### And then over the past year
##### We break these up separately because the search over the year takes
##### An even longer time
##### As above, note that this takes a fair amount of time to run
annual_length_expanded_discharge <- longer_record_expanded_discharge %>%
  mutate(max_prior_annual_q = rollapply(log_observed_flow, 
                                          width = list(-((24*365):1)), max,
                                  align = "right", fill = NA)) %>%
   dplyr::group_by(station) %>%
   mutate(date_year_max = slider::slide2(log_observed_flow, 
                                    dateTime,
                                #dateTime,
                                    .f = ~as_datetime(.y[which.max(.x)]),
                                    .before = (24*365),
                             .complete = FALSE)) %>%
  unnest(cols = date_year_max) %>%
  dplyr::ungroup()

##### Finally, we want to 
##### calculate the time since the 3 month, 6 month, and annual peak
##### And then finally select all the variables of interest
longer_term_expanded_discharge_all_sites <- annual_length_expanded_discharge %>%
  mutate(time_since_3month_peak = abs(difftime(date_3month_max, 
                                               dateTime, units = "hours"))) %>%
  mutate(time_since_6month_peak = abs(difftime(date_6month_max, 
                                               dateTime, units = "hours"))) %>%
  mutate(time_since_annual_peak = abs(difftime(date_year_max, 
                                               dateTime, units = "hours"))) %>%
  dplyr::select(station, site_no, date, dateTime, 
                max_prior_threemonth_log_q,
                max_prior_sixmonth_log_q,
                max_prior_annual_q,
                time_since_3month_peak,
                time_since_6month_peak,
                time_since_annual_peak)
```
