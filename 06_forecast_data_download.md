06_forecast_data_download
================
JTK
2025-01-02

################################################################################ 

This script downloads two different hydrological streamflow forecasts

1)  It downloads National Water Model forecasts from an open-source
    Google Bucket, where operational forecasts are archived. It uses
    cloud-based resources to trim the large files output by the NWM to
    just our stations of interest. Users can change the COMIDs to better
    reflect their locations of interest. The NWM download relies heavily
    on the nwmTools package written by Mike Johnson, but with some
    rewrites of the core functions, which I believe were written for
    past iterations. \*\*\* Note that this can take a LONG time to run

2)  It downloads Northeast River Forecast Center (NERFC) forecasts from
    GitHub folder. These forecasts are archived on the Iowa State
    Mesonet, which lacks a readily accessible mechanism for downloading
    directly from R. So I have downloaded them manually from their
    interface, and archived them on GitHub specifically for
    reproducibility purposes.

################################################################################ 

# Housekeeping

### Packages

``` r
### Data mgmt

require(tidyverse)
require(glue)

### NWM download and NetCDF mgmt

require(terra)
require(tidync)
require(nwmTools)
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
```

# Download NWM Forecasts

### Generate the urls where forecasts might be found

``` r
#### First, get the dates that we are interested in
#### We only want to download forecasts from v2.1 of the NWM
#### Which was operational from from 21 April 2021 to 20 September 2023

download_dates_mt <- tibble(date = seq.Date(from = as.Date("2021-04-21"), 
                                            to = as.Date("2023-09-20"), 
                                            by = "day")) %>%
  .$date

#### Now generate the Google Bucket URLs where each of those forecasts is archived
#### Note that the medium-term forecast is initialized every 6 hours, and forecasts
#### out 204 hrs (8.5 days) in the future
#### We only want to download one daily initialization, which is that made at 
#### midnight UTC
#### There are also seven members of the medium-term NWM
#### We want to download them all

#### URLS for V2.1 ######################################################################
#########################################################################################

#### Make some empty lists to save things

urls_per_member_v21 <- list()

urls_all_days_mt_v21 <- list()

#### Loop over all dates

for(j in 1:length(download_dates_mt)) {
  
  forecast_date_mt <- download_dates_mt[j]
  
  #### Loop over each member
  
  for(i in seq(1, 7, 1)){
    
        #### Print progress
  
        print(paste(forecast_date_mt, "_ ens", i ))
        
        #### Generate URLs using out rewrite of the nwmTools function
      
        urls_v21 <- get_gcp_urls2(domain = "conus",
          output = "channel_rt",
          config = "medium_range",
          ensemble = i,
          date = forecast_date_mt,
          hour = "00",
        minute = "00",
        num = 204)
        
        #### Clean-up the urls
        
        urls_v21 <- urls_v21 %>%
          #mutate(urls = sub("(.*)f ", "\\1f0\\2", urls)) %>%
          mutate(init_time = 0) %>%
          mutate(init_date = forecast_date_mt) %>%
          mutate(member = paste0("mem", i))
        
        #### Store each member for a given date
        
        urls_per_member_v21[[i]] <- urls_v21
    
  }
  
  #### Store all members for each date
  
  urls_all_days_mt_v21[[j]] <- urls_per_member_v21
}





### Join all the urls together

all_urls_mt <- bind_rows(urls_all_days_mt_v21) %>%
  mutate(predict_date = date(dateTime)) %>%
  mutate(lead_time = str_extract(str_extract(urls, "f\\d+"), "\\d+")) %>%
  mutate(init_date_time = as_datetime(init_date)) 



#############################################################################################
#############################################################################################
```

### Format URLs and COMID dataframes to allow for download

``` r
#### First, nest the URL dataframe

all_urls_and_files_mt_nest <- all_urls_mt %>%
  rename(predict_dateTime = dateTime) %>%
  filter(lead_time <= 192) %>% ### Trim off the half-day at the end of each forecast
  mutate(membr = member ) %>%
  nest_by(init_date, membr) %>%
  ungroup() %>%
  mutate(month = format(as.Date(init_date), "%b%Y"))

#### Now, subset the files to download
#### We do this so that we can iteratively save things because downloading 
#### the full years-long record would take forever
#### This allows us to track our progress better 

#### Subset of files to download

#################Change this to download month of interest######################

trim_urls_and_files_mt_nest <- all_urls_and_files_mt_nest %>%
  filter(month == "Oct2022") 

################################################################################

#### Unnest the dataframe

trim_urls_and_files_mt_unnest <- trim_urls_and_files_mt_nest %>%
  unnest(data) %>%
  dplyr::select(!membr) 

#### Declare where to write things 

write_file <- paste0(here("nwm_operational/medium_term"), "/",
                     trim_urls_and_files_mt_unnest$month[1],
                     ".csv")
```

### Download NWM

``` r
#### Extract the COMIDs that we are interested in
#### Note that this inherits the selected_gages variable from
#### 01_data_discovery.Rmd (so it requires that script to be run)

stations <- selected_gages %>%
  distinct(nhdpv2_comid) %>%
  drop_na(nhdpv2_comid) %>%
  mutate(comid = str_trim(nhdpv2_comid)) %>%
  .$comid %>%
  as.numeric(.) 


#### Do the download
#### Do it
#### Note that this function tries a given URL five times maximum with a 
#### delay of 60 seconds between tries
#### This allows minor blips in internet connection to not break the download
#### If none of those tries returns actual data, it then returns a dataframe
#### with NAs that is formatted (i.e., contains all the fields) of the actual
#### data

walk(trim_urls_and_files_mt_unnest$urls,
     .f = possibly(insistently(get_timeseries3,
                               rate = rate_delay(pause  = 60,
                                                 max_times = 5)),
                   otherwise = tibble(comid = NA, 
                                      init_date = NA, 
                                      lead_time = NA,
                                      member = NA,
                                      predict_dateTime = NA,
                                      modeled_q_cms = NA)),
     stations, ### Constant fed to .f 
     write_file,
     .progress = TRUE)
```

### Check to see if download completed

``` r
#### Since we are downloading a month at a time, the download may break before
#### The entire month is downloaded. To check, we want to see what the last date
#### downloaded was
#### If the entire month was downloaded it should be, for example, 2022-10-31

### Read in the file
nwm_forecasts <- read_csv(write_file)

### See if we downloaded the whole month
tail(nwm_forecasts)

### Check the last date
last_date <- last(nwm_forecasts)

last_date

### Find index of last timestep downloaded in the original df
last_downloaded_index <- which(trim_urls_and_files_mt_unnest$init_date == 
                                 last_date$init_date &  
                               trim_urls_and_files_mt_unnest$predict_dateTime == 
                                 last_date$predict_dateTime &
                               trim_urls_and_files_mt_unnest$member ==
                                 last_date$member)

last_downloaded_index 


### Check for missing
missing_dates_indexes <- which(is.na(nwm_forecasts$predict_dateTime))

missing_dates_indexes

### If they exist, where are they

missing_data <- nwm_forecasts %>%
  mutate(date_mem = paste(init_date, member, sep = "_")) %>%

unique(missing_data$date_mem)

### Check all days are present
unique(nwm_forecasts$init_date)

unique(nwm_forecasts$member)

### Check the time zone
nwm_forecasts$predict_dateTime[1]
```

# Download NERFC forecasts

### Download from local

Again, we have to We have to download these manually from:
<https://mesonet.agron.iastate.edu/wx/afos/list.phtml>, store them
locally, and then bring them in here. I have done this for the NERFC
forecasts for the Coldbrook station for 2023, which I have uploaded to
GitHub under the /data folder.

``` r
### Set the file path where 
### we have stored the NERFC forecast .txt files we have manually downloaded

nerfc_file_path <- here("data/archived_nerfc_forecasts_wy_2023")

#### Now we want to create a data frame with the paths of those files
#### and also extract some information from the file path name
#### namely the datetime of the forecast
#### which is when a given forecast was initialized

nerfc_files_wy_2023 <- tools::list_files_with_exts(nerfc_file_path, "txt") %>%
    map_chr(~tools::file_path_as_absolute(.)) %>%
  as_tibble() %>%
  rename(path = value) %>%
  mutate(year = str_extract(path, "(?<=RVFALY_?)\\d{4}"),
         month =  substr(str_extract(path, "(?<=RVFALY_?)\\d+"),5,6),
         day =  substr(str_extract(path, "(?<=RVFALY_?)\\d+"),7,8),
         hour_min =  substr(str_extract(path, "(?<=RVFALY_?)\\d+"),9,12)) %>%
  mutate(date = ymd(paste0(year,month,day, sep = "-"))) %>%
  mutate(initialized_datetime = paste(date, hour_min))

### Now read in the files
### !!!!!!!!!!!This generates a warning but its fine!!!!!!!!!!!!!!!!!!!!

nerfc_files_wy_2023 <- nerfc_files_wy_2023 %>%
  mutate(forecasts = map(path, ~nerfc_converter1(.x)))

  

### Now filter only the text files that contain the station of interest
### which here is Esopus at Cold Brook
### IMPORTANT: the station name needs to match exactly what is in the files

nerfc_files_wy_2023 <- nerfc_files_wy_2023 %>%
  filter(map_lgl(forecasts, ~ "Esopus Creek - Cold Brook, NY" %in% .x$station))




### Use the another function to process the Cold Brook forecast text files
### Then unnest them so we have a long data frame
### !!!!!!!!!!!This generates a warning but its fine!!!!!!!!!!!!!!!!!!!!

processed_nerfc_esopus <- nerfc_files_wy_2023 %>%
  mutate(coldbrook_forecasts = map(forecasts, nerfc_converter2)) %>%
  mutate(rows = map_dbl(coldbrook_forecasts, nrow)) %>%
  dplyr::select(path, year, date, 
                initialized_datetime, coldbrook_forecasts, rows) %>%
  rename(initialized_date = date) %>%
  unnest(coldbrook_forecasts)

#### Now, finish up by using the various dates and times from the file name
#### and the forecast files itself to get the forecast timestamps 
#### in the proper format

cleaned_nerfc_esopus <- processed_nerfc_esopus %>%
  dplyr::select(!rows) %>% #don't need this, was just a check earlier
  mutate(forecast_date = paste(year,month,day, sep = "-")) %>%
  mutate(forecast_dtm = paste(forecast_date, time)) %>%
  mutate(forecast_datetime = parse_date_time(forecast_dtm, "YmdHp", tz = "US/Eastern")) %>%
  mutate(initialized_datetime = parse_date_time(initialized_datetime, 
                                                "YmdHM", tz = "UTC")) %>%
  dplyr::select(!c(forecast_date, forecast_dtm, year, month, day, time)) %>%
  mutate_at("initialized_datetime", ~with_tz(., tzone = "US/Eastern"))
```

### Download USGS rating curve

We need to do this because NERFC serves stage rather than discharge in
their public facing forecasts. So we need to transform back to
discharge. This is not as much of an issue as one might anticpate, since
behind the scenes NERFC models are actually outputting discharge, so
little information is lost in the backtransformation

``` r
#### Download rating curve from NWIS

esopus_rating_raw <- dataRetrieval::readNWISrating("01362500", type = "exsa")

esopus_rating_curve <- esopus_rating_raw %>%
  dplyr::select(!STOR) %>%
  rename(stage = INDEP,
         shift = SHIFT,
         Q = DEP) 
```

### Convert stage to discharge

Here we want to us the USGS rating curve at the Esopus station to
transform the archived NERFC forecasts from stage to discharge.

``` r
#### Join the stage-discharge table to the forecasted stage data
#### The stage-discharge table is essentially a lookup table in lieau of an equation
#### for the stage-discharge relationship
#### Use inner join to only add the lookup stage values that appear in the forecast record
#### This avoids lots of NAs but is essentially the same as right_join(....) %>% drop_na()

discharge_nerfc_esopus <- cleaned_nerfc_esopus %>%
  rename(stage = forecast_stage) %>%
  inner_join(., esopus_rating_curve, by = "stage") %>%
  rename(forecasted_Q = Q) %>%
  mutate(modeled_flow_cms = forecasted_Q*0.0283168) %>%
  rename(dateTime = forecast_datetime) %>%
  mutate(initialized_datetime = ifelse(hour(initialized_datetime) < 11, 
                                       ceiling_date(initialized_datetime, unit = "hours"), 
                                     floor_date(initialized_datetime, unit = "hours"))) %>%
  mutate(initialized_datetime = as_datetime(initialized_datetime)) %>%
  mutate(initialized_datetime = with_tz(initialized_datetime, "US/Eastern")) %>%
  mutate(lead_time = difftime(dateTime, initialized_datetime,  unit = "hours")) %>%
  mutate(lead_group = case_when(lead_time <= 24 ~ "6-24",
                                lead_time > 24 & lead_time <= 48 ~ "30-48",
                                lead_time >48 & lead_time <= 72 ~ "54-72")) %>%
  relocate(lead_time, .after = "dateTime") %>%
  relocate(lead_group, .after = "lead_time") 
```

### Select one forecast per day

``` r
#### Extract only the one daily initialization at ~11 am. This forecast is issued each day, 
#### and though others are often initialized/updated and distributed "as needed" 
#### i.e., during large storms, it becomes overly complex to incorporate those 
#### (and who knows how those are used by operations/practioners). 

discharge_nerfc_esopus_once_daily <- discharge_nerfc_esopus %>%
   filter(hour(initialized_datetime) >= 10 & hour(initialized_datetime) < 12) %>%
  group_by(initialized_date, dateTime) %>%
  dplyr::slice(1) %>%
  mutate(lead_time = as.numeric(lead_time)) %>%
  rename(init_date = initialized_date,
         init_dateTime = initialized_datetime,
         predict_dateTime = dateTime) %>%
  mutate(tributary = "ESOPUSCREEKATCOLDBROOKNY") %>%
  mutate(log_modeled_flow = log10(modeled_flow_cms)) %>%
  dplyr::select(!path) %>%
  filter(init_date < "2023-09-20") %>%
  dplyr::ungroup()
```