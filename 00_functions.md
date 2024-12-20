00_functions
================
JTK
2024-12-20

## Hourly Flow Transformer

``` r
#### Transform 15-minute flow data to hourly and calculaute log discharge
#### First, convert flow data from cubic feet/second to cubic meters/second
#### Then simply take the average discharege over that hour
#### After that, we want to fill in gaps that are 24 hours or less 
#### By linear interpolation
#### Finally, we want to transform discharge to log discharge


hourly_Q_maker <- function(df) {
  
  df %>%
    dplyr::select(!contains("_cd")) %>%
    mutate(observed_flow_cms = Flow*0.0283168) %>%
    mutate(date = date(dateTime),
           hour = hour(dateTime)) %>%
    group_by(station, site_no, date, hour) %>%
    summarise(dateTime = floor_date(dateTime[1], unit = "hour"),
              mean_observed_flow_cms = mean(observed_flow_cms)) %>%
    ungroup() %>%
    as_tsibble(key = c(station, site_no), index = "dateTime") %>%
    fill_gaps(.full = FALSE) %>%
    as_tibble() %>%
    mutate(mean_observed_flow_cms =
             zoo::na.approx(mean_observed_flow_cms, maxgap = 24)) %>%
    as_tibble() %>%
    mutate(log_observed_flow = log10(mean_observed_flow_cms),
         log_squared_observed_flow = log_observed_flow^2) %>%
    dplyr::ungroup()

  
  
}
```
