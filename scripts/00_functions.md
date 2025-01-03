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

## Quantile Predictor

``` r
#### This functions predicts turbidity from discharge data
#### Using the a quantile regression model
#### It does so both for predictions (so using observed discharge data)
#### And for forecasts (so using forecasted discharge)

quantile_predictor <- function(df, type_strng) {
  
  ### Predict using forecasted flow
  
      if(type_strng == "forecast"){
        
        #### Bind observations to models
        nested_df <- df %>% 
          ungroup() %>% 
          nest()
        
        ### Convert Q to turbidity
        qqm_raw %>%
          dplyr::select(!data) %>%
          bind_cols(., nested_df) %>%
           mutate(data = map2(data, model, 
                             ~mutate(.x, 
                                     log_predicted_turbidity = .y$coefficients[[1]] + 
                                       .y$coefficients[[2]]*log10(modeled_flow_cms) + 
                      .y$coefficients[[3]]*(log10(modeled_flow_cms))^2))) %>%
          dplyr::select(!model) %>%
          mutate(data = map(data, 
                        ~nest_by(.x, lead_time))) %>%
          unnest(data) %>%
          nest_by(tau, lead_time) %>%
          dplyr::ungroup()
    
          
        
      }
  
  
  ### Predict using observed flow (i.e., benchmark the quantile model)
      else if(type_strng == "benchmark"){
        
        qqm_raw %>%
          dplyr::select(!data) %>%
          bind_cols(., df %>% nest()) %>%
           mutate(data = map2(data, model, 
                             ~mutate(.x, 
                                     log_predicted_turbidity = .y$coefficients[[1]] + 
                                       .y$coefficients[[2]]*log_observed_flow + 
                      .y$coefficients[[3]]*log_squared_observed_flow))) %>%
          dplyr::select(!model) %>%
          unnest() %>%
          dplyr::ungroup()
            
        
        
      }
  
  
  else{print("Error - type must be 'forecast' or 'benchmark'")}

  
  
}
```

## Quantile Smearer

``` r
#### This function estimates the smearing factor for predictions made using the 
#### quantile model
#### The smearing factor is used to adjust for the tendency for back-transformations
#### from log-scaled data to systematically underestimate

qqm_smearer <- function(raw_prediction_df,
                        D_fact = D_qqm) {
  
  
  raw_prediction_df <- raw_prediction_df %>%
    mutate(predicted_turbidity = (10^log_predicted_turbidity)*D_fact) %>%
    dplyr::select(!c(log_predicted_turbidity, log_observed_turbidity
                     ))
  
  
  return(raw_prediction_df)
  
  
}
```

## Decompose KGE

``` r
#### This function decomposes KGE into its constituent parts
#### of bias, variability, and correlation (r)
#### See Gupta et al. (2009) for more information
#### (https://doi.org/10.1016/j.jhydrol.2009.08.003)


decompose_kge <- function(sim, obs) {
  
  #### Calculate the correlation between modeled and observed
  
  r <- cor(sim, obs, method = "pearson", use = "pairwise.complete.obs")
  
  #### And the bias
  
  bias <- mean(sim)/mean(obs)
  
  #### And the variability 

  variability <- sd(sim)/sd(obs)
  
  #### Now calculate the terms as they appear in eqn. 9 in Gupta et al. (2009)
  #### Which are used to calculate KGE 
  
  r_term <- (r-1)^2
  
  variability_term <- (variability-1)^2
  
  bias_term <- (bias-1)^2
  
  #### And the final KGE calculation
  
  kge_manual <- 1 - sqrt(r_term + variability_term + bias_term)
  
  #### Finally, make a summary table of each 
  
  decomposed_kge <- tibble(r, variability, bias,
                           r_term, variability_term, bias_term,
                           kge_manual)
  
  
  return(decomposed_kge)
  
}
```

## LGBM Tuner

``` r
##### FUNCTION THAT TUNES A CHOSEN LGBM MODEL ##################################

lgbm_tuner <- function(observational_df, model_df) {
  
  set.seed(913)
  
      ### First, trim down the dataset to just our identified predictors
      trimmed_data <- observational_df %>%
        ungroup() %>%
        dplyr::select(model_df$Feature, log_observed_turbidity, water_year)
      
      
      ### Now, pull out the training data
      training_data <- trimmed_data %>%
        filter(water_year < 2023) %>%
        dplyr::select(!water_year)
      
      
      ### Now, build our training & validation split
      ### This is an expanding window design
      ### Here, we are telling tidymodels which bits of the training data
      ### We want to use as resamples (validation data)
      ### To tune the hyperparameters
      ### This is akin to 3-fold CV but for time series data
      ### We are basically training the model on 2017-2019 and then validating on 2020
      ### Training on 2017-2020 and validating on 2021
      ### And training on 2017-2022 and validating on 2023
      
      
      ### Make splits
      splits_list <- train_valid_splitter(trimmed_data)
      
      
      ### Declare the training/validation splits
      valid_data_expand <- manual_rset(splits_list, c("Split1", "Split2", "Split3"))
      
      
      
      ### Create a model recipe
      
      ### First, find the variables we have selected
      rec <- recipe(log_observed_turbidity ~ .,
                         data = training_data) %>%
        prep()
      
      
      ### Declare the specific parameters we want tuned
      ### You can uncomment to tune others
      lgbm_model <- parsnip::boost_tree(mode = "regression",
                                             #trees = 1000,
                                              min_n = tune(),
                                             #tree_depth = tune(),
                                             #trees = tune(),
                                             #num_threads = 10L,
                                             #learn_rate = tune()
                                             #bagging_freq = 10,
                                             #sample_size = tune(),
                                             ) %>%
        set_engine("lightgbm", objective = "rmse", verbose = -1, 
                   num_leaves = tune(), num_threads = 10)
      
      
      
      
      ### Supply ranges to examine those parameters
          lgbm_params <- dials::parameters(
                                           min_n(c(2L,60L), trans = NULL),
                                           #tree_depth(c(3,12L)),
                                           # learn_rate(c(-7, -1),
                                           #            trans = log10_trans()),
                                           num_leaves(c(2,256))
                                           #sample_prop(c(1/10,1))
                                           #trees(c(100,2000))
                                           )
          
          
          #### Construct the grid over which we are going to do the search 
          lgbm_grid <- dials::grid_max_entropy(lgbm_params,
                                                size = 100)
          
      
          #### Set up the workflow
          lgbm_wf <- workflows::workflow() %>%
            add_model(lgbm_model) %>%
            add_formula(log_observed_turbidity ~ .)
          
          ## DO it
          lgbm_tuned <- tune::tune_grid(object = lgbm_wf,
                                        resamples = valid_data_expand,
                                        grid = lgbm_grid,
                                        metrics = yardstick::metric_set(rmse),
                                        control = tune::control_grid(verbose = TRUE))
          
          
          #### Extract scores  
         rmse_scores  <- lgbm_tuned %>%
            tune::show_best(metric = "rmse", n = 100) %>%
            mutate(mean_rmse_rank = dense_rank(mean)) %>%
            mutate(mean_se_rank = dense_rank(std_err)) %>%
            mutate(mean_plus_se_rank  = mean_rmse_rank + mean_se_rank)
          
      return(rmse_scores)
             
} ### End function
```

## Train-Valid Splitter

``` r
################################################################################
########## FUNCTION THAT SPLITS UP DATA INTO TRAIN-VALID SPLITS ################

train_valid_splitter <- function(trimmed_df) {
  
  set.seed(913)
  
  
    split_list <- list()
    
        for(h in 2020:2022) {
    
        test_year <- h
    
        train_years <- seq(2017, h-1, 1)
    
        #### Training data
            trimmed_train <- trimmed_df %>%
            filter(water_year %in% train_years) %>%
              dplyr::select(!water_year)
    
          ### Now subset the testing data to that same subset
          trimmed_test <- trimmed_df %>%
            filter(water_year == test_year) %>%
              dplyr::select(!water_year)
          
          k = 2023 - h
          
          split_list[[k]] <- (make_splits(trimmed_train, 
                                         assessment = trimmed_test))
          
        }
    
      return(split_list)

}

################################################################################
################################################################################
```

## LGBM Runner

``` r
############################ FUNCTION TO RUN LGBM ##############################
### Importantly, this is really most useful when you only want to run it once
### The runs we have set up with the leave one out are more complex and require 
### Their own custom thing to be written
### Okay here's the function 
lgbm_runner <- function(training_data = NULL, 
                        testing_data = NULL, 
                        model = NULL,
                        save = FALSE,
                        save_file = NULL,
                        tuned = FALSE,
                        tuned_params = NULL) {
  
        stats_and_importance <- list()
        
        #save_file <- as.character(save_file)
      
      
          ################## SET UP MODEL ############################################
  
          ### These are the variables in our training data
        chosen_model_train <- training_data %>%
            ungroup() %>%
            dplyr::select(c(model$Feature, log_observed_turbidity))
        
    
        
            ### Now subset the testing data to that same subset
           chosen_model_test <- testing_data %>%
              ungroup() %>%
              dplyr::select(colnames(chosen_model_train))
      
        
              ### Declare the predictor and response variables 
            preds <- data.matrix(chosen_model_train %>%
                                        dplyr::select(!log_observed_turbidity))
            
            
            response <- chosen_model_train$log_observed_turbidity
            
            ### Set up the environment - this is just preparing the dataset API to be used by lightgbm. 
            ### This is our training data
            train_lgbm <- lgb.Dataset(preds, 
                                             label = response,
                                             #feature_pre_filter = FALSE,
                                             ) 
            
            ### Declare the test data
            test_lgbm <- data.matrix(chosen_model_test %>%
                                              dplyr::select(!log_observed_turbidity))
            

            ### Declare the hyperparameters
            ### These are defaults, but we have spelled them out 
            ### To be most clear
            
            ### User can specify whether they want tuned params or the defaults
            
            if(tuned == FALSE) {
              
                params <- list(objective = "regression",
                                num_leaves = 31L,
                                learning_rate = 0.1,
                                min_data_in_leaf = 20L,
                                num_threads = 10L
                               
                               )
            
            } else if(tuned == TRUE) {
              
                params <- list(objective = "regression",
                            num_leaves = tuned_params$num_leaves,
                            min_data_in_leaf = tuned_params$min_n)
              
            }
            
            
    ############################################################################
    
    
    ############################# MAKE MODEL #####################################
            

            ### Train the model
            
            set.seed(913)
            

            
            model_lgbm <- lgb.train(params,
                                    data = train_lgbm,
                                    verbose = 1L, 
                                    nrounds = 100L) ###nrounds is the default 100
            
            
            #### Save the model 
            ### According to user specifications
            ### Default is not to save the model 
            
            if(save == TRUE) {
              
                          lgb.save(model_lgbm, 
                                   filename = paste0(here("r_images"), "/",
                                          save_file,
                                          ".txt"))
              
            }
            
            #### Predict #######################################################
            
            
            ### Predict with the model on training data
            ### This allows for calculate of the smearing factor
            turb_fits <- predict(model_lgbm, 
                                 data = preds) %>%
                          as_tibble() %>% rename(log_predicted_turbidity = 1)
                        
            
            ### Predict with the model on testing (really, validation) data
            turb_predicted <- predict(model_lgbm, 
                                           data = test_lgbm) %>%
              as_tibble() %>% rename(log_predicted_turbidity = 1)
            
            
            ####################################################################
            
            ### Calculate smearing 
            
            #### Bind predictions on training data to training observations 
            #### and estimate smearing coefficient
            fitted_observed_smear <- bind_cols(training_data %>%
                                                 dplyr::select(dateTime,
                                                               log_observed_turbidity),
                                                                       turb_fits) %>%
                          mutate(exp_log_model_residuals = 10^(log_observed_turbidity -
                                                                 log_predicted_turbidity))
                        
            ### Estimate smearing coefficient  
            D <- mean(fitted_observed_smear$exp_log_model_residuals)
            
    
           #### Bind to observations and estimate smearing coefficient
          predicted_observed <- bind_cols(testing_data %>%
                                                  dplyr::select(c(model$Feature, 
                                                                  log_observed_turbidity,
                                                                  fw_mean_observed_turbidity,
                                                                  mean_observed_flow_cms, dateTime)),
                                                turb_predicted) 
        
        ### Now use the smearing coefficient to convert back to non-log
        turb_predicted_observed <- predicted_observed %>%
          mutate(predicted_turbidity = (10^log_predicted_turbidity)*D) %>%
          mutate(sqrerr = (fw_mean_observed_turbidity - predicted_turbidity)^2,
                 predicted_turb_flux = mean_observed_flow_cms*predicted_turbidity,
                 observed_turb_flux = mean_observed_flow_cms*fw_mean_observed_turbidity,
                 predicted_turb_inst_load = predicted_turb_flux*60*60,
                 observed_turb_inst_load = observed_turb_flux*60*60,
                 abs_error = abs((fw_mean_observed_turbidity - predicted_turbidity)),
                 abs_pct_error = abs_error/fw_mean_observed_turbidity)
        
        
        
    ###########################################################################
    ############################################################################
     
    ################# EVALUATE MODEL ###########################################
    

        #### Evaluate
        
        stats_high_turb <- turb_predicted_observed %>%
          ungroup() %>%
          filter(fw_mean_observed_turbidity >= 10) %>%
          summarise(high_turb_rmse = sqrt(mean(sqrerr)),
                    high_turb_mae = mean(abs_error),
                    high_turb_mape = mean(abs_pct_error),
                    high_turb_pbias = hydroGOF::pbias(predicted_turbidity,
                                            fw_mean_observed_turbidity)
                    ) 
    
        #### Calculate summary statistics
        
       chosen_model_stats_lgbm <- turb_predicted_observed %>%
          ungroup() %>%
          summarise(rmse = sqrt(mean(sqrerr)),
                    mae = mean(abs_error),
                    predicted_turb_load = sum(predicted_turb_inst_load),
                    observed_turb_load = sum(observed_turb_inst_load),
                    nse = hydroGOF::NSE(predicted_turbidity, 
                                        fw_mean_observed_turbidity),
                          logNSE = hydroGOF::NSE(log_predicted_turbidity, 
                                        log_observed_turbidity),
                    kge = hydroGOF::KGE(predicted_turbidity, 
                                        fw_mean_observed_turbidity),
                    mape = mean(abs_pct_error),
                    pbias = hydroGOF::pbias(predicted_turbidity,
                                            fw_mean_observed_turbidity),
                    corr = cor(predicted_turbidity, fw_mean_observed_turbidity,
                               use = "pairwise.complete.obs",
                               method = "pearson"),
                    spearman_corr = cor(predicted_turbidity, fw_mean_observed_turbidity,
                                        use = "pairwise.complete.obs",
                                        method = "spearman"),
                    r_sq = hydroGOF::R2(predicted_turbidity,fw_mean_observed_turbidity)
                    ) %>%
          mutate(err_load = (observed_turb_load - predicted_turb_load)
                 ) %>%
          bind_cols(., stats_high_turb)
       
       stats_and_importance[[1]] <- chosen_model_stats_lgbm
        
        #### Calculate variable importance
       
        chosen_lgbm_var_imp <- lgb.importance(model_lgbm , percentage = TRUE)
        
        stats_and_importance[[2]] <- chosen_lgbm_var_imp
        
        #### Save the output timeseries
        
        stats_and_importance[[3]] <- turb_predicted_observed
        
        #### Save the smearing factor 
        
        stats_and_importance[[4]] <- D
        
        #### Return what we want
        
        return(stats_and_importance)
        
          
} ### End function

    
    
##############################################################################    
###############################################################################
```

## New Get GCP URLS

``` r
### We need to re-write some functions from the nwmTools package
### Here we re-write the geturls command to better reflect 
### the URLS of the medium-term forecasts (the built-in function had syntax
### more reflective of the short-term urls)

get_gcp_urls2 <- function (config = "short_range", 
                                domain = "conus", date, hour = "00",
                                minute = "00", num, ensemble = NULL, 
                                output = "channel_rt") 
{
    meta = nwm_filter(source = "gcp", version = NULL, config = config, 
        date = date, ensemble = ensemble, output = output, domain = domain)
    dates = seq.POSIXt(as.POSIXlt(paste(date, hour, minute), 
        tz = "UTC"), length.out = num+1, by = "1 hour")
    YYYYDDMM = rep(format(as.POSIXct(date, tz = "UTC"), "%Y%m%d"), num)
    forward = sprintf("%03d", seq(1, num))
    urls = glue(meta$http_pattern, YYYYDDMM = YYYYDDMM, config = meta$config[1], 
        HH = hour, foward = forward, output = meta$output[1], 
        domain = meta$domain, ensemble = meta$ensemble[1], prefix = meta$prefix[1])
    dates = ymd_hm(paste0(date[1], hour, "00")) + hours(1:(num))
    data.frame(dateTime = dates, urls = urls, output = output)
} 

#### Must do this so that some functions internal to nwmTools can be used

environment(get_gcp_urls2) <- environment(get_gcp_urls)



###################################################################################
```

## File Reader

``` r
#### Function to read downloaded NetCDF files and retrieve various bits of 
#### information from those files 

file_reader <- function(file = NULL, ids = NULL) {
  
channel_params <- tidync(file) %>%
  activate("D0") %>%
  hyper_tibble() %>%
  filter(feature_id %in% ids)

channel_plus_time <- channel_params %>%
  mutate(feature_id  = as.character(feature_id)) %>%
  as_tibble() 

return(channel_plus_time)
  
  
}
```

## Temp File Maker

``` r
### Function to generate the temporary files to store downloaded NWM forecasts

temp_file_maker <- function(urls = NULL) {
  
 urls %>%
    mutate(date_time_mem = paste0(init_date_time, "_", init_time, "_", 
                                  member)) %>%
      mutate(filename = tempfile(pattern = paste0(date_time_mem,"plot"), 
                             fileext = ".nc")) %>%
   dplyr::select(!c(date_time_mem)) 
  
  
}
```

## NWM Downloader

``` r
### A function that downloads the NWM operational forecast from the
### Google Bucket archive 
### This function downloads the ENTIRE NWM output for all of the continental US
### And then extracts our COMIDs of interest and then deletes that big file
### This is probably the non-ideal way to do this


### Function to download the files, trim them, and save them 

nwm_downloader <- function(urls_df = NULL) {
  
  
  ### Show file being processed
  
  print(paste("Downloading", 
              urls_df$predict_date[1],
              urls_df$member[1]))
  
  ### Create temp files to store the downloaded .nc files
  
  urls_and_files <- temp_file_maker(urls_df)
  
  ### Download the .nc files 
  ### From the Google Bucket
  
  download.file(url = urls_and_files$urls,
                  destfile = urls_and_files$filename, 
              mode = "wb", method = "libcurl",
              quiet = TRUE)
  
 
  
  downloaded <- urls_and_files %>%
    mutate(data = map(filename, 
                       .f = possibly(file_reader, 
                             otherwise = tibble(feature_id = NA,
                                                streamflow = NA,
                                                nudge = NA,
                                                velocity = NA,
                                                qSfcLatRunoff = NA,
                                                qBucket = NA,
                                                qBtmVertRunoff = NA)),
                      stations,
                      .progress = TRUE)) %>%
    unnest(cols = data) 
  
  #### Write all the final data to local files 
  
  write_csv(downloaded %>%
              dplyr::select(!c(urls, filename)),
            file = paste0(here("nwm_operational/medium_term"), "/", 
                          downloaded$init_date_time[1], "_",
                          downloaded$member[1],
                          ".csv"))
  
  ### Remove the giant temp files 
  
  walk(urls_and_files$filename, file.remove)
  

  
}
```

## NWM Downloader w/ cloud-based operations

``` r
### This function dowloads archived NWM forecasts from the Google Bucket
### where they are stored. It utilizes cloud-based operations to extract
### the reaches we are interested in BEFORE downloading the entire NWM file
### to our local machine. In this way we avoid downloading giant files that 
### contain largely extraneous data. This is perhaps the more "correct" way to 
### do this. 
### Note that this is a rewrite of the nwmTools get_timeseries function within 
### Mike Johnson's nwmTools package. We are heavily indebted to the great work
### Mike has done. Thanks Mike!!!!!! (https://github.com/mikejohnson51)
### Also, *******It is NOT FAST*******
### Perhaps it could be rewritten for efficiency, but hey, it works



get_timeseries3 <- function(urls = NULL, 
                            ids = NULL,
                            outfile = NULL,
                            index_id = "feature_id",
                            varname = "streamflow"

                            
) {
  
            #### Get values function #######################################################
            get_values <- function(url, var, ind = NULL) {
                        v = suppressWarnings(values(terra::rast(glue("{url}://{var}"))))
                        
                        if (!is.null(ind)) {
                            v = v[ind]
                        }
                        else {
                            v = v
                        }
                        
                        return(v)
            }
            ################################################################################


### Get lead time from URL 
### And get init_date from URL
            
lead_time <- str_extract(str_extract(urls, "f\\d+"), "\\d+")

init_date <- as_date(str_extract(str_extract(urls, ".\\d+"), "\\d+"))

member <- str_extract(urls, "mem\\d+")

#### Little updater

print(paste("Downloading", init_date, lead_time, member, " "))

### First, set up a URL that turns the netCDF on the Google Bucket
### Into a HD5 file
### And utilizes external pointer (vsicurl) to access file "on the fly"

nwm_url2 <- glue("HDF5:\"/vsicurl/{urls}\"")


### Now get the feature_ids (comids) from the file

all_ids <- get_values(url = nwm_url2, index_id)


### Now find the indexes of our comids (reaches) of interest
### In the file that contains all the comids
### The thinking here is that the index of a given comid in the feature_id "layer"
### Should be the same index of where the streamflow value corresponding to that comid
### Is located
### We need to put in a bunch of safety if-else statements to keep from breaking 
### if the file is not found for whatever reason

 
   if (!is.null(index_id)) {
     
     
     
      all_ids = get_values(url = nwm_url2, index_id)
                
     ### If no COMIDs are entered, return every streamflow ID
     ### So all 2.7 million reaches in the NWM
     ### But if particular COMIDs are desired
     ### Find the index of those COMIDs
     ### in the feature_id "layer"
     
        if (is.null(ids)) {
            ind = NULL
            ids = all_ids
        }
     
        else {
         
                ind <- which(all_ids %in% ids)
            
        }
   }

    else {
      
        ind = NULL
        ids = NULL
        
    } ### Returns null if error in reading files



#### Now let's get streamflow
#### It comes in without decimals (not sure why)

q_values <- get_values(nwm_url2, varname, ind)

q_values <- q_values/100

#### And time 
#### Which we need to multiply by 60
#### to turn into "true" POSICxt time (seconds from 1970-01-01)

nwm_time <- get_values(nwm_url2, "time")[1]*60


#### Now, we have to actually extract the feature_id in the same way as we did
#### for discharge
#### This gets us a vector ordered in the same order as our discharge vector
#### Without this, we will bind things that our in different orders
#### and our final output dataframe will be meaningless 
#### THIS IS EXTREMELY IMPORTANT

comids <- get_values(nwm_url2, index_id, ind)

### Bind the time, COMIDs, and modeled streamflow values into one tibble
### Make into a tibble

streamflow_by_reach <- tibble(comid = comids, 
                              init_date = init_date, 
                              lead_time = lead_time,
                              member = member,
                              predict_dateTime = as_datetime(nwm_time),
                              modeled_q_cms = q_values
                              )



### And write that to file 

write_csv(streamflow_by_reach, outfile,
          append = TRUE,
          col_names = !file.exists(outfile),
          )


#return(streamflow_by_reach)

}
```

## NERFC Converter Functions

These functions translate Northeast River Forecast Center (NERFC)
forecasts files, which are archived as .txt files, into a workable
format (tibble)

``` r
######################## NERFC_CONVERTER1 #################################

#### A function to read in the text files 
nerfc_converter1 <- function(file_strng) {
  
  print(file_strng)
  
  df <- read_fwf(file_strng) %>% rename(stuff = 1)
  
  ### Here we find the string that occurs after a row of ******
  ### This indicates the station name and the start of the information we're interested it
  ### We then discard all other rows (by NAs) that don't include the strings 
  ### FCST, which is the row with the necessary header information (times)
  ### and then rows that include the string .E{number}, which are the rows
  ### with the actual forecasts
  
  df2 <- df %>%
    drop_na() %>%
    mutate(station = ifelse(lag(str_detect(stuff, ":[*]")), 
                            .$stuff, NA))   %>%
    filter(!is.na(station) | str_detect(stuff, "FCST") |
             str_detect(stuff, ".E\\d"))%>%
    fill(station, .direction = "down") %>%
    mutate(station = str_remove_all(station, ":")) %>%
  mutate(stuff = str_squish(stuff)) %>%
  mutate(stuff = str_remove_all(stuff, ":")) %>%
  mutate(stuff = str_remove_all(stuff, "/")) 
  
  return(df2)

  
}

################################################################################
################################################################################
###############################################################################



########################### NERFC_CONVERTER2 ###################################

#### Another function to manipulate the files into the form that we need
#### Currently, the file is just one column with strings in each sell
#### We split by space into the number of columns we need
#### Which is the model member, the date, and the times. 
#### Luckily the formatting is consistent enough that this requires into splitting
#### into six columns, the last four of which are forecasts that have a consistent 
#### time stamp 
#### Then pivot the file to be in long format
#### and extrac the month and day of each forecasted discharge
nerfc_converter2 <- function(df) {
  
    df %>%
      dplyr::filter(str_detect(station, "Cold")) %>%
      separate(stuff, 
               into = c("mem", "date", "time1", "time2", "time3", "time4"), 
               sep = " ") %>%
      dplyr::slice(-1) %>%
      janitor::row_to_names(row_number = 1, remove_row = TRUE) %>%
      rename(date = 2, station = 7) %>%
      dplyr::select(-1) %>%
      pivot_longer(cols = -c(date, station), 
                   names_to = "time", 
                   values_to = "forecast_stage") %>%
      mutate(forecast_stage = replace(forecast_stage, 
                                      forecast_stage == "", NA)) %>%
      mutate_at("forecast_stage", ~as.numeric(.)) %>%
      mutate(month = substr(date, 1, 2)) %>%
      mutate(day = substr(date, 3, 4)) %>%
      #mutate(month_day = paste(month,day,sep = "-")) %>%
      #mutate(month_day_hr = paste(month_day, time)) %>%
      dplyr::select(!date)


  
}

################################################################################
################################################################################
```

## Antecedent Condition Calculator

``` r
#################### FUNCTION TO CALCULATE ANTECEDENT MAXES AND MEANS ##########

antecedent_calculator <- function(datetime,
                              hours_from_gage = NULL, 
                              hours_from_model = NULL,
                              monthly_hours_from_gage = NULL,
                              monthly_hours_from_model = NULL,
                              init_date,
                              tributary,
                              #member = NULL,
                              time_span = NULL,
                              metric = NULL,
                              obs_df = NULL,
                              model_df = NULL,
                              forecast_model = "NWM"
                              ) {

########################## SET UP ##############################################
  
  ### Find the index of the dateTime of interest in the observed data (minus 1)
  datetime_index_obs <- which(obs_df$dateTime == datetime &
                                obs_df$Station == tributary) 
  
  ### Find the index of the datet
  datetime_index_model <- which(model_df$predict_dateTime == datetime & 
                              model_df$tributary == tributary & 
                              #nwm_forecasts_mt_trim$member == member &
                              model_df$init_date == init_date
                              )
  
      if(forecast_model == "NWM") {
    
    hours_from_model_adj <- hours_from_model
    
    monthly_hours_from_model_adj <- monthly_hours_from_model
    
    
  } else if (forecast_model == "NERFC") {
    
    hours_from_model_adj <- hours_from_model*6
    
    monthly_hours_from_model_adj <- monthly_hours_from_model*6
    
    
  }
  
  
  ########## WEEKLY ANTECEDENTS ###################################################
    if (hours_from_gage != 0){
          log_obs_flow <- obs_df %>%
            dplyr::ungroup() %>%
            dplyr::slice(((datetime_index_obs - hours_from_model_adj)
                          - hours_from_gage):(datetime_index_obs - 
                                                hours_from_model_adj - 1)) %>%
            .$log_observed_flow
        
    } else{
        
      log_obs_flow <- NA
      
      } ### End log observed flow ifelse 

    if(hours_from_model != 0 ){
      
          log_modeled_flow <- model_df %>%
            dplyr::filter(tributary == tributary) %>%
            dplyr::ungroup() %>%
            dplyr::slice((datetime_index_model - hours_from_model):(datetime_index_model -
                                                                  1)) %>%
            .$log_modeled_flow
        
      
        }  else {
        
        log_modeled_flow <- NA
      }

    obs_and_modeled_flow <-  c(log_obs_flow, log_modeled_flow) %>%
      na.omit() 
      
      mean_prior_weekly_flow <- mean(obs_and_modeled_flow) %>%
            as_tibble() %>%
            rename(mean_prior_weekly_log_q = 1)
  

  ################################################################################
    
  ############# MONTHLY ANTECEDENTS ##############################################

    if (monthly_hours_from_gage > 0 & monthly_hours_from_model <= 168){

          log_obs_flow_minus3 <- obs_df %>%
            dplyr::slice((datetime_index_obs - 720):(datetime_index_obs -
                                                       168 - 1)) %>%
            .$log_observed_flow

          obs_and_modeled_flow_monthly <- c(obs_and_modeled_flow,
                                            log_obs_flow_minus3) %>%
            na.omit()

    } else if(monthly_hours_from_model > 168) {

      log_modeled_flow_monthly <- model_df %>%
              dplyr::filter(tributary == tributary) %>%
            dplyr::slice((datetime_index_model -
                            monthly_hours_from_model):(datetime_index_model - 1)) %>%
            .$log_modeled_flow

      log_observed_flow_monthly <- obs_df %>%
            dplyr::slice(((datetime_index_obs - monthly_hours_from_model_adj)
                          - monthly_hours_from_gage):(datetime_index_obs -
                                                        monthly_hours_from_model_adj - 1)) %>%
            .$log_observed_flow

      obs_and_modeled_flow_monthly <- c(log_modeled_flow_monthly,
                                        log_observed_flow_monthly) %>%
        na.omit()

    } else {

      obs_and_modeled_flow_monthly <- NA

      }



  ################################################################################
    
    
  ###### 3 MONTHLY ANTECEDENTS ####################################################
    
    if(time_span == "3month" & metric == "max") {

      log_observed_flow_3monthy <- obs_df %>%
            dplyr::slice((datetime_index_obs - (24*30*3)):(datetime_index_obs -
                                                       (24*30) - 1)) %>%
            .$log_observed_flow

    obs_and_modeled_flow_3month <- c(log_observed_flow_3monthy, obs_and_modeled_flow_monthly) %>%
      na.omit()

    # mean_prior_3month_flow <- mean(obs_and_modeled_flow_3month) %>%
    #   as_tibble() %>%
    #   rename(mean_3monthly_ant = 1)

    max_prior_3month_flow <- max(obs_and_modeled_flow_3month) %>%
      as_tibble() %>%
      rename(max_prior_threemonth_log_q = 1)

    return(max_prior_3month_flow)

    } else if (time_span == "week_month" & metric == "mean") {

          mean_prior_monthly_flow <- mean(obs_and_modeled_flow_monthly) %>%
            as_tibble() %>%
            rename(mean_prior_monthly_log_q = 1)

          mean_prior_weekly_flow <- mean(obs_and_modeled_flow) %>%
            as_tibble() %>%
            rename(mean_prior_weekly_log_q = 1)

          mean_prior_weekly_monthly <- bind_cols(mean_prior_monthly_flow,
                                                 mean_prior_weekly_flow)

          return(mean_prior_weekly_monthly)

    } else if(time_span == "week_month" & metric == "max") {

      max_prior_monthly_flow <- max(obs_and_modeled_flow_monthly) %>%
        as_tibble() %>%
        rename(max_prior_monthly_log_q = 1)

      return(max_prior_monthly_flow)

    } 
      else {

      return(NA)

    }

} ### End function antecedent_calculator
```

## Find Peaks in Record

``` r
#### This function essentially finds the peak discharge, turbidity, etc.
#### Over a user-specified length of time

#######################FUNCTION TO CALC. PEAK TIMES#############################
peak_time_getter <- function(datetime,
                              yearly_hours_from_gage = NULL, 
                              yearly_hours_from_model = NULL,
                              init_date = NULL,
                              tributary = NULL,
                              length = NULL,
                              return_value = FALSE,
                              obs_df = NULL,
                              model_df = NULL,
                              forecast_model = "NWM"
                             ) {
  
  
  ################# SET UP #####################################################
  
    ### Find the index of the dateTime of interest in the observed data (minus 1)
  datetime_index_obs <- which(obs_df$dateTime == datetime &
                                obs_df$Station == tributary) 
  
  ### Find the index of the datet
  datetime_index_model <- which(model_df$predict_dateTime == datetime & 
                              model_df$tributary == tributary & 
                              model_df$init_date == init_date
                              )
  

############# EXTRACTS THE PEAKS ###############################################


  max_extractor <- function(hrs_from_gage, hrs_from_model, frcst_mod) {
    
    
        #### Adjust for the fact that NERFC model "timesteps needed" are really 
        #### lumped 6-hrs in the timesteps_needed dataframe
        #### so to combine with the oberved timesteps_needed we need to convert back
        #### to hourly (i.e., one timestep = one row)
        
          if(frcst_mod == "NWM") {
          
          hrs_from_model_adj <- hrs_from_model
          
          
        } else if (frcst_mod == "NERFC") {
          
          hrs_from_model_adj <- hrs_from_model*6
          
          
        }
        
          log_obs_flow_df <- obs_df %>%
                dplyr::ungroup() %>%
                dplyr::slice(((datetime_index_obs - hrs_from_model_adj)
                                  - hrs_from_gage):(datetime_index_obs - 
                                                        hrs_from_model_adj - 1)) %>%
                dplyr::select(dateTime, log_observed_flow)
          
        
            if(yearly_hours_from_model != 0 ){
              
                  log_modeled_flow_df <- model_df %>%
                    dplyr::ungroup() %>%
                    dplyr::slice((datetime_index_model - hrs_from_model):(datetime_index_model -
                                                                          1)) %>%
                    dplyr::select(predict_dateTime, log_modeled_flow)
                  
                    
                }  else {
                
                log_modeled_flow_df <-  tibble(predict_dateTime = NA,
                                               log_modeled_flow = NA)
                }
          
            flow_peak_datetime <-  bind_rows(log_obs_flow_df %>%
                                                    rename(log_flow = log_observed_flow), 
                                                  log_modeled_flow_df %>%
                                                    rename(dateTime = predict_dateTime,
                                                           log_flow = log_modeled_flow)) %>%
              drop_na() %>%
              slice_max(order_by = log_flow) %>%
              arrange(desc(dateTime)) %>%
              dplyr::slice(1) %>% ### Remove duplicates and choose more recent time of occurence
              dplyr::ungroup()
            
            
            return(flow_peak_datetime)
        
        
  }
  
################################################################################
      
  
########## YEARLY ANTECEDENTS ###################################################
  
  
  
    if(length == "annual") {
      
        
       annual_max_datetime <- max_extractor(yearly_hours_from_gage,
                                              yearly_hours_from_model,
                                            forecast_model) %>%
         rename(annual_peak_dateTime = 1) %>%
         dplyr::ungroup()
        
            if(return_value == FALSE) {
              
                  annual_max_datetime <- annual_max_datetime %>%
                    dplyr::ungroup() %>%
                    dplyr::select(annual_peak_dateTime) 
                    
                  return(annual_max_datetime) 
                
            } 
       
       return(annual_max_datetime)
       
       
                } else if (length == "6month") {
                  
                  ### Modify the hours we need
                  ### Doing this here keeps us from having to give the function
                  ### Like twenty-billion arguments
                    six_month_hours_from_gage <- yearly_hours_from_gage - (365*24- (24*30*6))
                    
                    six_month_hours_from_model <- (24*30*6) - six_month_hours_from_gage
                    
                    if(forecast_model == "NERFC") {
                      
                      six_month_hours_from_model <- round(six_month_hours_from_model/6,0)
                      
                    }
                    
                    
                    #### Extract the maximum over the past six months
                    
                    six_month_max_datetime <- max_extractor(six_month_hours_from_gage,
                                                            six_month_hours_from_model,
                                                            forecast_model) %>%
                      rename(six_month_peak_dateTime = 1) %>%
                      dplyr::ungroup()
                      
                    if(return_value == FALSE) {
                            
                      six_month_max_datetime <- six_month_max_datetime %>%
                        dplyr::ungroup() %>%
                                  dplyr::select(annual_peak_dateTime)
                    
                                  
                      return(six_month_max_datetime) 
                                
                          }
       
                    return(six_month_max_datetime)
                    
                } else if (length == "3month") {
                  
                  
                    ### Modify the hours we need
                  
                    three_month_hours_from_gage <- yearly_hours_from_gage - (365*24- (24*30*3))
                    
                    three_month_hours_from_model <- (24*30*3) - three_month_hours_from_gage
                    
                    if(forecast_model == "NERFC") {
                      
                      three_month_hours_from_model <- round(three_month_hours_from_model/6,0)
                      
                    }
                    
                    ### Get values
                    
                    three_month_max_datetime <- max_extractor(three_month_hours_from_gage,
                                                            three_month_hours_from_model,
                                                            forecast_model) %>%
                      rename(three_month_peak_dateTime = 1) %>%
                      dplyr::ungroup()
                      
                    if(return_value == FALSE) {
                            
                      three_month_max_datetime <- three_month_max_datetime %>%
                        dplyr::ungroup() %>%
                                  dplyr::select(annual_peak_dateTime) 
                                  
                      return(three_month_max_datetime) 
                                
                          }
       
                    return(three_month_max_datetime)
                
                  
                }
         
        } 

  ################################################################################
```

## Find time since peak

``` r
#### This function calculates the time from an identified peak in a timeseries
#### (as detected above)
#### to any given time t in a time series 

peak_time_calculator <- function(modeled_times_df = NULL, 
                                 obs_peak_time_df = NULL,
                                 length = NULL) {
  
  obs_peak_time_df <- obs_peak_time_df %>%
    rename(peak_dateTime = 1)
  
  var_name <- paste0("time_since_", length, "_peak")
  
  bind_cols(modeled_times_df, obs_peak_time_df) %>%
    dplyr::select(!contains("hours_needed")) %>%
    mutate(time_since_peak = as.numeric(abs(difftime(predict_dateTime,
                                          peak_dateTime,
                                          units = "hours")))) %>%
    dplyr::select(time_since_peak) %>%
    setNames(var_name)
  
  
    
  
  
}
```

## Calculate delta discharge

``` r
#### This function calculates the change in discharge from time
#### t-1 to time t
#### Effectively, this provides information for which "limb" of the hydrograph
#### A given measurement is taken on. 

limb_getter <- function(model_df){
  
  
  model_df %>%
    mutate(init_dateTime_minus_one = init_dateTime - hours(2)) %>%
    filter(tributary == "ESOPUSCREEKATCOLDBROOKNY") %>%
    group_by(init_date, tributary) %>% #### This is to calculate limb
    mutate(limb_modeled = log_modeled_flow - lag(log_modeled_flow)) %>%
    inner_join(., coldbrook_turbidity_plus_drivers_clean %>%
                 dplyr::select(station, dateTime, log_observed_flow) %>%
                 rename(tributary = station),
               join_by(init_dateTime_minus_one == dateTime,
                       tributary == tributary)) %>%
    mutate(first_limb = log_modeled_flow - log_observed_flow) %>%
    mutate(log_limb = coalesce(limb_modeled, first_limb)) %>%
    dplyr::ungroup() %>%
    dplyr::select(c(predict_dateTime, log_limb, init_date))
    
  
}
```

## Calculate antecedent daily discharge

``` r
#### A function that calculates the daily antecedent discharge across different
#### lead times for NWM and NERFC forecasts

daily_mean_calculator <- function(datetime,
                              hours_from_gage = NULL, 
                              hours_from_model = NULL,
                              init_date,
                              tributary,
                              obs_df = NULL,
                              model_df = NULL,
                              forecast_model = "NWM"
                              ) {
  
  #### Set up #########################################################

  ### Find the index of the dateTime of interest in the observed data (minus 1)
  datetime_index_obs <- which(obs_df$dateTime == datetime &
                                obs_df$Station == tributary) 
  
  ### Find the index of the datet
  datetime_index_model <- which(model_df$predict_dateTime == datetime & 
                              model_df$tributary == tributary & 
                              model_df$init_date == init_date
                              )
  
  if(forecast_model == "NWM") {
    
    hours_from_model_adj <- hours_from_model
    
    
  } else if (forecast_model == "NERFC") {
    
    hours_from_model_adj <- hours_from_model*6
    
    
  }
  
  
  ########## Extract ###################################################
  
  ### Get the timesteps and data from the gage
    if (hours_from_gage != 0){
          log_obs_flow <- obs_df %>%
            ungroup() %>%
            dplyr::slice(((datetime_index_obs - (hours_from_model_adj))
                          - hours_from_gage):(datetime_index_obs - 
                                                (hours_from_model_adj) - 1)) %>%
            .$log_observed_flow
        
    } else{
        
      log_obs_flow <- NA
      
      } ### End log observed flow ifelse 
  
  ####################
  
  ### Get the timesteps and data from the forecast model

    if(hours_from_model != 0 ){
      
          log_modeled_flow <- model_df %>%
            dplyr::filter(tributary == tributary) %>%
            ungroup() %>%
            dplyr::slice((datetime_index_model - hours_from_model):(datetime_index_model -
                                                                  1)) %>%
            .$log_modeled_flow
        
      
        }  else {
        
        log_modeled_flow <- NA
        }
  
  #######################
  
  
  #### Put them all together & calculate the mean
    obs_and_modeled_flow <-  c(log_obs_flow, log_modeled_flow) %>%
      na.omit() 
      
      mean_prior_daily_flow <- mean(obs_and_modeled_flow) %>%
            as_tibble() %>%
            rename(mean_prior_daily_log_q = 1)
      
  #######################    
      
}
```

## Error Estimator

``` r
### This is a function to estimate various error metrics from a dataframe 
### that contains simulated (i.e., modeled) and observed values 

error_estimator <- function(observed_and_modeled_df, type = "forecast",
                            flux = FALSE) {
  
  if(type == "Benchmark" & flux == TRUE) {
    
    observed_and_modeled_df <- observed_and_modeled_df %>%
      dplyr::select(!c(predicted_turbidity, fw_mean_observed_turbidity)) %>%
      rename(forecasted_turbidity = predicted_inst_load,
             fw_mean_observed_turbidity = observed_inst_load)
    
  } else if(type == "Benchmark" & flux == FALSE) {
    
    observed_and_modeled_df <- observed_and_modeled_df %>%
      rename(forecasted_turbidity = predicted_turbidity)
    
  }
  
  observed_and_modeled_df %>%
    mutate(
            raw_error = forecasted_turbidity - fw_mean_observed_turbidity,
             sqrerr = (fw_mean_observed_turbidity - forecasted_turbidity)^2,
             abs_error = abs((fw_mean_observed_turbidity - forecasted_turbidity)),
             raw_pct_error = raw_error/fw_mean_observed_turbidity,
             abs_pct_error = abs_error/fw_mean_observed_turbidity) %>%
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
              r_sq = hydroGOF::R2(forecasted_turbidity,fw_mean_observed_turbidity)
                ) 
  
}
```

## Load Error Calculator

########### Function to calculate event-based error

``` r
#### This is a function that calculates the turbidity "load" error for 
#### all storm events in 2023

load_error_calculator <- function(forecast_df = NULL, 
                                  obs_storm_df = NULL,
                                  forecast_source = NULL,
                                  model = NULL,
                                  type = "Model"){
  
  if(forecast_source == "NERFC") {
    
    multiplier = 6
    
  } else if (forecast_source == "NWM") {
    
    multiplier = 1
    
  }
  
  if(type == "Benchmark") {
    
    
    forecast_df <- forecast_df %>%
      mutate(log_modeled_flow = log10(mean_observed_flow_cms),
             log_observed_flow = log10(mean_observed_flow_cms)) %>%
      mutate(lead_days = 0) %>%
      rename(predict_dateTime = dateTime,
             forecasted_turbidity = predicted_turbidity)
    
    
  } else {forecast_df <- forecast_df}
  
  
  forecast_df %>%
    full_join(., obs_storm_df %>%
               dplyr::select(dateTime, storm),
             join_by(predict_dateTime == dateTime)) %>%
    drop_na(forecasted_turbidity) %>%
    dplyr::select(predict_dateTime, 
                  storm, lead_days,
                  forecasted_turbidity, log_modeled_flow,
                  fw_mean_observed_turbidity, log_observed_flow) %>%
    mutate(forecasted_flow = 10^log_modeled_flow,
           observed_flow = 10^log_observed_flow) %>%
    dplyr::select(!c(log_observed_flow, log_modeled_flow)) %>%
    mutate(modeled_inst_flux = forecasted_flow*forecasted_turbidity*60*60*multiplier,
           observed_inst_load = observed_flow*fw_mean_observed_turbidity*60*60*multiplier) %>%
    group_by(storm, lead_days) %>%
    summarise(observed_load = sum(observed_inst_load),
              forecasted_load = sum(modeled_inst_flux),
              observed_peak = max(fw_mean_observed_turbidity),
              forecasted_peak = max(forecasted_turbidity)) %>%
    dplyr::ungroup() %>%
    mutate(raw_error_load = (forecasted_load - observed_load),
           raw_error_peak = (forecasted_peak - observed_peak),
           sqr_error_load = (raw_error_load^2),
           sqr_error_peak = (raw_error_peak^2),
           abs_err_load = abs(raw_error_load),
           abs_err_peak = abs(raw_error_peak),
           raw_pct_error_load = raw_error_load/observed_load,
          raw_pct_error_peak = raw_error_peak/observed_peak,
          abs_pct_err_load = abs_err_load/observed_load,
           abs_pct_err_peak = abs_err_peak/observed_peak) %>%
    inner_join(., storms_2023 %>%
                 group_by(storm) %>%
                 dplyr::slice(1) %>%
                 dplyr::ungroup() %>%
                 dplyr::select(storm, dateTime),
                 by = "storm") %>%
    inner_join(., storms_2023 %>%
                 group_by(storm) %>%
                 slice_max(mean_observed_flow_cms) %>%
                 dplyr::slice(1) %>%
                 dplyr::ungroup() %>%
                 dplyr::select(storm, mean_observed_flow_cms) %>%
                 rename(peak_storm_q = mean_observed_flow_cms),
               by = "storm") %>%
    mutate(forecast_source = forecast_source,
           model = model,
           type = type)
    
  
}
```
