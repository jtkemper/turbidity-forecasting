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
