05_model_development
================
JTK
2024-12-31

################################################################################ 

This script builds three models ultimately used to forecast turbidity in
Esopus Creek, NY.

These three models are a 1) quantile regression model trained on data
from just the Coldbrook station (Quantile-Coldbrook), 2) a LightGBM
(gradient-boosted decision tree) model trained also on data only from
the Coldbrook station (LightGBM-Coldbrook), and 3) a LightGBM model
trained on data from all the major upstream tributary gaging stations
(LightGBM-Network).

In this code we first train the quantile model using the model structure
from Mukundan et al 2018 (<https://doi.org/10.2134/jeq2018.06.0229>). We
then use a backwards variable selection method to identify the most
relevant variables for each LightGBM model. Once those are identified,
we then train those models on 2016-22 data. We also tune the
hyperparameters

In each section, we also calculate model performance on test data (from
2023).

################################################################################ 

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

### Tuning 
require(recipes)
require(parsnip)
require(tune)
require(dials)
require(workflows)
require(yardstick)
require(bonsai)
require(tidymodels)
```

### Load prior scripts

``` r
source(knitr::purl(here("Rmd-files/02_observational_data_download_and_clean.Rmd"), 
                   quiet=TRUE))

source(knitr::purl(here("Rmd-files/03_observational_data_prep.Rmd"), 
                   quiet=TRUE))

source(knitr::purl(here("Rmd-files/04_event_delineation.Rmd"), 
                   quiet=TRUE))
```

# Split data

### Divide data in training, validation, and test data

``` r
#### Training data

cold_train <- coldbrook_turbidity_plus_drivers_clean %>%
  filter(water_year < 2022)

#### Validation data

cold_valid <- coldbrook_turbidity_plus_drivers_clean %>%
  filter(water_year == 2022)

#### Testing data

cold_test <- coldbrook_turbidity_plus_drivers_clean %>%
  filter(water_year == 2023)

#### Training AND validation data

cold_train_entire <- coldbrook_turbidity_plus_drivers_clean %>%
  filter(water_year < 2023)
```

# Quantile Regression Model (Quantile-Coldbrook)

### Build the quantile model

``` r
#### Now is the quantile model, focusing on the median model. This uses equation 2 
#### from Wang et al., 2021
#### and eqn 2. from Mukundan et al 2018 ( https://doi.org/10.2134/jeq2018.06.0229)
#### Calculate models at a 0.01 interval from 0.05 to 0.95. 
#### This is similar to what Mukundan did, but at a larger range because 
#### the hourly data gives us more to work with. 
#### The primary thing we want here is the coefficients of the model

##### Generate the quantiles we want to regress against (taus)
##### We also want to select only the median quantile
##### Because this is the one model that we have chosen to use 

qqm_taus <- tibble(tau = round(seq(0.05, 0.95, 0.01),2)) %>%
  bind_cols(., cold_train_entire %>% 
              dplyr::select(log_observed_turbidity, log_observed_flow, 
                            log_squared_observed_flow)
            %>% nest()) %>%
  filter(tau == 0.5)


##### Bind the observed data to each tau and calculate that model for that tau

qqm_raw <- qqm_taus %>%
  mutate(model = map2(data, tau, ~rq(log_observed_turbidity ~ 1 + 
                                       log_observed_flow + 
                                       log_squared_observed_flow,
         data = .x,
         tau = .y)))

##### Now extract just the coefficients 

qqm_coefficients <- qqm_raw %>%
  mutate(beta0q = map_dbl(model, ~.x$coefficients[[1]]),
         beta1q = map_dbl(model, ~.x$coefficients[[2]]),
         beta2q = map_dbl(model, ~.x$coefficients[[3]])) %>%
  dplyr::select(!c(data, model))

qqm_coeff_nest <- qqm_coefficients %>% 
  nest_by(tau, .key = "coeffs") 


############################
```

### Predict turbidity with training data

``` r
########### This is mostly for illustrative purposes ###########################

#### Predict discharge using the model for each given quantile

qqm_pred <- qqm_raw %>%
  mutate(data = map2(data, model, 
                     ~mutate(.x, 
                             log_predicted_turbidity = .y$coefficients[[1]] + 
                               .y$coefficients[[2]]*log_observed_flow + 
              .y$coefficients[[3]]*log_squared_observed_flow))) %>%
  mutate(data = map(data, ~mutate(.x, predicted_turbidity = 10^log_predicted_turbidity))) %>%
  unnest(data) %>%
  mutate(mean_observed_flow_cms = 10^log_observed_flow)


#### Calculate the smearing factor

D_qqm <- qqm_pred %>%
  mutate(log_resids = 10^(log_predicted_turbidity - log_observed_turbidity)) %>%
  summarise(mean_log_resids = mean(log_resids)) %>%
  .$mean_log_resids
```

### Predict turbidity with test data

``` r
###################### BENCHMARK DATA ##########################################

#### Create a benchmark dataset
#### This essentially uses the observed discharge to say
#### "How good is the model if we knew the discharge perfectly"
#### It essentially is a way to get at model error (for the quantile model)
#### vs error from NWM projections

##### First, predict turbidity
qqm_benchmark_median <- quantile_predictor(cold_test %>%
                                          dplyr::select(station,
                                                        date,
                                                        dateTime,
                                                        log_observed_turbidity, 
                                                        log_observed_flow, 
                                                        log_squared_observed_flow,
                                                        log_observed_turbidity,
                                                        fw_mean_observed_turbidity),
                                          "benchmark") %>%
  rename(model = tau) %>%
  filter(model == 0.5)

##### Then account for the skew of the back transformation from log-scale
##### By using the smearing factor 

qqm_benchmark_median <- qqm_smearer(qqm_benchmark_median,
                                    D_fact = D_qqm) 

################################################################################
```

### Evaluate model performance

``` r
#### Calculate some summary statistics for the predictions made with 
#### observed discharge and the quantile model
#### The summary statistics we want to see are the root mean squared error (rmse)
#### the mean absolute error (MAE)
#### The Nash-Sutcliffe Efficiency (NSE)
#### The Kling-Gupta Efficiency (KGE)
#### And the decomposed KGE
#### Percent Bias (%)
#### And correlation 


qqm_bench_summary_stats <- qqm_benchmark_median %>%
  mutate(raw_error_qqm = predicted_turbidity - fw_mean_observed_turbidity,
         abs_error_qqm = abs(raw_error_qqm),
         sqr_error_qqm = raw_error_qqm^2,
         pct_raw_error_qqm = raw_error_qqm/fw_mean_observed_turbidity,
         pct_abs_error_qqm = abs_error_qqm/fw_mean_observed_turbidity) %>%
  summarise(rmse = sqrt(mean(sqr_error_qqm)),
            mae = mean(abs_error_qqm),
            nse = hydroGOF::NSE(predicted_turbidity, 
                                        fw_mean_observed_turbidity),
            kge = hydroGOF::KGE(predicted_turbidity, 
                                        fw_mean_observed_turbidity),
            mape = mean(pct_abs_error_qqm),
            pbias = hydroGOF::pbias(predicted_turbidity,
                                            fw_mean_observed_turbidity),
            corr = cor(predicted_turbidity, fw_mean_observed_turbidity),
            r_sq = hydroGOF::R2(predicted_turbidity,
                             fw_mean_observed_turbidity),
            decompose_kge(predicted_turbidity,
                          fw_mean_observed_turbidity)) %>%
  mutate(model = "QQM")
```

# LightGBM-Coldbrook

################################################################################ 

Here we are essentially using backwards variable selection to pick the
best model. We run LGBM for each testing/valid split starting with n
variables (121), calculate the mean Gain for each feature, remove the
least important feature, run LGBM with that feature removed, calculated
the mean Gain for each feature, remove the least important feature from
this (n-1), and so on and so forth until we run all potential different
models (with the last model having only one feature). From these we then
select the best model using a combination of NSE, KGE, and pBIAS. This
is our final model, which we will tune in the next steps.
\################################################################################

### Variable selection

``` r
#### First, declare some empty lists so that we can save things
#### Note that these are named to reflect the fact that this model can be fed
#### forecasts from the Northeast River Forecast Center

model_stats_lgbm_nerfc <- list()

nerfc_lgbm_var_imp<- list()

vars_nerfc <- list()

all_model_stats_nerfc <- list()

#### Now, make a data frame that contains all the predictors we want to include
#### In the format that allows for them to all be used in variable selection

nerfc_predictors <- coldbrook_turbidity_plus_drivers_clean %>%
        dplyr::ungroup() %>%
        dplyr::select(!c(
          fw_mean_observed_turbidity, 
          hour, station, site_no,
          date, dateTime, 
                       )) %>%
      dplyr::select(!contains("mean_observed_flow")) %>%
      dplyr::select(!contains("log_squared_observed_flow")) %>%
      dplyr::select(!contains("tunnel"))  %>%
      dplyr::select(!contains(expanded_discharge_tribs$station)) %>%
      mutate(water_year2 = water_year) ### Allows water year to be a predictor
      

#### Now do the actual variable selection

for(i in 1:ncol(nerfc_predictors)) {
  
    run <- paste0("run", i)
              
    cat(crayon::cyan("\nModeling Run", i, "\n")) 
    

    for(j in 2020:2022) {
  
      yr <- paste0("Year", j)
              
    cat(crayon::yellow("\nRunning ", yr, "\n")) 
    
    test_year <- j
    
    train_years <- seq(2017, j-1, 1)
    
    ################## SET UP MODEL ############################################
    
    #### Split training data
    
    nerfc_predictors_train <- nerfc_predictors %>%
        filter(water_year %in% train_years)
 
    #### Now subset the testing data to that same subset
    
    nerfc_predictors_test <- nerfc_predictors %>%
        filter(water_year == test_year) %>%
        dplyr::select(colnames(nerfc_predictors))
    
  
    #### Declare the predictor and response variables 
    
    nerfc_preds <- data.matrix(nerfc_predictors_train %>%
                                dplyr::select(!c(log_observed_turbidity,
                                                 water_year)))
    
    
    nerfc_response <- nerfc_predictors_train$log_observed_turbidity
    
    #### Set up the environment - this is just preparing the dataset API 
    #### to be used by lightgbm. 
    #### This is our training data
    
    nerfc_lgbm_train <- lgb.Dataset(nerfc_preds, 
                                     label = nerfc_response
                                     ) 
    
    
            
    #### Declare the test data
    
    nerfc_lgbm_test <- data.matrix(nerfc_predictors_test %>%
                                          dplyr::select(!c(log_observed_turbidity,
                                                           water_year)))
    
    #### Declare the hyperparameters - these are just the defaults for LightGBM
    #### But I have spelled them out to be most clear
    nerfc_params <- list(objective = "regression",
                            num_leaves = 31L,
                            learning_rate = 0.1,
                            min_data_in_leaf = 20L,
                            num_threads = 10L
                        )
    
    ############################################################################
    
    
    ############################# MAKE MODEL #####################################
    
    
    set.seed(913)
    
    #### Declare the model & train
    nerfc_lgbm_model <- lgb.train(nerfc_params,
                                      data = nerfc_lgbm_train,
                                      verbose = 1L,
                                      nrounds = 100L)
    

    #### Predict with the trained model
    nerfc_turb_predicted <- predict(nerfc_lgbm_model, 
                                   data = nerfc_lgbm_test) %>%
      as_tibble() %>% rename(log_predicted_turbidity = 1)
    
    
    #### Bind to observations and estimate smearing coefficient
    nerfc_predicted_observed_smear <- bind_cols(nerfc_predictors_test,
                                                   nerfc_turb_predicted) %>%
      mutate(exp_log_model_residuals = 10^(log_observed_turbidity - log_predicted_turbidity))
    
    ### Estimate smearing coefficient  
    D_nerfc <- mean(nerfc_predicted_observed_smear$exp_log_model_residuals)
    
    ### Now use the smearing coefficient to convert back to non-log
    nerfc_predicted_observed <- nerfc_predicted_observed_smear %>%
      mutate(predicted_turbidity = (10^log_predicted_turbidity)*D_nerfc) %>%
      mutate(fw_mean_observed_turbidity = 10^log_observed_turbidity) %>%
       mutate(sqrerr = (fw_mean_observed_turbidity - predicted_turbidity)^2,
             abs_error = abs((fw_mean_observed_turbidity - predicted_turbidity)),
             abs_pct_error = abs_error/fw_mean_observed_turbidity)
    
    ###########################################################################
    ############################################################################
     
    ################# EVALUATE MODEL ###########################################
    
     #### Evaluate high turb performance
    stats_high_turb_nerfc <- nerfc_predicted_observed %>%
      ungroup() %>%
      filter(fw_mean_observed_turbidity >= 10) %>%
      summarise(high_turb_rmse = sqrt(mean(sqrerr)),
                high_turb_mae = mean(abs_error),
                high_turb_mape = mean(abs_pct_error),
                high_turb_pbias = hydroGOF::pbias(predicted_turbidity,
                                        fw_mean_observed_turbidity)
                ) 

    
    
    #### Evaluate
    model_stats_lgbm_nerfc[[j]] <- nerfc_predicted_observed %>%
      ungroup() %>%
      summarise(rmse_inst_conc = sqrt(mean(sqrerr)),
                mae_inst_conc = mean(abs_error),
                NSE = hydroGOF::NSE(predicted_turbidity, 
                                    fw_mean_observed_turbidity),
                logNSE = hydroGOF::NSE(log_predicted_turbidity, 
                                    log_observed_turbidity),
                KGE = hydroGOF::KGE(predicted_turbidity, 
                                    fw_mean_observed_turbidity),
                mape = mean(abs_pct_error),
                pbias = hydroGOF::pbias(predicted_turbidity,
                                        fw_mean_observed_turbidity),
                corr = cor(predicted_turbidity, fw_mean_observed_turbidity)
                ) %>%
      mutate(r_sq = corr^2) %>%
      bind_cols(., stats_high_turb_nerfc) %>%
      mutate(test_year = j)
    
    
    nerfc_lgbm_var_imp[[j]] <- lgb.importance(nerfc_lgbm_model , 
                                                     percentage = TRUE)
    
    }
    
    all_nerfc_lgbm_var_imp <- bind_rows(nerfc_lgbm_var_imp) 
    
   all_model_stats_nerfc[[i]] <- bind_rows(model_stats_lgbm_nerfc) %>%
      mutate(model = i)
    
    summary_var_imp <- all_nerfc_lgbm_var_imp %>%
      group_by(Feature) %>%
      summarise(mean_Gain = mean(Gain)) %>%
      ungroup()
    
    
    one_removed_predictors <- summary_var_imp %>%
      ungroup() %>%
      arrange(desc(mean_Gain)) %>%
      dplyr::slice(-nrow(.))
    
    vars_nerfc[[i]] <- summary_var_imp %>%
      ungroup() %>%
      mutate(model = i)
    
    ### See how many we have left
    var_count <- length(one_removed_predictors$Feature)
    
    if(var_count == 0) break 
    
    
      ### Update variable list
    nerfc_predictors <- nerfc_predictors %>%
      dplyr::select(one_removed_predictors$Feature,
                    log_observed_turbidity,
                    water_year)
    

}

nerfc_all_all_model_stats <- bind_rows(all_model_stats_nerfc)

nerfc_all_var_imp <- bind_rows(vars_nerfc)

#### Now examine the statistics and pick the model we want
# examine the model summary statistics

nerfc_summary_model_stats <- nerfc_all_all_model_stats %>%
  group_by(model) %>%
  rename(
            kge = (KGE),
            nse = (NSE),
            mae = (mae_inst_conc),
            rmse = (rmse_inst_conc),
            mape = mape,
            lognse = logNSE,
            ) %>%
  ungroup()


nerfc_collapsed_models <- nerfc_all_var_imp %>%
  group_by(model) %>%
  arrange(Feature, .by_group = TRUE) %>%
  summarise(all_vars = paste(Feature, collapse = ",")) %>%
  full_join(., nerfc_summary_model_stats, 
            by = "model")

nerfc_collapsed_models_w_stats <- nerfc_collapsed_models %>%
  group_by(model, all_vars) %>%
  summarise(#n = n(),
            mean_kge = mean(kge),
            mean_nse = mean(nse),
            mean_lognse = mean(lognse),
            mean_mae = mean(mae),
            mean_rmse = mean(rmse),
            mean_pbias = mean(pbias),
            mean_high_turb_pbias = mean(high_turb_pbias),
            mean_corr = mean(corr),
            mean_r_sq = mean(r_sq),
            sd_kge = sd(kge),
            sd_nse = sd(nse),
            sd_mae = sd(mae),
            sd_rmse = sd(rmse),
            sd_pbias = sd(pbias),
            ) %>%
  mutate(lognse_nse = mean_nse + mean_lognse) %>%
  mutate(nse_lognse_pbias = lognse_nse + mean_pbias) %>%
  mutate(kge_nse = mean_kge+mean_nse)

#### Pick the model
### Choose
nerfc_chosen_model <- models_we_like_nerfc %>%
  dplyr::filter(model == 8)
```

### Hyperparameter tuning

``` r
####################### TUNE ###################################################

#### Tune the hyper paramaters of our chosen model 
scores_nerfc <- lgbm_tuner(coldbrook_turbidity_plus_drivers_clean, nerfc_chosen_model)

################################################################################


################################################################################
###################### CHOSE BEST HYPERPARAMS ##################################

### Select the best parameters 
nerfc_best_params <- scores_nerfc %>%
  arrange(mean) %>%
  dplyr::slice(1)

nerfc_best_params

################################################################################
```

### Run & eval tuned model on test data

``` r
#### Run it and save the model
tuned_lgbm_nerfc <- lgbm_runner(cold_train_entire, 
                cold_test, 
                nerfc_chosen_model,
                save = TRUE,
                save_file = "with_nerfc",
                tuned = TRUE,
                tuned_params = nerfc_best_params)

################### EVALUATE ###################################################

#### Calculate summary stats 

summary_stats_final_nerfc <- tuned_lgbm_nerfc[[1]] %>%
  dplyr::select(!c(predicted_turb_load,
                observed_turb_load
                )) %>%
  ungroup() %>%
  mutate(model = "LGBM-Coldbrook")

### And get variable importance

varmp_imp_final_nerfc <- tuned_lgbm_nerfc[[2]] %>% as_tibble()


#### And get the predicted final time series

nerfc_pred_obs_ts <- tuned_lgbm_nerfc[[3]] %>%
  mutate(raw_error = predicted_turbidity - fw_mean_observed_turbidity)  %>%
  dplyr::select(dateTime, 
                fw_mean_observed_turbidity, predicted_turbidity, log_observed_flow,
                abs_pct_error, raw_error) %>%
  rename(raw_error_bench = raw_error,
         abs_pct_error_bench = abs_pct_error)

#### And the smearing factor

d_nerfc <- tuned_lgbm_nerfc[[4]]


### Decomposed kge

nerfc_kge_decomp_benchmark <- decompose_kge(nerfc_pred_obs_ts$predicted_turbidity, 
                                         nerfc_pred_obs_ts$fw_mean_observed_turbidity)

#### Bind all together

summary_stats_final_nerfc <- bind_cols(summary_stats_final_nerfc, nerfc_kge_decomp_benchmark)
```

\#LightGBM-Network

################################################################################ 

Here, we are developing, training, and testing a LightGBM model that is
capable of ingesting data from various upstream stations to make
turbidity predictions for the Coldbrook station. The inclusion of
upstream data is the primary difference from the LightGBM-Coldbrook
model; the various variables considered are otherwise the same (i.e.,
they differ only in *where* they are calculated). For example, the
Network model could potentially consider as maxmimum prior monthly
discharge at the Stony Clove station as a predictive feature for
turbidity at Coldbrook.

We start by a backwards variable selection process logically identical
to that above. We then pick which model we like best, train it, tune it,
and evaluate it.
\################################################################################

### Variable Selection

``` r
#### First, declare some empty lists so that we can save things

model_stats_lgbm <- list()

esopus_cold_lgbm_var_imp <- list()

vars <- list()

all_model_stats <- list()


#### Now, make a data frame that contains all the predictors we want to include
#### In the format that allows for them to all be used in variable selection

esopus_cold_predictors <- coldbrook_turbidity_plus_drivers_clean %>%
        dplyr::select(!c(
          fw_mean_observed_turbidity, 
          hour, station, site_no,
          date, dateTime, 
                       )) %>%
      dplyr::select(!contains("mean_observed_flow")) %>%
      dplyr::select(!contains("log_squared_observed_flow")) %>%
      dplyr::select(!contains("tunnel"))  %>%
      mutate(water_year2 = water_year) ### Allows water year to be a predictor
    

### Run a loop of different years and different models to pick our best model

for(i in 1:ncol(esopus_cold_predictors)) {
  
    #### Print some status variables to the console to track progress
  
    run <- paste0("run", i)
              
    cat(crayon::cyan("\nModeling Run", i, "\n")) 
    
    #### Loop over different testing years
          
    for(j in 2020:2022) {
  
          yr <- paste("Year", j)
                    
          cat(crayon::yellow("\nRunning", yr, "\n")) 
          
          test_year <- j
          
          train_years <- seq(2017, j-1, 1)
          
          #### Training data
          
          esopus_cold_predictor_dataset <- esopus_cold_predictors %>%
              filter(water_year %in% train_years)
       
          #### Now subset the testing data to that same subset
          
          esopus_cold_predictor_dataset_test <- esopus_cold_predictors %>%
              filter(water_year == test_year) %>%
              dplyr::select(colnames(esopus_cold_predictor_dataset))
            
          ######################################################################
          
          #### Declare the predictor and response variables 
          
          cold_preds <- data.matrix(esopus_cold_predictor_dataset %>%
                                      dplyr::select(!c(log_observed_turbidity,
                                                       water_year)))
          
          
          cold_response <- esopus_cold_predictor_dataset$log_observed_turbidity
          
          #### Set up the environment - 
          #### this is just preparing the dataset API to be used by lightgbm. 
          #### This is our training data
          
          esopus_cold_train_lgbm <- lgb.Dataset(cold_preds, 
                                           label = cold_response
                                           ) 
          
          #### Declare the test data
          
          esopus_cold_test_lgbm <- data.matrix(esopus_cold_predictor_dataset_test %>%
                                                dplyr::select(!c(log_observed_turbidity,
                                                                 water_year)))
          
          
          #### Declare the hyperparameters
          #### Declare the hyperparameters - these are just the defaults for LightGBM
          #### But I have spelled them out to be most clear
          
          cold_params <- list(objective = "regression",
                                  num_leaves = 31L,
                                  learning_rate = 0.1,
                                  min_data_in_leaf = 20L,
                                  num_threads = 10L
                              )
          
          
          
          
          #### Train the model
          
          set.seed(913)
          
          cold_turb_model_lgbm <- lgb.train(cold_params,
                                            data = esopus_cold_train_lgbm,
                                            verbose = 1L,
                                            nrounds = 100L)
          
          #### Predict with the model
          
          cold_turb_predicted <- predict(cold_turb_model_lgbm, 
                                         data = esopus_cold_test_lgbm) %>%
            as_tibble() %>% rename(log_predicted_turbidity = 1)
          
          
          #### Bind to observations and estimate smearing coefficient
          
          cold_turb_predicted_observed_smear <- bind_cols(coldbrook_turbidity_plus_drivers_clean %>% 
                                                            drop_na() %>%
                                                            filter(water_year == test_year),
                                                         cold_turb_predicted) %>%
            mutate(exp_log_model_residuals = 10^(log_observed_turbidity - log_predicted_turbidity))
          
          #### Estimate smearing coefficient  
          
          D <- mean(cold_turb_predicted_observed_smear$exp_log_model_residuals)
          
          ####  Now use the smearing coefficient to convert back to non-log
          
          cold_turb_predicted_observed <- cold_turb_predicted_observed_smear %>%
            mutate(predicted_turbidity = (10^log_predicted_turbidity)*D) %>%
             mutate(sqrerr = (fw_mean_observed_turbidity - predicted_turbidity)^2,
                   predicted_turb_flux = mean_observed_flow_cms*predicted_turbidity,
                   observed_turb_flux = mean_observed_flow_cms*fw_mean_observed_turbidity,
                   predicted_turb_inst_load = predicted_turb_flux*60*60,
                   observed_turb_inst_load = observed_turb_flux*60*60,
                   abs_error = abs((fw_mean_observed_turbidity - predicted_turbidity)),
                   abs_pct_error = abs_error/fw_mean_observed_turbidity)
          
          #### Evaluate performance on high turbidity 
          #### Which here we are defining as anything over 10 FNU
          
          stats_high_turb <- cold_turb_predicted_observed %>%
            ungroup() %>%
            filter(fw_mean_observed_turbidity >= 10) %>%
            summarise(high_turb_rmse = sqrt(mean(sqrerr)),
                      high_turb_mae = mean(abs_error),
                      high_turb_mape = mean(abs_pct_error),
                      high_turb_pbias = hydroGOF::pbias(predicted_turbidity,
                                              fw_mean_observed_turbidity)
                      ) 
      
          
          
          #### Evaluate overall performance
          
          model_stats_lgbm[[j]] <- cold_turb_predicted_observed %>%
            ungroup() %>%
            summarise(rmse_inst_conc = sqrt(mean(sqrerr)),
                      mae_inst_conc = mean(abs_error),
                      predicted_turb_load = sum(predicted_turb_inst_load),
                      observed_turb_load = sum(observed_turb_inst_load),
                      NSE = hydroGOF::NSE(predicted_turbidity, 
                                          fw_mean_observed_turbidity),
                      logNSE = hydroGOF::NSE(log_predicted_turbidity, 
                                          log_observed_turbidity),
                      KGE = hydroGOF::KGE(predicted_turbidity, 
                                          fw_mean_observed_turbidity),
                      mape = mean(abs_pct_error),
                      pbias = hydroGOF::pbias(predicted_turbidity,
                                              fw_mean_observed_turbidity)
                      ) %>%
            mutate(err_load = (observed_turb_load - predicted_turb_load)
                   ) %>%
            bind_cols(., stats_high_turb) %>%
            mutate(test_year = j)
          
          #### Calculate variable importance
          
          esopus_cold_lgbm_var_imp[[j]] <- lgb.importance(cold_turb_model_lgbm , 
                                                           percentage = TRUE)
          
    
    }
    
    #### Bind results together
    
    all_esopus_cold_lgbm_var_imp <- bind_rows(esopus_cold_lgbm_var_imp) 
    
    all_model_stats[[i]] <- bind_rows(model_stats_lgbm) %>%
      mutate(model = i)
    
    summary_var_imp <- all_esopus_cold_lgbm_var_imp %>%
      group_by(Feature) %>%
      summarise(mean_Gain = mean(Gain)) %>%
      ungroup()
    
    #### Remove the predictor feature with the lowest variable importance
    
    one_removed_predictors <- summary_var_imp %>%
      ungroup() %>%
      arrange(desc(mean_Gain)) %>%
      dplyr::slice(-nrow(.))
    
    vars[[i]] <- summary_var_imp %>%
      ungroup() %>%
      mutate(model = i)
    
    #### See how many we have left
    
    var_count <- length(one_removed_predictors$Feature)
    
    if(var_count == 1) break 
    
    
    #### Update variable list
    
    esopus_cold_predictors <- esopus_cold_predictors %>%
      dplyr::select(one_removed_predictors$Feature,
                    log_observed_turbidity,
                    water_year)
    

}

#### Bind all model run results together 

all_all_model_stats <- bind_rows(all_model_stats)

all_var_imp <- bind_rows(vars)


#### examine each model summary statistics

summary_model_stats <- all_all_model_stats %>%
  group_by(model) %>%
  rename(
            kge = (KGE),
            nse = (NSE),
            mae = (mae_inst_conc),
            rmse = (rmse_inst_conc),
            mape = mape,
            lognse = logNSE,
            ) %>%
  dplyr::select(!c(predicted_turb_load,
                observed_turb_load
                )) %>%
  ungroup()

#### Pair those summary statistics with a list of variables included
#### in each model

collapsed_models <- all_var_imp %>%
  group_by(model) %>%
  arrange(Feature, .by_group = TRUE) %>%
  summarise(all_vars = paste(Feature, collapse = ",")) %>%
  full_join(., summary_model_stats, 
            by = "model")

#### Calculate the overall performance all training/validation splits

collapsed_models_w_stats <- collapsed_models %>%
  group_by(model, all_vars) %>%
  summarise(
            mean_kge = mean(kge),
            mean_nse = mean(nse),
            mean_lognse = mean(lognse),
            mean_mae = mean(mae),
            mean_rmse = mean(rmse),
            mean_pbias = mean(pbias),
            mean_high_turb_pbias = mean(high_turb_pbias),
            mean_err_load = mean(err_load),
            sd_kge = sd(kge),
            sd_nse = sd(nse),
            sd_mae = sd(mae),
            sd_rmse = sd(rmse),
            sd_pbias = sd(pbias),
            sd_err_load = sd(err_load)) %>%
  mutate(lognse_nse = mean_nse + mean_lognse) %>%
  mutate(nse_lognse_pbias = lognse_nse + mean_pbias) %>%
  mutate(kge_nse = mean_kge + mean_nse)

#### Choose a subset of the best performing models 
#### And examine their performance statistics

models_we_like <- all_var_imp %>%
  dplyr::filter(model %in% c(130:137))

models_we_like_stats <- collapsed_models_w_stats %>%
  dplyr::filter(model %in% models_we_like$model)

################################################################################

################## EVALUATE VISUALLY ###########################################

collapsed_models %>%
  filter(model %in% models_we_like$model) %>%
  pivot_longer(cols = rmse:err_load, names_to = "metric", values_to = "score") %>%
  ggplot() +
    geom_boxplot(aes(x = model, y = score, color= metric, group = model)) +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = seq(0,142, 1)) + 
    theme_few() +
    theme(panel.grid.major.y = element_line(color = "gray90"),
          legend.position = "bottom") +
    facet_wrap(~metric, scales = "free_y",
               nrow = 8)

################################################################################

################################################################################

#### Pick the model we like best after variable selection above

picked_vars <- models_we_like %>%
      filter(model == "134")
```

### Hyperparameter tuning

``` r
#### Tune the hyperparameters of our chosen model
#### Tuned hyperparameters were those controlling the complexity of the decision trees 
#### (num_leaves and min_data_in_leaf). 
#### Users can edit the function *lgbm_tuner* to tune other hyperparams

################################################################################
#################### TUNE THE MODEL ############################################

scores <- lgbm_tuner(coldbrook_turbidity_plus_drivers_clean, picked_vars)

################################################################################

################################################################################
###################### CHOSE BEST HYPERPARAMS ##################################

#### Select the best parameters 

best_params <- scores %>%
  arrange(mean) %>%
  dplyr::slice(1)

################################################################################
```

### Run and evaluate final tuned model on test data

``` r
#### Run the final model with the tuned parameters 

tuned_lgbm_nwm <- lgbm_runner(cold_train_entire, 
                cold_test, 
                picked_vars,
                save = TRUE,
                save_file = "with_nwm",
                tuned = TRUE,
                tuned_params = best_params)

################### EVALUATE ###################################################

#### Calculate summary stats 

summary_stats_final_model <- tuned_lgbm_nwm[[1]] %>%
  dplyr::select(!c(predicted_turb_load,
                observed_turb_load
                )) %>%
  ungroup() %>%
  mutate(model = "LGBM-Network")

#### And variable importance

varmp_imp_final_nwm <- tuned_lgbm_nwm[[2]] %>% as_tibble()

#### And get the predicted final time series

nwm_pred_obs_ts <- tuned_lgbm_nwm[[3]] %>%
  mutate(raw_error = predicted_turbidity - fw_mean_observed_turbidity) %>%
  dplyr::select(dateTime, fw_mean_observed_turbidity, predicted_turbidity, 
                log_predicted_turbidity, log_observed_flow)

#### And the smearing factor 

d_lgbm_network <- tuned_lgbm_nwm[[4]]

#### Decomposed KGE

kge_decomp_benchmark_network <- decompose_kge(nwm_pred_obs_ts$predicted_turbidity, 
                                              nwm_pred_obs_ts$fw_mean_observed_turbidity)

#### Bind all together

summary_stats_final_model <- bind_cols(summary_stats_final_model, kge_decomp_benchmark_network)
```
