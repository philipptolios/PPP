### Parametric Portfolio Policies Model
## PMP Academic Team
## Version 2.0

## PPP optimization

## last changes 2024/01/11

################################################################################
#This is the optimization script for the Parametric Portfolio Policy (PPP) model 
#
# This script runs the optimization (the loop only serves to "backtest" the model for the past XYZ months)
# 
#
# 
# Input: this script takes the cleaned and pre-processed data after running the "PPP_2_data_loading.R" script as input
# Output: it returns a list of past optimization weights as .csv file
#         
#
# Note: - 
#
################################################################################







###------------------------- Optimization loop---------------------------------###
vector_months <- sim.data |> 
  ungroup() |>
  select(month) |> 
  distinct(month, .keep_all = TRUE) |>   
  arrange(month) |>
  subset( month>  max(month)-months(timehorizon) & month <= max(month))

coef_save = cbind(vector_months,matrix(NA, timehorizon, n_parameters))       # matrix of coefficients (starting values zero)
colnames(coef_save) <- c("month", names(theta))

coef_save2 = cbind( vector_months,matrix(NA, timehorizon, n_parameters))      # matrix of coefficients  (starting values OLS)
colnames(coef_save2) <- c("month", names(theta))

coefOLS_save = cbind( vector_months,matrix(NA, timehorizon, n_parameters))      # matrix of coefficients from OLS
colnames(coefOLS_save) <- c("month", names(theta))

#weightsfull = vector("list", timehorizon)   # initializes list of lists for all weights (long-short over the whole universe)
#weights = rep(list(matrix(NA, 1, 50, dimnames = list(NULL, rep(NA, 50))), timehorizon))  # empty list of lists of top 50 weights for each iteration


pb <- txtProgressBar(min = 0, max = timehorizon,style = 3, width = 50, char = "=")       # progress-bar (to be displayed during optimization)

#weighted_stocks <- sim.data |> select(stock, month)

# create empty dataframe for stocks + weights after optimization (appended in every iteration by portfolio)
portfolios_long <- data.frame(stock=character(),month=as.Date(character()))


#### identifying global hinsight top 100 stocks (buy and hold)
top100_stocks = sim.data |> 
  group_by(stock) |> 
  mutate(top100=sum(ret_excess)) |> 
  ungroup() |> distinct(stock, .keep_all = TRUE) |>
  mutate(top100_dummy=case_when(top100 %in% rev(sort(top100))[1:100] ~ 1,.default=0))

sim.data <- sim.data |> 
  left_join(top100_stocks[,c("stock","top100_dummy")], 
            by="stock") 

sim.data <- sim.data |> 
  group_by(month) |> 
  mutate(weight_top100_buyhold=case_when(top100_dummy==1 ~ 1/100,.default=0)
  )


## actual optimization loop
for(i in 1:timehorizon){
  if(i==1){
    print("Initializing Optimization...")
  }
  #test <- data_portfolios |> group_by(stock) |> filter(month == max(month))
  # create for loop;
  # slice data_portfolios into optimization windows
  # optimal_theta for optimization window - 1 month
  # compute weights for last month of optimization window via:
  
  # slice data into optimization window size
  # lower bound: max(month)  - time horizon [earliest optimization begins e.g. 10 years ago] 
  #                          - optimization window [with e.g. 5 years of previous past data] 
  #                          + i (shifts starting point every iteration)
  # upper bound: lower bound + optimization window size   
  #data_window   <- sim.data |> 
  #     group_by(stock) |> 
  #     filter(between(month,max(month) - months(timehorizon+opt_window-i),max(month) - months(timehorizon-i)))
  
  # build case_when(rolling_window=TRUE) before "subset" into optimization
  # expanding window: max(month) - months(timehorizon) - months(opt_window)
  # rolling window: max(month) - months(timehorizon) - months(opt_window) + months(i)
  
  ## collect data for optimization window 
  if (rolling_window==TRUE){
    data_window <- sim.data |>   
      group_by(stock) |>
      subset( month > max(month)-months(timehorizon+opt_window-i) & month <= max(month) - months(timehorizon-i))
  } else {
    data_window <- sim.data |>   
      group_by(stock) |>
      subset( month > max(month)-months(timehorizon+opt_window) & month <= max(month) - months(timehorizon-i))
  }
  
  ##### Trim / Truncate Returns
  if(trim.return!=0 & trim.price!=0){
    ret.bounds   <- quantile(data_window$ret_excess, probs=c(trim.return/100/2 , 1-trim.return/100/2), na.rm = TRUE)
    price.bound  <- quantile(data_window$price, probs=c(trim.price/100/2 , 1), na.rm = TRUE)[1]
    
    data_window <- data_window |> 
      filter(between(ret_excess, ret.bounds[1], ret.bounds[2])) |>
      filter(price > price.bound) |>
      drop_na(ret_excess)
  }
  ######
  
  #prepare data window for optimization (standardize )
  data_window <- data_window |>
    group_by(month) |>
    mutate(
      n = n(),
      relative_mktcap_lag = mktcap_lag / sum(mktcap_lag),
      across(contains("lag"), ~ (. - mean(.)) / sd(.)),
    )|>
    ungroup() |>
    select(-mktcap_lag)
  
  
  ### Optimization starting values
  #  OLS coefs as starting values for optimization 
  ### and last optimization results otherwise
  if (starting_value_OLS==TRUE) {
    if (i==1){
      model.ols <- lm(
        as.formula(
          paste("ret_excess ~ ",paste(colnames(data_window)[str_detect(colnames(data_window), "lag")],collapse=" + "))),data_window)
      theta <- model.ols$coefficients[-1]
      coefOLS_save[i,-1] <- t(model.ols$coefficients[-1])
    }else
    {
      theta=coef_save[i-1,-1]
    }
  } else {  # if not OLS as starting values --> vector of zeros
    theta <- rep(0, n_parameters)
    names(theta) <- colnames(data_window)[str_detect(
      colnames(data_window), "lag")]
  }
  
  # optimization routine with zero starting values
  if (particleswarm==TRUE){
    optimal_theta <- psoptim(     # particle swarm optimization [experiment]
      par = theta,
      fn = compute_objective_function,
      objective_measure = "Expected utility",
      data = data_window,
      value_weighting = TRUE,
      allow_short_selling = FALSE,
      lower=-10,upper=10,
      control=list(abstol=1e-2,trace=0,REPORT=0,maxit=30,vectorize=TRUE)
    )
  }else{
    optimal_theta <- optim(
      par = theta,                      # theta0 for 0 as starting value; alternatively "theta" for OLS starting values and last optimization result otherwise
      fn = compute_objective_function,
      objective_measure = "Expected utility",
      data = data_window,
      value_weighting = TRUE,
      allow_short_selling = FALSE,
      method = "Nelder-Mead"
      #method = "SANN"
    )
  }
  
  # store resulting optimization coefficients
  coef = optimal_theta$par
  coef_save[i,-1] = t(coef)
  
  
  # optimization routine with coefs from OLS result as starting values
  # optimal_theta <- optim(
  #   par = theta,
  #   fn = compute_objective_function,
  #   objective_measure = "Expected utility",
  #   data = data_window,
  #   value_weighting = FALSE,
  #   allow_short_selling = FALSE,
  #   method = "Nelder-Mead"
  # )
  # # store resulting optimization coefficients
  # coef = optimal_theta$par
  # coef_save2[i, -1] = t(coef)  
  
  # store coefs and weights in matrix
  # do the rest as before
  
  # compute portfolio weights
  processed_data = compute_portfolio_weights(
    coef, 
    data_window,     
    value_weighting=TRUE,
    allow_short_selling=FALSE
  )
  
  # test <- processed_data |> select(stock, month, weight_tilt) |> rename("iteration_{i}" := weight_tilt) 
  # weighted_stocks <- weighted_stocks |>
  #   left_join(test, by = c("stock", "month")) |> filter(month==max(month))
  
  # store only weights from last month (since we shift the window and compute new weights in every iteration)
  # portfolio that we would buy after optimization
  #current_portfolio2 <- processed_data |> select(stock, month, weight_tilt) |> filter(month==max(month))
  current_portfolio <- processed_data |> 
    select(stock, month, weight_tilt, ret_excess, weight_benchmark, weight_top100_buyhold) |> 
    filter(month==max(month)) |>
    mutate(weight_EW=1/n()
    )
  # bind portfolios from every iteration together in one dataframe in long format
  portfolios_long <- rbind(portfolios_long, current_portfolio) 
  
  setTxtProgressBar(pb, i)          # update progress bar
  if(i==timehorizon){
    print("Optimization complete")
  }
}



################   export portfolio weights  ###############
portfolio_weights <- portfolios_long |> 
  select(stock, month, weight_tilt) |> 
  arrange(month) |>
  pivot_wider(names_from = month, values_from = weight_tilt)

export.weights <- portfolios_long |> 
  select(stock, month, weight_tilt) |> 
  mutate(month = month %m+% months(1)) |>
  arrange(month) |> filter(weight_tilt!=0)

write_ods(export.weights, paste(path.output, "ppp", country, n_parameters, "char_portfolio_weights_pso_10ywin_nopenny.ods", sep="_")) # export .ods file


########################################################################################
################################## END OF SCRIPT #######################################  
########################################################################################