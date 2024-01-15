
weight_constraints <- function(weight_tilt,
                               #portfolio_type = "long_only",
                               n_weights=50,
                               lower_bound=0.01,
                               upper_bound=0.05) {
  
  
  weight_tilt=weight_tilt + runif(n=length(weight_tilt),min=10^-6,max=10^-5)# workaround for exaclty equally weights > n_weights
  weights_constrained = case_when(weight_tilt %in% rev(sort(weight_tilt))[1:n_weights] ~ weight_tilt,.default=0)
  
  # filter nonzero weights and only work on nonzero weights from here on
  weights_constrained_index <- which(weights_constrained!=0)   # defines index of nonzero weights (all to be constrained)
  weights_constrained <- weight_tilt[weights_constrained_index]
  
  constraints_met = all(between(weights_constrained, lower_bound, upper_bound))
  leverage_met = between(sum(abs(weights_constrained)),0.995,1.005)
  #print(constraints_met)
  #print(leverage_met)
  constraint.loopcount=0
  while (constraints_met == FALSE | leverage_met==FALSE){
    
    weights_constrained = ifelse((weights_constrained>0 & weights_constrained<lower_bound), 
                                 lower_bound, 
                                 weights_constrained) # apply the weight constraint twice
    weights_constrained = ifelse(weights_constrained>upper_bound, 
                                 upper_bound, 
                                 weights_constrained)   # weights have to be above 0.1% and below 5%, yielding generally weights between 1-10% (this could be more elegant)
    leverage.loopcount=0
    while (between(sum(abs(weights_constrained)),0.995,1.005)==FALSE){
      #print(sum(abs(weights_constrained)))
      weights_constrained = weights_constrained / sum(weights_constrained)  # normalize such that they add up to 1 again  
      leverage_met = between(sum(abs(weights_constrained)),0.995,1.005)
      leverage.loopcount = leverage.loopcount + 1
      if (leverage.loopcount==250){
        #print("leverage correction not met. Aborted")
        {break}
      }
    }

    weights_constrained = round(weights_constrained,4)
    
    constraints_met = all(between(weights_constrained, lower_bound, upper_bound)) # to do: add tolerance
    constraint.loopcount = constraint.loopcount + 1
    #print(constraint.loopcount)
    #print(leverage.loopcount)
    
    #print(weights_constrained)
    #print(length(weights_constrained))
    #print(sum(weights_constrained))
    if (constraint.loopcount==100){
      #print("weight constraints not met. Aborted")
      {break}
    } else {
      #if (constraints_met==TRUE) {print("weight constraints met")}
    }
  }
  # before return --> merge zero weights from above with optimized weights
  weight_tilt[weights_constrained_index] <- weights_constrained
  
  weight_tilt = case_when(weight_tilt %in% rev(sort(weight_tilt))[1:n_weights] ~ weight_tilt,.default=0)
  
  return(weight_tilt)
}


compute_portfolio_weights <- function(theta,
                                      data,
                                      value_weighting = TRUE,
                                      allow_short_selling = FALSE) {
  
  data |>
    group_by(month) |>
    bind_cols(
      characteristic_tilt = data |>
        transmute(across(contains("lag"), ~ . / n)) |>
        as.matrix() %*% theta |> as.numeric()
    ) |>
    mutate(
      # Definition of benchmark weight
      weight_benchmark = case_when(
        value_weighting == TRUE & allow_short_selling == TRUE ~ relative_mktcap_lag,
        value_weighting == TRUE & allow_short_selling == FALSE ~ (relative_mktcap_lag + abs(min(relative_mktcap_lag)))/sum(abs(relative_mktcap_lag)),
        value_weighting == FALSE ~ 1 / n
      ),
      
      # Parametric portfolio weights
      weight_tilt = weight_benchmark + characteristic_tilt,
      # Short-sell constraint (to do: incorporate into the weight constraints function)
      weight_tilt = case_when(
        allow_short_selling == TRUE ~ weight_tilt,
        allow_short_selling == FALSE ~ pmax(0, weight_tilt)
      ),
      #weight_tilt2 = case_when(weight_tilt %in% rev(sort(weight_tilt))[1:50] ~ weight_tilt, .default=0),   ##### test
      weights_unconstrained = weight_tilt / sum(weight_tilt),
      weight_tilt = weight_constraints(weight_tilt),
      
      # Weights sum up to 1
      #weight_tilt2 = weight_tilt2 / sum(weight_tilt2)
      #################################### addition
      # weight_benchmark = case_when(
      #   allow_short_selling == TRUE &
      #     value_weighting == TRUE &
      #     min(weight_benchmark)<0 ~  weight_benchmark + abs(min(weight_benchmark)),
      #   .default = weight_benchmark
      # ),
      # weight_benchmark = case_when(
      #   allow_short_selling == TRUE &
      #     value_weighting == TRUE ~ weight_benchmark / sum(weight_benchmark))
      # ##############################################
    ) |>
    ungroup()
}



power_utility <- function(r, gamma = 5) {
  (1 + r)^(1 - gamma) / (1 - gamma)
}

evaluate_portfolio <- function(weights_sim.data,
                               capm_evaluation = TRUE,
                               full_evaluation = TRUE,
                               length_year = 12) {
  
  evaluation <- weights_sim.data |>
    group_by(month) |>
    summarize(
      tilt = weighted.mean(ret_excess, weight_tilt),
      benchmark = weighted.mean(ret_excess, weight_benchmark)
    ) |>
    pivot_longer(-month,
                 values_to = "portfolio_return",
                 names_to = "model"
    )
  
  evaluation_stats <- evaluation |>
    group_by(model) |>
    left_join(factors_ff3_monthly, by = "month") |>
    summarize(tibble(
      "Expected utility" = mean(power_utility(portfolio_return)),
      "Average return" = 100 * mean(length_year * portfolio_return),
      "SD return" = 100 * sqrt(length_year) * sd(portfolio_return),
      "Sharpe ratio" = sqrt(length_year) * mean(portfolio_return) / sd(portfolio_return),
      "Sortino ratio" = sqrt(length_year) * mean(portfolio_return) / sqrt(sum((portfolio_return[portfolio_return<0])^2)/length(portfolio_return))
    )) |>
    mutate(model = str_remove(model, "return_"))
  
  if (capm_evaluation) {
    evaluation_capm <- evaluation |>
      left_join(factors_ff3_monthly, by = "month") |>
      group_by(model) |>
      summarize(
        "CAPM alpha" = coefficients(lm(portfolio_return ~ mkt_excess))[1],
        "Market beta" = coefficients(lm(portfolio_return ~ mkt_excess))[2]
      )
    
    evaluation_stats <- evaluation_stats |>
      left_join(evaluation_capm, by = "model")
  }
  
  if (full_evaluation) {
    evaluation_weights <- weights_sim.data |>
      select(month, contains("weight")) |>
      pivot_longer(-month, values_to = "weight", names_to = "model") |>
      group_by(model, month) |>
      mutate(
        "Avg. absolute weight" = abs(weight),
        "Max. weight" = max(weight),
        "Min. weight" = min(weight),
        "Avg. sum of negative weights" = -sum(weight[weight < 0]),
        "Avg. fraction of negative weights" = sum(weight < 0) / n(),
        .keep = "none"
      ) |>
      group_by(model) |>
      summarize(across(-month, ~ 100 * mean(.))) |>
      mutate(model = str_remove(model, "weight_"))
    
    evaluation_stats <- evaluation_stats |>
      left_join(evaluation_weights, by = "model")
  }
  
  evaluation_output <- evaluation_stats |>
    pivot_longer(cols = -model, names_to = "measure") |>
    pivot_wider(names_from = model)
  
  return(evaluation_output)
}



compute_objective_function <- function(theta,
                                       data,
                                       objective_measure = "Expected utility",
                                       value_weighting = TRUE,
                                       allow_short_selling = FALSE) {
  processed_data <- compute_portfolio_weights(
    theta,
    data,
    value_weighting,
    allow_short_selling
  )
  
  objective_function <- evaluate_portfolio(
    processed_data,
    capm_evaluation = FALSE,
    full_evaluation = FALSE
  ) |>
    filter(measure == objective_measure) |>
    pull(tilt)
  
  return(-objective_function)
}



# ### first test(s):
# #
# 
# 
# 
# 
#  

# 
# 
# theta <- rep(0, n_parameters)
# names(theta) <- colnames(data_window)[str_detect(
#   colnames(data_window), "lag")]
# 
# optimal_theta <- psoptim(
#      par = theta,
#      fn = compute_objective_function,
#      objective_measure = "Expected utility",
#      data = data_portfolios,
#      value_weighting = TRUE,
#      allow_short_selling = FALSE,
#      lower=-5,upper=5,
#      control=list(abstol=1e-2,trace=0,REPORT=0,maxit=20,vectorize=TRUE)
# )
#   #   #method = "Nelder-Mead"
#   #   method = "SANN",
#   #   #control = list(maxit = 3000, 
#   #   #               temp = 2000, 
#   #   #              trace = TRUE,
#   #   #               REPORT = 100)
#   # )
# # optimal_theta <- optim(
# #   par = theta,
# #   fn = compute_objective_function,
# #   objective_measure = "Expected utility",
# #   data = data_portfolios,
# #   value_weighting = TRUE,
# #   allow_short_selling = FALSE,
# #   #method = "Nelder-Mead"
# #   method = "SANN",
# #   #control = list(maxit = 3000, 
# #   #               temp = 2000, 
# #   #              trace = TRUE,
# #   #               REPORT = 100)
# # )
# # 
# optimal_theta$par
# 
#  weights_sim.data <- compute_portfolio_weights(
#   optimal_theta$par,
#   data_portfolios,
#   value_weighting = TRUE,
#   allow_short_selling = FALSE
# )
#   leverage_evaluation_overall <- weights_sim.data |>
#    group_by(month) |>
#    mutate(leverage=sum(weight_tilt)-1) |>
#    select (month,leverage) |>
#    distinct()
# 
# sum(leverage_evaluation_overall$leverage)
#   #
# #
# #
# evaluate_portfolio(weights_sim.data) |>
#   print(n = Inf)

########################################################################################
################################## END OF SCRIPT #######################################  
########################################################################################