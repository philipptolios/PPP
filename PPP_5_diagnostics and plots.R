### Diagnostics

#################### compute returns for different portfolios   #################### 
# match stocks with computed weights with returns of following period
# ( computing portfolio returns needs weights * next_period returns)

portfolio_weights_lagged <- portfolios_long |> arrange(stock, month) |>
  mutate(month = month %m+% months(1)) |>
  select(-ret_excess) |>
  left_join(select(sim.data, month, stock, ret_excess), by = c("month", "stock")) |>
  group_by(month) |> 
  mutate(weight_top100_mom=case_when(ret_excess %in% rev(sort(ret_excess))[1:100] ~ 1/100,.default=0,)) |>
  ungroup() |>
  drop_na(ret_excess)



# summary statistics
evaluate_portfolio(portfolio_weights_lagged) |>
  print(n = Inf)



######################### Diagnostics ####################################
### backtesting ###

# compute returns for different portfolios
portfolio_weights_lagged = portfolio_weights_lagged |> 
  group_by(month) |> 
  mutate(VW_return        = ret_excess * weight_benchmark,
         EW_return        = ret_excess / length(unique(stock)), 
         top100_bh_return = ret_excess * weight_top100_buyhold,
         PPPlong50_return = ret_excess * weight_tilt,
         top100_mom_ret   = ret_excess * weight_top100_mom
  )

##################

diagnostics_mom <- portfolio_weights_lagged |> 
  group_by(month) |>
  summarize(ret_EW=sum(EW_return)*100,
            ret_VW=sum(VW_return)*100,
            ret_PPPlong50=sum(PPPlong50_return)*100,
            ret_top100_bh=sum(top100_bh_return)*100,
            ret_top100_mom=sum(top100_mom_ret)*100,
            excess_ret_PPP_vs_VW=ret_PPPlong50-ret_VW,
            excess_ret_PPP_vs_EW=ret_PPPlong50-ret_EW,
            excess_ret_PPP_vs_top100_bh=ret_PPPlong50-ret_top100_bh,
            excess_ret_PPP_vs_top100_mom=ret_PPPlong50-ret_top100_mom
  ) |>
  mutate(acc_EW=cumsum(ret_EW),
         acc_VW=cumsum(ret_VW),
         acc_top100=cumsum(ret_top100_bh),
         acc_PPPlong50=cumsum(ret_PPPlong50),
         acc_top100_mom=cumsum(ret_top100_mom)
  ) 

diagnostics_sum <- diagnostics_mom |> 
  summarize(acc_EW=sum(acc_EW),
            acc_VW=sum(acc_VW),
            acc_PPPlong50=sum(acc_PPPlong50),
            acc_top100=sum(acc_top100),
            acc_top100_mom=sum(acc_top100_mom),
            outperf_PPP_vs_VW=sum(ret_PPPlong50>ret_VW)/length(month),
            outperf_PPP_vs_EW=sum(ret_PPPlong50>ret_EW)/length(month),
            outperf_PPP_vs_top100_bh=sum(ret_PPPlong50> ret_top100_bh)/length(month),
            outperf_PPP_vs_top100_mom=sum(ret_PPPlong50>ret_top100_mom)/length(month),
            excess_ret_PPP_vs_VW=acc_PPPlong50-acc_VW,
            excess_ret_PPP_vs_EW=acc_PPPlong50-acc_EW,
            excess_ret_PPP_vs_top100=acc_PPPlong50-acc_top100,
            excess_ret_PPP_vs_top100_mom=acc_PPPlong50-acc_top100_mom
  )


###additional diagnostics
############################# Defining objects for diagnostics
#create evaluation matrix
eval_parameters <- c("Average return",
                     "SD return ",
                     "Sharpe ratio",
                     "Sortino ratio",
                     "CAPM alpha",
                     "Market beta",
                     "Avg. sum negative weights"
)
eval_models <- c("VW", 
                 "PPPlong50", 
                 "top100_buyhold", 
                 "top100_mom",
                 "EW")


evaluation = cbind( vector_months, matrix(NA, timehorizon, length(eval_parameters)*length(eval_models)))
colnames(evaluation) <- c("month", outer(eval_parameters,eval_models, paste))

model <- split(colnames(evaluation)[-1], cut(seq_along(colnames(evaluation)[-1]), length(eval_models), labels=FALSE))


##  diagnostics loop
for(i in 1:timehorizon){
  if(i==1){
    print("Initializing Diagnostics...")
  }
  #colnames(evaluation)[-1]  
  #seq(2,dim(evaluation)[2])
  #,length(eval_models))
  # create expanding window of different portfolios
  current_portfolio <- portfolio_weights_lagged |> arrange(month) |>
    subset( month >=  min(month)+months(pmax(0,i-12)) & month <= min(month) + months(i-1)) 
  
  # evaluates VW
  evaluation[i,model[[1]]] <- current_portfolio |> 
    evaluate_portfolio() |> 
    select("benchmark") |>     
    slice(c(2:7,11))  |> 
    t()
  
  # evaluates PPPlong50
  evaluation[i,model[[2]]] <- current_portfolio |>
    evaluate_portfolio() |> 
    select("tilt") |>     
    slice(c(2:7,11))  |> 
    t()
  
  # evaluates top100 buy and hold
  evaluation[i,model[[3]]] <- current_portfolio |>  
    select(-weight_tilt) |> 
    rename(weight_tilt = weight_top100_buyhold) |> 
    evaluate_portfolio() |> 
    select("tilt") |>     
    slice(c(2:7,11))  |> 
    t()
  
  # evaluates top100 mom
  evaluation[i,model[[4]]] <- current_portfolio |>  
    select(-weight_tilt) |> 
    rename(weight_tilt = weight_top100_mom) |> 
    evaluate_portfolio() |> 
    select("tilt") |>     
    slice(c(2:7,11))  |> 
    t()
  
  # evaluates EW
  evaluation[i,model[[5]]] <- current_portfolio |>  
    select(-weight_tilt) |> 
    rename(weight_tilt = weight_EW) |>
    evaluate_portfolio() |> 
    select("tilt") |>     
    slice(c(2:7,11))  |> 
    t() 
  
  
  setTxtProgressBar(pb, i)          # update progress bar
  
  if(i==timehorizon){
    print("Diagnostics complete")
  }
}

diagnostics_mom <- diagnostics_mom |>
  left_join(evaluation, by="month")
######################## plots #######################
# test <- portfolios_long |> 
#   pivot_longer(cols=-month, names_to = "portfolio", values_to="return") ### here delete "acc_"; rename portfolios
#   group_by(month) |>
#   arrange(month) |> 
#   filter(weight_tilt!=0) |>
#   select(stock, month, PPPlong50_return) 
# 

df <- diagnostics_mom |>
  select(month, starts_with("ret_")) |> 
  rename_with(~trimws(sub('ret_?', '', .), whitespace = '_')) |>
  pivot_longer(cols=-month, names_to = "portfolio", values_to="return") ### here delete "acc_"; rename portfolios
# saving means to plot as lines
dfmean <- df |> group_by(portfolio) |> summarise(meanreturns=mean(return))

## histogram of monthly returns 
ggplot(df, aes(x = return, fill = portfolio)) + 
  facet_wrap(~portfolio, scales = "free_y", ncol = 1) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 0.5) + 
  scale_x_continuous(breaks = seq(-20, 50, 5)) +
  geom_vline(xintercept=0, color="darkgrey", linewidth=0.5) + 
  geom_vline(data= dfmean, aes(xintercept=meanreturns), color="red", size=0.5) + 
  geom_density(kernel = "epanechnikov", adjust=1/2) + labs(title="Returns (MoM, %) of different Portfolios") +
  guides(fill = guide_legend(title = "Portfolios")) #+ 
#scale_fill_hue(labels = eval_models)


## histogram of monthly excess returns 
df <- diagnostics_mom |>
  select(month, starts_with("excess_ret_")) |> 
  rename_with(~trimws(sub('excess_ret_?', '', .), whitespace = '_')) |>
  pivot_longer(cols=-month, names_to = "portfolio", values_to="return") ### here delete "acc_"; rename portfolios
# saving means to plot as lines
dfmean <- df |> group_by(portfolio) |> summarise(meanreturns=mean(return))

## histogram of monthly excess returns 
ggplot(df, aes(x = return, fill = portfolio)) + 
  facet_wrap(~ portfolio, scales = "free_y", ncol = 1) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 0.25) + 
  geom_vline(xintercept=0, color="darkgrey", linewidth=0.5) + 
  geom_vline(data= dfmean, aes(xintercept=meanreturns), color="red", size=0.5) + 
  geom_density() + labs(title="Excess returns (MoM, %) of different Portfolios") +
  guides(fill = guide_legend(title = "Portfolios"))



## plot of accumulated returns (work in progress --> rename portfolios)
df <- diagnostics_mom |>
  select(month, starts_with("acc_"))|> 
  select(-acc_top100_mom) |>
  rename_with(~trimws(sub('acc_?', '', .), whitespace = '_')) |>
  pivot_longer(cols=-month, names_to = "portfolio", values_to="return") ### here delete "acc_"; rename portfolios


ggplot(df, aes(x = month, y = return, group = portfolio)) + geom_line(aes(col = portfolio)) +
  scale_color_discrete(name="Portfolio") + 
  theme(axis.text.x = element_text(color="#000000", size=8, angle=90)) +
  scale_y_continuous(breaks = seq(0,200,10)) +
  scale_x_date(breaks="3 months", date_labels = "%m / %Y") +
  labs(title="Accumulated Excess Rate of Returns", x = "Dates", y = "accumulated excess returns (in %)")



#### average returns
df <- diagnostics_mom |>
  select(month, starts_with("Average return"))|> 
  select(-"Average return top100_mom") |>
  pivot_longer(cols=-month, names_to = "portfolio", values_to="Average return") ### here delete "acc_"; rename portfolios


ggplot(df, aes(x = month, y = `Average return`, group = portfolio)) + geom_line(aes(col = portfolio)) +
  scale_color_discrete(name="Portfolio") + # labels at position 1: "PPP long-short whole univ.", 
  theme(axis.text.x = element_text(color="#000000", size=8, angle=90)) +
  scale_x_date(breaks="3 months", date_labels = "%m / %Y") +
  labs(title="Averaged annualized Excess Rate of Returns", x = "Dates", y = "Avg. excess returns (in %)")


#### Sharpe Ratio
df <- diagnostics_mom |>
  select(month, starts_with("Sharpe ratio "))|> 
  select(-"Sharpe ratio top100_mom") |>
  pivot_longer(cols=-month, names_to = "portfolio", values_to="Sharpe ratio") ### here delete "acc_"; rename portfolios


ggplot(df, aes(x = month, y = `Sharpe ratio`, group = portfolio)) + geom_line(aes(col = portfolio)) +
  scale_color_discrete(name="Portfolio") + # labels at position 1: "PPP long-short whole univ.", 
  theme(axis.text.x = element_text(color="#000000", size=8, angle=90)) +
  scale_x_date(breaks="3 months", date_labels = "%m / %Y") +
  labs(title="Sharpe ratio (past 12 months)", x = "Dates", y = "Sharpe Ratio")

#### Sortino Ratio
df <- diagnostics_mom |>
  select(month, starts_with("Sortino ratio"))|> 
  select(-"Sortino ratio top100_mom") |>
  pivot_longer(cols=-month, names_to = "portfolio", values_to="Sortino ratio") ### here delete "acc_"; rename portfolios


ggplot(df, aes(x = month, y = `Sortino ratio`, group = portfolio)) + geom_line(aes(col = portfolio)) +
  scale_color_discrete(name="Portfolio") + # labels at position 1: "PPP long-short whole univ.", 
  theme(axis.text.x = element_text(color="#000000", size=8, angle=90)) +
  scale_x_date(breaks="3 months", date_labels = "%m / %Y") +
  labs(title="Sortino ratio (past 12 months)", x = "Dates", y = "Sortino Ratio")


#### CAPM alpha
df <- diagnostics_mom |>
  select(month, starts_with("CAPM alpha"))|> 
  select(-"CAPM alpha top100_mom") |>
  pivot_longer(cols=-month, names_to = "portfolio", values_to="CAPM alpha") ### here delete "acc_"; rename portfolios


ggplot(df, aes(x = month, y = `CAPM alpha`, group = portfolio)) + geom_line(aes(col = portfolio)) +
  scale_color_discrete(name="Portfolio") + # labels at position 1: "PPP long-short whole univ.", 
  theme(axis.text.x = element_text(color="#000000", size=8, angle=90)) +
  scale_x_date(breaks="3 months", date_labels = "%m / %Y") +
  labs(title="CAPM alpha (past 12 months)", x = "Dates", y = "CAPM alpha")


#### Market beta
df <- diagnostics_mom |>
  select(month, starts_with("Market beta"))|> 
  select(-"Market beta top100_mom") |>
  pivot_longer(cols=-month, names_to = "portfolio", values_to="Market beta") ### here delete "acc_"; rename portfolios


ggplot(df, aes(x = month, y = `Market beta`, group = portfolio)) + geom_line(aes(col = portfolio)) +
  scale_color_discrete(name="Portfolio") + # labels at position 1: "PPP long-short whole univ.", 
  theme(axis.text.x = element_text(color="#000000", size=8, angle=90)) +
  scale_x_date(breaks="3 months", date_labels = "%m / %Y") +
  labs(title="Market beta (past 12 months)", x = "Dates", y = "Market beta")


#plot of coefficients
df <- df |> 
  arrange(month) |>
  distinct(month) |> 
  left_join(coef_save, by="month")

df <- df |> 
  group_by(month) |> 
  mutate(total = sum(abs(c_across(where(is.numeric)))),
         scaled_m2b_lag=m2b_lag/abs(total),
         scaled_PEr_lag=PEr_lag/abs(total),
         scaled_beta_lag=beta_lag/abs(total),
         scaled_momentum_lag=momentum_lag/abs(total),
         scaled_volatility_lag=volatility_lag/abs(total),         
         scaled_relative_mrktcap_lag=relative_mktcap_lag/abs(total)
  )

dfm <- df |>
  select(month, starts_with("scaled_"))|> 
  rename_with(~trimws(sub('(scaled_*)', '', .), whitespace = '_')) |>
  rename_with(~trimws(sub('(*_lag)', '', .), whitespace = '_')) |>
  pivot_longer(cols=-month, names_to = "characteristic", values_to="coefficients") ### here delete "acc_"; rename portfolios

#colnames(dfm) = c("month", "characteristic", "coefficients")
# plotting diagnostics for "PPP long-top50"
ggplot(dfm, aes(x = month, y = coefficients, group =characteristic )) + 
  geom_line(aes(col = characteristic)) +
  facet_wrap(~ characteristic, scales = "free_y", ncol = 1) +
  geom_hline(yintercept=0, color="darkgrey") + 
  theme(axis.text.x = element_text(color="#000000", size=8, angle=90)) +
  scale_x_date(breaks="3 months", date_labels = "%m / %Y") +
  labs(title="Optimization Factor Coefficients (scaled)", x = "Dates", y = "Factor Coefficients (scaled)")


########################################################################################
################################## END OF SCRIPT #######################################  
########################################################################################
