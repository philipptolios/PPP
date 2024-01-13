### Parametric Portfolio Policies Model
## PMP Academic Team
## Version 2.0

## data loading and pre-processing script

## last changes 2024/01/11

################################################################################
#This is the data preprocessing script for the Parametric Portfolio Policy (PPP) model 
#
# This script loads the data files after they've been cleaned by "--Pauls data cleaning file's name----" 
# It combines different countries (if applicable) and characteristics, 
# load and joins data with risk free and market returns rate from Kenneth French's Data Library 
# (https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html),
# creates long format data and deletes all NA observations. nd ll variables and parameters that are needed in running the PPP
# 
# --> It points towards other specialized scripts for operations
# --> Input: filepaths for in- and output locations, parameters for optimization and diagnostics
# --> new features should be implemented either in the specialized scripts or via new Scripts
# --> every script has a header like this. 
#
#     Key information to be stored in the header: 
#     - What does the file do?
#     - What does it take as input?
#     - What does it produce as output?
#     - important notes
#
# Notes:
# IT'S A TOTAL MESS AND DESPERATELY NEEDS TO BE PROGRAMMED IN A MORE FLEXIBLE AND LESS HARD-CODED WAY!!
################################################################################
#
# Summary:
# 
# Step 1: load needed libraries
# Step 2: load and preprocess data
# Step 3: define functions (developers only! no data handling here!)
# Step 4: PPP optimization
# Step 5: export weights
# Step 6: diagnostics & plots
#
################################################################################








################################# load data


px   <- read_csv(paste0(path,country, "_price_data.csv"))
rets <- read_csv(paste0(path,country, "_returns_data.csv"))
mtb  <- read_csv(paste0(path,country, "_mrkt2book_data.csv"))
mc   <- read_csv(paste0(path,country, "_mrktvl_data.csv"))
per  <- read_csv(paste0(path,country, "_PER_data.csv"))
bt   <- read_csv(paste0(path,country, "_beta_data.csv"))


px2   <- read_csv(paste0(path2,country2, "_price_data.csv"))
rets2 <- read_csv(paste0(path2,country2, "_returns_data.csv"))
mtb2  <- read_csv(paste0(path2,country2, "_mrkt2book_data.csv"))
mc2   <- read_csv(paste0(path2,country2, "_mrktvl_data.csv"))
per2  <- read_csv(paste0(path2,country2, "_PER_data.csv"))
bt2   <- read_csv(paste0(path2,country2, "_beta_data.csv"))
#characteristics=c("Market to book", "P/E-Ratio","Beta", "Market Cap.")


## Reshape variables to matrices ##
# dropped first date in characteristics because we loose one observation to calculate the returns
# drop first column as it is the dates, not firms (ugly build around, should be fixed in data loading)
PEratio = per[-1,-1]
mkt2bk  = mtb[-1,-1]
beta    = bt[-1,-1]
mktcap  = mc[-1,-1]
prices  = px[-1,-1]
ret     = rets[,-1] # returns don't have observations in first row ()

## european stocks 
prices2  = px2[-1,-1]
mkt2bk2  = mtb2[-1,-1]
beta2    = bt2[-1,-1]
mktcap2  = mc2[-1,-1]  
PEratio2 = per2[-1,-1]
ret2     = rets2[,-1]  # returns don't have observations in first row ()

### all start on 01 of month, while downloaded at the end of the month
sim.m2b     <- merge(mkt2bk,mkt2bk2, by="dates")
sim.mc      <- merge(mktcap,mktcap2, by="dates")
sim.PEr     <- merge(PEratio,PEratio2, by="dates")
sim.beta    <- merge(beta,beta2, by="dates")
sim.returns <- merge(ret,ret2, by="dates")
sim.price   <- merge(prices,prices2,by="dates")


colnames(sim.m2b)[1]     <-"month"
colnames(sim.mc)[1]      <-"month"
colnames(sim.PEr)[1]     <-"month"
colnames(sim.beta)[1]    <-"month"
colnames(sim.returns)[1] <-"month"
colnames(sim.price)[1]  <-"month"



sim.m2b     <- sim.m2b |> pivot_longer(-month, names_to="stock", values_to = "m2b")
sim.mc      <- sim.mc |> pivot_longer(-month, names_to="stock", values_to = "mktcap")
sim.PEr     <- sim.PEr|> pivot_longer(-month, names_to="stock", values_to = "PEr")
sim.beta    <- sim.beta|> pivot_longer(-month, names_to="stock", values_to = "beta")
sim.returns <- sim.returns|> pivot_longer(-month, names_to="stock", values_to = "ret_adj")
sim.price   <- sim.price|> pivot_longer(-month, names_to="stock", values_to = "price")


name_cuttoff      <- regexpr('_number_t', sim.m2b$stock)
sim.m2b$stock     <- substr(sim.m2b$stock,start=1,stop=name_cuttoff-1)
sim.mc$stock      <- substr(sim.mc$stock,start=1,stop=name_cuttoff-1)
sim.PEr$stock     <- substr(sim.PEr$stock,start=1,stop=name_cuttoff-1)
sim.beta$stock    <- substr(sim.beta$stock,start=1,stop=name_cuttoff-1)
sim.returns$stock <- substr(sim.returns$stock,start=1,stop=name_cuttoff-1)
sim.price$stock   <- substr(sim.price$stock,start=1,stop=name_cuttoff-1)

# 






# #################################################################

#characteristics=c("Market to book", "Beta", "Market Capital.")
# 
# colnames(sim.returns) <- c("month", "stock", "ret_adj")
# colnames(sim.m2b)     <- c("month", "stock", "m2b")
# colnames(sim.mc)      <- c("month", "stock", "mktcap")
# colnames(sim.PEr)     <- c("month", "stock", "PEr")
# colnames(sim.beta)    <- c("month", "stock", "beta")

# 
# sim.m2b$stock     <- strtrim(sim.m2b$stock,10)
# sim.mc$stock      <- strtrim(sim.mc$stock,10)
# sim.PEr$stock     <- strtrim(sim.PEr$stock,10)
# sim.beta$stock    <- strtrim(sim.beta$stock,10)
# sim.returns$stock <- strtrim(sim.returns$stock,10)
# 
# 
# #################################################################
sim.m2b <- sim.m2b |>
  distinct(stock, month, .keep_all = TRUE)
sim.mc      <- sim.mc |>
  distinct(stock, month,.keep_all = TRUE)
sim.PEr     <- sim.PEr |>
  distinct(stock, month,.keep_all = TRUE)
sim.beta    <- sim.beta |>
  distinct(stock, month,.keep_all = TRUE)
sim.returns <- sim.returns |>
  distinct(stock, month,.keep_all = TRUE)
sim.price   <- sim.price |>
  distinct(stock, month,.keep_all = TRUE)

# 
# sim.m2b     <- sim.m2b[complete.cases(sim.m2b),]
# sim.mc      <- sim.mc[complete.cases(sim.mc),]
# sim.PEr     <- sim.PEr[complete.cases(sim.PEr),]
# sim.beta    <- sim.beta[complete.cases(sim.beta),]
# sim.returns <- sim.returns[complete.cases(sim.returns),]
#############

### create joint dataset
# We need: ret_excess, momentum_lag size_lag _lag _lag, _lag, relative_mktrcap
###############################this is how it should work --> stock names are not unique (characteristics in name)! ############################
sim.data <- sim.returns |>
  left_join(sim.m2b,
            by = c("month", "stock")
  )  |>
  left_join(sim.mc,
            by = c("month", "stock")
  ) |>
  left_join(sim.PEr,
            by = c("month", "stock")
  ) |>
  left_join(sim.beta,
            by = c("month", "stock")
  ) |>
  left_join(sim.price,
            by = c("month", "stock")
  )
#####################################################################################
# sim.data <- cbind(sim.returns, sim.m2b$m2b, sim.mc$mktcap, sim.PEr$PEr, sim.beta$beta)
# colnames(sim.data) <- c("month", "stock", "ret_adj", "m2b", "mktcap", "PEr", "beta")
head(sim.data)

### kick out penny stocks


# ## load data from SQL database
# tidy_finance <- dbConnect(
#   SQLite(),
#   "C:/Users/pako1/Desktop/PMP/0_PPP tidyfinance/SQL_data/tidy_finance_r.sqlite",
#   extended_types = TRUE
# )
# 
# 
# factors_ff3_monthly <- tbl(tidy_finance, "factors_ff3_monthly")
# factors_ff3_monthly <- factors_ff3_monthly |> collect()
# #factors_ff3_monthly <- factors_ff3_monthly |> mutate(rf=0, mkt_excess=0, month=unique(sim.data$month))
sim.data$month = as.Date(paste("01/",sim.data$month, sep=""), format='%d/%m/%Y')
#### retrieve Fama-French market data (used to compute CAPM)
start_date <- min(sim.data$month)
end_date <- max(sim.data$month)

factors_ff3_monthly_raw <- download_french_data("Fama/French 3 Factors")
factors_ff3_monthly <- factors_ff3_monthly_raw$subsets$data[[1]] |>
  mutate(
    month = floor_date(ymd(str_c(date, "01")), "month"),
    across(c(RF, `Mkt-RF`, SMB, HML), ~as.numeric(.) / 100),
    .keep = "none"
  ) |>
  rename_with(str_to_lower) |>
  rename(mkt_excess = `mkt-rf`) |> 
  filter(month >= start_date & month <= end_date)


########################################################################
#### incase most recent month is not (yet) in Fama-French dataset -->
factors_ff3_monthly <- factors_ff3_monthly |> 
  filter(month == max(month)) |> 
  mutate(month = month %m+% months(1)) |>
  bind_rows(factors_ff3_monthly)
########################################################################



# factors_ff3_monthly <- cbind.data.frame(month=seq(as.Date(min(sim.data$month)), by = "month", max(sim.data$month)),
#       rf=0, mkt_excess=0
# )

sim.data <- sim.data |>
  left_join(factors_ff3_monthly,
            by = "month"
  ) |>
  mutate(
    ret_excess = ret_adj - rf,
    ret_excess = pmax(ret_excess, -1) 
  ) |>
  select(-ret_adj, -rf)

## lagging firm level fundamentals 
characteristics_lagged <- sim.data |>
  mutate(month = month %m+% months(1)) |>
  select(stock, 
         month, 
         mktcap_lag = mktcap, 
         m2b_lag = m2b, 
         PEr_lag = PEr, 
         beta_lag = beta)

sim.data <- sim.data |>
  left_join(characteristics_lagged, by = c("month", "stock"))

## dropping NAs and datasets
sim.data <- sim.data |>
  drop_na(ret_excess, mktcap, mktcap_lag, beta, beta_lag, PEr, PEr_lag, m2b, m2b_lag )


sim.data <- sim.data |> 
  group_by(stock) |> 
  arrange(month) |>
  mutate(
    #momentum_lag = mktcap_lag / mktcap_13,
    #size_lag = log(mktcap_lag)
    momentum_lag = rollsum(ret_excess, k=12,fill = NA, align="right"),
    volatility_lag = 100 * sqrt(12) * roll_sd(ret_excess, width=12)
  ) |>
  drop_na(contains("lag"))

# 
data_portfolios <- sim.data |>
  group_by(month) |>
  mutate(
    n = n(),
    relative_mktcap_lag = mktcap_lag / sum(mktcap_lag),
    across(contains("lag"), ~ (. - mean(.)) / sd(.)),   # all lagged variables are normalized --> interpretion: coef = effect of 1 std.dev of characteristic
  ) |>
  ungroup() |>
  select(-mktcap_lag)



n_parameters <- sum(str_detect(
  colnames(data_portfolios), "lag"
))



theta <- rep(0, n_parameters)

names(theta) <- colnames(data_portfolios)[str_detect(
  colnames(data_portfolios), "lag"
)]



rm( sim.beta,
    sim.m2b,
    sim.mc,
    sim.PEr,
    sim.returns,
    characteristics_lagged
)


########################################################################################
################################## END OF SCRIPT #######################################  
########################################################################################