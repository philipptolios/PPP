### Parametric Portfolio Policies Model
## PMP Academic Team
## Version 2.0

## data cleaning

## last changes 2023/01/20
## by: Paul Kudlek

################################################################################
#This is the data cleaning script for the Parametric Portfolio Policy (PPP) model 
#
# This script loads the raw xlsx files as downloaded from EIKON/Refinitiv and creates separate .csv files for each of the chracteristics (returns, market cap. PER,...)
# It deletes erroneous entries, and sets entries from delisted companies to NA after timeof delisting 
# 
#
# 
# Input: raw .xlsx files as downloaded from EIKON/Refinitiv  
# Output: separate R data files as well ass .csv files for each of the characteristics. Rows = dates; columns = stocks
#         
#
# Note: needs to be adapted to deal with flexible number of characteristics
#
################################################################################

#set working directory to where your data file is. Output will also be saved there
setwd("C:\\Users\\paulk\\OneDrive - Humboldt-Universitaet zu Berlin, CMS\\Master_VWL\\PMP\\Code\\data")

#check how many rows you have to skip in .xlsx file (usually 1)
files <- list.files(pattern='*.xlsm')
all_data_raw_spx <- lapply(files, function(file) read_excel(file, sheet = 4))
all_data_raw_stoxx <- lapply(files, function(file) read_excel(file, sheet = 5))
rawdata_spx <- bind_rows(all_data_raw_spx)
rawdata_stoxx <- bind_rows(all_data_raw_stoxx)

#clean
names(rawdata_stoxx)[1] <- "variable"
names(rawdata_spx)[1] <- "variable"

#set number of characteristics
n_var <- 5

factors <- c("price","mval","mk2b","pe","beta")

rawdata <- rbind(rawdata_spx,rawdata_stoxx)%>%
  mutate(factor = rep(factors, length.out = n()), .before = "variable")

static_data <- rawdata%>%
  select(3:11)%>%
  clean_names()%>%
  distinct()

#use only month and year for date since we optimize monthly
names(rawdata)[-c(1:12)] <-  format(as.Date(as.numeric(names(rawdata)[-c(1:12)]), 
                                            origin = "1899-12-30"), "%m/%Y")

#deal with missing values
data_clean <- rawdata%>%
  as.data.frame()%>%
  mutate(group = rep(1:(n()/5), each=5))%>% 
  mutate(variable = na_if(variable, "#ERROR"))%>%
  group_by(group) %>%
  ungroup()%>%
  fill(variable, .direction = "downup")%>%
  ungroup()%>%
  mutate(variable = gsub("-","", variable))%>%
  mutate_all(funs(replace(., grepl("^\\$\\$ER", .), NA)))%>%
  t()%>%
  as.data.frame()

unique_colnames <- as.data.frame(paste(data_clean["ISIN",],data_clean["factor",],data_clean["group",], sep = "_"))%>%
  t()%>%
  as.data.frame()

data_clean <- bind_rows(data_clean,unique_colnames)

data_clean_final <- data_clean%>%
  row_to_names(315, remove_rows_above = FALSE, remove_row = TRUE)%>%
  clean_names()

data_clean_sub <- data_clean_final[-c(1:13,314), ]



#save date vector
dates <- rownames(data_clean_sub)

data_clean_sub <- data_clean_sub%>%
  lapply(as.numeric)%>%
  as.data.frame()%>%
  clean_names()

#turn all values of dead companies to NA from the time they delist

smaller_dataframes <- split.default(data_clean_sub, (seq_along(data_clean_sub) - 1) %/% n_var)

replace_first_na_with_na <- function(df) {
  df[apply(df, 1, function(row) is.na(row[1])), ] <- NA
  return(df)
}

# Apply the function to each smaller dataframe
smaller_dataframes <- lapply(smaller_dataframes, replace_first_na_with_na)

data_clean_final <- do.call(cbind, smaller_dataframes)

colnames(data_clean_final) <- colnames(data_clean_sub)

#create dataframe for each characteristic
data.price     = data_clean_final[,seq(1, ncol(data_clean_final), n_var) ]     
data.mrktvl    = data_clean_final[,seq(2, ncol(data_clean_final), n_var) ]    
data.mrkt2book = data_clean_final[,seq(3, ncol(data_clean_final), n_var) ]
data.PER       = data_clean_final[,seq(4, ncol(data_clean_final), n_var) ]
data.beta      = data_clean_final[,seq(5, ncol(data_clean_final), n_var) ]

#calculate returns
data.returns = ((data.price/shift(data.price))-1)[-1,]   # arithmetic returns instead of log returns
data.returns = data.frame(data.returns)

data.price     = cbind(dates,data.price)    
data.mrktvl    = cbind(dates,data.mrktvl)   
data.mrkt2book = cbind(dates,data.mrkt2book) 
data.PER       = cbind(dates,data.PER) 
data.beta      = cbind(dates,data.beta) 

dates <- dates[-1]

data.returns   = cbind(dates,data.returns) 
colnames(data.returns) <- gsub("_.+", "_returns", colnames(data.returns))

#save as R Data file
saveRDS(data.price, "price_data")
saveRDS(data.mrktvl, "mrktvl_data")
saveRDS(data.mrkt2book, "mrkt2book_data")
saveRDS(data.PER, "PER_data")
saveRDS(data.beta, "beta_data")
saveRDS(data.returns, "returns_data")
saveRDS(static_data, "static_data")

# save as CSV files
write.csv(data.price, "_price_data.csv") # add; "_", lastdate, "_", firstdate,
write.csv(data.mrktvl, "_mrktvl_data.csv") # add; "_", lastdate, "_", firstdate,
write.csv(data.mrkt2book,"_mrkt2book_data.csv") # add; "_", lastdate, "_", firstdate,
write.csv(data.PER, "_PER_data.csv") # add; "_", lastdate, "_", firstdate,
write.csv(data.beta, "_beta_data.csv") # add; "_", lastdate, "_", firstdate,
write.csv(data.returns, "_returns_data.csv")# add; "_", lastdate, "_", firstdate,
