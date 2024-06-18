### Parametric Portfolio Policies Model
## PMP Academic Team
## Version 2.0

## data cleaning

## last changes 2024/06/17
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

#check how many rows you have to skip in .xlsx file (usually 1)
files <- list.files(path.rawdata,pattern='*.xlsx')
all_data_raw <- lapply(files, function(file) read_excel(paste(path.rawdata,file,sep="")))
rawdata <- bind_rows(all_data_raw)

#use only month and year for date since we optimize monthly
names(rawdata)[-1] <-  format(as.Date(as.numeric(names(rawdata)[-1]), 
                                      origin = "1899-12-30"), "%m/%Y")

#excel sometimes has weird name for first column
names(rawdata)[1] <- "Name"

#deal with missing values
data_clean <-  rawdata %>%
  as.data.frame() %>%
  mutate(group = rep(1:(n()/5), each=5)) %>% 
  select(group, everything()) %>%
  mutate(Name = na_if(Name, "#ERROR")) %>%
  group_by(group) %>%
  fill(Name, .direction = "downup") %>%
  ungroup() %>%
  mutate(Name = gsub("-","", Name)) %>%
  mutate_all(funs(replace(., grepl("^\\$\\$ER", .), NA))) %>%
  t() %>%
  as.data.frame() %>%
  slice(-1) %>% 
  row_to_names(row_number = 1)

#save date vector
dates <- rownames(data_clean)

#deal with duplicate names
colnames(data_clean) <- make.unique(colnames(data_clean), sep = "_")

#colnames are easier processed when cleaned
data_clean <- clean_names(data_clean)

#turn all columns to numeric for later processing
setDT(data_clean)

data_clean <- data_clean[, lapply(.SD, as.numeric)]


data_clean <- setDF(data_clean)

#turn all values of dead companies to NA from the time they delist
n <- 5

# Split the dataframe into smaller dataframes
smaller_dataframes <- split.default(data_clean, (seq_along(data_clean) - 1) %/% n)

replace_first_na_with_na <- function(df) {
  df[apply(df, 1, function(row) is.na(row[1])), ] <- NA
  return(df)
}

# Apply the function to each smaller dataframe
smaller_dataframes <- lapply(smaller_dataframes, replace_first_na_with_na)

data_clean_final <- do.call(cbind, smaller_dataframes)


rownames(data_clean_final) <- rownames(data_clean)

#create dataframe for each characteristic
data.price     = data_clean[,seq(1, ncol(data_clean_final), 5) ]     
data.mrktvl    = data_clean[,seq(2, ncol(data_clean_final), 5) ]    
data.mrkt2book = data_clean[,seq(3, ncol(data_clean_final), 5) ]
data.PER       = data_clean[,seq(4, ncol(data_clean_final), 5) ]
data.beta      = data_clean[,seq(5, ncol(data_clean_final), 5) ]

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


#save as R Data file
saveRDS(data.price, paste(path.data, "price_data_25Y",sep=""))
saveRDS(data.mrktvl,paste(path.data, "mrktvl_data_25Y",sep=""))
saveRDS(data.mrkt2book,paste(path.data, "mrkt2book_data_25Y",sep=""))
saveRDS(data.PER,paste(path.data, "PER_data_25Y",sep=""))
saveRDS(data.beta,paste(path.data, "beta_data_25Y",sep=""))


# save as CSV files
write.csv(data.price, paste(path.data, country,  "_price_data.csv", sep="")) # add; "_", lastdate, "_", firstdate,
write.csv(data.mrktvl, paste(path.data, country,  "_mrktvl_data.csv", sep="")) # add; "_", lastdate, "_", firstdate,
write.csv(data.mrkt2book, paste(path.data, country,  "_mrkt2book_data.csv", sep="")) # add; "_", lastdate, "_", firstdate,
write.csv(data.PER, paste(path.data, country,  "_PER_data.csv", sep="")) # add; "_", lastdate, "_", firstdate,
write.csv(data.beta, paste(path.data, country,  "_beta_data.csv", sep="")) # add; "_", lastdate, "_", firstdate,
write.csv(data.returns, paste(path.data, country,  "_returns_data.csv", sep=""))# add; "_", lastdate, "_", firstdate,
