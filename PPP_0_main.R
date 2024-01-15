### Parametric Portfolio Policies Model
## PMP Academic Team
## Version 2.0

## main script

## last changes 2024/01/11

################################################################################
#This is the main script for the Parametric Portfolio Policy (PPP) model 
#
# This file defines all variables and parameters that are needed in running the PPP
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
#
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
## To do:
# to better understand the algorithm:
# 1. create simulated data (where we know the true parameters of maximum returns);  try to recover them
#Steps:
#  a. Create fundamentals for stocks from chosen values
#  b. Create fundamentals for stocks out of random draws from (known) distributions
#  c. define data generating process (returns as a result of weighted fundamentals)

# 2. check how different starting values influence results
# 3. check how different optimization routines influence results
# 4. introduce 2 data generating processes (favorable stocks and unfavorable stocks)
#     compute returns and weights and try to recover only favorable stocks
# 5. introduce break in structure of simulated data and test time shifting optimization windows
#     computing returns and weights (with weight constraints)

# To do:
## fix weight constraints function
# filter nonzero weights and run constraints only on nonzero weights
# set count to higher number (10)
# deal with double listings / stocks with same/similar names (ISIN?) --> Paul

#2. different starting values for optimization (last versions optimum [makes more stable]) 


library(tidyverse)
library(reshape2)
library(lubridate)
library(tidyquant)
library(data.table)
library(pheatmap)
library(rio)
library(dplyr)
library(ggplot2)
library(zoo)
library(optimParallel)
library(readODS)
library(roll)
library(frenchdata)

## particple swarm optimization
library(pso)



########################################################################################
# Step 1: Define Parameters (for all subsequent functions)
########################################################################################
# # how much of the return distribution should be trimmed (in each optimization run) 
trim.return = 1  # trim.return=1 --> 1% of returns deleted
try(if(between(trim.return,0,100)==FALSE) stop("trim parameter needs to be between 0 and 100. It signifies % of the return distribution to be trimmed"))
# trimming prices (only lower bound to filter penny-stocks)
trim.price = 1
try(if(between(trim.price,0,100)==FALSE) stop("trim parameter needs to be between 0 and 100. It signifies % of the (lower end of the) price distribution to be trimmed"))

# CRRA Risk aversion parameter
#gamma = 5 

## input file paths
path <- "C:\\Users\\pako1\\Desktop\\PMP\\PMP material\\0_PPP model 2023\\Data\\25Y\\2024_01\\SPX 25Y\\"
path2 <- "C:\\Users\\pako1\\Desktop\\PMP\\PMP material\\0_PPP model 2023\\Data\\25Y\\2024_01\\STOXX 600 25Y\\"

## output file paths
path.output = "C:\\Users\\pako1\\Desktop\\PMP\\0_PPP tidyfinance\\Output 2024-01-10\\"

## Program file paths
path.program = "C:\\Users\\pako1\\Desktop\\PMP\\0_PPP tidyfinance\\PPP Code\\"

# country files used (for naming)
country <- "Spx_25Y"
country2 <- "Stoxx_600_25Y"

### Set weight constraints
#n_weights=50  # number of weights to be estimated
#weigths.lb=0.01   # lower bound of weights
#weigths.ub=0.05    # upper bound of weights

#Optimization routine
# time horizon over which optimization takes place (e.g. the past 25 years)
timehorizon = 120 # how far back should the optimization start? 

opt_window  = 120   # window of time periods, that is used in the optimization function (e.g. 5 years of data)
# mind that the optimization takes past data to run. You need at least [timehorizon + optimization window size] months of data

rolling_window=TRUE  # alternatively: expanding window

#optimization starting values
starting_value_OLS=FALSE # otherwise vector of zeros as starting value for optimization

#particle swarm optimization
particleswarm=TRUE
# delete not used objects at the end of each script?
# cleanup=TRUE

# to ensure reproducability
set.seed(123)

########################################################################################
# Step 2: Load Data
########################################################################################
## To do: check respective parameters
source(paste0(path.program,"\\PPP_2_data_loading.R"), echo=TRUE, max.deparse.length=10000, keep.source=TRUE)

########################################################################################
# Step 3: Define Functions
########################################################################################
## To do: check respective parameters
source(paste0(path.program,"\\PPP_3_functions.R"), echo=TRUE, max.deparse.length=10000, keep.source=TRUE)

########################################################################################
# Step 4: Optimization
########################################################################################
## To do: check respective parameters
source(paste0(path.program,"\\PPP_4_optimization_loop.R"), echo=TRUE, max.deparse.length=10000, keep.source=TRUE)

########################################################################################
# Step 5: Diagnostics and Plots
########################################################################################
## To do: check respective parameters
source(paste0(path.program,"\\PPP_5_diagnostics and plots.R"), echo=TRUE, max.deparse.length=10000, keep.source=TRUE)

########################################################################################
################################## END OF SCRIPT #######################################  
########################################################################################

