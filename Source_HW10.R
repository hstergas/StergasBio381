# -------------------------------
# Source script for Homework 10 functions
# 03 Apr 2020
# HRS
# -------------------------------
#
# Preliminaries ------------------------------------------
library(ggplot2)
library(TeachingDemos)
library(tidyverse)

# setting seed
char2seed("zebrafish retina")

# Build Functions ------------------------------------------
# -------------------------------
# FUNCTION read_data
# description: read in (or generate) data set for analysis
# inputs: file name (or nothing, as in this demo)
# outputs: 3 column data frame of observed data (ID, x, y)
##################################
read_data <- function(z=NULL) {
  if(is.null(z)) {
    trt <- c(rep("trt1", 10), rep("trt2",10))
    var1_obs <- rnorm(20)
    df <- data.frame(ID=seq_along(var1_obs),
                     trt,
                     var1_obs) } 
  df <- read.table(file=z,
                   header=TRUE,
                   sep=",",
                   stringsAsFactors=FALSE)
  return(df)
} # end of read_data
#---------------------------------
# read_data()

# must complete this for my data set ("data") to move on to next function
# df %>% rename(ID=ID, trt=Genotype,     var1_obs=EyeDiam.BodyLength)

# -------------------------------
# FUNCTION get_metric
# description: calculate metric for randomization test
# inputs: 2-column data frame for treatment mean analysis
# outputs: difference in treatment means (that's the metric X)
##################################
get_metric <- function(z=NULL) {
  if(is.null(z)) {
    trt <- c(rep("trt1", 10), rep("trt2",10))
    var1_obs <- rnorm(20)
    z <- data.frame(ID=seq_along(var1_obs),
                    trt,
                    var1_obs) } 
  meanVar1_obs <- z %>% 
    group_by(trt) %>%
    summarize(meanVar1=mean(var1_obs)) 
  # using dyplr functions to group the data by trt, and then using  summarize to find the means of each group. I piped these functions together. 
  # use "z" as the data input, not "df". "df" is the actual name of our data frame, where "z" is the data input for the function, they should be two different things!
  X_obs <- meanVar1_obs[2,2] - meanVar1_obs[1,2]
  # creating the metric "X" by subtracting the means from eachother
  return(as.numeric(X_obs)) # return "as.numeric" to get just the X_obs value, not the associated data frame
  
} # end of get_metric
#---------------------------------
# get_metric() 

# -------------------------------
# FUNCTION shuffle_data
# description: randomize data for regression analysis
# inputs: 3 column data frame (ID, xvar, yvar)
# outputs: 3 column data frame (ID, xvar. yvar)
##################################
shuffle_data <- function(z=NULL) {
  if(is.null(z)) {
    trt <- c(rep("trt1", 10), rep("trt2",10))
    var1_obs <- rnorm(20)
    z <- data.frame(ID=seq_along(var1_obs),
                    trt,
                    var1_obs) }
  z$var1_obs <- sample(z$var1_obs) # this will reshuffle the var1 values, uncouple them from the trt
  # use "z" in the call, not "df"!!! 
  return(z)
  
} # end of shuffle_data
#---------------------------------
# shuffle_data() 

# -------------------------------
# FUNCTION get_pval
# description: calculate p value from simulation
# inputs: list of observed metric and vector of simulated metrics
# outputs: lower and upper tail probability value
##################################
get_pval <- function(z=NULL) {
  if(is.null(z)){
    z <- list(rnorm(1), rnorm(1000)) }
  p_lower <- mean(z[[2]] <= z[[1]])
  # double bracket will allow us to pull out the         values for the element of the list
  # setting up a conditional and doing math on it
  # answers what is the proportion of the simulated mean         value that is less than the observed mean?
  p_upper <- mean(z[[2]] >= z[[1]])
  
  
  return(c(pL=p_lower, pU=p_upper))
  
} # end of get_pval
#---------------------------------
# get_pval() # p_values must add up to 1.0

# -------------------------------
# FUNCTION plot_ran_test
# description: create a ggplot of histogram of simulated values
# inputs: list of observed metric and vector simulated metrics
# outputs: saved ggplot graph
##################################
plot_ran_test <- function(z=NULL) {
  if(is.null(z)) {
    z <- list(rnorm(1),rnorm(1000))}
  df <- data.frame(ID=seq_along(z[[2]]), sim_x=z[[2]])
  p1 <- ggplot(data=df, mapping=aes(x=sim_x))
  p1 + geom_histogram(mapping=aes(fill=I("goldenrod"),
                                  color=I("black"))) +
    geom_vline(aes(xintercept=z[[1]],col="blue"))
  # dont need a "print(p1)" statement, it will only print the background of the plot!
} # end of plot_ran_test
#---------------------------------
# plot_ran_test()
