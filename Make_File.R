


###################### MAKE FILE ############################

# Run this first!

# It loads all the necessary libraries, and loads the function files for the model in the correct order so that functions that depend on objects or functions defined in other files are loaded without errors. 


### Load libraries
library(tidyverse)
library(cowplot)
library(readr)
library(data.table)




### Load bespoke functions
# Note: This is one time that you don't want to aim for efficiency by using an apply function rather than repeating code.
# Run the source() command separately for each function file so that, if something throws an error, the console will give you an error message that tells you exactly which file is glitching when it tries to load. 
source("./Functions//All Functions - How to Survive a Killer Language.R")
#source("./Parental Language Choice Model Function.R")
source("./Functions//Plotting_Functions.R")








