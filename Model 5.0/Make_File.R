


###################### MAKE FILE ############################

# Run this first!

# It loads all the necessary libraries, and loads the function files for the model in the correct order so that functions that depend on objects or functions defined in other files are loaded without errors. 


### Load libraries
library(tidyverse)
library(cowplot) # for producing compound plots of model output summaries with plot_grid()
library(readr)
library(data.table)
library(stringr)
library(janitor) # for cleaning/standardizing the variable names in a data frame
library(jsonlite) # for writing json file that can store data as lists and allow it to be readable by the human eye. 
library(scales) # for beautifying graph axes
library(viridis) # for colorblind-friendly color mapping in graphs. 



### Load bespoke functions
# Note: This is one time that you don't want to aim for efficiency by using an apply function rather than repeating code.
# Run the source() command separately for each function file so that, if something throws an error, the console will give you an error message that tells you exactly which file is glitching when it tries to load. 
source("./Model 5.0/Functions/All Functions - How to Survive a Killer Language.R")
source("./Model 5.0/Model 5.0 - How to Survive a Killer Language.R") # Run_ABM() lives here.
source("./Model 5.0/workflow.R") # functions to generate a systematic folder structure for parameter sweeps
# source("./Model 5.0/Model Plotting Code.R")







