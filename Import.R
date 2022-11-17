#### 1. Libraries and Reading the Data ####
# Libraries and Reading in data are moved this file #

library(shiny)   # reactivity (user input)
library(shinydashboard)    # the dashboard layout
library(tidyverse)    # the "readability" of code
library(lubridate)    # dates
library(DT)    # the HTML tables
library(readxl)    # reading in the excel files
library(waiter)    # the waiting/loading circles
library(shinyalert)    # the Information System warning popup - mandatory verbage for the RMF
# library(rlist)
library(leaflet)     # for the map
# library(maps)
library(htmltools)     # dependency package for leaflet
library(terra)     # dependency package for leaflet
library(raster)     # dependency package for leaflet
library(rlang)     # dependency package for tidyverse

####


Sys.setenv(TZ="America/New_York")    # sets timezone


####

# This loops through the T2T_Imput tabs to create the exercise and admin dataframe 

multiplesheets <- function(fname) {
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname) 
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet =x)) #, col_types = c("text", "text", "text", "text", "text", "text", "date", "date")))
  data_frame <- lapply(tibble, as.data.frame)
  
  
  # assigning names to data frames
  names(data_frame) <- sheets
  
  # print data frame
  print(data_frame)
}

path <- "T2T_Input.xlsx"
T2T_test2 <- multiplesheets(path) # reads the T2T_Input into dataframes within a list

# Append the exercise tabs together
### THE FIRST TAB IS ROSTER, AND IT IS HIDDEN BUT STILL COUNTS!!!!! ###
T2T_test3 <- T2T_test2[-c(1:6)] # removes the first 6 tabs (Roster and EXD, SSD, CTR, FUT, OPS Admin tabs) in the T2T_Input
T2T_test4 <- do.call(rbind, T2T_test3)

# Append the Admin tabs together
### THE FIRST TAB IS ROSTER, AND IT IS HIDDEN BUT STILL COUNTS!!!!! ###
T2T_test5 <- T2T_test2[c(2:6)] # selects only the 2nd-6th tabs (EXD, SSD, CTR, FUT, OPS Admin tabs)
T2T_test6 <- do.call(rbind, T2T_test5)

# Merge the exercise and Admin tabs
T2T_test7 <- full_join(T2T_test4, T2T_test6)


####

T2T_Input_Historic_FY21 <- read_excel("T2T_Input_Historic_FY21.xlsx", 
                                      col_types = c("text", "text", "text", "text", "text", "text", "date", "date"))

####


Roster <- read_excel("T2T_Input.xlsx", 
                     sheet = "Roster", 
                     col_types = c("text", "text", "text", "text", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip")) %>%
  mutate(Name = `Name List`)


####


location <- T2T_Input <- read_excel("T2T_Input.xlsx", 
                                    sheet = "Roster", col_types = c("skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "text", "numeric", "numeric", "skip", "skip", "skip"))

