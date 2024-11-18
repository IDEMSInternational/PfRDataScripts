library(rio)
library(ExcelToShiny)
library(shiny)
library(shinyjs)
library(plotly)
library(shinydashboard)
library(jsonlite)
library(rjson)
library(ggplot2)
library(tibble)
library(stringr)
library(forcats)
library(lubridate)
library(purrr)
library(tidyr)
library(dplyr)
library(gt)
library(readxl)
library(openappr)
library(ggthemes)
library(shinyauthr)
library(zoo)
library(stringdist)

# source any R files which you use in the app:
source("Personal Setup.R")              # file containing personal set up info
source("PfR_loading_data.R")
source("Credentials_data.R")
# file containing functions used in the creation and the R script to import/tidy the function

# Excel file with the specifications in it
data_l <- import_list("PfR_Shiny.xlsx")


# Run the shiny dashboard
build_shiny(title = "PfR Dashboard",
          data_list = data_l,
          data_frame = plhdata_org,   # put name of data frame
          status = "primary",
          key_var = "id")             # irrelevant for you, but, set to be any variable in there. 

