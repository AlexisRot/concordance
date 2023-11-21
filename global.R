## LOADING LIBRARIES FREQUENTLY USED
library(shiny)
library(shinydashboard)
library(DT)
library(knitr)
library(plotly)
library(dplyr)

# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("DT")
# install.packages("knitr")
# install.packages("plotly")
# install.packages("dplyr")

## APP USED LESS FREQUENTLY
# library(rmarkdown)
# library(magrittr)
# library(shinythemes)
# library(rvg)
# library(jtools)
# library(stringr)
# library(shinycssloaders)
# library(base64enc)
# library(ggpubr)
# library(shinyFiles)


## SOURCE SOME FILES
# source("global_functions_DataViz.R")

source("concordance_files/WRITE.HTML.TABLEN_1.0.R")
source("concordance_files/QUALIFICATION_2_4_1.R")
source("concordance_files/formatting.R")
source("global_concordance.R")

create_folder <- function(main_dir,sub_dir){

  output_dir <- file.path(main_dir, sub_dir)

  if (!dir.exists(output_dir)){
    print("Dir doesn't exist!")
  }
  else {
    print("Dir already exists!")
    unlink(output_dir, recursive = TRUE)
    print(paste0("Files in ",sub_dir," deleted !"))

  }

  dir.create(output_dir)
  print(paste0("Files ",sub_dir," created !"))
}

