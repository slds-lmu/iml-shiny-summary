# (1) Shiny dashboard ----
# install.packages("ggplot2")
library(ggplot2)

# install.packages("shiny")
library(shiny)

# install.packages("shinydashboard")
library(shinydashboard)

# install.packages("sparkline")
library(sparkline)

# install.packages("shinycssloaders") 
library(shinycssloaders) # withSpinner

# install.packages("shinyjs")
library(shinyjs) # useShinyjs()

# install.packages("V8")
library(V8) # extendShinyjs

# install.packages("plyr")
library(plyr) # empty

# install.packages("dplyr")
library(dplyr)

# (2) ML and IML ----
# install.packages("mlr")
library(mlr) 

# install.packages("devtools")
library(devtools)

# devtools::install_github("christophM/iml")
# packageVersion("iml")
library(iml)

# install.packages("checkmate") 
library(checkmate) 

# devtools::install_github("compstat-lmu/ame")
library(ame) # ame changes

# install.packages("sensitivity")
# library(sensitivity) # morris method

# (3) Adaptions to data frame ----

# install.packages("BBmisc") 
library(BBmisc) # dropNamed

# install.packages("DT")
library(DT)

# install.packages("rlist")
library(rlist) # list.append, creating spark.data

# install.packages("tibble")
library(tibble) # tibble, create spark.data


# install.packages("data.table") 
library(data.table)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


  



