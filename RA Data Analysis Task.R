### Youth Impact RA Task

## Instaling required packages

install.packages("haven")
library(haven)

## Set working directory
setwd("D:/Job Applications/Youth Impact/Youth Impact_RA Deliverable/candidate_materials")

## Load datasets
data_sensitization <- read_dta("01_sensitization_data.dta")
data_implementation <- read_dta("02_implementation_data.dta")
data_endline <- read_dta("03_endline_data.dta")