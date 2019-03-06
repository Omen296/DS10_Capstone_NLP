# Text Prediction Widget
# DS Module 10 / Capstone Project
# Paul Ringsted 4th March 2019
# Global.R - data load part

library(shiny)
library(DT)
library(data.table)

# Load dictionary
dic <- readRDS("dic.RDS")
setkey(dic,feature)

# Load ngrams
ng <- readRDS("ngramTOP.RDS")
setkey(ng,ngram_len,idx1,idx2,idx3,idx4,idx5)

# Define functions
source("ng.R")