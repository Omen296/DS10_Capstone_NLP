#################################################################################################
## Project	: Module 10 / Capstone - Text Prediction
## Author	: Paul Ringsted
## Date		: 2018-02-24
## Module	: ng_load.R
##		: Script to load and index the data tables, source other functions
#################################################################################################

library(data.table)

print("Reading dictionary...")
flush.console()
dic<-readRDS("./dic.RDS")
setkey(dic,feature)
print(paste0("Done... rows: ",dic[,.N]))

print("Reading n-grams...")
ng<-readRDS("./ngramTOP.RDS")
setkey(ng,ngram_len,idx1,idx2,idx3,idx4,idx5)
print(paste0("Done... rows: ",ng[,.N]))

print("Define functions...")
source('ng.R')
source('ng_predict_app.R')

