library(readr)
dataSetPredict <- read_csv("DatasetModif/dataSetPredict.csv", 
                           col_types = cols(X1 = col_skip()))

probY <- dataSetPredict$Pop_male / dataSetPredict$Pop * dataSetPredict$Pop_16_26 / dataSetPredict$Pop

dataSetPredict$Pop_Male_16_26
round(probY* dataSetPredict$Pop ) 

testInde <- chisq.test(dataSetPredict$Pop_Male_16_26, round(probY* dataSetPredict$Pop ))
