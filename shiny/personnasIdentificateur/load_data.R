# Shapefile
FSA.shape <- rgdal::readOGR(dsn = "shapefileFSA",
                     layer = "grta000a11a_f")
FSA.shapeQC <- subset(FSA.shape, substr(FSA.shape$RTACIDU, 1, 1) %in% c("G", "H", "J"))

# Import dataset
library(readr)
dataSetPredict <- read_csv("DatasetModif/dataSetPredict.csv", 
                           col_types = cols(X1 = col_skip()))

probColocation <- read_csv("DatasetModif/probColocation.csv", 
                           col_types = cols(X1 = col_skip()))

dataEducation <- read_csv("DatasetModif/dataEducation.csv", 
                          col_types = cols(X1 = col_skip()))
dataEmploi <- read_csv("DatasetModif/dataEmploi.csv", 
                       col_types = cols(X1 = col_skip()))
dataEmploi_female <- read_csv("DatasetModif/dataEmploi_female.csv", 
                              col_types = cols(X1 = col_skip()))
dataEmploi_male <- read_csv("DatasetModif/dataEmploi_male.csv", 
                            col_types = cols(X1 = col_skip()))
