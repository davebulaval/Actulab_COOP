library(rgdal)

# Shapefile
LatLongData <- rgdal::readOGR(dsn = "~/DatasetModif/StatCan_FSA_boundaries", 
                       layer = "StatsCanada_FSA_boundaries")
LatLongDataQC <- subset(LatLongData, substr(LatLongData$CFSAUID, 1, 1) %in% c("G", "H", "J"))

FSA.shape <- rgdal::readOGR(dsn = "~/DatasetModif//shapefileFSA",
                     layer = "grta000a11a_f")
FSA.shapeQC <- subset(FSA.shape, substr(FSA.shape$RTACIDU, 1, 3) %in% LatLongDataQC$CFSAUID)

# Import dataset
dataSetPredict <- read_csv("~/DatasetModif/dataEducation.csv", 
                           col_types = cols(X1 = col_skip()))

probColocation <- read_csv("~/DatasetModif/probColocation.csv",
                           col_types = cols(X1 = col_skip()))
dataEducation <- read_csv("~/DatasetModif/dataEducation.csv",
                          col_types = cols(X1 = col_skip()))
dataEmploi <- read_csv("~/DatasetModif/dataEmploi.csv",
                       col_types = cols(X1 = col_skip()))
dataEmploi_female <- read_csv("~/DatasetModif/dataEmploi_female.csv",
                              col_types = cols(X1 = col_skip()))
dataEmploi_male <- read_csv("~/DatasetModif/dataEmploi_male.csv",
                            col_types = cols(X1 = col_skip()))
