# Création carte avec shapefile ####
# install.packages("rgdal")
library(rgdal)
(path <-setwd(paste("..", "~/GitHub/Actulab_COOP", sep = "")))
path <- getwd()
LatLongData <- readOGR(dsn = paste(path, "/Dataset/StatCan_FSA_boundaries", sep = ""), 
                       layer = "StatsCanada_FSA_boundaries")
LatLongDataQC <- subset(LatLongData, substr(LatLongData$CFSAUID, 1, 1) %in% c("G", "H", "J"))

FSA.shape <- readOGR(dsn = paste(path,"/Dataset/shapefile",sep = ""),
                     layer = "grta000a11a_f")
FSA.shapeQC <- subset(FSA.shape, substr(FSA.shape$RTACIDU, 1, 3) %in% LatLongDataQC$CFSAUID)

library(readr)
# Importation du recensement 2016 ####
dataset <- read_csv("~/GitHub/Actulab_COOP/Dataset/98-401-X2016036_eng_CSV/98-401-X2016036_English_CSV_data.csv", col_names = c("CENSUS_YEAR", "GEO_CODE", "GEO_LEVEL", "GEO_NAME", "GNR", "GNR_LF", "DATA_QUALITY_FLAG", "ALT_GEO_CODE", "Profile", "Row", "Notes", "Total", "Male", "Female"), skip = 1, na = "...")

# Retrait GEO_code inadéquat
datasetQC <- subset(dataset, substr(dataset$GEO_CODE, 1, 3) %in% LatLongDataQC$CFSAUID)


dataSetPredict <- data.frame(subset(datasetQC, Row == 1)$GEO_CODE, LatLongDataQC$Latitude, LatLongDataQC$Longitude, subset(datasetQC, Row == 8)$Total, subset(datasetQC, Row == 8)$Male, subset(datasetQC, Row == 8)$Female, subset(datasetQC, Row == 14)$Total * 4 / 5 + subset(datasetQC, Row == 15)$Total + subset(datasetQC, Row == 16)$Total * 2 / 5, subset(datasetQC, Row == 17)$Total * 3 / 5 + subset(datasetQC, Row == 18)$Total, subset(datasetQC, Row == 19)$Total + subset(datasetQC, Row == 20)$Total + subset(datasetQC, Row == 21)$Total * 3 / 5, subset(datasetQC, Row == 21)$Total * 2 / 5 + subset(datasetQC, Row == 22)$Total + subset(datasetQC, Row == 23)$Total + subset(datasetQC, Row == 25)$Total * 1 / 5, subset(datasetQC, Row == 25)$Total * 4 / 5 + subset(datasetQC, Row == 26)$Total + subset(datasetQC, Row == 27)$Total * 2 / 5, subset(datasetQC, Row == 39)$Total, subset(datasetQC, Row == 40)$Total,
                             subset(datasetQC, Row == 8)$Male, subset(datasetQC, Row == 8)$Female, subset(datasetQC, Row == 14)$Male * 4 / 5 + subset(datasetQC, Row == 15)$Male + subset(datasetQC, Row == 16)$Male * 2 / 5, subset(datasetQC, Row == 14)$Female * 4 / 5 + subset(datasetQC, Row == 15)$Female + subset(datasetQC, Row == 16)$Female * 2 / 5, subset(datasetQC, Row == 17)$Male * 3 / 5 + subset(datasetQC, Row == 18)$Male, subset(datasetQC, Row == 17)$Female * 3 / 5 + subset(datasetQC, Row == 18)$Female, subset(datasetQC, Row == 19)$Male + subset(datasetQC, Row == 20)$Male + subset(datasetQC, Row == 21)$Male * 3 / 5, subset(datasetQC, Row == 19)$Female + subset(datasetQC, Row == 20)$Female + subset(datasetQC, Row == 21)$Female * 3 / 5, subset(datasetQC, Row == 21)$Male * 2 / 5 + subset(datasetQC, Row == 22)$Male + subset(datasetQC, Row == 23)$Male + subset(datasetQC, Row == 25)$Male * 1 / 5, subset(datasetQC, Row == 21)$Female * 2 / 5 + subset(datasetQC, Row == 22)$Female + subset(datasetQC, Row == 23)$Female + subset(datasetQC, Row == 25)$Female * 1 / 5, subset(datasetQC, Row == 25)$Male * 4 / 5 + subset(datasetQC, Row == 26)$Male + subset(datasetQC, Row == 27)$Male * 2 / 5, subset(datasetQC, Row == 25)$Female * 4 / 5 + subset(datasetQC, Row == 26)$Female + subset(datasetQC, Row == 27)$Female * 2 / 5,
                             sapply(708:718, function(ligne) subset(datasetQC, Row == ligne)$Total),
                             sapply(708:718, function(ligne) subset(datasetQC, Row == ligne)$Male),
                             sapply(708:718, function(ligne) subset(datasetQC, Row == ligne)$Female),
                             subset(datasetQC, Row == 1614)$Total, subset(datasetQC, Row == 1615)$Total,
                             subset(datasetQC, Row == 1618)$Total,
                             
                             subset(datasetQC, Row == 63)$Total, subset(datasetQC, Row == 62)$Total,
                             subset(datasetQC, Row == 61)$Total,
                             
                             subset(datasetQC, Row == 63)$Male, subset(datasetQC, Row == 62)$Male,
                             subset(datasetQC, Row == 61)$Male,
                             
                             subset(datasetQC, Row == 63)$Female, subset(datasetQC, Row == 62)$Female,
                             subset(datasetQC, Row == 61)$Female,
                             
                             subset(datasetQC, Row == 41)$Total,
                             subset(datasetQC, Row == 42)$Total + subset(datasetQC, Row == 44)$Total 
                             + subset(datasetQC, Row == 45)$Total + subset(datasetQC, Row == 46)$Total, 
                             subset(datasetQC, Row == 43)$Total + subset(datasetQC, Row == 47)$Total + 
                             subset(datasetQC, Row == 48)$Total,
                             
                             subset(datasetQC, Row == 103)$Total,
                             
                             sapply(84:86, function(ligne) subset(datasetQC, Row == ligne)$Total)
                             )
colnames(dataSetPredict) <- c("GEO_CODE", "Lat", "Long", "Pop", "Pop_male", "Pop_female", "Pop_16_26", "Pop_32_39", "Pop_40_52", "Pop_53_65", "Pop_66_76", "Age_Moyen", "Age_Median", 
"Male_total", "Female_total", "Pop_Male_16_26", "Pop_Female_16_26", "Pop_Male_32_39", "Pop_Female_32_39", "Pop_Male_40_52", "Pop_Female_40_52", "Pop_Male_53_65", "Pop_Female_53_65", "Pop_Male_66_76", "Pop_Female_66_76", 
                           "sal_1", "sal_2", "sal_3", "sal_4", "sal_5", "sal_6", "sal_7", "sal_8", 
                           "sal_9", "sal_10", "sal_11",
                           "sal_1_m", "sal_2_m", "sal_3_m", "sal_4_m", "sal_5_m", "sal_6_m", 
                           "sal_7_m", "sal_8_m", "sal_9_m", "sal_10_m", "sal_11_m",
                           "sal_1_f", "sal_2_f", "sal_3_f", "sal_4_f", "sal_5_f", "sal_6_f", 
                           "sal_7_f", "sal_8_f", "sal_9_f", "sal_10_f", "sal_11_f",
"Total_Habitation", "Propriétaire", "Locataire", "Condo",
"Celibataire", "Conjoint_fait", "Marie", 
"Celibataire_M", "Conjoint_fait_M", "Marie_M",
"Celibataire_F", "Conjoint_fait_F", "Marie_F",
"Maison_Detache", "Appartement", 
"Bilingue", 
"Couple_1", "Couple_2", "Couple_3"
                           )

# Retrait des données NA
dataSetPredict <-  subset(dataSetPredict, !is.na(dataSetPredict$Pop))

#Écriture CSV
write.csv(dataSetPredict, paste(path, "/Dataset/DatasetModif/dataSetPredict.csv", sep = ""), fileEncoding = "UTF-8")

#Hypothèse colocation ####
dataColocation <- matrix(scan("~/GitHub/Actulab_COOP/Dataset/dataColocation.data"),
                         nrow = 19, ncol = 7, byrow = TRUE)

colnames(dataColocation) <- c ("Nombre_Total", "Pourcentage_Femme", "nombre_Homme", 
                               "Nomre_Femme", "Pourcentage_Total", "Pourcentage_Homme", "Pourcentage_Femme")


probColocation <- sapply(c(1,3,4), function(i) data.frame( 
     (dataColocation[4,] * (4 / 5) + dataColocation[5,] + dataColocation[6,] * (2 / 5))[i], 
                   (dataColocation[7,] * (3 / 5) + dataColocation[8,])[i],
                   (dataColocation[9,] +  dataColocation[10,] +  dataColocation[11,] * (3/5))[i],
                   ( dataColocation[11,] * (2/5) + dataColocation[12,] + dataColocation[13,] + 
                          dataColocation[14,] * (1/5))[i],
                   (dataColocation[14,] * (4/5) + dataColocation[15,] + dataColocation[16,] * (2/5))[i],
                   
                   (dataColocation[4,] * (4 / 5) + dataColocation[5,] + 
                         dataColocation[6,] * (2 / 5))[i] + 
                   (dataColocation[7,] * (3 / 5) + dataColocation[8,])[i] + 
                   (dataColocation[9,] +  dataColocation[10,] +  dataColocation[11,] * (3/5))[i] + 
                   ( dataColocation[11,] * (2/5) + dataColocation[12,] + dataColocation[13,] + 
                          dataColocation[14,] * (1/5))[i] +
                   (dataColocation[14,] * (4/5) + dataColocation[15,] + dataColocation[16,] * (2/5))[i] 
                   ))
colnames(probColocation) <- c("Total", "Homme", "Femme")
rownames(probColocation) <- c("16_26", "32_39", "40_52", "53_65", "66_76", "cumul")

popQuebec <- sum(dataSetPredict$Pop)

probColocation <- data.frame(sapply(1:6, function(i) probColocation[i,]$Total / popQuebec), 
                             sapply(1:6, function(i) probColocation[i,]$Homme / popQuebec),
                             sapply(1:6, function(i) probColocation[i,]$Femme / popQuebec))
colnames(probColocation) <- c("Total", "Homme", "Femme")
rownames(probColocation) <- c("16_26", "32_39", "40_52", "53_65", "66_76", "cumul")

#Écriture CSV
write.csv(probColocation, paste(path, "/Dataset/DatasetModif/probColocation.csv", sep = ""), fileEncoding = "UTF-8")


# Hypothèse étudiant ####
dataEducation <- matrix(scan("~/GitHub/Actulab_COOP/Dataset/dataEnseignement.data"),
                         nrow = 7, ncol = 10, byrow = TRUE)

colnames(dataEducation) <- c ("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011")
rownames(dataEducation) <- c("Total_collegiale", "colle_ordinaire", "colle_continue", "Total_uni", "BAC", "Maitrise", "Doctorat")

#Écriture CSV
write.csv(dataEducation, paste(path, "/Dataset/DatasetModif/dataEducation.csv", sep = ""), fileEncoding = "UTF-8")

# Hypothèse occupation ####
emploi_maleQC <- read_csv("~/GitHub/Actulab_COOP/Dataset/employement/10604220171107080858_male_QC.CSV", 
                        col_names = FALSE, skip = 7 )
emploi_femaleQC <- read_csv("~/GitHub/Actulab_COOP/Dataset/employement/10604220171107080932_femaleQC.CSV", 
                            col_names = FALSE, skip = 7)
emploi_totalQC <- read_csv("~/GitHub/Actulab_COOP/Dataset/employement/10604220171107081001_total.CSV", 
                           col_names = FALSE, skip = 7)
colnames(emploi_maleQC) <- c("Profession", "Total_age", "15_24", "15_19", "20_24", "25_64", "25_34", "25_29", "30_34", "35_44", "45_54", "55_64", "65_74", "75_+")
colnames(emploi_femaleQC) <- c("Profession", "Total_age", "15_24", "15_19", "20_24", "25_64", "25_34", "25_29", "30_34", "35_44", "45_54", "55_64", "65_74", "75_+")
colnames(emploi_totalQC) <- c("Profession", "Total_age", "15_24", "15_19", "20_24", "25_64", "25_34", "25_29", "30_34", "35_44", "45_54", "55_64", "65_74", "75_+")

emploi_maleQCModif <- data.frame( c( emploi_maleQC[["15_19"]][1:11] * (4/5) + emploi_maleQC[["20_24"]][1:11] + emploi_maleQC[["25_34"]][1:11] * (2 / 10), 0) , c(emploi_maleQC[["30_34"]][1:11] * (3/5) + emploi_maleQC[["35_44"]][1:11] * (5/10), 0) , c(emploi_maleQC[["35_44"]][1:11] * (5/10) + emploi_maleQC[["45_54"]][1:11] * (8/10), 0) , c( emploi_maleQC[["45_54"]][1:11] * (2/10) + emploi_maleQC[["55_64"]][1:11] + emploi_maleQC[["65_74"]][1:11] * (1/10), 0) , c(rep(0, 11), 1) )

colnames(emploi_maleQCModif) <- c("16_26", "32_39", "40_52", "53_65", "66_76")
rownames(emploi_maleQCModif) <- c("Total", "Gestion", "Affaire", "Science_naturelle", "Santé", "Enseignement_service_sociaux", "arts_sport", "Vente_service" ,"Metiers_transport", "ressources_nat", "Service_public", "Retraite" )

emploi_femaleQCModif <- data.frame( c( emploi_femaleQC[["15_19"]][1:11] * (4/5) + emploi_femaleQC[["20_24"]][1:11] + emploi_femaleQC[["25_34"]][1:11] * (2 / 10), 0) , c(emploi_femaleQC[["30_34"]][1:11] * (3/5) + emploi_femaleQC[["35_44"]][1:11] * (5/10), 0) , c(emploi_femaleQC[["35_44"]][1:11] * (5/10) + emploi_femaleQC[["45_54"]][1:11] * (8/10), 0) , c( emploi_femaleQC[["45_54"]][1:11] * (2/10) + emploi_femaleQC[["55_64"]][1:11] + emploi_femaleQC[["65_74"]][1:11] * (1/10), 0) , c(rep(0, 11), 1) )

colnames(emploi_femaleQCModif) <- c("16_26", "32_39", "40_52", "53_65", "66_76")
rownames(emploi_femaleQCModif) <- c("Total", "Gestion", "Affaire", "Science_naturelle", "Santé", "Enseignement_service_sociaux", "arts_sport", "Vente_service" ,"Metiers_transport", "ressources_nat", "Service_public", "Retraite" )

emploi_totalQCModif <- data.frame( c( emploi_totalQC[["15_19"]][1:11] * (4/5) + emploi_totalQC[["20_24"]][1:11] + emploi_totalQC[["25_34"]][1:11] * (2 / 10), 0) , c(emploi_totalQC[["30_34"]][1:11] * (3/5) + emploi_totalQC[["35_44"]][1:11] * (5/10), 0) , c(emploi_totalQC[["35_44"]][1:11] * (5/10) + emploi_totalQC[["45_54"]][1:11] * (8/10), 0) , c( emploi_totalQC[["45_54"]][1:11] * (2/10) + emploi_totalQC[["55_64"]][1:11] + emploi_totalQC[["65_74"]][1:11] * (1/10), 0) , c(rep(0, 11), 1) )

colnames(emploi_totalQCModif) <- c("16_26", "32_39", "40_52", "53_65", "66_76")
rownames(emploi_totalQCModif) <- c("Total", "Gestion", "Affaire", "Science_naturelle", "Santé", "Enseignement_service_sociaux", "arts_sport", "Vente_service" ,"Metiers_transport", "ressources_nat", "Service_public", "Retraite" )


#Écriture CSV
write.csv(emploi_maleQCModif, paste(path, "/Dataset/DatasetModif/dataEmploi_male.csv", sep = ""), fileEncoding = "UTF-8")
write.csv(emploi_femaleQCModif, paste(path, "/Dataset/DatasetModif/dataEmploi_female.csv", sep = ""), fileEncoding = "UTF-8")
write.csv(emploi_totalQCModif, paste(path, "/Dataset/DatasetModif/dataEmploi.csv", sep = ""), fileEncoding = "UTF-8")

# Test hypothèse indépendance ####
# Chi square
## Test prob indépendance des variables

propTest <- dataset$Male[15] / dataset$Total[8]
propTest
propTest * dataset$Total[8]

# Prob d'être âgé de 20 à 24 ans FOIS prob d'être un homme au canada
probCJT <- dataset$Total[15] / dataset$Total[8]  * dataset$Male[8] / dataset$Total[8] 
probCJT * dataset$Total[8]

# Prob d'être divorcé homme au canada entre 20 et 24 ans
dataset$Total[15] / dataset$Total[8]  * dataset$Male[8] / dataset$Total[8] * dataset$Total[66] / dataset$Total[8] * dataset$Total[8]
