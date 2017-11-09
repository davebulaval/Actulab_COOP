#Doit utiliser le fichier serveur pour obtenir les dataset

#### persona 1 ####
probProduitCumulCity <- 1
probProduitCumulCity <- probProduitCumulCity * 
     ((dataEducation[1,10] + dataEducation[4,10]) / sum(dataSetPredict$Pop))[[1]] *
     dataSetPredictCity$Pop_Male_16_26 / 
     dataSetPredictCity$Pop * (dataSetPredictCity$sal_2 / dataSetPredictCity$Pop)

predictionCity_1 <- round(probProduitCumulCity * dataSetPredictCity$Pop)
predictionCity_1
write.csv(predictionCity_1, "C:/Users/David/Documents/GitHub/Actulab_COOP/rapport/DatasetModif/predictionCity_1.csv")

Laval_1 <- data.frame(predictionCity_1, dataSetPredictCity$Pop_Male_16_26, 
                    ((dataEducation[1,10] + dataEducation[4,10]) / sum(dataSetPredict$Pop)), dataSetPredictCity$sal_2)
rownames(Laval_1) <- c(dataSetPredictCity$GEO_CODE)
colnames(Laval_1) <- c("Prediction", "Pop homme 16_26", "Pop etudiante", "Salaire")
Laval_1
write.csv(Laval_1, "C:/Users/David/Documents/GitHub/Actulab_COOP/rapport/DatasetModif/Laval_1.csv")


library(fitdistrplus)
fit.normal <- fitdist(predictionCity,"norm")
fit.gamma <- fitdist(predictionCity, "gamma")
fit.lognormal <- fitdist(predictionCity, "lnorm")
fit.weibull <- fitdist(predictionCity, "weibull")

plot(fit.normal)
plot(fit.gamma)
plot(fit.lognormal)
plot(fit.weibull)

#### persona 2 ####
probProduitCumulCity <- 1
probProduitCumulCity <- probProduitCumulCity  * dataSetPredictCity$Pop_Female_32_39 / 
     dataSetPredictCity$Pop * (dataSetPredictCity$sal_6 / dataSetPredictCity$Pop) * 
     ( (dataEmploi[2, 2] + dataEmploi[3, 2] + dataEmploi[8, 2]) / 
            sum(dataSetPredict$Pop))[[1]]


predictionCity_2 <- round(probProduitCumulCity * dataSetPredictCity$Pop)
predictionCity_2
write.csv(predictionCity_2, "C:/Users/David/Documents/GitHub/Actulab_COOP/rapport/DatasetModif/predictionCity_2.csv")

Laval_2 <- data.frame(predictionCity_2, dataSetPredictCity$Pop_Female_32_39, 
                      (dataEmploi[2, 2] + dataEmploi[3, 2] + dataEmploi[8, 2]) / 
                           sum(dataSetPredict$Pop))[[1]], dataSetPredictCity$sal_6)
rownames(Laval_2) <- c(dataSetPredictCity$GEO_CODE)
colnames(Laval_2) <- c("Prediction", "Pop femme 32_39", "Pop service", "Salaire")
Laval_2
write.csv(Laval_2, "C:/Users/David/Documents/GitHub/Actulab_COOP/rapport/DatasetModif/Laval_2.csv")


library(fitdistrplus)
fit.normal <- fitdist(predictionCity_2 ,"norm")
fit.gamma <- fitdist(predictionCity_2 , "gamma")
fit.lognormal <- fitdist(predictionCity_2 , "lnorm")

plot(fit.normal)
plot(fit.gamma)
plot(fit.lognormal)
plot(fit.weibull)

#### Persona 3 ####
probProduitCumulCity <- 1
probProduitCumulCity <- probProduitCumulCity * dataSetPredictCity$Pop_Male_40_52 / 
     dataSetPredictCity$Pop * (dataSetPredictCity$sal_11 / dataSetPredictCity$Pop) * 
     ( (dataEmploi[2, 3] + dataEmploi[3, 3]) / 
            sum(dataSetPredict$Pop))[[1]]


predictionCity_3 <- round(probProduitCumulCity * dataSetPredictCity$Pop)
predictionCity_3
write.csv(predictionCity_3, "C:/Users/David/Documents/GitHub/Actulab_COOP/rapport/DatasetModif/predictionCity_3.csv")

Laval_3 <- data.frame(predictionCity_3, dataSetPredictCity$Pop_Male_40_52, 
                      ( (dataEmploi[2, 3] + dataEmploi[3, 3]) / 
                             sum(dataSetPredict$Pop))[[1]], dataSetPredictCity$sal_11)
rownames(Laval_3) <- c(dataSetPredictCity$GEO_CODE)
colnames(Laval_3) <- c("Prediction", "Pop homme 40_52", "Pop PME", "Salaire")
Laval_3
write.csv(Laval_3, "C:/Users/David/Documents/GitHub/Actulab_COOP/rapport/DatasetModif/Laval_3.csv")


library(fitdistrplus)
fit.normal <- fitdist(predictionCity_3 ,"norm")
fit.gamma <- fitdist(predictionCity_3 , "gamma")
fit.lognormal <- fitdist(predictionCity_3 , "lnorm")

plot(fit.normal)
plot(fit.gamma)
plot(fit.lognormal)
plot(fit.weibull)

#### Persona 4 ####
probProduitCumulCity <- 1
probProduitCumulCity <- probProduitCumulCity * dataSetPredictCity$Pop_Female_53_65 / 
     dataSetPredictCity$Pop * (dataSetPredictCity$sal_6 / dataSetPredictCity$Pop) * 
     ( dataEmploi[6, 4] / 
            sum(dataSetPredict$Pop))[[1]]

predictionCity_4 <- round(probProduitCumulCity * dataSetPredictCity$Pop)
predictionCity_4
write.csv(predictionCity_4, "C:/Users/David/Documents/GitHub/Actulab_COOP/rapport/DatasetModif/predictionCity_4.csv")

Laval_4 <- data.frame(predictionCity_4, dataSetPredictCity$Pop_Female_53_65, 
                      ( dataEmploi[6, 4] / 
                             sum(dataSetPredict$Pop))[[1]], dataSetPredictCity$sal_6)
rownames(Laval_4) <- c(dataSetPredictCity$GEO_CODE)
colnames(Laval_4) <- c("Prediction", "Pop femme 53_65", "Pop enseign", "Salaire")
Laval_4
write.csv(Laval_4, "C:/Users/David/Documents/GitHub/Actulab_COOP/rapport/DatasetModif/Laval_4.csv")


library(fitdistrplus)
fit.normal <- fitdist(predictionCity_4 ,"norm")
fit.gamma <- fitdist(predictionCity_4 , "gamma")
fit.lognormal <- fitdist(predictionCity_4 , "lnorm")

plot(fit.normal)
plot(fit.gamma)
plot(fit.lognormal)
plot(fit.weibull)

#### Persona 5 ####
probProduitCumulCity <- 1
probProduitCumulCity <- probProduitCumulCity * dataSetPredictCity$Pop_Male_66_76 / 
     dataSetPredictCity$Pop * (dataSetPredictCity$sal_5 / dataSetPredictCity$Pop) * 1

predictionCity_5 <- round(probProduitCumulCity * dataSetPredictCity$Pop)
predictionCity_5
write.csv(predictionCity_5, "C:/Users/David/Documents/GitHub/Actulab_COOP/rapport/DatasetModif/predictionCity_5.csv")

Laval_5 <- data.frame(predictionCity_5, dataSetPredictCity$Pop_Male_66_76, 
                      ( dataEmploi[12, 5] / 
                             sum(dataSetPredict$Pop))[[1]], dataSetPredictCity$sal_5)
rownames(Laval_5) <- c(dataSetPredictCity$GEO_CODE)
colnames(Laval_5) <- c("Prediction", "Pop homme 66_76", "Pop retraite", "Salaire")
Laval_5
write.csv(Laval_5, "C:/Users/David/Documents/GitHub/Actulab_COOP/rapport/DatasetModif/Laval_5.csv")


library(fitdistrplus)
fit.normal <- fitdist(predictionCity_5 ,"norm")
fit.gamma <- fitdist(predictionCity_5 , "gamma")
fit.lognormal <- fitdist(predictionCity_5 , "lnorm")

plot(fit.normal)
plot(fit.gamma)
plot(fit.lognormal)
