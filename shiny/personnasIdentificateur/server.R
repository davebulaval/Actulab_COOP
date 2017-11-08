#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#




library(shiny)
library(leaflet)
library(readr)

# Import dataset
dataSetPredict <- read_csv("~/GitHub/Actulab_COOP/Dataset/DatasetModif/dataSetPredict.csv", 
                           col_types = cols(X1 = col_skip()))
probColocation <- read_csv("~/GitHub/Actulab_COOP/Dataset/DatasetModif/probColocation.csv",
                           col_types = cols(X1 = col_skip()))
dataEducation <- read_csv("~/GitHub/Actulab_COOP/Dataset/DatasetModif/dataEducation.csv",
                          col_types = cols(X1 = col_skip()))
dataEmploi <- read_csv("~/GitHub/Actulab_COOP/Dataset/DatasetModif/dataEmploi.csv",
                       col_types = cols(X1 = col_skip()))
dataEmploi_female <- read_csv("~/GitHub/Actulab_COOP/Dataset/DatasetModif/dataEmploi_female.csv",
                              col_types = cols(X1 = col_skip()))
dataEmploi_male <- read_csv("~/GitHub/Actulab_COOP/Dataset/DatasetModif/dataEmploi_male.csv",
                            col_types = cols(X1 = col_skip()))

# Variable revenu unisexe OK
# Variable revenu Male/femme OK
# Variable population unisexe OK
# Variable population Male/female OK
# Variable type d'occupation unisexe OK 
# Variable type de résidence unisexe OK
# Variable nombre d'enfant et couple OK
# Variable sexe OK

shinyServer(function(input, output) {
     
     #      probProduitCumul <- 1
     output$map <- renderLeaflet({
          probProduitCumul <- 1
          if(input$selectRegion == 1){
               FSA.shapeCity <- subset(FSA.shapeQC, substr(FSA.shapeQC$RTACIDU, 1, 3) %in% c("J8P", "J8R", 
                                                                                             "J8T", "J8V"))
               dataSetPredictCity <- subset(dataSetPredict, 
                                            substr(dataSetPredict$GEO_CODE, 1, 3) %in% c("J8P", "J8R",
                                                                                         "J8T", "J8V"))
          } else if (input$selectRegion == 2) {
               FSA.shapeCity <- subset(FSA.shapeQC, substr(FSA.shapeQC$RTACIDU, 1, 2) %in% "H7") 
               dataSetPredictCity <- subset(dataSetPredict, 
                                            substr(dataSetPredict$GEO_CODE, 1, 2) %in% "H7")
          } else if (input$selectRegion == 3) {
               FSA.shapeCity <- subset(FSA.shapeQC, substr(FSA.shapeQC$RTACIDU, 1, 3) %in% c("J4G", "J4H", 
                                                                                             "J4J", "J4K", 
                                                                                             "J4L", "J4M", 
                                                                                             "J4N"))
               dataSetPredictCity <- subset(dataSetPredict, 
                                            substr(dataSetPredict$GEO_CODE, 1, 3) %in% c("J4G", "J4H", 
                                                                                         "J4J", "J4K", 
                                                                                         "J4L", "J4M", 
                                                                                         "J4N"))
          } else if (input$selectRegion == 4) {
               FSA.shapeCity <- subset(FSA.shapeQC, substr(FSA.shapeQC$RTACIDU, 1, 2) %in% c("H1", "H2", "H3",
                                                                                             "H4", "H5", "H8",
                                                                                             "H9"))
               dataSetPredictCity <- subset(dataSetPredict, 
                                            substr(dataSetPredict$GEO_CODE, 1, 2) %in% c("H1", "H2", "H3",
                                                                                         "H4", "H5", "H8",
                                                                                         "H9"))
          }
          
          #
          # Variable salaire et sexe-salaire
          #
          if(input$selectRevenu == 1){
               if(input$checkGroupSexe == 1){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_1_f / dataSetPredict$Pop)
               } else if (input$checkGroupSexe == 2){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_1_m / dataSetPredict$Pop)
               } else {
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_1 / dataSetPredict$Pop)
               } 
          } else if (input$selectRevenu == 2){
               if(input$checkGroupSexe == 1){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_2_f / dataSetPredict$Pop)
               } else if (input$checkGroupSexe == 2){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_2_m / dataSetPredict$Pop)
               } else {
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_2 / dataSetPredict$Pop)
               }
          } else if (input$selectRevenu == 3){
               if(input$checkGroupSexe == 1){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_3_f / dataSetPredict$Pop)
               } else if (input$checkGroupSexe == 2){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_3_m / dataSetPredict$Pop)
               } else {
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_3 / dataSetPredict$Pop)
               }
          } else if (input$selectRevenu == 4){
               if(input$checkGroupSexe == 1){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_4_f / dataSetPredict$Pop)
               } else if (input$checkGroupSexe == 2){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_4_m / dataSetPredict$Pop)
               } else {
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_4 / dataSetPredict$Pop)
               }
          } else if (input$selectRevenu == 5){
               if(input$checkGroupSexe == 1){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_5_f / dataSetPredict$Pop)
               } else if (input$checkGroupSexe == 2){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_5_m / dataSetPredict$Pop)
               } else {
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_5 / dataSetPredict$Pop)
               }
          } else if (input$selectRevenu == 6){
               if(input$checkGroupSexe == 1){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_6_f / dataSetPredict$Pop)
               } else if (input$checkGroupSexe == 2){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_6_m / dataSetPredict$Pop)
               } else {
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_6 / dataSetPredict$Pop)
               }
          } else if (input$selectRevenu == 7){
               if(input$checkGroupSexe == 1){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_7_f / dataSetPredict$Pop)
               } else if (input$checkGroupSexe == 2){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_7_m / dataSetPredict$Pop)
               } else {
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_7 / dataSetPredict$Pop)
               }
          } else if (input$selectRevenu == 8){
               if(input$checkGroupSexe == 1){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_8_f / dataSetPredict$Pop)
               } else if (input$checkGroupSexe == 2){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_8_m / dataSetPredict$Pop)
               } else {
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_8 / dataSetPredict$Pop)
               }
          } else if (input$selectRevenu == 9){
               if(input$checkGroupSexe == 1){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_9_f / dataSetPredict$Pop)
               } else if (input$checkGroupSexe == 2){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_9_m / dataSetPredict$Pop)
               } else {
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_9 / dataSetPredict$Pop)
               }
          } else if (input$selectRevenu == 10){
               if(input$checkGroupSexe == 1){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_10_f / dataSetPredict$Pop)
               } else if (input$checkGroupSexe == 2){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_10_m / dataSetPredict$Pop)
               } else {
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_10 / dataSetPredict$Pop)
               }
          } else if (input$selectRevenu == 11){
               if(input$checkGroupSexe == 1){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_11_f / dataSetPredict$Pop)
               } else if (input$checkGroupSexe == 2){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_11_m / dataSetPredict$Pop)
               } else {
                    probProduitCumul <- probProduitCumul * (dataSetPredict$sal_11 / dataSetPredict$Pop)
               }
          }
          
          #
          # Variable occupation
          #
          
          if(input$selectOccupation == 1){ #étudiant
               if(input$selectRegion == 1){
                    probProduitCumul <- probProduitCumul * 
                         ((dataEducation[1,10] + dataEducation[4,10]) / sum(dataSetPredict$Pop))[[1]]
               } else if (input$selectRegion == 2) {
                    probProduitCumul <- probProduitCumul * 
                         ((dataEducation[1,10] + dataEducation[4,10]) / sum(dataSetPredict$Pop))[[1]]
               } else if (input$selectRegion == 3) {
                    probProduitCumul <- probProduitCumul * 
                         ((dataEducation[1,10] + dataEducation[4,10]) / sum(dataSetPredict$Pop))[[1]]
               } else if (input$selectRegion == 4) {
                    probProduitCumul <- probProduitCumul * 
                         ((dataEducation[1,10] + dataEducation[4,10]) / sum(dataSetPredict$Pop))[[1]]
               }
          } else if (input$selectOccupation > 1){ 
               if (input$checkGroupSexe == 1) { #Femme
                    if(as.numeric(input$selectAge)){
                         probProduitCumul <- probProduitCumul * 
                              dataEmploi_female[as.numeric(input$selectOccupation), 
                                                as.numeric(input$selectAge)] / 
                              sum(dataSetPredict$Pop)
                    } else {
                         probProduitCumul <- probProduitCumul * 
                              sum(dataEmploi_female[as.numeric(input$selectOccupation),]) / 
                              sum(dataSetPredict$Pop)
                    }
               } else if (input$checkGroupSexe == 2) { #Homme
                    if(as.numeric(input$selectAge)){
                         probProduitCumul <- probProduitCumul * 
                              dataEmploi_male[as.numeric(input$selectOccupation), 
                                              as.numeric(input$selectAge)] /
                              sum(dataSetPredict$Pop)
                    } else {
                         probProduitCumul <- probProduitCumul * 
                              sum(dataEmploi_male[as.numeric(input$selectOccupation),]) /
                              sum(dataSetPredict$Pop)
                    }
                    
               } else { #Total
                    if(as.numeric(input$selectAge)){
                         probProduitCumul <- probProduitCumul * 
                              dataEmploi[as.numeric(input$selectOccupation), 
                                         as.numeric(input$selectAge)] /
                              sum(dataSetPredict$Pop)
                    } else {
                         probProduitCumul <- probProduitCumul * 
                              sum(dataEmploi[as.numeric(input$selectOccupation), ]) /
                              sum(dataSetPredict$Pop)
                    }
               }
          }
          
          #
          # Variable âge et sexe-âge
          #
          if (input$checkGroupSexe == 1) { #Female
               if (input$selectAge == 1){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Pop_Female_16_26 / 
                         dataSetPredict$Pop
               } else if (input$selectAge == 2){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Pop_Female_32_39 / 
                         dataSetPredict$Pop
               } else if (input$selectAge == 3){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Pop_Female_40_52 / 
                         dataSetPredict$Pop
               } else if (input$selectAge == 4){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Pop_Female_53_65 / 
                         dataSetPredict$Pop
               } else if (input$selectAge == 5){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Pop_Female_66_76 / 
                         dataSetPredict$Pop
               }
          } else if (input$checkGroupSexe == 2){ #Male
               if (input$selectAge == 1){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Pop_Male_16_26 / 
                         dataSetPredict$Pop
               } else if (input$selectAge == 2){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Pop_Male_32_39 / 
                         dataSetPredict$Pop
               } else if (input$selectAge == 3){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Pop_Male_40_52 / 
                         dataSetPredict$Pop
               } else if (input$selectAge == 4){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Pop_Male_53_65 / 
                         dataSetPredict$Pop
               } else if (input$selectAge == 5){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Pop_Male_66_76 / 
                         dataSetPredict$Pop
               }
          } else if (input$checkGroupSexe == 3 | input$checkGroupSexe == 0){ 
               # Unisexe et sans variable sexe
               if (input$selectAge == 1){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Pop_16_26 / 
                         dataSetPredict$Pop
               } else if (input$selectAge == 2){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Pop_32_39 / 
                         dataSetPredict$Pop
               } else if (input$selectAge == 3){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Pop_40_52 / 
                         dataSetPredict$Pop
               } else if (input$selectAge == 4){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Pop_53_65 / 
                         dataSetPredict$Pop
               } else if (input$selectAge == 5){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Pop_66_76 / 
                         dataSetPredict$Pop
               }
          }
          #
          # Variable type d'occupation
          #
          if( input$checkGroupHousing == 1){
               probProduitCumul <- probProduitCumul * dataSetPredict$Propriétaire / 
                    dataSetPredict$Total_Habitation
          } else if ( input$checkGroupHousing == 2){
               probProduitCumul <- probProduitCumul * dataSetPredict$Locataire / 
                    dataSetPredict$Total_Habitation
          }
          
          #
          # Variable état civil et sexe - état civil
          #
          if( input$checkGroupSexe == 1){
               if( input$checkGroupStatut == 1){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Celibataire_F / 
                         dataSetPredict$Pop
               } else if ( input$checkGroupStatut == 2){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Conjoint_fait_F / 
                         dataSetPredict$Pop
               } else if (input$checkGroupStatut == 3){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Marie_F / 
                         dataSetPredict$Pop
               }
          } else if (input$checkGroupSexe == 2){
               if( input$checkGroupStatut == 1){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Celibataire_M / 
                         dataSetPredict$Pop
               } else if ( input$checkGroupStatut == 2){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Conjoint_fait_M / 
                         dataSetPredict$Pop
               } else if (input$checkGroupStatut == 3){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Marie_M / 
                         dataSetPredict$Pop
               }
          } else if (input$checkGroupSexe == 3){
               if( input$checkGroupStatut == 1){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Celibataire / 
                         dataSetPredict$Pop
               } else if ( input$checkGroupStatut == 2){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Conjoint_fait / 
                         dataSetPredict$Pop
               } else if (input$checkGroupStatut == 3){
                    probProduitCumul <- probProduitCumul * dataSetPredict$Marie / 
                         dataSetPredict$Pop
               }
          }
          
          #
          # Variable type de résidence
          #
          if( input$checkGroupResidence == 1){ #Appartement
               if (input$checkGroupHousing == 1){ #Condo
                    probProduitCumul <- probProduitCumul * dataSetPredict$Condo  / 
                         dataSetPredict$Pop
               } else if (input$checkGroupHousing == 2){ #Appartement
                    probProduitCumul <- probProduitCumul * dataSetPredict$Appartement  / 
                         dataSetPredict$Pop
               } else if (input$checkGroupHousing == 0){ #Les deux
                    probProduitCumul <- probProduitCumul * (dataSetPredict$Condo + 
                                                                 dataSetPredict$Appartement)  / 
                         dataSetPredict$Pop
               }
          } else if ( input$checkGroupResidence == 2){ # Maison
               probProduitCumul <- probProduitCumul * dataSetPredict$Maison_Detache / 
                    dataSetPredict$Pop
          }
          
          #
          # Variable billingue
          #
          
          if( input$checkGroupLangue == 1){
               probProduitCumul <- probProduitCumul * dataSetPredict$Bilingue / dataSetPredict$Pop
          }
          
          #
          # Variable colocation, âge-colocation, sexe-âge colocation, sexe-colocation
          #
          
          if (input$checkGroupColocation == 1){
               if (input$selectAge != 0){
                    if (input$checkGroupSexe != 0){
                         probProduitCumul <- probProduitCumul * 
                              probColocation[as.numeric(input$selectAge), 
                                             (4 - as.numeric(input$checkGroupSexe))][[1]]
                    } else {
                         probProduitCumul <- probProduitCumul * 
                              probColocation[as.numeric(input$selectAge),]$Total
                    }
               } else {
                    if (input$checkGroupSexe != 0){
                         probProduitCumul <- probProduitCumul * 
                              probColocation[6, 
                                             (4 - as.numeric(input$checkGroupSexe))]
                    } else {
                         probProduitCumul <- probProduitCumul * probColocation[6, ]$Total
                    }
               }
          }
          
          #
          # Variable enfant et enfant-statut
          #
          if (input$checkGroupStatut == 2){
               if(input$checkGroupEnfant == 1){
                    if(input$num == 1){
                         probProduitCumul <- probProduitCumul * (dataSetPredict$Couple_1) / 
                              dataSetPredict$Pop
                    } else if (input$num == 2){
                         probProduitCumul <- probProduitCumul * dataSetPredict$Couple_2 / 
                              dataSetPredict$Pop
                    } else if (input$num == 3){
                         probProduitCumul <- probProduitCumul * dataSetPredict$Couple_3 / 
                              dataSetPredict$Pop
                    }
               }
          }
          
          if (input$selectRegion != ""){
               if (length(probProduitCumul) > 1){
                    #Calcul de la prédiction
                    prediction <- round(probProduitCumul * dataSetPredict$Pop)
                    # Descriptiosn du popup de la carte
                    descriptions <- paste("<b><FONT COLOR=#31B404> Détails du RTA</FONT></b> <br>",
                                          "<b> RTA: </b> ", FSA.shapeCity$RTACIDU, "<br>",
                                          "<b>Prédiction :</b>", prediction,"<br>",
                                          "<b>Population :</b>", dataSetPredict$Pop,"<br>")
                    # palette de couleur
                    pal <- colorNumeric(
                         palette = "Red",
                         domain = prediction)
                    
                    leaflet(FSA.shapeCity) %>% addTiles() %>%
                         addPolygons(color = "#444444", weight = 1, smoothFactor = 1,
                                     opacity = 1.0, fillOpacity = 0.5,
                                     fillColor = ~colorQuantile("OrRd", unique(prediction))(prediction),
                                     highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                         bringToFront = TRUE)) %>%
                         addMarkers(data = dataSetPredictCity,
                                    ~Long, 
                                    ~Lat,
                                    popup = descriptions)
               } else if (probProduitCumul != 1){
                    #Calcul de la prédiction
                    prediction <- round(probProduitCumul * dataSetPredict$Pop)
                    # Descriptiosn du popup de la carte
                    descriptions <- paste("<b><FONT COLOR=#31B404> Détails du RTA</FONT></b> <br>",
                                          "<b> RTA: </b> ", FSA.shapeCity$RTACIDU, "<br>",
                                          "<b>Prédiction :</b>", prediction,"<br>",
                                          "<b>Population :</b>", dataSetPredict$Pop,"<br>")
                    # palette de couleur
                    pal <- colorNumeric(
                         palette = "Red",
                         domain = prediction)
                    
                    leaflet(FSA.shapeCity) %>% addTiles() %>%
                         addPolygons(color = "#444444", weight = 1, smoothFactor = 1,
                                     opacity = 1.0, fillOpacity = 0.5,
                                     fillColor = ~colorQuantile("OrRd", unique(prediction))(prediction),
                                     highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                         bringToFront = TRUE)) %>%
                         addMarkers(data = dataSetPredictCity,
                                    ~Long, 
                                    ~Lat,
                                    popup = descriptions)
               } else {
                    leaflet(FSA.shapeCity) %>%
                         addPolygons(color = "#444444", weight = 1, smoothFactor = 1,
                                     opacity = 1.0, fillOpacity = 0.5,
                                     highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                         bringToFront = TRUE))
               }
          }
     })
})
