library(leaflet)
library(shiny)

source( "load_data.R" )

# Variable revenu unisexe OK
# Variable revenu Male/femme OK
# Variable population unisexe OK
# Variable population Male/female OK
# Variable type d'occupation unisexe OK 
# Variable type de résidence unisexe OK
# Variable nombre d'enfant et couple OK
# Variable sexe OK

shinyServer(function(input, output) {
     
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
          # Variable salaire
          #
          if(input$selectRevenu == 1){
               probProduitCumul <- probProduitCumul * (dataSetPredict$sal_1 / dataSetPredict$Pop)
          } else if (input$selectRevenu == 2){
               probProduitCumul <- probProduitCumul * (dataSetPredict$sal_2 / dataSetPredict$Pop)
          } else if (input$selectRevenu == 3){
               probProduitCumul <- probProduitCumul * (dataSetPredict$sal_3 / dataSetPredict$Pop)
          } else if (input$selectRevenu == 4){
               probProduitCumul <- probProduitCumul * (dataSetPredict$sal_4 / dataSetPredict$Pop)
          } else if (input$selectRevenu == 5){
               probProduitCumul <- probProduitCumul * (dataSetPredict$sal_5 / dataSetPredict$Pop)
          } else if (input$selectRevenu == 6){
               probProduitCumul <- probProduitCumul * (dataSetPredict$sal_6 / dataSetPredict$Pop)
          } else if (input$selectRevenu == 7){
               probProduitCumul <- probProduitCumul * (dataSetPredict$sal_7 / dataSetPredict$Pop)
          } else if (input$selectRevenu == 8){
               probProduitCumul <- probProduitCumul * (dataSetPredict$sal_8 / dataSetPredict$Pop)
          } else if (input$selectRevenu == 9){
               probProduitCumul <- probProduitCumul * (dataSetPredict$sal_9 / dataSetPredict$Pop)
          } else if (input$selectRevenu == 10){
               probProduitCumul <- probProduitCumul * (dataSetPredict$sal_10 / dataSetPredict$Pop)
          } else if (input$selectRevenu == 11){
               probProduitCumul <- probProduitCumul * (dataSetPredict$sal_11 / dataSetPredict$Pop)
          }
          
          #
          # Variable occupation
          #
          
          if(input$selectOccupation == 1){ #étudiant
               probProduitCumul <- probProduitCumul * 
                    ((dataEducation[1,10] + dataEducation[4,10]) / sum(dataSetPredict$Pop))[[1]]
          } else if (input$selectOccupation > 1){ 
               if (input$selectOccupation == 12){
                    if(input$selectAge == 5){
                         probProduitCumul <- probProduitCumul * 1 # retraite ""obligatoire"" à + 65 ans. Voir hypothèses
                    } else {
                         probProduitCumul <- probProduitCumul * 0 # Aucune retraite avant 65 ans
                    }
               } else {
                    if(input$selectAge > 0){
                         probProduitCumul <- probProduitCumul * 
                              ( dataEmploi[as.numeric(input$selectOccupation), 
                                           as.numeric(input$selectAge)] /
                                     sum(dataSetPredict$Pop))[[1]]
                    } else {
                         probProduitCumul <- probProduitCumul * 
                              ( sum(dataEmploi[as.numeric(input$selectOccupation), ]) /
                                     sum(dataSetPredict$Pop))[[1]]
                    }
               }
          }
          
          #
          # Variable sexe
          #
          if (input$checkGroupSexe == 1){
               probProduitCumul <- probProduitCumul * dataSetPredict$Pop_female / 
                    dataSetPredict$Pop
          } else if (input$checkGroupSexe == 2){
               probProduitCumul <- probProduitCumul * dataSetPredict$Pop_male / 
                    dataSetPredict$Pop
          } else if (input$checkGroupSexe == 3){
               probProduitCumul <- probProduitCumul * 1 #ensemble population
          }
          
          #
          # Variable âge 
          #
          if (input$checkGroupSexe == 0){ 
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
               } else if (input$selectAge == 6){
                    probProduitCumul <- probProduitCumul * (dataSetPredict$Pop_16_26 + 
                                                                 dataSetPredict$Pop_32_39 + 
                                                                 dataSetPredict$Pop_40_52 + 
                                                                 dataSetPredict$Pop_53_65 + 
                                                                 dataSetPredict$Pop_66_76) / 
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
          # Variable état civil
          #
          if( input$checkGroupSexe == 1){
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
                    probProduitCumul <- probProduitCumul * 
                         probColocation[as.numeric(input$selectAge),]$Total
               } else {
                    probProduitCumul <- probProduitCumul * probColocation[6, ]$Total
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
                    if (length(unique(prediction)) == 1){
                         descriptions <- paste("<b><FONT COLOR=#31B404> Détails du RTA</FONT></b> <br>",
                                               "<b> RTA: </b> ", FSA.shapeCity$RTACIDU, "<br>",
                                               "<b>Prédiction :</b>", prediction,"<br>",
                                               "<b>Population :</b>", dataSetPredict$Pop,"<br>")%>% lapply(htmltools::HTML) 
                         leaflet(FSA.shapeCity) %>% addTiles() %>%
                              addPolygons(color = "#444444", weight = 1, smoothFactor = 1,
                                          opacity = 1.0, fillOpacity = 0.5,
                                          fillColor = "White",
                                          highlightOptions = highlightOptions(color = "Black", weight = 2,
                                                                              bringToFront = TRUE),
                                          label = descriptions,
                                          labelOptions = labelOptions( 
                                               style = list("front-weight" = "normal", padding = "3px 8px"), 
                                               textsize = '15px', 
                                               direction = 'auto' 
                                          )) 
                    } else {
                                             # Descriptiosn du popup de la carte
                    descriptions <- paste("<b><FONT COLOR=#31B404> Détails du RTA</FONT></b> <br>",
                                          "<b> RTA: </b> ", FSA.shapeCity$RTACIDU, "<br>",
                                          "<b>Prédiction :</b>", prediction,"<br>",
                                          "<b>Population :</b>", dataSetPredict$Pop,"<br>")%>% lapply(htmltools::HTML) 
                    leaflet(FSA.shapeCity) %>% addTiles() %>%
                         addPolygons(color = "#444444", weight = 1, smoothFactor = 1,
                                     opacity = 1.0, fillOpacity = 0.5,
                                     fillColor = ~colorQuantile("OrRd", unique(prediction))(prediction),
                                     highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                         bringToFront = TRUE),
                                     label = descriptions,
                                     labelOptions = labelOptions( 
                                          style = list("front-weight" = "normal", padding = "3px 8px"), 
                                          textsize = '15px', 
                                          direction = 'auto' 
                                     ))
                    }
               } else {
                    #Calcul de la prédiction
                    prediction <- round(probProduitCumul * dataSetPredict$Pop)
                    # Descriptiosn du popup de la carte
                    descriptions <- paste("<b><FONT COLOR=#31B404> Détails du RTA</FONT></b> <br>",
                                          "<b> RTA: </b> ", FSA.shapeCity$RTACIDU, "<br>",
                                          "<b>Prédiction :</b>", prediction,"<br>",
                                          "<b>Population :</b>", dataSetPredict$Pop,"<br>")%>% lapply(htmltools::HTML) 
                    leaflet(FSA.shapeCity) %>% addTiles() %>%
                         addPolygons(color = "#444444", weight = 1, smoothFactor = 1,
                                     opacity = 1.0, fillOpacity = 0.5,
                                     fillColor = ~colorQuantile("OrRd", unique(prediction))(prediction),
                                     highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                         bringToFront = TRUE),
                                     label = descriptions,
                                     labelOptions = labelOptions( 
                                          style = list("front-weight" = "normal", padding = "3px 8px"), 
                                          textsize = '15px', 
                                          direction = 'auto' 
                                     )) 
               }
          }
     }) #map
     
     output$mapPersona <- renderLeaflet({
          probProduitCumul <- 1
          if(input$selectRegionPersona == 1){
               FSA.shapeCity <- subset(FSA.shapeQC, substr(FSA.shapeQC$RTACIDU, 1, 3) %in% c("J8P", "J8R", 
                                                                                             "J8T", "J8V"))
               dataSetPredictCity <- subset(dataSetPredict, 
                                            substr(dataSetPredict$GEO_CODE, 1, 3) %in% c("J8P", "J8R",
                                                                                         "J8T", "J8V"))
          } else if (input$selectRegionPersona == 2) {
               FSA.shapeCity <- subset(FSA.shapeQC, substr(FSA.shapeQC$RTACIDU, 1, 2) %in% "H7") 
               dataSetPredictCity <- subset(dataSetPredict, 
                                            substr(dataSetPredict$GEO_CODE, 1, 2) %in% "H7")
          } else if (input$selectRegionPersona == 3) {
               FSA.shapeCity <- subset(FSA.shapeQC, substr(FSA.shapeQC$RTACIDU, 1, 3) %in% c("J4G", "J4H", 
                                                                                             "J4J", "J4K", 
                                                                                             "J4L", "J4M", 
                                                                                             "J4N"))
               dataSetPredictCity <- subset(dataSetPredict, 
                                            substr(dataSetPredict$GEO_CODE, 1, 3) %in% c("J4G", "J4H", 
                                                                                         "J4J", "J4K", 
                                                                                         "J4L", "J4M", 
                                                                                         "J4N"))
          } else if (input$selectRegionPersona == 4) {
               FSA.shapeCity <- subset(FSA.shapeQC, substr(FSA.shapeQC$RTACIDU, 1, 2) %in% c("H1", "H2", "H3",
                                                                                             "H4", "H5", "H8",
                                                                                             "H9"))
               dataSetPredictCity <- subset(dataSetPredict, 
                                            substr(dataSetPredict$GEO_CODE, 1, 2) %in% c("H1", "H2", "H3",
                                                                                         "H4", "H5", "H8",
                                                                                         "H9"))
          }
          
          if (input$checkGroupPersona == 1){
               probProduitCumul <- probProduitCumul * 
                    ((dataEducation[1,10] + dataEducation[4,10]) / sum(dataSetPredict$Pop))[[1]] *
                    dataSetPredict$Pop_Male_16_26 / 
                    dataSetPredict$Pop * (dataSetPredict$sal_2 / dataSetPredict$Pop)
          } else if (input$checkGroupPersona == 2){
               # Comptable fait parti de catégorie gestion, affaire et service
               probProduitCumul <- probProduitCumul * dataSetPredict$Pop_Female_32_39 / 
                    dataSetPredict$Pop * (dataSetPredict$sal_6 / dataSetPredict$Pop) * 
                    ( (dataEmploi[2, 2] + dataEmploi[3, 2] + dataEmploi[8, 2]) / 
                           sum(dataSetPredict$Pop))[[1]]
          } else if (input$checkGroupPersona == 3){
               probProduitCumul <- probProduitCumul * dataSetPredict$Pop_Male_40_52 / 
                    dataSetPredict$Pop * (dataSetPredict$sal_11 / dataSetPredict$Pop) * 
                    ( (dataEmploi[2, 3] + dataEmploi[3, 3]) / 
                           sum(dataSetPredict$Pop))[[1]]
          } else if (input$checkGroupPersona == 4){
               probProduitCumul <- probProduitCumul * dataSetPredict$Pop_Female_53_65 / 
                    dataSetPredict$Pop * (dataSetPredict$sal_6 / dataSetPredict$Pop) * 
                    ( dataEmploi[6, 4] / 
                           sum(dataSetPredict$Pop))[[1]]
          } else if (input$checkGroupPersona == 5){
               probProduitCumul <- probProduitCumul * dataSetPredict$Pop_Male_66_76 / 
                    dataSetPredict$Pop * (dataSetPredict$sal_5 / dataSetPredict$Pop) 
          }
          if (input$selectRegionPersona != ""){
               #Calcul de la prédiction
               prediction <- round(probProduitCumul * dataSetPredict$Pop)
               # Descriptiosn du popup de la carte
               descriptions <- paste("<b><FONT COLOR=#31B404> Détails du RTA</FONT></b> <br>",
                                     "<b> RTA: </b> ", FSA.shapeCity$RTACIDU, "<br>",
                                     "<b>Prédiction :</b>", prediction,"<br>",
                                     "<b>Population :</b>", dataSetPredict$Pop,"<br>")%>% lapply(htmltools::HTML) 
               
               leaflet(FSA.shapeCity) %>% addTiles() %>%
                    addPolygons(color = "#444444", weight = 1, smoothFactor = 1,
                                opacity = 1.0, fillOpacity = 0.5,
                                fillColor = ~colorQuantile("OrRd", unique(prediction))(prediction),
                                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                    bringToFront = TRUE),
                                label = descriptions,
                                labelOptions = labelOptions( 
                                     style = list("front-weight" = "normal", padding = "3px 8px"), 
                                     textsize = '15px', 
                                     direction = 'auto' 
                                ))
          }
     }) #mapPersona
})
