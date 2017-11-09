library(leaflet)
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
     
     # Application title
     navbarPage("Sélection des caractéristiques",
                tabPanel("Manuelle",
                         sidebarLayout(
                              sidebarPanel(
                                   fluidRow(
                                        column(6,
                                               selectInput("selectRegion", h3("Sélection de la région"), 
                                                           choices = list("", "Gatineau" = 1, "Laval" = 2, "Longueuil" = 3, 
                                                                          "Montréal" = 4))), 
                                        column(6,
                                               selectInput("selectRevenu", h3("Sélection du revenu"), 
                                                           choices = list("Aucun Choix" = 0, 
                                                                          "moins de 10 000$" = 1, 
                                                                          "entre 10 000$ et 19 999$" = 2, 
                                                                          "entre 20 000$ et 29 999$" = 3, 
                                                                          "entre 30 000$ et 39 999$" = 4, 
                                                                          "entre 40 000$ et 49 999$" = 5, 
                                                                          "entre 50 000$ et 59 999$" = 6, 
                                                                          "entre 60 000$ et 69 999$" = 7, 
                                                                          "entre 70 000$ et 79 999$" = 8,
                                                                          "entre 80 000$ et 89 999$" = 9, 
                                                                          "entre 90 000$ et 99 999$" = 10,
                                                                          "plus de 100 000$" = 11))),
                                        column(6, 
                                               selectInput("selectAge", h3("Intervalle d'âge"), 
                                                           choices = list("Aucun choix" = 0, "entre 16 et 26 ans" = 1, 
                                                                          "entre 32 et 39 ans" = 2, 
                                                                          "entre 40 et 52 ans" = 3, "entre 53 et 65 ans" = 4, 
                                                                          "entre 66 et 76 ans" = 5,
                                                                          "Population entre 16 et 76 ans" = 6))),
                                        column(6, 
                                               selectInput("selectOccupation", h3("Occupation"), 
                                                           choices = list("Aucun choix" = 0, "Etudiant" = 1, 
                                                                          "Gestion" = 2,
                                                                          "Affaires et finance" = 3, 
                                                                          "Sciences naturelles et appliquées" = 4, 
                                                                          "Secteur de la santé" = 5, 
                                                                          "Enseignement, droit et services sociaux" = 6, 
                                                                          "Arts, culture et sports" = 7, 
                                                                          "ventes et service" = 8, 
                                                                          "Métiers, transport et machinerie" = 9, 
                                                                          "Ressources naturelles et agriculture" = 10, 
                                                                          "Service publique" = 11,
                                                                          "Retraite" = 12))),
                                        column(6, 
                                               radioButtons("checkGroupSexe", 
                                                            h3("Sexe"), 
                                                            choices = list("Aucun choix" = 0, "Femme" = 1, 
                                                                           "Homme" = 2, 
                                                                           "Femme et homme" = 3))),
                                        column(6, 
                                               radioButtons("checkGroupHousing", 
                                                            h3("Type d'occupation"), 
                                                            choices = list("Aucun choix" = 0, "Propriétaire" = 1, 
                                                                           "Locataire" = 2))),
                                        column(6, 
                                               radioButtons("checkGroupStatut", 
                                                            h3("État civil"), 
                                                            choices = list("Aucun choix" = 0, "Celibataire" = 1, 
                                                                           "Conjoint de fait/Marié" = 2))),
                                        column(6, 
                                               radioButtons("checkGroupResidence", 
                                                            h3("Type de résidence"), 
                                                            choices = list("Aucun choix", "Appartement" = 1, 
                                                                           "Maison unifamiliale" = 2))),
                                        column(6, 
                                               radioButtons("checkGroupLangue", 
                                                            h3("Bilingue"), 
                                                            choices = list("Oui" = 1, "Non" = 2), selected = 2)),
                                        column(6, 
                                               radioButtons("checkGroupColocation", 
                                                            h3("Colocation"), 
                                                            choices = list("Oui" = 1, "Non" = 2), selected = 2))
                                   ),
                                   fluidRow(
                                        radioButtons("checkGroupEnfant", h3("Parents d'enfant"),
                                                     choices = list("Oui" = 1, "Non" = 2), selected = 2),
                                        conditionalPanel(
                                             condition = "input.checkGroupEnfant == 1",
                                             selectInput("num", "Nombre d'enfants",
                                                         list("1" = 1, "2" = 2, "3 et plus" = 3))
                                        )  
                                   )
                              ),
                              mainPanel(
                                   leafletOutput("map")
                              )
                         )
                ),   
                tabPanel("Personas",
                         sidebarLayout(
                              sidebarPanel(
                                   fluidRow(
                                        column(5, 
                                               radioButtons("checkGroupPersona", 
                                                            h3("Sélectionner le persona"), 
                                                            choices = list("Aucun choix" = 0, 
                                                                           "Jacob" = 1, 
                                                                           "Maria" = 2, 
                                                                           "David" = 3,
                                                                           "Claire" = 4,
                                                                           "Daniel" = 5
                                                            )
                                               )
                                        ),
                                        column(6,
                                               selectInput("selectRegionPersona", h3("Sélection de la région"), 
                                                           choices = list("", "Gatineau" = 1, "Laval" = 2, "Longueuil" = 3, 
                                                                          "Montréal" = 4))) 
                                   )
                              ),
                              mainPanel(
                                   leafletOutput("mapPersona")
                              )
                         )
                )
     )
))