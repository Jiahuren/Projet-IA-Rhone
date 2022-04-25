library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(SwimmeR)
library(readxl)
library(stringr)
library(labelled)
library(lattice)
library(gapminder)
library(ggforce)
library(gh)
library(globals)
library(openintro)
library(profvis)
library(RSQLite)
library(shinycssloaders)
library(shinythemes)
library(testthat)
library(thematic)
library(tidyverse)
library(vroom)
library(waiter)
library(xml2)
library(zeallot)
library(readxl)
library(sf)
library(tidyverse)
library(dplyr)
library(shiny)
library(shinydashboard)
library(leaflet)
library(maps)

polluants_provence_cote_azur <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Provence Alpes Cote d Azur/polluants provence cote azur.xlsx")
polluants_corse <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Corse/polluants corse.xlsx")
polluants_occitanie <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Occitanie/polluants occitanie.xlsx")
polluants_grand_est <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Grand Est/polluants grand est.xlsx")
polluants_centre_val_de_loire <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Centre Val de Loire/polluants centre val de loire.xlsx")
polluants_nouvelle_aquitaine <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Nouvelle Aquitaine/polluants nouvelle aquitaine.xlsx")
polluants_auvergne_rhone_alpes <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Auvergne Rhone ALpes/polluants auvergne rhone alpes.xlsx")
polluants_pays_de_la_loire <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Pays de la Loire/polluants pays de la loire.xlsx")
polluants_hauts_de_france <- read_excel("C:/Users/William/OneDrive/Bureau/Projet IA/Data Hauts de France/polluants hauts de france.xlsx")

View(polluants_auvergne_rhone_alpes)
View(polluants_centre_val_de_loire)
View(polluants_corse)
View(polluants_grand_est)
View(polluants_hauts_de_france)
View(polluants_nouvelle_aquitaine)
View(polluants_occitanie)
View(polluants_pays_de_la_loire)
View(polluants_provence_cote_azur)

polluants_auvergne_rhone_alpes$date<- as.Date(polluants_auvergne_rhone_alpes$date_debut, format = "%Y/%m/%d" )
polluants_centre_val_de_loire$date<- as.Date(polluants_centre_val_de_loire$date_debut, format = "%Y/%m/%d" )
polluants_corse$date<- as.Date(polluants_corse$date_debut, format = "%Y/%m/%d" )
polluants_grand_est$date<- as.Date(polluants_grand_est$date_debut, format = "%Y/%m/%d" )
polluants_hauts_de_france$date<- as.Date(polluants_hauts_de_france$date_debut, format = "%Y/%m/%d" )
polluants_nouvelle_aquitaine$date<- as.Date(polluants_nouvelle_aquitaine$date_debut, format = "%Y/%m/%d" )
polluants_occitanie$date<- as.Date(polluants_occitanie$date_debut, format = "%Y/%m/%d" )
polluants_pays_de_la_loire$date<- as.Date(polluants_pays_de_la_loire$date_debut, format = "%Y/%m/%d" )
polluants_provence_cote_azur$date<- as.Date(polluants_provence_cote_azur$date_debut, format = "%Y/%m/%d" )

polluants_auvergne_rhone_alpes <- polluants_auvergne_rhone_alpes %>%
  select(nom_com, X, Y, nom_station, nom_poll, valeur, unite, date)

polluants_centre_val_de_loire <- polluants_centre_val_de_loire %>%
  select(nom_com, nom_station, nom_poll, valeur, unite, date)

polluants_corse <-  polluants_corse %>%
  select(nom_com, nom_station, nom_poll, valeur, unite, date)

polluants_grand_est <- polluants_grand_est %>%
  select(nom_com, nom_station, nom_poll, valeur, unite, date)

polluants_hauts_de_france <- polluants_hauts_de_france %>%
  select(nom_com, nom_station, nom_poll, valeur, unite, date)

polluants_nouvelle_aquitaine <- polluants_nouvelle_aquitaine %>%
  select(nom_com, nom_station, nom_poll, valeur, unite, date)

polluants_occitanie <- polluants_occitanie %>%
  select(nom_com, nom_station, nom_poll, valeur, unite, date)

polluants_pays_de_la_loire <- polluants_pays_de_la_loire %>%
  select(nom_com, nom_station, nom_poll, valeur, unite, date)

polluants_provence_cote_azur <- polluants_provence_cote_azur %>%
  select(nom_com, nom_station, nom_poll, valeur, unite, date)

#auvergne rhone alpes
polluants_auvergne_rhone_alpes$nom_com <- factor(polluants_auvergne_rhone_alpes$nom_com)
nom_com_ara <- levels(polluants_auvergne_rhone_alpes$nom_com)
nom_com_ara
polluants_auvergne_rhone_alpes$nom_poll <- factor(polluants_auvergne_rhone_alpes$nom_poll)
nom_poll_ara <- levels(polluants_auvergne_rhone_alpes$nom_poll)
nom_poll_ara

#centre val de loire
polluants_centre_val_de_loire$nom_com <- factor(polluants_centre_val_de_loire$nom_com)
nom_com_cvl <- levels(polluants_centre_val_de_loire$nom_com)
nom_com_cvl
polluants_centre_val_de_loire$nom_poll <- factor(polluants_centre_val_de_loire$nom_poll)
nom_poll_cvl <- levels(polluants_centre_val_de_loire$nom_poll)
nom_poll_cvl

#corse
polluants_corse$nom_com <- factor(polluants_corse$nom_com)
nom_com_corse <- levels(polluants_corse$nom_com)
nom_com_corse
polluants_corse$nom_poll <- factor(polluants_corse$nom_poll)
nom_poll_corse <- levels(polluants_corse$nom_poll)
nom_poll_corse

#grand est
polluants_grand_est$nom_com <- factor(polluants_grand_est$nom_com)
nom_com_ge <- levels(polluants_grand_est$nom_com)
nom_com_ge
polluants_grand_est$nom_poll <- factor(polluants_grand_est$nom_poll)
nom_poll_ge <- levels(polluants_grand_est$nom_poll)
nom_poll_ge

#hauts de france
polluants_hauts_de_france$nom_com <- factor(polluants_hauts_de_france$nom_com)
nom_com_hf <- levels(polluants_hauts_de_france$nom_com)
nom_com_hf
polluants_hauts_de_france$nom_poll <- factor(polluants_hauts_de_france$nom_poll)
nom_poll_hf <- levels(polluants_hauts_de_france$nom_poll)
nom_poll_hf

#nouvelle aquitaine
polluants_nouvelle_aquitaine$nom_com <- factor(polluants_nouvelle_aquitaine$nom_com)
nom_com_aqui <- levels(polluants_nouvelle_aquitaine$nom_com)
nom_com_aqui
polluants_nouvelle_aquitaine$nom_poll <- factor(polluants_nouvelle_aquitaine$nom_poll)
nom_poll_aqui <- levels(polluants_nouvelle_aquitaine$nom_poll)
nom_poll_aqui

#occitanie
polluants_occitanie$nom_com <- factor(polluants_occitanie$nom_com)
nom_com_occi <- levels(polluants_occitanie$nom_com)
nom_com_occi
polluants_occitanie$nom_poll <- factor(polluants_occitanie$nom_poll)
nom_poll_occi <- levels(polluants_occitanie$nom_poll)
nom_poll_occi

#pays de la loire
polluants_pays_de_la_loire$nom_com <- factor(polluants_pays_de_la_loire$nom_com)
nom_com_pl <- levels(polluants_pays_de_la_loire$nom_com)
nom_com_pl
polluants_pays_de_la_loire$nom_poll <- factor(polluants_pays_de_la_loire$nom_poll)
nom_poll_pl <- levels(polluants_pays_de_la_loire$nom_poll)
nom_poll_pl

#provence cote azur
polluants_provence_cote_azur$nom_com <- factor(polluants_provence_cote_azur$nom_com)
nom_com_pca <- levels(polluants_provence_cote_azur$nom_com)
nom_com_pca
polluants_provence_cote_azur$nom_poll <- factor(polluants_provence_cote_azur$nom_poll)
nom_poll_pca <- levels(polluants_provence_cote_azur$nom_poll)
nom_poll_pca

data <- polluants_auvergne_rhone_alpes %>%
  st_as_sf(coords = c("X", "Y")) %>%
  st_set_crs(4326)
  
data <- data %>%
  mutate(popup_ingo = paste('<b>', 'commune :', '</b>', data$nom_com , "<br/>")) %>%
  filter( date == "2022-01-01") %>%
  filter( nom_poll == "dioxyde d'azote")

################################################################################
################################################################################
################################################################################
#Rshiny
ui <- fluidPage(
  
  navbarPage("Projet IA", theme = shinytheme("journal"),
             
             tabPanel("Introduction", fluid = TRUE,
                      
                      h1("Introduction Projet IA"),
                      p("Objectifs refaire ça sur Rshiny pour avoir une bonne 
                        visualisation, modifier les tableaux (éventuellement 
                        récupérer les données automatiquement et ainsi faire des
                        mises à jour automatiquement pour avoir une bdd à jour),
                        et mettre en relation avec d'autres villes, métropoles 
                        françaises en ajoutant plusieurs données."),
                      
                      p("Faire des prévisions sur l'avenir mais aussi comprendre
                        ce qu'il peut se passer.", style = "font-family: 'times'
                        ; font-si16pt"),
                      
                      strong("strong() makes bold text."),
                      em("em() creates italicized (i.e, emphasized) text."),
                      br(),
                      code("code displays your text similar to computer code"),
                      div("div creates segments of text with a similar style. This division of text is
all blue because I passed the argument 'style = color:blue' to div", style =
                            "color:blue"),
                      br(),
                      p("span does the same thing as div, but it works with",
                        span("groups of words", style = "color:blue"),
                        "that appear inside a paragraph.")),
             
             tabPanel("Datas", fluid = TRUE,
                      selectInput(inputId = "region",
                                  label = "Région",
                                  choices = c("AuvergneRhoneAlpes", "BourgogneFrancheComte", "Bretagne", "CentreValdeLoire", "Corse", "GrandEst", "HautsdeFrance", "IledeFrance", "Normandie", "NouvelleAquitaine", "Occitanie", "PaysdelaLoire", "ProvenceAlpesCotedAzur"),
                                  selected = "AuvergneRhoneAlpes"),
                      selectInput(inputId = "Polluant",
                                  label = "Choix du polluant",
                                  choices = c(nom_poll_pl[1], nom_poll_pl[2], nom_poll_pl[3], nom_poll_pl[4], nom_poll_pl[5], nom_poll_pl[6], nom_poll_pl[7], nom_poll_pl[8], nom_poll_pl[9]),
                                  selected = nom_poll_pl[1],
                                  width = "220px"),
                      dataTableOutput("table"),
                      tableOutput("table2")
             ),
             
             
             tabPanel("Carte", fluid = TRUE,
                      textOutput("carte"),
                      selectInput(inputId= "map_polluant",
                                  label = "Polluant",
                                  choices = c(nom_poll_pl[1], nom_poll_pl[2], nom_poll_pl[3], nom_poll_pl[4], nom_poll_pl[5], nom_poll_pl[6], nom_poll_pl[7], nom_poll_pl[8], nom_poll_pl[9]),
                                  selected = nom_poll_pl[1]
                                  ),
                      dateInput(inputId = "date",
                                label = "Date"),
                      dashboardBody(leafletOutput("map"))
                      
             ),
             
          
             tabPanel("Visualisation",  fluid = TRUE,
                      plotOutput(outputId = "plot_ara"),
                      checkboxGroupInput(inputId = "check_ara",
                                         label = "Nom de la Commune", 
                                         choices = c(nom_com_ara[1], nom_com_ara[2], nom_com_ara[3], nom_com_ara[4], nom_com_ara[5]),
                                         selected = nom_com_ara[1]),
                      plotOutput(outputId = "plot_pca"),
                      checkboxGroupInput(inputId = "check_pca",
                                         label = "Nom de la Commune", 
                                         choices = c(nom_com_pca[1], nom_com_pca[2], nom_com_pca[3], nom_com_pca[4], nom_com_pca[5]),
                                         selected = nom_com_pca[1])
                      
             ),
             
             tabPanel("Etude", fluid = TRUE,
                      h1("Etude de la pollution à Lyon avec les autres régions de France")
             ), #ajout de html et de textes
             
             
             tabPanel("Doc", fluid = TRUE,
                      h1("Liens utilisés pour l'étude")
             ) 
             )
)




server <- function(input, output, session){
  
  output$table <- renderDataTable(
    d(), 
    options = list(pageLength = 5)
  )
  
  output$table2 <- renderTable(
    head(d())
  )
  
  
  #Mettre un filtre/select pour n'avoir que certaines valeurs
  
  #output$table2 <- renderTable({
  #  subset(C6H6, select = c("valeur", "date"))
  #})
  
  observe({
    leafletProxy("map", data = data) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      clearMarkers() %>%
      addCircleMarkers(
        radius = 7,
        color = "blue", 
        stroke = FALSE, 
        fillOpacity = 0.3, 
        popup = ~popup_ingo
      )
  })
  
  output$map <- renderLeaflet({
    map <- leaflet() %>% setView(lng = 8.22, lat = 46.81, zoom = 5)
  })
  
  
  
  
  filtre_station_ara <- reactive({
    polluants_auvergne_rhone_alpes %>% 
      filter( nom_station %in%  input$check) %>%
      select(date, valeur )
  })
  output$plot_ara <- renderPlot({
    plot(filtre_station_ara())
  })
  
  filtre_station_pca <- reactive({
    polluants_provence_cote_azur %>% 
      filter( nom_station %in%  input$check_pca) %>%
      select(date, valeur )
  })
  output$plot_pca <- renderPlot({
    plot(filtre_station_pca())
  })
  
  
  
  d <- reactive({
    switch (input$region,
            AuvergneRhoneAlpes = polluants_auvergne_rhone_alpes,
            CentreValdeLoire = polluants_centre_val_de_loire, 
            Corse = polluants_corse, 
            GrandEst = polluants_grand_est,
            HautsdeFrance = polluants_hauts_de_france,
            NouvelleAquitaine = polluants_nouvelle_aquitaine,
            Occitanie = polluants_occitanie,
            PaysdelaLoire= polluants_pays_de_la_loire,
            ProvenceAlpesCotedAzur= polluants_provence_cote_azur
    ) 
    
    
  })
  
  
}






