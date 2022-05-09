library(shiny)
library(leaflet)
library(sp)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)

cities_merges_coords <- c('(45.764043, 4.835659)' , '(44.933393, 4.89236)', '(44.556944, 4.749496)', '(43.949317, 4.805528)')
cities_lat <- c(45.764043, 44.933393, 44.556944, 43.949317)
cities_lng <- c(4.835659, 4.89236, 4.749496, 4.805528)
cities_coords <- data.frame(cities_merges_coords, cities_lat, cities_lng)
colnames(cities_coords) <- c('coordonnees', 'lat', 'lng')
rownames(cities_coords) <- c('lyon-bron', 'valence', 'montelimar-ancone', 'avignon')

updateTMJA <- function() {
  TMJA <- read_excel("/Users/paulfaguet/Desktop/Projet-IA-Rhone/TMJA_moyennes.xlsx")
  as.data.frame(TMJA)
  colnames(TMJA) <- c('year', 'TMJA_T1', 'TMJA_T2', 'TMJA_T3', 'RatioPL_T1', 'RatioPL_T2', 'RatioPL_T3', 'Nb_PL_T1', 'Nb_PL_T2', 'Nb_PL_T3')
  TMJA <- transform(TMJA, year = as.character(year))
  TMJA['TMJA_T1'] <- round(TMJA['TMJA_T1'], 0)
  TMJA['TMJA_T2'] <- round(TMJA['TMJA_T2'], 0)
  TMJA['TMJA_T3'] <- round(TMJA['TMJA_T3'], 0)
  TMJA['Nb_PL_T1'] <- round(TMJA['Nb_PL_T1'], 0)
  TMJA['Nb_PL_T2'] <- round(TMJA['Nb_PL_T2'], 0)
  TMJA['Nb_PL_T3'] <- round(TMJA['Nb_PL_T3'], 0)
  return(TMJA)
}

TMJA <- updateTMJA()
sapply(temperature_datas, mode)
view(TMJA)

temperature_datas <- read_excel("/Users/paulfaguet/Desktop/Projet-IA-Rhone/temperature_villes_annees.xlsx")
temperature_datas <- t(temperature_datas)
temperature_datas <- as.data.frame(temperature_datas)
temperature_datas <- temperature_datas[-1,]
rownames(temperature_datas) <- c(1:10)
colnames(temperature_datas) <- c('year', 'Température_T1', 'Température_T2', 'Température_T3')
temperature_datas <- transform(temperature_datas, year = as.numeric(year))
colnames(temperature_datas) <- c('year', 'Température_T1', 'Température_T2', 'Température_T3')
view(temperature_datas)

ui <- dashboardPage(
  dashboardHeader(title = "Projet IA"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(
        leafletOutput("map_rhone", height = 700),
        height = 750
      ),
      box(
        selectInput(
          inputId = 'troncon', 
          choices = c('Lyon-Valence', 'Valence-Montélimar', 'Montélimar-Avignon'), 
          label = "Choisissez un tronçon de l'A7",
          width = '50%'
        ),
        title = "Évolution du trafic routier sur l'A7",
        plotOutput("trafic_plot"),
        tableOutput('trafic_table'),
        #infoBox(title = "Trafic moyen journalier annuel en 2010"),
        #infoBoxOutput('trafic_2019_infobox'),
        #infoBox(title = "evolution qualite de l'air")
        #plotOutput('air_quality_plot', width = '100%')
      )
    )
  )
)

server <- function(input, output, session) { 
  
  output$map_rhone <- renderLeaflet({
    leaflet(cities_coords) %>%
      setView(zoom = 8, lat = 44.739776, lng = 4.787098) %>%
      addTiles(
          #urlTemplate = "https://tile.jawg.io/jawg-terrain/{z}/{x}/{y}.png?access-token=ZR6n0aKfW6aoU1Pa9hV58bYyeqYkudIHTH9rsWQzN99G012BkHnFTiZkZhPJLUl2"
          #urlTemplate = "https://tile.jawg.io/9f1dddd5-2beb-486f-96e5-da456d6241c1/{z}/{x}/{y}.png?access-token=ZR6n0aKfW6aoU1Pa9hV58bYyeqYkudIHTH9rsWQzN99G012BkHnFTiZkZhPJLUl2"
          urlTemplate = "https://tile.jawg.io/3b1e49d6-9654-45cd-b9fe-05e6e8ade00d/{z}/{x}/{y}.png?access-token=ZR6n0aKfW6aoU1Pa9hV58bYyeqYkudIHTH9rsWQzN99G012BkHnFTiZkZhPJLUl2"  
        ) %>%
      addCircleMarkers(
        lat = cities_coords$lat,
        lng = cities_coords$lng,
        stroke = FALSE,
        fillOpacity = 0.5
      )
  })

  getTroncon <- reactive({
    switch(input$troncon, "Lyon-Valence" = "T1", "Valence-Montélimar" = "T2", "Montélimar-Avignon" = "T3", "Tous" = "TMJA_T1, TMJA_T2, TMJA_T3")
  })
  
  output$trafic_table <- renderTable({
    chosen_troncon <- getTroncon()
    
    troncon <- gsub(" ", "", paste("TMJA_", chosen_troncon))
    ratio_pl <- gsub(" ", "", paste("RatioPL_", chosen_troncon))
    nombre_pl <- gsub(" ", "", paste("Nb_PL_", chosen_troncon))
    temperature <- gsub(" ", "", paste("Température_", chosen_troncon))
    
    first_table <- TMJA %>%
      select(year, troncon, ratio_pl, nombre_pl) %>%
      rename("Trafic Moyen Journalier" = troncon, "Années" = year, "Ratio PL" = ratio_pl, "Nombre PL" = nombre_pl)
    second_table <- temperature_datas %>%
      select(temperature) %>%
      rename("Température" = temperature)
    table_zer <- cbind(first_table, as.data.frame(second_table))
  },
  align = 'c'
  )
  
  output$trafic_plot <- renderPlot({
    chosen_troncon <- getTroncon()
    
    troncon <- gsub(" ", "", paste("TMJA_", chosen_troncon))
    ratio_pl <- gsub(" ", "", paste("RatioPL_", chosen_troncon))
    nombre_pl <- gsub(" ", "", paste("Nb_PL_", chosen_troncon))
    temperature <- gsub(" ", "", paste("Température_", chosen_troncon))
    
    coeff <- 3000
    plot_zer <- TMJA %>%
      select(year, troncon, nombre_pl, ratio_pl) %>%
      rename("Trafic Moyen Journalier" = troncon, "Année" = year, "Nombre PL" = nombre_pl, 'Ratio PL' = ratio_pl)
    ggplot(plot_zer) +
      geom_col(aes(x = `Année`, y = `Trafic Moyen Journalier`), fill = 'grey') + geom_col(aes(x = `Année`, y = `Nombre PL`), size = 1, fill = 'lightgreen') +
      geom_line(aes(x = `Année`, y = `Ratio PL`*coeff), size = 1.5, color = 'red', group = 1) + geom_point(aes(x = `Année`, y = `Ratio PL`*coeff)) + scale_y_continuous(sec.axis = sec_axis(~./coeff, name = 'Nb PL')) 
  })
}

shinyApp(ui, server)
