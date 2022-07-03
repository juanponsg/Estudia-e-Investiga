#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)
library(rsconnect)
library(readr)
library(dplyr)
library(flexdashboard)
library(readxl)
library(tidyverse)
library(stringr)
library(rgdal)
library(rgeos)
library(plotly)
library(maptools)
library(mapproj)
library(shinyalert)
library(fda)
library(fda.usc)
library(sf)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    ################################################
    #####       FECHA DE ACTUALIZACION         #####
    ################################################
    output$data_actualizacio <- renderPrint(data_actualitzacio)
    
    ################################################
    #####        WIDGETS RANGO VALORES         #####
    ################################################
    
    output$mitjana <- flexdashboard::renderGauge({
        valor <- tab$Mitjana[nombr_id[input$Zona]]
        gauge(valor, min = 0, max = tab$Mitjana[1], gaugeSectors(
            success = c(0, tab$Mitjana[1]/10), warning = c(tab$Mitjana[1]/10, tab$Mitjana[1]), danger = c(tab$Mitjana[1]/2, tab$Mitjana[1])
        ))
    })
    
    output$incidencia <- flexdashboard::renderGauge({
        valor <- tab$Incidencia[nombr_id[input$Zona]]
        
        gauge(valor, min = 0, max = 250, gaugeSectors(
            success = c(0, 50), warning = c(50, 150), danger = c(150, 250)
        ))
    })
    
    output$mitjana14 <- flexdashboard::renderGauge({
        valor <- round(tab$Casos_Ultims_14_Dies[nombr_id[input$Zona]]/14,3)
        gauge(valor, min = 0, max = tab$Casos_Ultims_14_Dies[1]/14, gaugeSectors(
            success = c(0, tab$Casos_Ultims_14_Dies[1]/140), warning = c(tab$Casos_Ultims_14_Dies[1]/140, tab$Casos_Ultims_14_Dies[1]/28), danger = c(tab$Casos_Ultims_14_Dies[1]/28, tab$Casos_Ultims_14_Dies[1]/14)
        ))
    })
    
    
    ################################################
    #####     GRAFICA CASOS POSTIVIOS          #####
    ################################################
    
    output$evolucion <- renderPlot({
        btn1 <- input$Zona
        btn0 <- "Data diagnòstic laboratori/fecha diagnóstico laboratorio"
        
        
        ggplot(datos_PDIA,aes(x = datos_PDIA[[btn0]], y = datos_PDIA[[btn1]]))+geom_line()+ scale_x_date()+labs(
            x = "Data",
            y = "Nombre de casos"
        )
        
    })
    
    ################################################
    #####                 MAPA                 #####
    ################################################
    # UTILIZAMOS UN CONDICIONAL YA QUE ALGUNAS ZONAS 
    # NO SON UN DEPARTAMENTOS Y ESTOS TIENEN UN 
    # NUM_DEPARTAMENTO CON VALOR 1
    
    output$mapa <- renderPlot({
        valor = nombr_dep[nombr_id[input$Zona]]
        valor2 = num_dep[input$Zona]
        if (valor == 1 ){
        
            INCIDENCIA<-tab$Incidencia[7:30]
            ggplot() +
                geom_sf(data = departamentos_mapa,aes(fill = INCIDENCIA), color = "black")  +  scale_colour_gradientn(colors = terrain.colors(20)) 
        
            
        }else {
            mapa_depart <- mapa_departamentos %>% filter(DEPARTAMENTO == valor)
            INCIDENCIA<-tab$Incidencia[7:30]
            ggplot() +
                geom_sf(data = departamentos_mapa,aes(fill = INCIDENCIA), color = "black")  +  scale_colour_gradientn(colors = terrain.colors(20)) +
                geom_sf(data = departamentos_mapa[valor2,],aes(fill = INCIDENCIA[valor2]), color = "red") +
                geom_sf_text(
                    data = departamentos_mapa[valor2,],
                    aes(label = INCIDENCIA[valor2]),
                    check_overlap = FALSE,
                    size = 5,
                    color = "black") 
        }
    })
    
    ################################################
    #####        BOTONES DE INFORMACON         #####
    ################################################
    
    observeEvent(input$btn1, {
        # Show a simple modal
        shinyalert(title = input$Zona, text = "Mitjana respecte a la mitjana de la Comunitat Valenciana", type = "info",
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE)
    })
    
    observeEvent(input$btn2, {
        # Show a simple modal
        shinyalert(title = input$Zona, text = "Incidència acumulada en els últims 14 dies", type = "info",
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE)
    })
    
    observeEvent(input$btn3, {
        shinyalert(title = input$Zona, text = "Mitjana dels Últims 14 dies respecte a la mitjana dels últims 14 dies a la Comunitat Valenciana", type = "info",
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE)
    })
    

    observeEvent(input$btn4, {
        shinyalert(title = input$Zona, text = "Mapa dels departaments de salut acolorits segons la incidència"
                   , type = "info",
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE)
    })
    
    observeEvent(input$btn5, {
        # Show a simple modal
        shinyalert(title = input$Zona, text = "Gràfica de l'evolució dels casos positius de Covid-19 des del 2020-01-02 fins a la última data d'actualització"
                   , type = "info",
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE)
    })

    
    
})
