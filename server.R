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
    #####                 MAPA                 #####
    ################################################
    # UTILIZAMOS UN CONDICIONAL YA QUE ALGUNAS ZONAS 
    # NO SON UN DEPARTAMENTOS Y ESTOS TIENEN UN 
    # NUM_DEPARTAMENTO CON VALOR 1
    
    output$mapa <- renderPlot({
        valor = nombr_dep[nombr_id[input$Zona]]
        valor2 = num_dep[input$Zona]
        if (valor == 1 ){
            mapa_departamentos %>%
                ggplot(aes(x = long, y = lat, group = group, fill=INCIDENCIA, Zona = DEPARTAMENTO, texto = INCIDENCIA)) +
                geom_polygon(color = "black") +
                coord_map("mercator") +
                scale_fill_viridis_c() + # Para mas vaierdad de colores 
                theme_bw() +
                theme(
                    legend.position="bottom",
                    axis.text.x = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks =  element_blank(),
                    axis.title = element_blank(),
                    panel.border = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank()
                )
        }else {
            #mapa_depart <- mapa_departamentos %>% filter(DEPARTAMENTO == valor)
            mapa_departamentos %>%
                ggplot(aes(x = long, y = lat, group = group, fill=INCIDENCIA, Zona = DEPARTAMENTO, texto = INCIDENCIA)) +
                geom_polygon(color = "black") +
                geom_polygon(
                    data = mapa_departamentos %>% filter(DPTOCRC.x == valor2),
                    aes(x = long, y = lat, group = group),
                    fill = "gray90",
                    color = "black") + 
                geom_polygon(
                    data = mapa_departamentos %>% filter(DEPARTAMENTO == valor),
                    aes(x = long, y = lat, group = group),
                    fill = "firebrick",
                    alpha = 0.5) + 
                geom_label(
                    data = cordenadaa_departamentos %>% filter(DEPARTAMENTOS == valor),
                    aes(x = long, y = lat, label = tab$Incidencia[nombr_id[input$Zona]]),
                    inherit.aes = FALSE,
                    size = 4
                ) +
                coord_map("mercator") +
                scale_fill_viridis_c() + # Para mas vaierdad de colores 
                theme_bw() +
                theme(
                    legend.position="bottom",
                    axis.text.x = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks =  element_blank(),
                    axis.title = element_blank(),
                    panel.border = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank()
                )
        }
    })
    
    
    ################################################
    #####     GRAFICA CASOS POSTIVIOS          #####
    ################################################
    
    output$evolucion <- renderPlot({
        btn1 <- input$Zona
        btn0 <- "Data diagnÃ²stic laboratori/fecha diagnÃ³stico laboratorio"
        
        
        ggplot(datos_PDIA,aes(x = datos_PDIA[[btn0]], y = datos_PDIA[[btn1]]))+geom_line()+ scale_x_date()+labs(
            x = "Data",
            y = "Nombre de casos"
        )
        
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
        shinyalert(title = input$Zona, text = "Incidencia acumulada en els Ultims 14 dies", type = "info",
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE)
    })
    
    observeEvent(input$btn3, {
        # Show a simple modal
        shinyalert(title = input$Zona, text = "Mitjana dels Ultims 14 dies respecte a la mitjana dels Ultims 14 dies a la Comunitat Valenciana", type = "info",
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE)
    })
    
    observeEvent(input$btn4, {
        # Show a simple modal
        shinyalert(title = input$Zona, text = "Mapa de la Comunitat Valenciana dividt per els departaments de salud.
                                                EL mapa esta acolorit segons la Incidencia en cada departament. 
                                                A mes ens indica la localitzacio del departament i la Incidencia actual."
                   , type = "info",
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE)
    })
    
    observeEvent(input$btn5, {
        # Show a simple modal
        shinyalert(title = input$Zona, text = "Grafica de l'evolucio dels casos positius de Covid-19 des del 2020-01-02 fins l'Ultima data d'actualitzaco"
                   , type = "info",
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE)
    })
    
    
})
