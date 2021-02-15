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


nombr_dep = c("C.Valenciana" = 1
              , "Homes/Hombres" = 1
              , "Dones/Mujeres" = 1
              , "Prov. Alacant/Alicante" = 1
              , "Prov. Castelló/Castellón" = 1
              , "Prov. València" = 1
              , "DEPARTAMENT DE SALUT DE VINAROS" = "VINAROS"
              , "DEPARTAMENT DE SALUT DE CASTELLO" = "CASTELLON"
              , "DEPARTAMENT DE SALUT DE LA PLANA" = "LA PLANA"
              , "DEPARTAMENT DE SALUT DE SAGUNT" = "SAGUNTO"
              , "DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA" = "VALENCIA - CLINICO"
              , "DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA" =  "VALENCIA - ARNAU DE VILANOVA"
              , "DEPARTAMENT DE SALUT DE VALENCIA - LA FE" = "VALENCIA - LA FE"
              , "DEPARTAMENT DE SALUT DE REQUENA" = "REQUENA"
              , "DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL" = "C.E. JUAN LLORENS - TORRENT - ALD"
              , "DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET" = "VALENCIA - DR. PESET"
              , "DEPARTAMENT DE SALUT DE LA RIBERA" = "LA RIBERA"
              , "DEPARTAMENT DE SALUT DE GANDIA" = "GANDIA"
              , "DEPARTAMENT DE SALUT DE DENIA" = "DENIA"
              , "DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT" = "XATIVA - ONTINYENT"
              , "DEPARTAMENT DE SALUT D'ALCOI" = "ALCOI"
              , "DEPARTAMENT DE SALUT DE LA MARINA BAIXA" = "VILA JOIOSA"
              , "DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT" = "ALICANTE - SAN JUAN"
              , "DEPARTAMENT DE SALUT D'ELDA" = "ELDA"
              , "DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL" = "ALICANTE"
              , "DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL" = "ELX"
              , "DEPARTAMENT DE SALUT D'ORIHUELA" = "ORIHUELA"
              , "DEPARTAMENT DE SALUT DE TORREVIEJA" = "TORREVIEJA"
              , "DEPARTAMENT DE SALUT DE MANISES" = "MANISES"
              , "DEPARTAMENT DE SALUT D'ELX-CREVILLENT" = "ELX-CREVILLENT"
) 
num_dep = c("C.Valenciana" = 0
            , "Homes/Hombres" = 0
            , "Dones/Mujeres" = 0
            , "Prov. Alacant/Alicante" = 0
            , "Prov. Castelló/Castellón" = 0
            , "Prov. València" = 0
            , "DEPARTAMENT DE SALUT DE VINAROS" = 01
            , "DEPARTAMENT DE SALUT DE CASTELLO" = 02
            , "DEPARTAMENT DE SALUT DE LA PLANA" = 03
            , "DEPARTAMENT DE SALUT DE SAGUNT" = 04
            , "DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA" = 05
            , "DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA" =  06
            , "DEPARTAMENT DE SALUT DE VALENCIA - LA FE" = 07
            , "DEPARTAMENT DE SALUT DE REQUENA" = 08
            , "DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL" = 09
            , "DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET" = 10
            , "DEPARTAMENT DE SALUT DE LA RIBERA" = 11
            , "DEPARTAMENT DE SALUT DE GANDIA" = 12
            , "DEPARTAMENT DE SALUT DE DENIA" = 13
            , "DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT" = 14
            , "DEPARTAMENT DE SALUT D'ALCOI" = 15
            , "DEPARTAMENT DE SALUT DE LA MARINA BAIXA" = 16
            , "DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT" = 17
            , "DEPARTAMENT DE SALUT D'ELDA" = 18
            , "DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL" = 19
            , "DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL" = 20
            , "DEPARTAMENT DE SALUT D'ORIHUELA" = 21
            , "DEPARTAMENT DE SALUT DE TORREVIEJA" = 22
            , "DEPARTAMENT DE SALUT DE MANISES" = 23
            , "DEPARTAMENT DE SALUT D'ELX-CREVILLENT" = 24
) 
########################## MAPA

Incidencia = tab$Incidencia[5:28]

mapa <- rgdal::readOGR(
    paste0(dsn = "./", layer ="departamentos_salud_ogr.json")
)


mapa_departamentos <- fortify(model = mapa, region = "DPTO_KEY")

info_municipios <- mapa@data



mapa_departamentos <- mapa_departamentos %>%
    left_join(info_municipios, by = c("id" = "DPTO_KEY"))

info_municipios$INCIDENCIA <- Incidencia
info_municipios$id <- info_municipios$DPTO_KEY

mapa_departamentos <- 
    left_join(
        x = mapa_departamentos,
        y  = info_municipios,
        by = "id"
    )
mapa_departamentos$DEPARTAMENTO <- mapa_departamentos$NOMBRE.x
# Se eliminan puntos (se reduce la resoluciÃÂÃÂ³n)
mapa_departamentos <- mapa_departamentos %>%
    slice(seq(1, nrow(mapa_departamentos), 5))

# Para etiqueratlo

nombre_departamentos <- info_municipios$NOMBRE
numero_incidencias <- info_municipios$INCIDENCIA
lat <- as.numeric(mapa_departamentos$lat)

long <- as.numeric(mapa_departamentos$long)

datos2 <- data.frame(
    "long" = long, "lat" = lat
)

datos1 <- data.frame(
    "id" = c(1:24), "Incidencia" = Incidencia
)

#cordenadaa_departamentos$nombre_departamentos <- nombre_departamentos
datos2$id <- as.numeric(mapa_departamentos$DPTOCRC.x)

datos2 <- left_join(
    x = datos2,
    y = datos1,
    by = "id"
)
cordenadaa_departamentos <- coordinates(mapa)
colnames(cordenadaa_departamentos)  <- c("long", "lat")
cordenadaa_departamentos  <-  as.data.frame(cordenadaa_departamentos)
cordenadaa_departamentos$DEPARTAMENTOS <- nombre_departamentos

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$departamento1 <- renderText(input$Zona)
    output$departamento2 <- renderText(input$Zona)
    output$departamento3 <- renderText(input$Zona)
    output$data_actualizacio <- renderPrint(data_actualitzacio)
    
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
    
    output$evolucion <- renderPlot({
        btn1 <- input$Zona
        btn0 <- "Data diagnòstic laboratori/fecha diagnóstico laboratorio"
        
        
        ggplot(datos_PDIA,aes(x = datos_PDIA[[btn0]], y = datos_PDIA[[btn1]]))+geom_line()+ scale_x_date()+labs(
            x = "Data",
            y = "Nombre de casos"
        )
        
    })
    
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
