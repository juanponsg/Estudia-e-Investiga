#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rsconnect)
library(maptools)
library(mapproj)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
library(fda)
library(fda.usc)
library(sf)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = shinytheme("superhero"),
    
    # TITULO Y LOGO
    titlePanel( div(column(width = 8, height = 8, h1("Estudia e Investiga 2020-2021")),
                    column(width = 4, height = 6, tags$img(src='Logo.png'))),
                windowTitle="Estudia e Investiga 2020-2021"),
    
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            # INTRO
            p("A continuació anem a comentar una sèrie de casos amb  Proves Diagnòstiques d’Infecció Activa, més conegut com PDIA, positives en la Comunitat Valenciana, segons data de diagnòstic de laboratori. Les PDIA inclouen dues proves de detecció d’infecció activa, la PCR (sigles de polymerase chain reaction, reacció en cadena de la polimerasa) i el test d’antígens, d’acord amb els procediments i documents tècnics vigents elaborats pel Ministeri de Sanitat."),
            
            # BOTON DE SELEECION DE LA ZONA
            selectInput (inputId = "Zona",
                         label = h4("ZONA:"),
                         selected = "C.Valenciana",
                         choices = c("C.Valenciana" = "C.Valenciana"
                                     , "Homes" = "Homes/Hombres"
                                     , "Dones" = "Dones/Mujeres"
                                     , "Prov. Alacant" = "Prov. Alacant/Alicante"
                                     , "Prov. Castelló" = "Prov. Castelló/Castellón"
                                     , "Prov. València" = "Prov. València"
                                     , "Dep. Vinaros" = "DEPARTAMENT DE SALUT DE VINAROS"
                                     , "Dep. Castelló" = "DEPARTAMENT DE SALUT DE CASTELLO"
                                     , "Dep. La Plana" = "DEPARTAMENT DE SALUT DE LA PLANA"
                                     , "Dep. Sagunt" = "DEPARTAMENT DE SALUT DE SAGUNT"
                                     , "Dep. VCIA Clinic-Malva Rosa" = "DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA"
                                     , "Dep. VCIA Aranau Vilanova Lliria" = "DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA"
                                     , "Dep. Valencia La Fe" = "DEPARTAMENT DE SALUT DE VALENCIA - LA FE"
                                     , "Dep. Requena" = "DEPARTAMENT DE SALUT DE REQUENA"
                                     , "Dep. Valencia Hosp. General" = "DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL"
                                     , "Dep. Valencia Docotor Peset" = "DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET"
                                     , "Dep. La Ribera" = "DEPARTAMENT DE SALUT DE LA RIBERA"
                                     , "Dep. Gandia" = "DEPARTAMENT DE SALUT DE GANDIA" 
                                     , "Dep. Denia" = "DEPARTAMENT DE SALUT DE DENIA"
                                     , "Dep. Xativa Ontinyent " = "DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT"
                                     , "Dep. Alcoi" = "DEPARTAMENT DE SALUT D'ALCOI"
                                     , "Dep. La Marina Baixa" = "DEPARTAMENT DE SALUT DE LA MARINA BAIXA"
                                     , "Dep. Sant Joan D'Alacant" = "DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT"
                                     , "Dep. Elda" = "DEPARTAMENT DE SALUT D'ELDA"
                                     , "Dep. Alacant Hosp. General" = "DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL"
                                     , "Dep. Elx Hosp. General" = "DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL"
                                     , "Dep. Orihuela" = "DEPARTAMENT DE SALUT D'ORIHUELA"
                                     , "Dep. Torrevieja" = "DEPARTAMENT DE SALUT DE TORREVIEJA"
                                     , "Dep. Manises" = "DEPARTAMENT DE SALUT DE MANISES"
                                     , "Dep. Elx-Crevillent"= "DEPARTAMENT DE SALUT D'ELX-CREVILLENT"
                         ) 
            ), 
            # FECHA ACTUALIZACION
            tags$p("Última data d'actualització :",data_actualitzacio,"."),
            
            # ENLACE AL COGIDO
            tags$a(href="https://github.com/juanponsg/Estudia-e-Investiga.git", "Codi"),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabPanel(
                title='INICIO', 
                
                # MOSTRAR WIDGETS EN UNA FILA CON TRES COLUMNAS Y SUS CORRESPONDIENTES BOTONES DE INFORMACION
                fluidRow(
                    div(column(4,div(column(3, tags$h4("Mitjana")),column(1,actionButton("btn1", (tags$img(src="info.png",width = 24, height = 24))))), flexdashboard::gaugeOutput("mitjana")),
                        column(4,div(column(4, tags$h4("Incidencia")),column(1,actionButton("btn2", (tags$img(src="info.png",width = 24, height = 24))))), flexdashboard::gaugeOutput("incidencia")),
                        column(4,div(column(7, tags$h4("Mitjana 14 dies")),column(1,actionButton("btn3", (tags$img(src="info.png",width = 24, height = 24))))), flexdashboard::gaugeOutput("mitjana14")))
                ),
                
                # MOSTRAR TITULOS MAPA Y GRAFICA CON LOS BOTONES DE INFORMACION EN UNA LINEA Y DOS COLUMNAS
                fluidRow(
                    div(column(6,div(column(5, tags$blockquote("Mapa")),column(1,actionButton("btn4", (tags$img(src="info.png",width = 24, height = 24)))) ) ),
                        column(6,div(column(5, tags$blockquote("Evolució")),column(1,actionButton("btn5", (tags$img(src="info.png",width = 24, height = 24)))))))
                ),
                
                # MOSTRAR EL MAPA Y LA GRAFICA EN UNA MISMA FILA Y DOS COLUMNAS
                fluidRow(
                    div(column(6, plotOutput("mapa")),
                        column(6, plotOutput("evolucion")), align = "center")
                ) 
            )
            
        )
    )
))
