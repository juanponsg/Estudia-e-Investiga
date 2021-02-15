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


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = shinytheme("superhero"),
    
    # Application title
    #titlePanel("Estudia e Investiga 2020-2021"),
    #img(src='Logo.png', align = "left"),
    titlePanel( div(column(width = 8, height = 8, h1("Estudia e Investiga 2020-2021")),
                    column(width = 4, height = 6, tags$img(src='Logo.png'))),
                windowTitle="Estudia e Investiga 2020-2021"),
    
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            tags$p("A continuació anem a comentar una sèrie de casos amb  Proves Diagnòstiques d’Infecció Activa, més conegut com PDIA, positives en la Comunitat Valenciana, segons data de diagnòstic de laboratori. Les PDIA inclouen dues proves de detecció d’infecció activa, la PCR (sigles de polymerase chain reaction, reacció en cadena de la polimerasa) i el test d’antígens, d’acord amb els procediments i documents tècnics vigents elaborats pel Ministeri de Sanitat."),
            
            selectInput (inputId = "Zona",
                         label = "ZONA:",
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
            tags$p("Última data d'actualització :",data_actualitzacio,".")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            useShinyalert(),
            
            fluidRow(
                div(column(4,div(column(3, tags$h4("Mitjana")),column(1,actionButton("btn1", (tags$img(src="info.png",width = 24, height = 24))))), flexdashboard::gaugeOutput("mitjana")),
                    column(4,div(column(4, tags$h4("Incidencia")),column(1,actionButton("btn2", (tags$img(src="info.png",width = 24, height = 24))))), flexdashboard::gaugeOutput("incidencia")),
                    column(4,div(column(7, tags$h4("Mitjana 14 dies")),column(1,actionButton("btn3", (tags$img(src="info.png",width = 24, height = 24))))), flexdashboard::gaugeOutput("mitjana14"))
                    #column(4,tabsetPanel(tabPanel(tags$h4("Mitjana"), flexdashboard::gaugeOutput("mitjana")),tabPanel("DescripciÃÂÃÂ³", "Mitjana de", textOutput("departamento1"), "respecte a la mitjana de la Comunitat Valenciana"))),
                    #column(4,tabsetPanel(tabPanel(tags$h4("Incidencia"), flexdashboard::gaugeOutput("incidencia")),tabPanel("DescripciÃÂÃÂÃÂÃÂ³", "Incidencia acumulada en els Ultims 14 dies a", textOutput("departamento2")))),
                    #column(4,tabsetPanel(tabPanel(tags$h4("Mitjana 14 dies"), flexdashboard::gaugeOutput("mitjana14")),tabPanel("DescripciÃÂÃÂÃÂÃÂ³", "Mitjana dels Ultims 14 dies de", textOutput("departamento3"), "respecte a la mitjana dels Ultims 14 dies de la Comunitat Valenciana"))),
                )
            ),
            
            fluidRow(
                div(column(6,div(column(5, tags$blockquote("Mapa")),column(1,actionButton("btn4", (tags$img(src="info.png",width = 24, height = 24)))) ) ),
                    column(6,div(column(5, tags$blockquote("Evolució")),column(1,actionButton("btn5", (tags$img(src="info.png",width = 24, height = 24))))))
                    #column(6,tabsetPanel(tabPanel(tags$blockquote("Mapa")), tabPanel("DescripcÃÂÃÂ³", "Mapa de la Comunitat Valenciana dividt per els departaments de salud.", tags$br(), "EL mapa esta acolorit segons la Incidencia en cada departament.", tags$br(), "A mes ens indica la localitzacio del departament i la Incidencia actual."))),
                    #column(6,tabsetPanel(tabPanel(tags$blockquote("EvoluciÃÂÃÂÃÂÃÂ³")), tabPanel("DescripciÃÂÃÂ³", "Grafica de l'evoluciÃÂÃÂ³ dels casos positius de Covid-19 des del 2020-01-02 fins ", data_actualitzacio)))
                )
            ),
            
            fluidRow(
                div(column(6, plotOutput("mapa")),
                    column(6, plotOutput("evolucion")), align = "center")
            )
            
        )
    )
))
