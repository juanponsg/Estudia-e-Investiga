

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
library(shinythemes)
library(shinyWidgets)
library(shinyalert)



datos_PDIA <- readr::read_delim("https://dadesobertes.gva.es/dataset/ce195af2-39ec-4f44-bb77-b14235519b0d/resource/cb50e7d2-0c0e-46b8-a359-a0fa35998577/download/covid-19-serie-de-casos-con-pdia-positiva-en-la-comunitat-valenciana.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE, col_types = cols())

ultima_pos = length(datos_PDIA$`Data diagnòstic laboratori/fecha diagnóstico laboratorio`)
ini = ultima_pos-15
Casos_Últimos_14_Dies = sum(datos_PDIA$C.Valenciana[ini:ultima_pos])
ini_prev = ini - 15 # PARA LA TASA DE CRECIMIENTO

tab = tibble(
  Regió = "C. Valenciana",
  Mitjana = round(mean(datos_PDIA$C.Valenciana),3),
  Máxim_Alcançat = round(max(datos_PDIA$C.Valenciana),3),
  Casos_Últim_Día = datos_PDIA$C.Valenciana[ultima_pos],
  Casos_Ultims_14_Dies = sum(datos_PDIA$C.Valenciana[ini:ultima_pos]),
  Casos_Totals = sum(datos_PDIA$C.Valenciana),
  Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[1])*100000, 3),
  Tendencia = (sum(datos_PDIA$C.Valenciana[ini_prev:ini]) - sum(datos_PDIA$C.Valenciana[ini:ultima_pos]))
) %>%
  add_row(
    Regió = "Homes",
    Mitjana = round(mean(datos_PDIA$`Homes/Hombres`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`Homes/Hombres`),3),
    Casos_Últim_Día = datos_PDIA$`Homes/Hombres`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`Homes/Hombres`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`Homes/Hombres`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[1])*100000, 3),
    Tendencia = (sum(datos_PDIA$`Homes/Hombres`[ini_prev:ini]) - sum(datos_PDIA$`Homes/Hombres`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dones",
    Mitjana = round(mean(datos_PDIA$`Dones/Mujeres`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`Dones/Mujeres`),3),
    Casos_Últim_Día = datos_PDIA$`Dones/Mujeres`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`Dones/Mujeres`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`Dones/Mujeres`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[1])*100000, 3),
    Tendencia = (sum(datos_PDIA$`Dones/Mujeres`[ini_prev:ini]) - sum(datos_PDIA$`Dones/Mujeres`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Prov. Alacant",
    Mitjana = round(mean(datos_PDIA$`Prov. Alacant/Alicante`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`Prov. Alacant/Alicante`),3),
    Casos_Últim_Día = datos_PDIA$`Prov. Alacant/Alicante`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`Prov. Alacant/Alicante`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`Prov. Alacant/Alicante`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[2])*100000, 3),
    Tendencia = (sum(datos_PDIA$`Prov. Alacant/Alicante`[ini_prev:ini]) - sum(datos_PDIA$`Prov. Alacant/Alicante`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Prov. Castello",
    Mitjana = round(mean(datos_PDIA$`Prov. Castelló/Castellón`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`Prov. Castelló/Castellón`),3),
    Casos_Últim_Día = datos_PDIA$`Prov. Castelló/Castellón`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`Prov. Castelló/Castellón`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`Prov. Castelló/Castellón`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[3])*100000, 3),
    Tendencia = (sum(datos_PDIA$`Prov. Castelló/Castellón`[ini_prev:ini]) - sum(datos_PDIA$`Prov. Castelló/Castellón`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Prov. Valencia",
    Mitjana = round(mean(datos_PDIA$`Prov. València`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`Prov. València`),3),
    Casos_Últim_Día = datos_PDIA$`Prov. València`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`Prov. València`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`Prov. València`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[4])*100000, 3), 
    Tendencia = (sum(datos_PDIA$`Prov. València`[ini_prev:ini]) - sum(datos_PDIA$`Prov. València`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. Vinaros",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE VINAROS`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE VINAROS`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE VINAROS`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VINAROS`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VINAROS`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[5])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VINAROS`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VINAROS`[ini:ultima_pos]))
  )%>%
  add_row(
    Regió = "Dep. Castello",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE CASTELLO`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE CASTELLO`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE CASTELLO`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE CASTELLO`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE CASTELLO`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[6])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE CASTELLO`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE CASTELLO`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. La Plana",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE LA PLANA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE LA PLANA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE LA PLANA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA PLANA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA PLANA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[7])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA PLANA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA PLANA`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. Sagunt",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE SAGUNT`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE SAGUNT`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE SAGUNT`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE SAGUNT`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE SAGUNT`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[8])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE SAGUNT`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE SAGUNT`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. VCIA Clinic-La Malva-Rosa",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[9])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. VCIA Arnau de Vilanova Lliria",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[10])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. Valencia- La Fe",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - LA FE`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - LA FE`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - LA FE`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - LA FE`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - LA FE`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[11])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - LA FE`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - LA FE`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. Requena",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE REQUENA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE REQUENA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE REQUENA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE REQUENA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE REQUENA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[12])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE REQUENA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE REQUENA`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. Valencia Hosp. General",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[13])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. Valencia Doctor",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[14])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. La Ribera",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE LA RIBERA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE LA RIBERA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE LA RIBERA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA RIBERA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA RIBERA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[15])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA RIBERA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA RIBERA`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. Gandia",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE GANDIA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE GANDIA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE GANDIA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE GANDIA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE GANDIA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[16])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE GANDIA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE GANDIA`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. Denia",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE DENIA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE DENIA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE DENIA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE DENIA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE DENIA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[17])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE DENIA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE DENIA`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. Xativa-Ontinyent",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[18])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. Alcoi",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT D'ALCOI`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT D'ALCOI`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT D'ALCOI`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALCOI`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALCOI`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[19])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALCOI`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALCOI`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. La Marina Baixa",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE LA MARINA BAIXA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE LA MARINA BAIXA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE LA MARINA BAIXA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA MARINA BAIXA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA MARINA BAIXA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[20])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA MARINA BAIXA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE LA MARINA BAIXA`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. Sant Joan D'Alacant",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[21])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. Elda",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT D'ELDA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT D'ELDA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT D'ELDA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELDA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELDA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[22])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELDA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELDA`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. Alacant Hosp. General",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[23])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. Elx Hosp. General",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[24])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. Orihuela",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT D'ORIHUELA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT D'ORIHUELA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT D'ORIHUELA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ORIHUELA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ORIHUELA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[25])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ORIHUELA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ORIHUELA`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. Torrevieja",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE TORREVIEJA`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE TORREVIEJA`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE TORREVIEJA`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE TORREVIEJA`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE TORREVIEJA`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[26])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE TORREVIEJA`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE TORREVIEJA`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. Manises",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT DE MANISES`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT DE MANISES`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT DE MANISES`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE MANISES`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT DE MANISES`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[27])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT DE MANISES`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT DE MANISES`[ini:ultima_pos]))
  ) %>%
  add_row(
    Regió = "Dep. Elx-Crevillent",
    Mitjana = round(mean(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX-CREVILLENT`),3),
    Máxim_Alcançat = round(max(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX-CREVILLENT`),3),
    Casos_Últim_Día = datos_PDIA$`DEPARTAMENT DE SALUT D'ELX-CREVILLENT`[ultima_pos],
    Casos_Ultims_14_Dies = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX-CREVILLENT`[ini:ultima_pos]),
    Casos_Totals = sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX-CREVILLENT`),
    Incidencia = round((Casos_Ultims_14_Dies/Poblacion$POBLACION[28])*100000, 3),
    Tendencia = (sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX-CREVILLENT`[ini_prev:ini]) - sum(datos_PDIA$`DEPARTAMENT DE SALUT D'ELX-CREVILLENT`[ini:ultima_pos]))
  )

nombr_id = c("C.Valenciana" = 1
             , "Homes/Hombres" = 2
             , "Dones/Mujeres" = 3
             , "Prov. Alacant/Alicante" = 4
             , "Prov. Castelló/Castellón" = 5
             , "Prov. València" = 6
             , "DEPARTAMENT DE SALUT DE VINAROS" = 7
             , "DEPARTAMENT DE SALUT DE CASTELLO" = 8
             , "DEPARTAMENT DE SALUT DE LA PLANA" = 9
             , "DEPARTAMENT DE SALUT DE SAGUNT" = 10
             , "DEPARTAMENT DE SALUT DE VCIA CLINIC-LA MALVA-ROSA" = 11
             , "DEPARTAMENT DE SALUT VCIA ARNAU DE VILANOVA LLIRIA" = 12
             , "DEPARTAMENT DE SALUT DE VALENCIA - LA FE" = 13
             , "DEPARTAMENT DE SALUT DE REQUENA" = 14
             , "DEPARTAMENT DE SALUT DE VALENCIA -HOSPITAL GENERAL" = 15
             , "DEPARTAMENT DE SALUT DE VALENCIA - DOCTOR PESET" = 16
             , "DEPARTAMENT DE SALUT DE LA RIBERA" = 17
             , "DEPARTAMENT DE SALUT DE GANDIA" = 18
             , "DEPARTAMENT DE SALUT DE DENIA" = 19
             , "DEPARTAMENT DE SALUT DE XATIVA - ONTINYENT" = 20
             , "DEPARTAMENT DE SALUT D'ALCOI" = 21
             , "DEPARTAMENT DE SALUT DE LA MARINA BAIXA" = 22
             , "DEPARTAMENT DE SALUT D'ALACANT-SANT JOAN D'ALACANT" =23
             , "DEPARTAMENT DE SALUT D'ELDA" = 24
             , "DEPARTAMENT DE SALUT D'ALACANT - HOSPITAL GENERAL" = 25
             , "DEPARTAMENT DE SALUT D'ELX - HOSPITAL GENERAL" = 26
             , "DEPARTAMENT DE SALUT D'ORIHUELA" = 27
             , "DEPARTAMENT DE SALUT DE TORREVIEJA" = 28
             , "DEPARTAMENT DE SALUT DE MANISES" = 29
             , "DEPARTAMENT DE SALUT D'ELX-CREVILLENT" = 30
)  

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

