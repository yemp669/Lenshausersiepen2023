################### Packete laden ###################

library(dplyr)
library(png)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(readODS)
library(shiny)
library(shinythemes)
library(lubridate)
library(shinyWidgets)
library(ineq)

################### Material laden ###################

setwd("C:/Users/yemp9/Meine Ablage/Uni Aktuell/BIO-FP1 - Tiere bespannen/r")

teichmaps <- readPNG("Map.png")
mapsektor1 <- readPNG("Sektor1.png")
mapsektor2 <- readPNG("Sektor2.png")
mapsektor3 <- readPNG("Sektor3.png")
mapsektor4 <- readPNG("Sektor4.png")
mapsektor5 <- readPNG("Sektor5.png")
mapsektor6 <- readPNG("Sektor6.png")

daten <- read_ods("Statistische Erhebung.ods", sheet = 5)
zeiten <- read_ods("Statistische Erhebung.ods", sheet = 6)
datenn <- read_ods("Statistische Erhebung.ods", sheet = 9)


source("shiny.R")
source("functions.R")
source("plots.R")

################### Shiny starten ###################

shinyApp(ui = ui, server = server)


