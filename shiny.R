####################################
#                                  #
#                 ui               #
#                                  #
####################################



ui <- fluidPage(theme = shinytheme("spacelab"), navbarPage("Lenzhausener Siepen 2023",
                                                           
tabPanel("Visualisierung I",
  sidebarPanel(
    selectInput("kartenwerttiere","Was soll auf der Karte abgebildet werden ?", choices = c("**Alle Tiere**","**Beobachtungstage pro Sektor**","Eichhörnchen","Fuchs","Graureiher","Hase","Weitere Vögel","Katze","Libelle","Maus","Marder","Mäusebussard","Waschbär","Reh","Stockente","Taube"), multiple = FALSE),
    checkboxGroupInput("kartenwertmonat",label = "Welche Monate sollen in der Karte berücksichtigt werden ?", choiceValues = c(2,3,4,5,6,7,8,9), choiceNames = c("Februar","März","April","Mai","Juni","Juli","August","September"), selected = c(2,3,4,5,6,7,8,9)),
    checkboxGroupInput("tageszeitwert", choices = c("Tag","Nacht","Dämmerung"), selected = c("Tag","Nacht","Dämmerung"), label = "Möchten sie nach der Tageszeit filtern ?", inline = TRUE),
    checkboxGroupInput("niederschlagswert", choices = c("kein Niederschlag","mittlerer Niederschlag","starker Niederschlag"),selected = c("kein Niederschlag","mittlerer Niederschlag","starker Niederschlag") ,label = "Wie stark ist der Niederschlag ?"),
    checkboxGroupInput("jahr", choices = c("Jahr_2023","Jahr_2021"), selected = c("Jahr_2023","Jahr_2021") ,label = "Welche Jahre sollen berücksichtigt werden ?", inline = TRUE),
    sliderInput("kartentemperaturwert",min = -10,max =  35, value = c(-10,35), label = "Welcher Temperaturintervall soll berücksichtigt werden ?"),
    materialSwitch(inputId = "relatvierungkarte",label = "Relativierung durch Beobachtungstage",value = FALSE,status = "primary")
    ),
  mainPanel(
    tags$h1("Überblickskarte zum Lenzhausenersiepen."),
    tags$h4("Die Karte visualisiert die beobachtete Anzahl an Individuen, abhängig von verschiedenen Faktoren."),
    tags$h4("Zudem kann betrachtet werden, wie viele Tage ein Sektor beobachtet wurde, auch in Abhängigkeit vom Monat."),
    tags$h4(""),
    plotOutput("karte",width = 675, height = 450),
    tags$h6(tags$b("Bildnachweiß:"), tags$i("Pastor J. (2014). Die Entwicklung der Amphibien-, Reptilien- und Libellenfauna in einem ökologisch umgestalteten Teichgelände in Wuppertal-Cronenberg. In Jahresberichte des naturwissenschftlichen Vereins Wuppertal e.V. 63: 95-116.")),
    tags$h4(" "),
    verbatimTextOutput("ktergebnis")
    )
  ),

tabPanel("Visualisierung II",
  sidebarPanel(
    selectInput("linienmapsektor", "Welchen Sektor möchten sie betrachten ?", choices = c(1,2,3,4,5,6)),
    checkboxGroupInput("linienmapmonat",label = "Welche Monate mit berücksichtigt werden ?", choiceValues = c(2,3,4,5,6,7,8,9), choiceNames = c("Februar","März","April","Mai","Juni","Juli","August","September"), selected = c(2,3,4,5,6,7,8,9))
    ),
  mainPanel(
    tags$h1("Bewegungen des Waschbären."),
    tags$h4("Die Karte visualisiert die Bewegungsmuster des Waschbären in Abhängigkeit vom Monat."),
    plotOutput("linienmap",width = 765, height = 405),
    tags$h6(tags$b("Bildnachweiß:"), tags$i("Eigene Aufnahmen, bearbeitet mit Gimp")),
    tags$h5(tags$b("Achtung !"),"Aufgrund eines Kopierfehlers sind die Daten für den Sektor 1 nicht vollständig. Die Positionsdaten von 16 Tieren aus dem September können nicht mit berücksichtigt werden.", style = "color: red;")
    )
  ),

tabPanel("Visualisierung III",
    tags$h1("Weitere Visualisierungen."),
    tags$h4("Auf dieser Seite sind diverse Visualisierungen abrufbar, die alle über die unten stehenden Parameter einstellbar sind.Alle Parameter wirken sich automatisch auf alle Visualisierungen aus, sofern die entsprechenden Parameter für diese relevant sind. Dieser Bereich ist als eine Art Spielplatz gedacht, auf den die Daten auf Herz und Niere untersucht werden können."),
    tags$h2("Absatz", style = "color: white;"),
    
  tags$h2("Verändern sie die gewünschten Parameter."),
  fluidRow(column(width = 12,
    column(width = 3,checkboxGroupInput("v3wertmonat",label = "Welche Monate sollen berücksichtigt werden ?", choiceValues = c(2,3,4,5,6,7,8,9), choiceNames = c("Februar","März","April","Mai","Juni","Juli","August","September"), selected = c(2,3,4,5,6,7,8,9))),
    column(width = 3,checkboxGroupInput("v3jahr", choices = c("Jahr_2023","Jahr_2021"), selected = c("Jahr_2023","Jahr_2021") ,label = "Welche Jahre sollen berücksichtigt werden ?", inline = TRUE)),
    column(width = 3,selectInput("v3werttiere","Welche Tiere sollen berücksichtigt werden ?", choices = c("Alle","Eichhörnchen","Fuchs","Graureiher","Hase","Weitere Vögel","Katze","Libelle","Maus","Marder","Mäusebussard","Waschbär","Reh","Stockente","Taube"), multiple = FALSE)),
    column(width = 3,checkboxGroupInput("v3niederschlagswert", choices = c("kein Niederschlag","mittlerer Niederschlag","starker Niederschlag"),selected = c("kein Niederschlag","mittlerer Niederschlag","starker Niederschlag") ,label = "Welche Niederschlagsintensität soll berücksichtigt werden ?")),
    tags$h2("Absatz", style = "color: white;"),
    column(width = 3,checkboxGroupInput("v3sektoren", "Welche Sektoren sollen berücksichtigt werden ?", choices = c(1,2,3,4,5,6,7,8), selected = c(1,2,3,4,5,6,7,8))),
    column(width = 3,checkboxGroupInput("v3tageszeitwert", choices = c("Tag","Nacht","Dämmerung"), selected = c("Tag","Nacht","Dämmerung"), label = "Welche Tageszeiten sollen berücksichtigt werden ?", inline = TRUE)),
    column(width = 3,sliderInput("v3temperaturwert",min = -10,max =  35, value = c(-10,35), label = "Welcher Temperaturintervall soll berücksichtigt werden ?")),
    column(width = 3,selectInput("v3regression","Welche Regression soll für die Temperatur verwendet werden ?", choices = c("Linear","Quadratisch","Exponentiell"), multiple = FALSE)),
  ),
  
    tags$h2("Absatz", style = "color: white;"),
    tags$h2("Visualisierungen in Diagrammform"),
  fluidRow(width = 12,
    column(width = 4,plotOutput("kreisdiagram")),
    column(width = 4,plotOutput("tagnachtdiagram")),
    column(width = 4,plotOutput("niederschlagsdiagram")),
    column(width = 6,plotOutput("säulendiagram")),
    column(width = 6,plotOutput("tempmap"))
  ),
    tags$h2("Absatz", style = "color: white;"),
  
  tags$h2("Visualisierungen in tabellarischer Form"),
  fluidRow(column(width = 12,
    column(width = 2,tableOutput("übersichtstabelleII")),
  )),

  tags$h2("Statistik"),
  fluidRow(column(width = 12,
    column(width = 6,verbatimTextOutput("vItergebnis")),
    column(width = 6,verbatimTextOutput("vIItergebnis")),
    column(width = 6,verbatimTextOutput("vIIItergebnis")),
    column(width = 6,verbatimTextOutput("vIIIItergebnis")),
  ))
)),

  tabPanel("Trivia","Die Oberfläche wurde im Rahmen eines Forschungsprojektes erstellt. Alle Daten und Aufnahmen stammen aus dem Jahr 2023. Für Rückfragen wenden sie sich an r-kaetker@web.de :).")
))



########################################
#                                      #
#                 server               #
#                                      #
########################################



server <- function(input, output){
  output$ktergebnis <- renderPrint({
    x <- filtern_nach(tier = input$kartenwerttiere,monat = input$kartenwertmonat,sektor = c(),tempmin = input$kartentemperaturwert[1],tempmax = input$kartentemperaturwert[2],niederschlag = input$niederschlagswert,jahr = input$jahr,tageszeit = input$tageszeitwert,daten)
    x <- as.data.frame(table(x$Sektor))
    print("Gesamtzahl")
    print(sum(x$Freq))
    print("Arithmetisches Mittel")
    print(round(mean(x$Freq)),2)
    print("Standartabweichung")
    print(round(sd(x$Freq)),2)
  })
  
  output$karte <- renderPlot({
    ### - Prüft ob Tiere oder Beobachtungstage dargestellt werden - ###
    if(input$kartenwerttiere == "**Beobachtungstage pro Sektor**"){
      x <- filtern_nach(tier = c("Alle"),monat = input$kartenwertmonat,sektor = c(),tempmin = c(),tempmax = c(),niederschlag = c(),jahr = c(),tageszeit = c(),tagfürtag)
    }else{
      x <- filtern_nach(tier = input$kartenwerttiere,monat = input$kartenwertmonat,sektor = c(),tempmin = input$kartentemperaturwert[1],tempmax = input$kartentemperaturwert[2],niederschlag = input$niederschlagswert,jahr = input$jahr,tageszeit = input$tageszeitwert,daten)
    }
    x <- as.data.frame(table(x$Sektor))
    
    ### - Erstellung der Sektorenzuweißung - ###
    mapdaten <- machekartendataframe(input$jahr)
    mapdaten$Freq[match(x$Var1, mapdaten$Sektorzahl)] <- x$Freq
    
    ### - Relativierung durch die Tagesanzahl - ###
    if(input$relatvierungkarte == TRUE & input$kartenwerttiere != "**Beobachtungstage pro Sektor**"){
      y <- filtern_nach(tier = c("Alle"),monat = input$kartenwertmonat,sektor = c(),tempmin = c(),tempmax = c(),niederschlag = c(),jahr = c(),tageszeit = c(),tagfürtag)
      y <- as.data.frame(table(y$Sektor))
      mapdaten$Freq <- round(mapdaten$Freq/y$Freq,2)
    }
    
    ### - Errechnet Prozentangaben - ###
    mapdaten$Prozent <- round(mapdaten$Freq/sum(mapdaten$Freq),2)
    
    ### - Erstellt die Karte - ###
    sektordiag(mapdaten)
  })
  
  output$säulendiagram <- renderPlot({
    monat_count <- filtern_nach(tier = input$v3werttiere,monat = c(),sektor = input$v3sektoren,tempmin = input$v3temperaturwert[1],tempmax = input$v3temperaturwert[2],niederschlag = input$v3niederschlagswert,jahr = input$v3jahr,tageszeit = input$v3tageszeitwert,daten)
    monat_count <- häufupro(monat_count, "Monat")
    datensäulendiagram <- matchframemaker(2,9,monat_count,"Monat")
    datensäulendiagram$Wert = c(1,2,3,4,5,6,7,8)
    
  
    monatdiag(datensäulendiagram)
  })
  
  output$kreisdiagram <- renderPlot({
    x1 <- filtern_nach(tier = c("Alle"),monat = input$v3wertmonat,sektor = input$v3sektoren,tempmin = input$v3temperaturwert[1],tempmax = input$v3temperaturwert[2],niederschlag = input$v3niederschlagswert,jahr = input$v3jahr,tageszeit = input$v3tageszeitwert,daten)
    x1 <- as.data.frame(table(x1$Tier))
    datenkreisdiagram <- erstellesonstige(x1, "Tier" ,c("Graureiher","Reh","Fuchs", "Waschbär", "Eichhörnchen"))
    datenkreisdiagram$Name <- paste(datenkreisdiagram$Tier, " - ",datenkreisdiagram$Prozent)
    
    kreisdiag(datenkreisdiagram, "Tier", "- Prozentuale Auswertung nach Tiergruppe -")
  })
  
  output$tagnachtdiagram <- renderPlot({
    datentagnachtdiagram <- filtern_nach(tier = input$v3werttiere,monat = input$v3wertmonat,sektor = input$v3sektoren,tempmin = input$v3temperaturwert[1],tempmax = input$v3temperaturwert[2],niederschlag = input$v3niederschlagswert,jahr = input$v3jahr,tageszeit = c(),daten)
    datentagnachtdiagram <- häufupro(datentagnachtdiagram, "Tageszeit")
    datentagnachtdiagram$Name <- paste(datentagnachtdiagram$Tageszeit, " - ",datentagnachtdiagram$Prozent, "%")
    kreisdiag(datentagnachtdiagram, "Tageszeit", "- Prozentuale Verteilung nach Tageszeit -")
  })
  
  output$niederschlagsdiagram <- renderPlot({
    datenniederschlagsdiagram <- filtern_nach(tier = input$v3werttiere,monat = input$v3wertmonat,sektor = input$v3sektoren,tempmin = input$v3temperaturwert[1],tempmax = input$v3temperaturwert[2],niederschlag = c(),jahr = input$v3jahr,tageszeit = input$v3tageszeitwert,daten)
    datenniederschlagsdiagram <- häufupro(datenniederschlagsdiagram, "Niederschlag")
    datenniederschlagsdiagram$Name <- paste(datenniederschlagsdiagram$Niederschlag, " - ",datenniederschlagsdiagram$Prozent, "%")
    
    kreisdiag(datenniederschlagsdiagram, "Niederschlag", "- Prozentuale Verteilung nach Niederschlag -")
  })
  
  output$linienmap <- renderPlot({
    sektorinputzubild <- data.frame(Var1 = c(1,2,3,4,5,6), Var2 = c("mapsektor1","mapsektor2","mapsektor3","mapsektor4","mapsektor5","mapsektor6"))
    bild <- sektorinputzubild[input$linienmapsektor, "Var2"]
    linienmapdaten <- filter(daten, Tier == "Waschbär", Monat %in% input$linienmapmonat, Sektor %in% input$linienmapsektor, Dateiname != "nicht mehr vorhanden")
    linienmapdatenII <- data.frame(XWerte = c(linienmapdaten$`Xstart`,linienmapdaten$`Xende`),YWerte = c(linienmapdaten$`Ystart`,linienmapdaten$`Yende`))

    verlaufsdiagram(linienmapdatenII, linienmapdaten, bild)
    
  })
  output$übersichtstabelleII <- renderTable({
    tabelle <- filtern_nach(tier = c("Alle"),monat = input$v3wertmonat,sektor = input$v3sektoren,tempmin = input$v3temperaturwert[1],tempmax = input$v3temperaturwert[2],niederschlag = input$v3niederschlagswert,jahr = input$v3jahr,tageszeit = input$v3tageszeitwert,daten)
    tabelle <- häufupro(tabelle, "Tier")
    colnames(tabelle) <- c("Tier","Anzahl an beobachteten Individuen", "Prozent")  
    tabelle
  },colnames = TRUE)
  
  output$tempmap <- renderPlot({
    hvtemp <- filtern_nach(tier = input$v3werttiere,monat = input$v3wertmonat,sektor = input$v3sektoren,tempmin = c(),tempmax = c(),niederschlag = input$v3niederschlagswert,jahr = input$v3jahr,tageszeit = input$v3tageszeitwert,daten)
    hvtemp <- häufupro(hvtemp, "Temperatur")
    temperaturdaten <- matchframemaker(-10,35,hvtemp,"Temperatur")
 
    temperaturdiag(temperaturdaten, input$v3regression)
  })
  
  output$vItergebnis <- renderPrint({
    I <- filtern_nach(tier = input$v3werttiere,monat = input$v3wertmonat,sektor = input$v3sektoren,tempmin = input$v3temperaturwert[1],tempmax = input$v3temperaturwert[2],niederschlag = input$v3niederschlagswert,jahr = input$v3jahr,tageszeit = c(),daten)
    I <- häufupro(I, "Tageszeit")
    print("Tageszeit")
    print("Absolute Zahl an beobachteten Individuen")
    print(sum(I$Freq))
    print("##############################")
    
    II <- filtern_nach(tier = input$v3werttiere,monat = input$v3wertmonat,sektor = input$v3sektoren,tempmin = input$v3temperaturwert[1],tempmax = input$v3temperaturwert[2],niederschlag = c(),jahr = input$v3jahr,tageszeit = input$v3tageszeitwert,daten)
    II <- häufupro(II, "Niederschlag")
    print("Niederschlag")
    print("Absolute Zahl an beobachteten Individuen")
    print(sum(II$Freq))
    print("##############################")
    
    III <- filtern_nach(tier = input$v3werttiere,monat = c(),sektor = input$v3sektoren,tempmin = input$v3temperaturwert[1],tempmax = input$v3temperaturwert[2],niederschlag = input$v3niederschlagswert,jahr = input$v3jahr,tageszeit = input$v3tageszeitwert,daten)
    III <- häufupro(III, "Monat")
    print("Monat")
    print("Absolute Zahl an beobachteten Individuen")
    print(sum(III$Freq))
    print("Arithmetisches Mittel")
    print(round(mean(III$Freq)),2)
    print("Standartabweichung")
    print(round(sd(III$Freq)),2)
    print("##############################")
    
    IIII <- filtern_nach(tier = input$v3werttiere,monat = input$v3wertmonat,sektor = input$v3sektoren,tempmin = c(),tempmax = c(),niederschlag = input$v3niederschlagswert,jahr = input$v3jahr,tageszeit = input$v3tageszeitwert,daten)
    IIII <- häufupro(IIII, "Temperatur")
    print("Temperatur")
    print("Absolute Zahl an beobachteten Individuen")
    print(sum(IIII$Freq))
  })
}