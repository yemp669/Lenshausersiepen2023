#########

c <- filtern_nach(tier = c("Alle"),monat = c(2,4),sektor = c(),tempmin = c(),tempmax = c(),niederschlag = c(),jahr = c(2023),tageszeit = c(),daten, häufigkeitstabelle = "nein", prozent = "sicha")
filtern_nach(tier = input$v3werttiere,monat = input$v3wertmonat,sektor = input$v3sektoren,tempmin = input$v3temperaturwert[1],tempmax = input$v3temperaturwert[2],niederschlag = input$v3niederschlagswert,jahr = 2023,tageszeit = input$v3tageszeitwert,daten, häufigkeitstabelle = "Monat", prozent = "jasicha")
x <- filtern_nach(tier = c("Alle"),monat = c(),sektor = c(),tempmin = c(),tempmax = c(),niederschlag = c(),jahr = c(),tageszeit = c(),daten, häufigkeitstabelle = "nein", prozent = "nein")

##########

schnellfick <- read_ods("Statistische Erhebung.ods", sheet = 7)
schnellfick <- data.frame(table(schnellfick$Sektor))
schnellfickI <- schnellfick
schnellfick$xWert = c(16.25,16,15,13,11,7.75,15,6)
schnellfick$yWert = c(1.5,3.5,5.25,5.5,5.75,5,1.5,5.5)
schnellfick$Name = c("S1","S2","S3","S4","S5","S6","S7","S8")
schnellfick$Var1 = c(1,2,3,4,5,6,7,8)
schnellfick$Freq = c(0,0,0,0,0,0,0,0)
schnellfick$Freq[match(schnellfickI$Var1,schnellfick$Var1)] <- schnellfickI$Freq


rm(schnellfick, schnellfickI)

sektordiag(schnellfick)

########

c <- filtern_nach(tier = c("Waschbär"),monat = c(),sektor = c(),tempmin = c(),tempmax = c(),niederschlag = c(),jahr = c("Jahr_2023"),tageszeit = c(),daten)
c <- as.data.frame(table(c$Tageszeit))
wilcox.test(c$Freq~c$Var1)
t <- statistischerTest(as.data.frame(c$Tier), as.data.frame(c$Tageszeit))
print(t$p.value)

######

mapdaten <- data.frame(Name = c("S1","S2","S3","S4","S5","S6"), 
                       Var1 = c(1,2,3,4,5,6), 
                       Freq = c(0,0,0,0,0,0),
                       xWert = c(16.25,16,15,13,11,7.75),
                       yWert = c(1.5,3.5,5.25,5.5,5.75,5))

#####


penis <- machekartendataframe(c("Jahr_2023","Jahr_2021"))
sektordiag(penis)

########

if(length(x$Freq) == 2){
  y <- wilcox.test(x$Freq~x$Var1)
}else if (length(x$Freq) > 2){
  y <- kruskal.test(x$Freq~x$Var1)
}
print(y$p)