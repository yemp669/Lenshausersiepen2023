###################
# Sektorendiagram #
###################

sektordiag <- function(mapdaten){
ggplot(mapdaten, aes(x=xWert, y=yWert))+
  background_image(teichmaps)+
  xlim(0,20) + ylim(0,10)+
  geom_point(aes(fill = Prozent), alpha = 0.5, size = 30, shape = 21) + 
  scale_fill_gradient(low = "lightblue", high = "darkblue")+
  geom_label(aes(x = xWert, y = yWert+0.75, label = Sektor))+
  geom_label(aes(x = xWert, y = yWert+1.5, label = Freq))+
  theme_void()+ 
  theme(panel.border = element_rect(linewidth = 1, fill = NA, colour = "black"))+
  theme(legend.position="none")
}

###################
# Monatsdiagram   #
###################

monatdiag <- function(datensäulendiagram){
ggplot(datensäulendiagram, aes(x = Wert, y = Freq)) +
  geom_col(colour = "black", fill = "lightgoldenrod1") +
  xlab("Monate") + ylab("Individuen pro Monat") +
  geom_text(aes(label = Freq), vjust = -0.25, colour = "black") +
  labs(title = "- Absolute Zahl an Individuen pro Monat -")+
  theme(plot.title = element_text(size = 18, colour = "darkgrey",margin = margin(b = 10), margin(t = 15)))+
  theme(axis.text=element_text(size=14, color = "grey35"),axis.title=element_text(size=16))+
  xlim(labels = c("Feb.", "Mär.", "Apr.", "Mai", "Jun.", "Jul.", "Aug.","Sep."))+
  theme(panel.border = element_rect(linewidth = 1, fill = NA, colour = "black"))
}

#####################
# Temperaturdiagram #
#####################

temperaturdiag <- function(tempdaten,regression){
  ggplot(tempdaten, aes(x = Wert, y = Freq)) +
    geom_col(colour = "black", fill = "grey60") +
    xlab("Temperatur") + ylab("Individuen pro Grad") +
    geom_text(aes(label = Wert), vjust = -0.25, colour = "black") +
    labs(title = "- Absolute Zahl an Individuen pro Grad -")+
    theme(plot.title = element_text(size = 18, colour = "darkgrey",margin = margin(b = 10), margin(t = 15)))+
    theme(axis.text=element_text(size=14, color = "grey35"),axis.title=element_text(size=16))+
    theme(panel.border = element_rect(linewidth = 1, fill = NA, colour = "black"))+
    if (regression == "Linear"){
      stat_smooth(method = "lm", se = FALSE, color = "red")
    }else if (regression == "Quadratisch"){
      stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") 
    }else if (regression == "Exponentiell"){
      stat_smooth(method = "glm", method.args = list(family = "poisson"), se = FALSE, color = "red")
    }
}

############################
# Kreisdiagramm in Prozent #
############################

kreisdiag <- function(datenkreisdiagram, dfill, Titel){
ggplot(datenkreisdiagram, aes(x="", y=Freq, fill=get(dfill)))+
  geom_bar(stat="identity", color="black")+
  coord_polar("y")+
  theme_void()+
  labs(fill= dfill)+
  theme(legend.text = element_text(size = 15),legend.title = element_text(size = 15))+
  theme(panel.border = element_rect(linewidth = 1, fill = NA, colour = "black"))+
  theme(panel.background = element_rect(fill = "azure3", colour = "azure3"))+
  labs(title = Titel)+
  scale_fill_discrete(labels = datenkreisdiagram$Name)+
  theme(plot.title = element_text(size = 18, colour = "darkgrey",margin = margin(b = 10), margin(t = 15)))
}

###################
# Verlaufsdaigram #
###################

verlaufsdiagram <- function(Punktkoordinaten, datenzummatchen, bilddatei){
ggplot(Punktkoordinaten, aes(x=XWerte, y=YWerte))+
  background_image(get(bilddatei))+
  geom_point(alpha = 1, size = 2)+
  scale_shape_identity() +
  theme_void()+
  theme(panel.border = element_rect(linewidth = 1, fill = NA, colour = "black"))+
  geom_segment(data = datenzummatchen, aes(x = Xstart, y = Ystart, xend = Xende, yend = Yende), color = "red", linewidth = 1) +  
  coord_cartesian(xlim = c(1, 25.5), ylim = c(1, 14))
}