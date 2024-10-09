################################################################################
# Code der einen Datenframe erstellt, welche mir ausgibt an welchem Tag        # 
# die Kameras wo standen und anschließend eine Häufigkeitstabelle für die      #
# Sektoren erstellt                                                            #
################################################################################

tagfürtag <- data.frame()
for (i in 1:nrow(zeiten)) {
  start_date <- as.Date(zeiten$Aufstellung[i])
  end_date <- as.Date(zeiten$Abstellung[i])
  neue_zeilen <- data.frame(Datum = seq(start_date, end_date, by = "day"))
  neue_zeilen$Sektor <- zeiten$Sektor[i]
  tagfürtag <- rbind(tagfürtag, neue_zeilen)
}
tagfürtag$Monat <- month(tagfürtag$Datum)
häufigkeitstabellezeitensektor <- as.data.frame(table(tagfürtag$Sektor))
datenI <- subset(daten, Jahr == "Jahr_2023")
HäufigkeitstabelleSektor <- as.data.frame(table(datenI$Sektor))
HäufigkeitstabelleSektor$Beobachtungstage <- table(tagfürtag$Sektor)
HäufigkeitstabelleSektor$Relative_Häufigkeit = round(HäufigkeitstabelleSektor$Freq/HäufigkeitstabelleSektor$Beobachtungstage, 1)

################################################################################
# Funktion die einen Dtenframe erstellt, indem eine Anzahl an Spalten erstellt #
# werden die den Wert Null tragen und anschließend mit einem kleineren Daten-  #
# Satz gemacht werden                                                          #
# Ausgabe: Spalte Wert mit dem Variablen Namen, Freq mit der Häufigkeit        #
################################################################################

matchframemaker <- function(minwert, maxwert, daten, Merkmal){
  gemachtchetedaten <- data.frame(Wert = minwert:maxwert, Freq = 0)
  match_pos <- match(daten[[Merkmal]], gemachtchetedaten$Wert)
  gemachtchetedaten$Freq[match_pos] <- daten$Freq
  return(gemachtchetedaten)}

################################################################################
# Funktion die einen zur verfügung gestellten Datenframe auf die angegebnen    #
# Parameter filter                                                             #
################################################################################

filtern_nach <- function(tier, monat, sektor, tempmin, tempmax, niederschlag, jahr, tageszeit, data){
  if(length(tier) > 0 & tier != "Alle" & tier != "**Alle Tiere**"){
    data <- filter(data, Tier %in% tier)}
  if(length(monat) > 0){
    data <- filter(data, Monat %in% monat)}
  if(length(sektor) > 0){
    data <- filter(data, Sektor %in% sektor)}
  if(length(tempmin) > 0 & length(tempmax) > 0){
    data <- filter(data, Temperatur >= tempmin & Temperatur <= tempmax)}
  if(length(niederschlag) > 0){
    data <- filter(data, Niederschlag %in% niederschlag)}
  if(length(jahr) > 0){
    data <- filter(data, Jahr %in% jahr)}
  if(length(tageszeit) > 0){
    data <- filter(data, Tageszeit %in% tageszeit)}
  return(as.data.frame(data))
}

################################################################################
# Funktion die einen zur verfügung gestellten Datenframe in eine Häufigkeits-  #
# tabelle mit Prozentangaben umwandelt                                         #
################################################################################

häufupro <- function(data, merkmal){
  data <- as.data.frame(table(data[merkmal]))
  data$Prozent <- round(data$Freq/sum(data$Freq)*100,2)
  return(data)
}

################################################################################
# Funktion die einen Datensatz bekommt als Häufigkeitstabelle und alle Ausprä- #
# gungen absehen von den genannten als Sonstige zusammenfasst und einen neue   #
# Häufigkeitstabelle zurückgibt, inklusive Prozentuale angaben                 #
################################################################################

erstellesonstige <- function(fdaten, Merkmal, alleaußerdiese){
  colnames(fdaten) <- c("x", "Freq")
  neuedaten <- fdaten %>% filter(x %in% alleaußerdiese)
  xx <- data.frame(x = "Sonstige", Freq = sum(fdaten$Freq)-sum(neuedaten$Freq))
  neuedaten <- rbind(neuedaten,xx)
  neuedaten$Prozent <- paste(round(neuedaten$Freq/sum(neuedaten$Freq)*100, 2), "%", sep = "")
  colnames(neuedaten) <- c(Merkmal, "Freq", "Prozent")
  return(neuedaten)
}

################################################################################
# Funktion die einen Vektor mit Jahreszahlen bekommt und einen Datenframe der  #
# nur noch mit den passenden Daten gematcht werden muss ausspuckt              #
################################################################################

machekartendataframe <- function(Jahre){
  SektorenJahresDatenbank <- list(Jahr_2021 = c(6,7,8), 
                                  Jahr_2023 = c(1,2,3,4,5,6))
  SektorenKoordinatenDatenbank <- data.frame(Sektor = c("S1","S2","S3","S4","S5","S6","S7","S8"), 
                                            xWert = c(16.25,16,15,13,11,9.25,13.5,6),
                                            yWert = c(1.5,3.5,5.25,5.5,5.5,4.5,2,5.25),
                                            Sektorzahl = c(1,2,3,4,5,6,7,8))
  Kontrollvariable <- list()
  dataframefürkarte <- data.frame()
  for (i in Jahre) {
    for (ii in SektorenJahresDatenbank[[i]]) {
      if (!(ii %in% unlist(Kontrollvariable))) {
        x <- SektorenKoordinatenDatenbank[SektorenKoordinatenDatenbank$Sektor == paste0("S", ii), ]
        dataframefürkarte <- rbind(dataframefürkarte, x)
        Kontrollvariable <- append(Kontrollvariable, ii)
      }}}
  dataframefürkarte$Freq = 0
  return(dataframefürkarte)
}