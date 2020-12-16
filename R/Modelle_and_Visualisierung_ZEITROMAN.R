###############################
### Korpus Zeitroman ##########
###############################

### Pakete laden
install.packages("ggplot2")
install.packages("psych")
install.packages("lme4")
install.packages("ordinal")
install.packages("ggsignif")
install.packages("plyr")
install.packages("sjPlot")
install.packages("RColorBrewer")
install.packages("ggpubr")
install.packages("snakecase")
install.packages("boot")
#install.packages("devtools")
#install.packages("broom")
install.packages("stargazer")
install.packages("reporttools")
install.packages("Rmisc")
#install.packages("tableone")
install.packages("AER") # overdispersion test
install.packages("sjstats")
install.packages("bucky") # um robuste Standardfehler zu erhalten
install.packages("pscl") # für zero inflated poisson

library(pscl) 
library(AER) 
library(Rmisc)
library("RColorBrewer")
library("lme4")
library("ordinal")
library("MASS")
library(ggplot2)
library(ggsignif)
library(plyr)
library(psych)
library(sjPlot)
library(ggpubr)
library(snakecase)
library(boot)
library(stargazer)
library(dplyr)
library(reporttools)
library(sjstats)
library(AER)
library(bucky)
library(MASS)


############################################
### Daten einlesen und Variablenformate ####
############################################

Sys.setlocale(locale = "German")
setwd("C:/Users/Benjamin/Dropbox/Wissenschaft/Fiktion und Genre/Habil/R")
d <- read.table("Zeitroman6_mitJahr.CSV",header=TRUE, sep=";",na.strings=c("NA","-"))
head(d)
d <- d[1:66,1:98]
str(d)

#Unabhängige Variablen als Faktor
#d$Werk_ab_1 <- ifelse((d$Werk == 0), 1, ifelse((d$Werk == 1), 2,ifelse((d$Werk == 2), 3, "NA")))
#d$Werk_ab_1 <- factor(d$Werk_ab_1)
#d$Werk_ab_1

#Reihenfolge der Autoren geändert (Jordan, Kretzer, Mauthner, Suttner)
#d$Werk <- ifelse((d$Werk == 0), 3, ifelse((d$Werk == 1), 2, ifelse((d$Werk == 2), 0, 
#              ifelse((d$Werk == 3), 1, "NA"))))
d$Werk <- factor(d$Werk)
d$Werk <- factor(d$Werk, levels = c(0,1,2,3), labels = c("Hollaender", "Keller", "Müller-Guttenbrunn", "Spielhagen"))
d$Werk
#d <- within(d, Werk <- relevel(Werk, ref = 3)) # andere baseline
d$Genrezuordnung_formal <- factor(d$Genrezuordnung_formal)
d$Genrezuordnung_formal <- factor(d$Genrezuordnung_formal, levels = c(0,1), labels = c("Roman", "Zeitroman"))
#d$Genrezuordnung_formal_weit <- factor(d$Genrezuordnung_formal_weit)
#d$Genrezuordnung_formal_weit <- factor(d$Genrezuordnung_formal_weit, levels = c(0,1), labels = c("Roman", "Zeitroman"))


#d$NrKritiker <- factor(d$NrKritiker)

# Ordinale Variablen als Faktor
#d$Moralische_Wertung_metrisch <- as.numeric(d$Moralische_Wertung)
#d$Moralische_Wertung <- factor(d$Moralische_Wertung)
#d$Ästhetische_Wertung_metrisch <- d$Ästhetische_Wertung 
d$Ästhetische_Wertung <- factor(d$Ästhetische_Wertung)
# für plot_grpfrq (muss mit 1 anfangen)
#d$Moralische_Wertung_neu <- ifelse((d$Moralische_Wertung == 0), 1, ifelse((d$Moralische_Wertung == 1), 2,ifelse((d$Moralische_Wertung == 2), 3, ifelse((d$Moralische_Wertung == 3), 4, ifelse((d$Moralische_Wertung ==4), 5, NA)))))
#d$Moralische_Wertung_neu <- factor(d$Moralische_Wertung_neu)
#d$Moralische_Wertung <- d$Moralische_Wertung_neu
d$Ästhetische_Wertung_neu <- ifelse((d$Ästhetische_Wertung == 0), 1, ifelse((d$Ästhetische_Wertung ==1), 2,ifelse((d$Ästhetische_Wertung == 2), 3, ifelse((d$Ästhetische_Wertung == 3), 4, ifelse((d$Ästhetische_Wertung ==4), 5, NA)))))
d$Ästhetische_Wertung_neu <- factor(d$Ästhetische_Wertung_neu)
d$Ästhetische_Wertung <- d$Ästhetische_Wertung_neu

d$Ästhetische_Wertung <- factor(d$Ästhetische_Wertung, levels = c(1,2,3,4,5), labels = c("sehr schlecht", "schlecht", "teils gut, teils schlecht", "gut", "sehr gut"))
#d$Moralische_Wertung <- factor(d$Moralische_Wertung, levels = c(1,2,3,4,5), labels = c("sehr verwerflich", "verwerflich", "neutral", "vorbildlich", "sehr vorbildlich"))
d$Ästhetische_Wertung
#d$Moralische_Wertung

#d$Moralische_Wertung_mitNA <- addNA(d$Moralische_Wertung)
#d$Moralische_Wertung_mitNA
d$Ästhetische_Wertung_mitNA <- addNA(d$Ästhetische_Wertung)
d$Ästhetische_Wertung_mitNA


#Referenzialisierbarkeit
#d$Referenzialisierbarkeit <- d$Objektbezug.basal_Ähnlichkeit + d$Objektbezug.basal_Genese
#d$Referenzialisierbarkeit


### OUTLIERS
#generate z-scores for variable A using the scale() function
z <-- scale(d$Explizite.Wahrheit.basal_Anzahl, center = TRUE, scale = TRUE)
z
tail(sort(d$Explizite.Wahrheit.basal_Anzahl),5)
# Ergebnis ZR-Rez64 ist ein outlier -> Korrektur nach Tabachnik nächstgrößerer Wert des zweitgrößten Werts zuweisen
d$Explizite.Wahrheit.basal_Anzahl[63]
d$Explizite.Wahrheit.basal_Anzahl[63] <- 5
max(d$Explizite.Wahrheit.basal_Anzahl, na.rm = TRUE)

# Skalentransformation explWahrheit (count -> ordinal) und dann die neue ordinale nur nehmen, wenn es noch keinen ordinalen Wert gab
# Skalentransformation-neu (cut-Funktion) Kategorie "nie" ist mit 0 besetzt
# Input ist "Explizite.Wahrheit.basal_Anzahl" und "Explizite.Wahrheit.basal_ordinal"
#Output: "Explizite.Wahrheit.basal" (eine ordinale Variable) u. "Explizite.Wahrheit.basal_metrisch" (eine ordinale, aber nicht als factor gemacht) 
max = 15 # maximale Anzahl Explizite_Wahrheit_Anzahl im Gesamtkorpus
d$Explizite.Wahrheit.basal_Anzahl
cut(d$Explizite.Wahrheit.basal_Anzahl, breaks=c(1, 1+(max-1)/4, 1+(max-1)/4*2, 1+(max-1)/4*3, max), ordered_result=FALSE, right=TRUE, include.lowest=TRUE)
d$Explizite_Wahrheit_temp_ordinal <- cut(d$Explizite.Wahrheit.basal_Anzahl, breaks=c(1, 1+(max-1)/4, 1+(max-1)/4*2, 1+(max-1)/4*3, max), labels=c("1", "2", "3", "4"), ordered_result=FALSE, right=TRUE, include.lowest=TRUE)
# NAs auf 0 setzen
d$Explizite_Wahrheit_temp_ordinal <- ifelse(is.na(d$Explizite_Wahrheit_temp_ordinal), "0",  d$Explizite_Wahrheit_temp_ordinal)
d$Explizite_Wahrheit_temp_ordinal
#Zusammenführen der Variablen
d$Explizite.Wahrheit.basal <- ifelse(is.na(d$Explizite.Wahrheit.basal_ordinal), as.numeric(as.character(d$Explizite_Wahrheit_temp_ordinal)),  d$Explizite.Wahrheit.basal_ordinal)
d$Explizite.Wahrheit.basal  
d$Explizite.Wahrheit.basal_metrisch <- d$Explizite.Wahrheit.basal #zum plotten als metrische Variable behalten
#d$Explizite.Wahrheit.basal <- factor(d$Explizite.Wahrheit.basal)
d$Explizite.Wahrheit.basal <- factor(d$Explizite.Wahrheit.basal, levels = c(0,1,2,3,4), labels = c("nie", "selten", "gelegentlich", "häufig", "immer"))
d$Explizite.Wahrheit.basal_metrisch
d$Explizite.Wahrheit.basal


#binäre Variablen aus ästhetische Wertung und moralische Wertung
#d$Ästhetische_Wertung_binary <- ifelse(!is.na(d$Ästhetische_Wertung), 1, 0) #Wert "1" wenn Wertung vorhanden
d$Ästhetische_Wertung_dichotom <- ifelse(!is.na(d$Ästhetische_Wertung), 1, 0) #Wert "1" wenn Wertung vorhanden
#d$Moralische_Wertung_dichotom <- ifelse(!is.na(d$Moralische_Wertung), 1, 0) #Wert "1" wenn Wertung vorhanden
as.integer(d$Ästhetische_Wertung_dichotom)
#as.integer(d$Moralische_Wertung_dichotom)
#d$Moralische_Wertung_dichotom

##Variablennamen Ordinal
dvordinalList <- c("Ästhetische_Wertung", "Explizite.Wahrheit.basal" ) 
cat("Ordinale Variablen (Skalen):")
dvordinalList

##Variablennamen binär
checkBinaryTrait = function(v, naVal="NA") {
  print(v)
  if(!is.numeric(v)) {
    return ("non_numeric")
    #stop("non-numeric")
  }
  vSet = unique(v)
  if(!missing(naVal)) vSet[which(vSet == naVal)] = NA
  vSet = vSet[which(!is.na(vSet))]
  if(any(as.integer(vSet) != vSet)) return("con")
  if(length(vSet) > 2) return("con")
  "bin"
}

dvList <- names(d)[7:56] # hier alle abhängigen Variablen
#dvList <- dvList[-c(44)] # Variable rauswerfen
print(dvList)
dvbinaryList <- NULL
x <- 1
for (i in dvList) {
  #print(i)
  check <- checkBinaryTrait(d[[i]])
  print(check)
  if (check=="bin") {
    dvbinaryList[x] <- i
    x <- x + 1
  }
  }
cat("Dichotome Variablen:")
print(dvbinaryList)

#Fälschlich binär erkannte Variablen rauswerfen
dvbinaryList <- dvbinaryList[-1:-5]
#dvbinaryList <- dvbinaryList[-c(3)]
# hinzu
dvbinaryList <- c(dvbinaryList, "Ästhetische_Wertung_dichotom", "Zeitkritik")
print(dvbinaryList)


##Variablennamen count data
x <- 1
dvcountList <- NULL
for (i in dvList) {
  print(i)
  check <- checkBinaryTrait(d[[i]])
  print(check)
  if ((check=="con") & !(i %in% dvbinaryList) & !(i %in% dvordinalList)) {
    dvcountList[x] <- i
    x <- x + 1
  }
}
cat("Count data Variablen:")
print(dvcountList)

# nicht erkannte Variablen hinzufügen
dvcountList <- c(dvcountList, "Exemplifikation.basal_Anteil_Handlung", "Exemplifikation.basal_Anteil_Gesellschaft", "Exemplifikation.basal_Anteil_Stimmung", "Instanziierung.basal") ## adding elements
#dvcountList <- dvcountList[c(-12:-13)]
print(dvcountList)


###############################################################
### Infos über einzelne Variablen und Variablenberechnung #####
##############################################################

d$Instanziierung.basal
d$Anzahl_historischer_Abweichungen_dichotom <- ifelse(d$Anzahl_historischer_Abweichungen > 0, 1, 0) 
#d$Anzahl_historischer_Abweichungen_dichotom <- factor(d$Anzahl_historischer_Abweichungen_dichotom)
describeBy(d$Objektbezug.basal_Referenz, d$Werk)

describeBy(d$Explizite.Wahrheit.basal_metrisch, d$Genrezuordnung_formal)

# Häufigkeit einer ordinalen Variablen
res <- d %>% group_by(Genrezuordnung_formal,Explizite.Wahrheit.basal) %>% summarise(Freq=n())
res

nrow(d[d$Ästhetische_Wertung_binary == "1", ])

#Berechnung konditionaler Variablen
d$Zeitkritik
d$Zeitkritik_kond <- ifelse((d$Implizite.Wahrheit.basal > 0), d$Zeitkritik, NA)
describeBy(d$Zeitkritik, d$Genrezuordnung_formal)
describeBy(d$Zeitkritik_kond, d$Genrezuordnung_formal)

d$Universelles_Thema
d$Universelles_und_epochenspezifisches_Thema <- ifelse((d$Universelles_Thema > 0 & d$Epochenspezifisches_Thema > 0), 1, 0)
describeBy(d$Universelles_und_epochenspezifisches_Thema, d$Genrezuordnung_formal)
d$Universelles_und_epochenspezifisches_Thema

d$Bewertung_explizite_Wahrheit_kond <- ifelse((d$Explizite.Wahrheit.basal != "nie"), d$Bewertung_expliziter_Wahrheiten, NA)
d$Bewertung_explizite_Wahrheit_kond
describeBy(d$Bewertung_expliziter_Wahrheiten, d$Genrezuordnung_formal)
describeBy(d$Bewertung_explizite_Wahrheit_kond, d$Genrezuordnung_formal)

d$Bewertung_implizite_Wahrheit_kond <- ifelse((d$Implizite.Wahrheit.basal > 0), d$Bewertung_impliziter_Wahrheiten, NA)
d$Bewertung_implizite_Wahrheit_kond
describeBy(d$Bewertung_impliziter_Wahrheiten, d$Genrezuordnung_formal)
describeBy(d$Bewertung_implizite_Wahrheit_kond, d$Genrezuordnung_formal)

d$Arg_Auseinandersetzung_impl_expl_Wahrheit_kond <- ifelse((d$Explizite.Wahrheit.basal != "nie" | d$Implizite.Wahrheit.basal > 0), d$Arg_Auseinandersetzung_impl_expl_Wahrheit, NA)
d$Arg_Auseinandersetzung_impl_expl_Wahrheit_kond
describeBy(d$Arg_Auseinandersetzung_impl_expl_Wahrheit, d$Genrezuordnung_formal)
describeBy(d$Arg_Auseinandersetzung_impl_expl_Wahrheit_kond, d$Genrezuordnung_formal)


#Korrelation
cor(d$Universelles_Thema, d$Thematisierung_Sprache_Stil)
cor(d$Sympathie_für_Tendenz, d$Ästhetische_Wertung_neu)



###############################
### Visualisierung Korpus #####
###############################
#Group <- c("Roman", "Zeitroman") # hier noch nicht vertauscht
Worklabels <- c("Hollaender", "Keller", "Müller-Guttenbrunn", "Spielhagen")
axistitles = c("Werk", "Anzahl")
#d$Genrezuordnung_formal_invertiert <- relevel(d$Genrezuordnung_formal, "Zeitroman") #macht historischer Roman zum ersten Level
#d$Genrezuordnung_formal_invertiert
#Group_invert = c("Zeitroman", "Roman") # invertierte Labels


dodge <- position_dodge(width=0.9)
farbig_theme <- function(titel) {
  list(geom_col(position=dodge), 
       scale_fill_brewer(palette="Set1"), 
       geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE, group=Group), position=dodge, width=0.25, size=0.25), 
       labs(x="", y = "Mittelwert"), 
       ggtitle(titel), 
       theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
             legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
             legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
             legend.margin=margin(0,0,0,0), legend.box.margin=margin(-15,0,0,0)) #Abstand der Legende von Plot
  )
}

farbig_theme_minusSE <- function(titel) {
  list(geom_col(position=dodge), 
       scale_fill_brewer(palette="Set1"), 
       #geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE, group=Group), position=dodge, width=0.25, size=0.25), 
       labs(x="", y = "Mittelwert"), 
       ggtitle(titel), 
       theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
             legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
             legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
             legend.margin=margin(0,0,0,0), legend.box.margin=margin(-15,0,0,0)) #Abstand der Legende von Plot
  )
}


grau_theme <- function() {
  list(theme_bw(), scale_fill_grey(start = 0.2, end = .6), 
       theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
             legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
             legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
             legend.margin=margin(0,0,0,0), legend.box.margin=margin(-15,0,0,0)) #Abstand der Legende von Plot
  )
}



#hier Speicherort eintragen
Pfad = "./Abbildungen/Zeitroman/"
speichern <- function(datei) {
  dateipfad <- paste(Pfad, "ZR-", datei, ".pdf", sep = "")
  ggsave(dateipfad)
  dateipfad <- paste(Pfad, "ZR-", datei, ".eps", sep = "")
  ggsave(dateipfad)
  dateipfad <- paste(Pfad, "ZR-", datei, ".png", sep = "")
  ggsave(dateipfad, dpi = 1200)
}
  
# Anzahl Rezensionen pro Werk
rez <- ggplot(d,aes(factor(Werk), group= Werk, fill=Werk)) + 
  geom_bar(show.legend = FALSE) + 
  scale_fill_brewer(palette="Paired") +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), position = position_stack(vjust = 0.9)) +
  ggtitle("Rezensionen je Werk") + 
  labs(x= "Werk", y = "Anzahl") + 
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
        legend.margin=margin(0,0,0,0), legend.box.margin=margin(-15,0,0,0))
rez
speichern("Rezensionen je Werk")
rez + grau_theme()
speichern("Rezensionen je Werk (grau)")

# Genrezuordnung nach Werk
axistitles = c("Werk", "Anzahl d. Rezensionen")
genrezu <- plot_grpfrq(d$Werk, d$Genrezuordnung_formal,
           title = "Genrezuordnung nach Werk",
           legend.title = "",
           axis.titles = axistitles,
           axis.labels = Worklabels,
           #legend.labels = Group,
           show.na=FALSE, 
           show.axis.values=TRUE, 
           show.prc=FALSE,
           expand.grid =TRUE,
           geom.colors= "Set1")
genrezu + geom_col(position=dodge) + theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
                legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
                legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
                legend.margin=margin(0,0,0,0), legend.box.margin=margin(-15,0,0,0))
speichern("Genrezuordnung nach Werk")
##grau
genrezu + geom_col(position=dodge) + grau_theme()
speichern("Genrezuordnung nach Werk (grau)")

### Ex post Analysen - Plots
labelsmoral <- c("sehr verwerflich", "verwerflich", "neutral", "vorbildlich", "sehr vorbildlich", "keine Wertung")

moral2 <- plot_grpfrq(d$Werk, d$Moralische_Wertung_mitNA,
           title = "Moralische Wertung nach Werk",
           legend.title = "",
           axis.titles = axistitles,
           axis.labels = Worklabels,
           legend.labels = labelsmoral,
           show.na=FALSE,
           show.axis.values=TRUE, 
           show.prc=FALSE,
           expand.grid =FALSE,
           geom.colors= "Paired",
           geom.spacing = 0,
           show.grpcnt= FALSE)
moral2 + theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
                 legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
                 legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
                 legend.margin=margin(0,0,0,0), legend.box.margin=margin(-5,0,0,0))
speichern("Moralische Wertung nach Werk")

?plot_grpfrq

moral1 <- plot_grpfrq(d$Werk, d$Moralische_Wertung,
           title = "Moralische Wertung (dichotom) nach Werk",
           legend.title = "",
           axis.titles = axistitles,
           axis.labels = Worklabels,
           legend.labels = c("keine moralische Wertung", "moralische Wertung"),
           show.na=FALSE, 
           show.axis.values=TRUE, 
           show.prc=FALSE,
           expand.grid =TRUE,
           geom.colors= "Dark1",
           geom.spacing = 0,
           show.grpcnt= TRUE,
           wrap.legend.labels = 100) # Legendenumbrauch nach x Buchstaben
moral1 + theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
                 legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
                 legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
                 legend.margin=margin(0,0,0,0), legend.box.margin=margin(-5,0,0,0))
speichern("Moralische Wertung (dichotom) nach Werk")
moral1 + grau_theme()
speichern("Moralische Wertung (dichotom) nach Werk (grau)")


refwerk <- plot_grpfrq(d$Werk, d$Objektbezug.basal_Referenz,
           title = "Anzahl referenzialisierter Figuren nach Werk",
           legend.title = "",
           axis.titles = axistitles,
           axis.labels = Worklabels,
           #legend.labels = c("keine moralische Wertung", "moralische Wertung"),
           show.na=FALSE, 
           show.axis.values=TRUE, 
           show.prc=FALSE,
           expand.grid =FALSE,
           geom.colors= "Paired",
           geom.spacing = 0) #abstand der bars
refwerk + theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
                legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
                legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
                legend.margin=margin(0,0,0,0), legend.box.margin=margin(-5,0,0,0))+
  guides(fill = guide_legend(nrow = 1))
speichern("Anzahl referenzialisierter Figuren nach Werk")

RefxMoral <- plot_grpfrq(d$Objektbezug.basal_Referenz, d$Moralische_Wertung_dichotom,
           title = "Moralische Wertung (dichotom) nach referenzialisierten Figuren",
           legend.title = "",
           axis.titles = c("Referenz", "Anzahl d. Rezensionen"),
           #axis.labels = c("keine moralische Wertung", "moralische Wertung"),
           legend.labels = c("keine moralische Wertung", "moralische Wertung"),
           show.na=FALSE, 
           show.axis.values=TRUE, 
           show.prc=FALSE,
           expand.grid =TRUE,
           geom.colors= "Set1",
           wrap.legend.labels = 100,
           geom.spacing = 0)
RefxMoral + theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
                    legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
                    legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
                    legend.margin=margin(0,0,0,0), legend.box.margin=margin(-5,0,0,0))+
  guides(fill = guide_legend(nrow = 1))
speichern("Moralische Wertung nach referenzialisierten Figuren")
RefxMoral + grau_theme()
speichern("Moralische Wertung nach referenzialisierten Figuren (grau)")


### Rezensionen mit bestimmten Eigenschaften suchen
Auswahl <- d[which(d$Moralische_Wertung_dichotom =='1' & (d$Moralische_Wertung_metrisch == '0' | d$Moralische_Wertung_metrisch == '1')
              & d$Objektbezug.basal_Referenz == 0), ]
Auswahl 

#######################################################
#### Berechnung der Kenngrößen für Visualisierung #####
#######################################################

# Binäre Variablen ###########################################################################
set.seed(1234)
y <- 0
plotdata_list = list()
plotdata_list = NULL
y <- 0 # counter
for (i in dvbinaryList) {
  Group <- c("Roman", "Zeitroman")
  y <- y+1
  dv <- eval(parse(text=paste("d$", dvbinaryList[[y]], sep=''))) 
  summary <- describeBy(dv, d$Genrezuordnung_formal, na.rm=TRUE)
  summary
  SE <- unlist(c(summary[[1]][13], summary[[2]][13]))
  SD <- unlist(c(summary[[1]][4], summary[[2]][13]))
  DvValue <- unlist(c(summary[[1]][3], summary[[2]][3])) # Mittelwert
  plotdata <- data.frame(Group, DvValue, SE, SD)
  plotdata_list[[y]] <-
    data.frame(i, Group, DvValue, SE) # für Visualisierung in einem Diagramm, s.u.
  plotdata_list[[y]]

  ## plot model
  print(ggplot(plotdata, aes(x = Group, y = DvValue, fill = Group)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE), width = .2) +
      ggtitle(dvbinaryList[[y]]) + labs(y = "Mittelwert") +
      theme(plot.title = element_text(hjust = 0.5)))
}
dvbinaryList
#dvbinaryList <- dvbinaryList[-1]
#dvbinaryList <- dvbinaryList[-28:-30]

#### Visualisierung in einem Diagramm ######
## Realismus, Zeitkritik
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(31:34, 39:40),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
plotdata_merged_sub$i <- gsub("Mangelnder_Realismus", "Mangelnder\nRealismus", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Lob_des_Realismus", "Lob des\nRealismus", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
#plotdata_merged_sub <- transform(plotdata_merged_sub,i=factor(i,levels=unique(i)))
#levels(plotdata_merged_sub$i) <- gsub(".", "\n", levels(plotdata_merged_sub$i), fixed = TRUE)

dodge <- position_dodge(width=0.9)
fallstudienspezifisch <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) + farbig_theme_minusSE("Fallstudienspezifische Variablen")
fallstudienspezifisch
speichern("Fallstudienspezifische Variablen")
fallstudienspezifisch + grau_theme() 
speichern("Fallstudienspezifische Variablen (grau)")


# F-Praxis: Erzähler und Bewertung expliziter Wahrheiten
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(3,4,13:16),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
plotdata_merged_sub$i <- gsub("Bewertung_expliziter_Wahrheiten", "Bewertung\nexplizite Wahrheit", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Identifikation_Autor_Erzählerfigur", "Identifikation\nAutor-\nErzählerfigur", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Impl_Identifikation_Autor_Erzähler", "Impl. Identifikation\nAutor-Erzähler", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames

dodge <- position_dodge(width=0.9)
f_praxis <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) + farbig_theme_minusSE("Spezifische Variablen Fiktionalitätspraxis")
f_praxis
speichern("Spezifische Variablen Fiktionalitätspraxis")
f_praxis + grau_theme() 
speichern("Spezifische Variablen Fiktionalitätspraxis (grau)")


## Literaturstatus
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(35:36, 17:26, 7:10, 29:30),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
plotdata_merged_sub$i <- gsub("Ästhetische.Wertung_dichotom", "Ästhetische\nWertung\ndichotom", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Arg_Auseinandersetzung_impl_expl_Wahrheit", "Arg.\nAuseinander-\nsetzung", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Bewertung_impliziter_Wahrheiten", "Bew.\nimpl.\nWahrheiten", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Epochenspezifisches_Thema", "Epochensp.\nThema", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Thematisierung_Sprache_Stil", "Thematis.\nSprache\nStil", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Universelles_Thema", "Univ.\nThema", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Ästhetische_Wertung_dichotom", "Ästh.\nWertung\ndichotom", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Absprechen_Literaturstatus", "Absprechen\nLiteratur-\nstatus", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Kritik_anderer_Interpretationen", "Kritik\nanderer\nInterpr.", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames


plotdata_merged_sub$i <- gsub("_", "\n", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
L_Praxis <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) + 
  farbig_theme_minusSE("Literaturpraxis nach Genrezuordnung") +
  scale_x_discrete(limits=c("Ästh.\nWertung\ndichotom","Absprechen\nLiteratur-\nstatus", "Univ.\nThema", "Epochensp.\nThema", "Thematis.\nSprache\nStil", "Bew.\nimpl.\nWahrheiten", "Arg.\nAuseinander-\nsetzung", "Kritik\nanderer\nInterpr."))+
  theme(axis.text=element_text(size=7))
L_Praxis
speichern("L-Praxis")
L_Praxis + grau_theme()
speichern("L-Praxis (grau)")



# Moralische vs. ästhetische (binär)
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- subset(plotdata_merged, i== "Ästhetische_Wertung_dichotom" |
                                i== "Moralische_Wertung_dichotom") # select subset of variables
plotdata_merged_sub$i <- gsub(".", "\n", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("_", "\n", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub <- transform(plotdata_merged_sub,i=factor(i,levels=unique(i)))
plotdata_merged_sub

dodge <- position_dodge(width=0.9)
multiplot <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
farbig_theme_minusSE("Ästhetische und moralische Wertung (dichotom)")  
multiplot

multiplot2 <- multiplot + 
  #geom_signif(annotations="***", xmin=0.7, xmax=1.2, y_position = 1.6, tip_length = 0.02, size=0.25, textsize = 2)
  geom_signif(annotations="*", xmin=1.75, xmax=2.2, y_position = 1, tip_length = 0.02, size=0.25, textsize = 2)
#geom_signif(annotations="**", xmin=3.75, xmax=4.2, y_position = 0.65, tip_length = 0.02, size=0.25, textsize = 2)
multiplot2
### Speichern
speichern("ästhetische moralische Wertung (dichotom)")
multiplot2 + grau_theme()
speichern("ästhetische moralische Wertung (dichotom) (grau)")


###Ordinale Variablen ##########################################################################
set.seed(1234)
y <- 0
for (i in dvordinalList) {
  print(i)
  Group <- c("Roman", "Zeitroman")
  labelsmoral <- c("sehr verwerflich", "verwerflich", "neutral", "vorbildlich", "sehr vorbildlich") # letzte Label fehlt, weil sonst Plot nicht funzt.
  labelsaesth <- c("sehr schlecht", "schlecht", "teils gut, teils schlecht", "gut", "sehr gut") #erste Kategorie raus, sonst funzt ggplot nicht
  labelshäufig <- c("nie", "selten", "gelegentlich", "oft")
  #d$Explizite.Wahrheit.basal
  y <- y+1
  dv <- eval(parse(text=paste("d$", dvordinalList[[y]], sep=''))) 
  
  ## plot model
  plot_grpfrq(d$Genrezuordnung_formal, d$Ästhetische_Wertung, 
             title = gsub(".", " ", dvordinalList[[1]], fixed = TRUE),
             legend.title = "",
             legend.labels = labelsaesth,
             axis.labels = Group,
             axis.titles = "",
             show.na=FALSE, 
             show.axis.values=TRUE, 
             show.prc=FALSE,
             show.n = TRUE,
             geom.colors= "Paired",
             show.grpcnt = TRUE
             )
}
speichern("Ästhetische Wertung")

#Explizite Wahrheit basal mit grpfrq
plot_grpfrq(d$Genrezuordnung_formal, d$Explizite.Wahrheit.basal, 
           title = gsub(".", " ", dvordinalList[[1]], fixed = TRUE),
           legend.title = "",
           legend.labels = labelshäufig,
           axis.labels = Group,
           axis.titles = "",
           show.na=FALSE, 
           show.axis.values=TRUE, 
           show.prc=FALSE,
           show.n = TRUE,
           geom.colors= "Paired",
           show.grpcnt = TRUE
)



##Ästhetische Wertung 
update_geom_defaults("text", list(size = 2.4)) # Größe der Labels über den Säulen
aesth <-plot_grpfrq(d$Genrezuordnung_formal, d$Ästhetische_Wertung, 
           title = "Ästhetische Wertung",
           legend.title = "",
           #legend.labels = labelsaesth,
           axis.titles = c("","Anzahl d. Rezenionen"),
           show.na=FALSE, 
           show.axis.values=TRUE, 
           show.prc=TRUE,
           show.n = TRUE,
           geom.colors= "Paired"
)
aesth + theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
                                     legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
                                     legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
                                     legend.margin=margin(0,0,0,0), legend.box.margin=margin(-5,0,0,0)) 

speichern("Ästhetische Wertung")

##Moralische Wertung 
moral <- plot_grpfrq(d$Genrezuordnung_formal, d$Moralische_Wertung, 
           title = "Moralische Wertung",
           legend.title = "",
           #legend.labels = labelsmoral,
           axis.titles = c("","Anzahl d. Rezensionen"),
           show.na=FALSE, 
           show.axis.values=TRUE, 
           show.prc=TRUE,
           show.n = TRUE,
           geom.colors= "Paired"
)
moral + theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
              legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
              legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
              legend.margin=margin(0,0,0,0), legend.box.margin=margin(-5,0,0,0)) 
speichern("Moralische Wertung")

#Explizite-Wahrheit-basal mit xtab
?plot_xtab
set_theme(
  geom.outline.size = 0, 
  geom.label.size = 2,
  geom.label.color = "black",
  title.color = "black", 
  title.size = 1.5, 
  axis.angle.x = 0, 
  axis.textcolor = "black"
)

set_theme(
  geom.outline.color = "grey", 
  geom.outline.size = 0, 
  geom.label.size = 2,
  geom.label.color = "black"
)

explW <- plot_xtab(d$Genrezuordnung_formal, d$Explizite.Wahrheit.basal, 
                  title = "Explizite-Wahrheit-basal",
                  legend.title = "",
                  margin = "row", 
                  show.total = FALSE, 
                  y.offset = 0.003, 
                  vjust = "center",
                  axis.titles = c("","Anteil d. Rezensionen"),
                  geom.colors= "Paired",
                  expand.grid = TRUE)
explW + theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
              legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
              legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
              legend.margin=margin(0,0,0,0), legend.box.margin=margin(-5,0,0,0))
speichern("Explizite-Wahrheit-basal")

#aest Wertung mit xtab
aesth <- plot_xtab(d$Genrezuordnung_formal, d$Ästhetische_Wertung_mitNA, 
        title = "Ästhetische Wertung",
        legend.title = "",
        margin = "row", 
         show.total = FALSE, 
         y.offset = 0.003, 
         vjust = "center",
         axis.titles = c("","Anteil d. Rezensionen"),
        geom.colors= "Paired")
aesth + theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
              legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
              legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
              legend.margin=margin(0,0,0,0), legend.box.margin=margin(-5,0,0,0),
              axis.text.y = element_text(size=8)) 
speichern("Ästhetische Wertung (mit NAs)")

#moral Wertung mit xtab
moral <- plot_xtab(d$Genrezuordnung_formal, d$Moralische_Wertung_mitNA, 
                  title = "Moralische Wertung",
                  legend.title = "",
                  margin = "row", 
                  show.total = FALSE, 
                  y.offset = 0.003, 
                  vjust = "center",
                  axis.titles = c("","Anteil d. Rezensionen"),
                  geom.colors= "Paired")
moral <- moral + theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
              legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
              legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
              legend.margin=margin(0,0,0,0), legend.box.margin=margin(-5,0,0,0)) 
moral
speichern("Moralische Wertung (mit NAs)")


## plot model Ästhetische Wertung (mit veränderter Reihenfolge, Zeitroman, Roman)
d$Genrezuordnung_formal_invertiert <- relevel(d$Genrezuordnung_formal, "1") #macht Zeitroman zum ersten Level
Group_invert = c("Zeitroman", "Roman") # invertierte Labels
plot_grpfrq(d$Genrezuordnung_formal_invertiert, d$Ästhetische_Wertung, 
           title = "Ästhetische Wertung",
           legend.title = "",
           legend.labels = labelsaesth,
           axis.labels = Group_invert,
           axis.titles = "",
           show.na=FALSE, 
           show.axis.values=TRUE, 
           show.prc=TRUE,
           show.n = FALSE,
           show.grpcnt = TRUE,
           geom.colors= "Paired"
           )
ggsave("./Abbildungen/Zeitroman/aesth_Wertung.pdf")
ggsave("./Abbildungen/Zeitroman/aesth_Wertung.eps")
ggsave("./Abbildungen/Zeitroman/aesth_Wertung.png", dpi=1200)

#Ästhetische Wertung mit allen Levels (bei HR wichtig)
library(reshape2)
#d$Ästhetische_Wertung <- factor(d$Ästhetische_Wertung, levels = c(0,1,2,3,4), labels = c("sehr schlecht", "schlecht", "teils gut, teils schlecht", "gut", "sehr gut"))
labelsaesth_alle <- c("sehr schlecht", "schlecht", "teils gut, teils schlecht", "gut", "sehr gut") #erste Kategorie raus, sonst funzt ggplot nicht

d2 = data.frame(type=factor(d$Genrezuordnung_formal_invertiert), group=factor(d$Ästhetische_Wertung))
dat = dcast(d2, type ~ group, fun.aggregate = length)
dat$`0` = c(0,0) # Kategorien mit der Anzahl 0 hinzufügen (nicht vorkomende Kategorien)
dat.melt = melt(dat, id.vars = "type", measure.vars = c("0","1","2","3","4"))
n.SR = 27
n.R = 43
dat.melt$prozent = ifelse(dat.melt$type==1, dat.melt$value/n.SR*100, dat.melt$value/n.R*100)
dat.melt

ggplot(dat.melt, aes(x = type,y = prozent, fill = variable)) + 
  geom_bar(stat = "identity", colour = "black", position = position_dodge(width = .8), width = 0.7) +
  geom_text(mapping = aes(label = paste0(round(prozent, digits=1), "%")), position = position_dodge(width = .8), vjust = -0.5) + 
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="right", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line")) +
  ggtitle("Ästhetische Wertung") + labs(y = "Anteil der Rezensionen je Gruppe") +
  scale_x_discrete(labels = c("1" = "Zeitroman", "0" = "Roman")) +
  #scale_fill_discrete(labels = labelsaesth_alle) +
  theme(axis.title.x=element_blank()) + 
  scale_fill_brewer(palette="PuBuGn", labels = labelsaesth_alle)
ggsave("./Abbildungen/Zeitroman/Ästhetische_Wertung.pdf")
ggsave("./Abbildungen/Zeitroman/Ästhetische_Wertung.eps")
ggsave("./Abbildungen/Zeitroman/Ästhetische_Wertung.png", dpi=1200)


### Count Variablen ##########################################################################
set.seed(1234)
y <- 0
plotdata_list = list()
plotdata_list = NULL
for (i in dvcountList) {
  Group <- c("Roman", "Zeitroman")
  y <- y + 1
  dv <- eval(parse(text = paste("d$", dvcountList[[y]], sep = '')))
  summary <- describeBy(dv, d$Genrezuordnung_formal, na.rm = TRUE)
  #print(summary)
  SE <- unlist(c(summary[[1]][13], summary[[2]][13]))
  DvValue <- unlist(c(summary[[1]][3], summary[[2]][3])) #nimmt Mittelwert
  plotdata <- data.frame(Group, DvValue, SE)
  plotdata_list[[y]] <-
    data.frame(i, Group, DvValue, SE) # für Visualisierung in einem Diagramm, s.u.
  plotdata_list[[y]]
}

### Visualisierung mehrerer Variablen in ein Diagramm ###

#dev.new(width=5, height=8)
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(1,2,21,22,31,32,25,26,43,44),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
levels(plotdata_merged_sub$i) <- gsub(".", "\n", levels(plotdata_merged_sub$i), fixed = TRUE)
levels(plotdata_merged_sub$i) <- gsub("Klassenidentität\nbasal", "Klassen-\nidentität\nbasal", levels(plotdata_merged_sub$i), fixed = TRUE)
levels(plotdata_merged_sub$i) <- gsub("Objektbezug\nbasal", "Objekt-\nbezug\nbasal", levels(plotdata_merged_sub$i), fixed = TRUE)

# Reihenfolge im Diagramm
plotdata_merged_sub$i <- factor(plotdata_merged_sub$i, levels = c('Objekt-\nbezug\nbasal', 'Exemplifikation\nbasal', 'Implizite\nWahrheit\nbasal', 'Klassen-\nidentität\nbasal', 'Instanziierung\nbasal'))

pvalue
dodge <- position_dodge(width=0.9)
multiplot <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
  geom_col(position=dodge) +
  scale_fill_brewer(palette="Set1") +
  geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE, group=Group), position=dodge, width=0.25, size=0.25) +
  #geom_signif(annotations="*", xmin=1.75, xmax=2.2, y_position = 2.8, tip_length = 0.02, size=0.25, textsize = 3) +
  ggtitle("Text-Welt-Verhältnisse") + labs(x="", y = "Mittelwert") +
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="top", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"))
multiplot
### Speichern
ggsave("./Abbildungen/Zeitroman/Text-Welt-Verhältnisse.pdf")
ggsave("./Abbildungen/Zeitroman/Text-Welt-Verhältnisse.png", dpi = 1200)

multiplot2 <- multiplot + 
  #geom_signif(annotations="***", xmin=0.77, xmax=1.2, y_position = 9.8, tip_length = 0.02, size=0.25, textsize = 2) +
  #geom_signif(annotations="*", xmin=1.75, xmax=2.2, y_position = 2.3, tip_length = 0.02, size=0.25, textsize = 2) +
  #geom_signif(annotations="**", xmin=3.7, xmax=4.2, y_position = 1.6, tip_length = 0.02, size=0.25, textsize = 2)
  ggtitle("Text-Welt-Verhältnisse: metrisch")
multiplot2
### Speichern
ggsave("./Abbildungen/Zeitroman/Text-Welt-Verhältnisse(mit Signifikanz).eps")
ggsave("./Abbildungen/Zeitroman/Text-Welt-Verhältnisse(mit Signifikanz).pdf")
ggsave("./Abbildungen/Zeitroman/Text-Welt-Verhältnisse(mit Signifikanz).png", dpi = 1200)
#grau
multiplot2g <- multiplot2 + theme_bw() + scale_fill_grey(start = 0.2, end = .6) + theme(legend.position = 'bottom') + theme(legend.title = element_blank())
ggsave("./Abbildungen/Zeitroman/Text-Welt-Verhältnisse(mit Signifikanz) (grau).eps")
ggsave("./Abbildungen/Zeitroman/Text-Welt-Verhältnisse(mit Signifikanz)  (grau).pdf")
ggsave("./Abbildungen/Zeitroman/Text-Welt-Verhältnisse(mit Signifikanz)  (grau).png", dpi = 1200)


#Isomorphie
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(11,12),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub

levels(plotdata_merged_sub$i) <- gsub(".", "\n", levels(plotdata_merged_sub$i), fixed = TRUE)
dodge <- position_dodge(width=0.9)
Isomorphie <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
  geom_col(position=dodge) +
  scale_fill_brewer(palette="Set1") +
  #geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE, group=Group), position=dodge, width=0.25, size=0.25) +
  ggtitle("dichotom") + 
  labs(x="", y = "Mittelwert") +
  scale_y_continuous(position = "right") +
  theme(plot.title = element_text(size=11,hjust = 0.5), axis.title=element_blank(), 
        legend.position="non", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"))
Isomorphie
#grau
Isomorphieg <- Isomorphie + theme_bw() + scale_fill_grey(start = 0.2, end = .6) +theme(legend.position = "none") +
  theme(plot.title = element_text(size=11,hjust = 0.5), axis.title=element_blank(), 
        legend.position="non", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"))
Isomorphieg

#explizite Wahrheit-basal (als metrisch behandelt)
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(35,36),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub

levels(plotdata_merged_sub$i) <- gsub(".", "\n", levels(plotdata_merged_sub$i), fixed = TRUE)

pvalue
dodge <- position_dodge(width=0.9)
explWahrheit <- ggplot(plotdata_merged_sub, aes(x = "Explizite Wahrheit \n basal", y = DvValue, fill = Group)) +
  geom_col(position=dodge) +
  scale_y_continuous(breaks=c(0,1,2), limits=c(0,2), label=c("nie", "selten", "gelegentlich"), position = "right")+
  scale_fill_brewer(palette="Set1") +
  #geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE, group=Group), position=dodge, width=0.25, size=0.25) +
  #geom_signif(annotations="*", xmin=0.75, xmax=1.2, y_position = 1.65, tip_length = 0.02, size=0.25, textsize = 3) +
  ggtitle("ordinal") + 
  labs(x="", y = "Mittelwert") +
  theme(plot.title = element_text(size=11,hjust = 0.5), axis.title=element_blank(), 
        legend.position="non", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"))
explWahrheit
explWahrheitg <- explWahrheit + theme_bw() + scale_fill_grey(start = 0.2, end = .6) +theme(legend.position = "none")+
  theme(plot.title = element_text(size=11,hjust = 0.5), axis.title=element_blank(), 
        legend.position="non", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"))
explWahrheitg

### 2 Diagramme nebeneinander ###
#dev.new(width=5, height=8)

ggarrange(
  multiplot2, explWahrheit, labels = c("A", "B"), ncol=2,
  common.legend = TRUE, legend = "bottom", align = "h", widths = 2:1)
ggsave("./Abbildungen/Zeitroman/Text-Welt-Verhältnisse2.pdf")
ggsave("./Abbildungen/Zeitroman/Text-Welt-Verhältnisse2.eps")
ggsave("./Abbildungen/Zeitroman/Text-Welt-Verhältnisse2.png", dpi=1200)

### 3 Diagramme nebeneinander ###
ggarrange(multiplot2, ggarrange(explWahrheit, Isomorphie, nrow=2, labels = c("B", "C"), align = "v"), 
  common.legend = TRUE, legend = "bottom", align = "no", widths = 2:1, ncol = 2, labels= "A")
ggsave("./Abbildungen/Zeitroman/Text-Welt-Verhältnisse3.pdf", width =7 , height = 6)
ggsave("./Abbildungen/Zeitroman/Text-Welt-Verhältnisse3.eps", width =7 , height = 6)
ggsave("./Abbildungen/Zeitroman/Text-Welt-Verhältnisse3.png", width =7 , height = 6, dpi=1200)

### 3 Diagramme nebeneinander grau ###
ggarrange(multiplot2g, ggarrange(explWahrheitg, Isomorphieg, nrow=2, labels = c("B", "C"), align = "v"), 
          common.legend = TRUE, legend = "bottom", align = "no", widths = 2:1, ncol = 2, labels= "A")
ggsave("./Abbildungen/Zeitroman/Text-Welt-Verhältnisse3 (grau).pdf", width =7 , height = 6)
ggsave("./Abbildungen/Zeitroman/Text-Welt-Verhältnisse3 (grau).eps", width =7 , height = 6)
ggsave("./Abbildungen/Zeitroman/Text-Welt-Verhältnisse3 (grau).png", width =7 , height = 6, dpi=1200)

dev.off()

## Figuren, Orte Ereignisse
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(5:10),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
levels(plotdata_merged_sub$i) <- gsub("Objektbezug.basal_Anteil_", "", levels(plotdata_merged_sub$i), fixed = TRUE)
plotdata_merged_sub

dodge <- position_dodge(width=0.9)
Entitäten <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
  farbig_theme("Objektbezug-basal: Typen von Entitäten") 
Entitäten
speichern("Typen_ref_Entitäten")
Entitäten + grau_theme() 
speichern("Typen_ref_Entitäten (grau)")


#Anteil verschiedener Relationen an Objektbezug-basal
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(17:20,29:30,15:16),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
levels(plotdata_merged_sub$i) <- gsub("Objektbezug.basal_", "", levels(plotdata_merged_sub$i), fixed = TRUE)
levels(plotdata_merged_sub$i) <- gsub("Objektbezug.basal_", "", levels(plotdata_merged_sub$i), fixed = TRUE)
levels(plotdata_merged_sub$i) <- gsub("_gesamt", "", levels(plotdata_merged_sub$i), fixed = TRUE)
plotdata_merged_sub

Relationen <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
  farbig_theme("Objektbezug-basal: Arten der Relation") +
  scale_x_discrete(limits=c("Ähnlichkeit","Genese", "Referenz", "unspezifisch")) # Reihenfolge
Relationen
speichern("Arten_Relation_Objektbezug-basal")
Relationen + grau_theme() 
speichern("Arten_Relation_Objektbezug-basal (grau)")  

# Relationen Objektbezug-basal: nur Figuren
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(17:20,23:24,29:30),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
levels(plotdata_merged_sub$i) <- gsub("Objektbezug.basal_", "", levels(plotdata_merged_sub$i), fixed = TRUE)
levels(plotdata_merged_sub$i) <- gsub("Objektbezug.basal_", "", levels(plotdata_merged_sub$i), fixed = TRUE)
levels(plotdata_merged_sub$i) <- gsub("_Figuren", "", levels(plotdata_merged_sub$i), fixed = TRUE)
plotdata_merged_sub

RelationenFiguren <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
  farbig_theme("Objektbezug-basal-Anteil-Figuren: Arten der Relation") +
  scale_x_discrete(limits=c("Ähnlichkeit","Genese", "Referenz", "unspezifisch")) + # Reihenfolge
  geom_signif(annotations="***", xmin=2.72, xmax=3.22, y_position = 2.93, tip_length = 0.02, size=0.25, textsize = 2)
RelationenFiguren
speichern("Arten_Relation_Objektbezug-basal-Figuren")
RelationenFiguren + grau_theme() 
speichern("Arten_Relation_Objektbezug-basal-Figuren (grau)")  

# Referenz vs. Referenzialisierbarkeit
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(59:60,29:30,57:58),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
levels(plotdata_merged_sub$i) <- gsub("Objektbezug.basal_", "", levels(plotdata_merged_sub$i), fixed = TRUE)
levels(plotdata_merged_sub$i) <- gsub("Objektbezug.basal_", "", levels(plotdata_merged_sub$i), fixed = TRUE)
#levels(plotdata_merged_sub$i) <- gsub("_Figuren", "", levels(plotdata_merged_sub$i), fixed = TRUE)
plotdata_merged_sub
RefRef <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
  farbig_theme("Objektbezug-basal-Anteil-Figuren: Arten der Relation") 
RefRef

# Referenz vs. Referenzialisierbarkeit (stacked)
plotdata_merged_sub[1,"referenzialisiert"] <- plotdata_merged_sub$DvValue[5]
plotdata_merged_sub[2,"referenzialisiert"] <- plotdata_merged_sub$DvValue[6]
plotdata_merged_sub[3,"referenzialisiert"] <- plotdata_merged_sub$DvValue[5]
plotdata_merged_sub[4,"referenzialisiert"] <- plotdata_merged_sub$DvValue[6]
plotdata_merged_sub[1,"nur_referenzialisierbar"] <- plotdata_merged_sub[1,"DvValue"] - plotdata_merged_sub$DvValue[5]
plotdata_merged_sub[2,"nur_referenzialisierbar"] <- plotdata_merged_sub[2,"DvValue"] - plotdata_merged_sub$DvValue[6]
plotdata_merged_sub[3,"nur_referenzialisierbar"] <- plotdata_merged_sub[3,"DvValue"] - plotdata_merged_sub$DvValue[5]
plotdata_merged_sub[4,"nur_referenzialisierbar"] <- plotdata_merged_sub[4,"DvValue"] - plotdata_merged_sub$DvValue[6]
plotdata_merged_sub <- plotdata_merged_sub[-c(5,6),]
#plotdata_merged_sub$DvValue <- NULL
#plotdata_merged_sub$SE <- NULL
plotdata_merged_sub$pvalue <- NULL
plotdata_merged_sub

library(reshape2) # for melt
plotdata_merged_sub
melted <- melt(plotdata_merged_sub, id=c("i", "Group", "DvValue", "SE"))
melted
melted$cat <- ''
melted[melted$Group == 'Roman',]$cat <- "Roman"
melted[melted$Group != 'Roman',]$cat <- "Zeitroman"
melted

RefRefstacked <- ggplot(melted, aes(x = cat, y = value, fill = variable)) +
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ i) +
  labs(title="Objektbezug-basal-Anteil-Figuren: Arten der Relation", x="", y="Mittelwert") +
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
        legend.margin=margin(0,0,0,0), legend.box.margin=margin(-15,0,0,0)) +
  geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE, group=Group), position=dodge, width=0.25, size=0.25)+
  scale_fill_discrete(breaks = "referenzialisiert", labels=c("referenzialisierbar & referenzialisiert"))+
  geom_text(x = 2,  y = 2, label = "***", colour = "black") +
  geom_segment(x = 1.75, xend = 3, y = 2, yend = 2, color = "black", inherit.aes = FALSE) 
RefRefstacked

# mit Stern
# Daten für Linien
anno <- data.frame(x1 = c(1, 2), x2 = c(2, 2), 
                   y1 = c(2.9, 2.9), y2 = c(3, 3), 
                   xstar = c(1.5, 1.5), ystar = c(3.1, 3.1),
                   lab = "***",
                   i = c("Referenz", "Referenz"))
RefRefstacked2 <- ggplot(melted, aes(x = cat, y = value, fill = variable)) +
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~i) +
  labs(title="Objektbezug-basal-Anteil-Figuren: Arten der Relation", x="", y="Mittelwert") +
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
        legend.margin=margin(0,0,0,0), legend.box.margin=margin(-15,0,0,0)) +
  geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE, group=Group), position=dodge, width=0.25, size=0.25)+
  scale_fill_discrete(breaks = "referenzialisiert", labels=c("referenzialisierbar & referenzialisiert"))+
  geom_text(data = anno, aes(x = xstar,  y = ystar, label = lab), inherit.aes = FALSE) +
  geom_segment(data = anno, aes(x = x1, xend = x1, 
                                y = y1, yend = y2), 
               colour = "black", inherit.aes = FALSE, size= 0.25)  +
  geom_segment(data = anno, aes(x = x2, xend = x2, 
                              y = y1, yend = y2, ),
             colour = "black", inherit.aes = FALSE, size= 0.25) +  
  geom_segment(data = anno, aes(x = x1, xend = x2, 
                              y = y2, yend = y2),
             colour = "black", inherit.aes = FALSE, size= 0.25)
RefRefstacked2
speichern("Referenz-Referenzialisierbarkeit")
RefRefstacked2  + scale_fill_discrete(breaks = "referenzialisiert", labels=c("referenzialisierbar & referenzialisiert")) + grau_theme()
speichern("Referenz-Referenzialisierbarkeit (grau)")  


## Exemplifkation Entitäten
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(23,24, 39,40, 41,42),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
levels(plotdata_merged_sub$i) <- gsub("Exemplifikation.basal_Anteil_", "", levels(plotdata_merged_sub$i), fixed = TRUE)
levels(plotdata_merged_sub$i) <- gsub("Exemplifikation_basal_Anteil:", "", levels(plotdata_merged_sub$i), fixed = TRUE)
plotdata_merged_sub

dodge <- position_dodge(width=0.9)
Entitäten_Exempl <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
  farbig_theme("Exemplifikation-basal: Typen von Entitäten")
Entitäten_Exempl
speichern("Typen_exempl_Entitäten")
Entitäten_Exempl + grau_theme()
speichern("Typen_exempl_Entitäten (grau)")


## Anzahl historischer Abweichungen
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(17:18),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
levels(plotdata_merged_sub$i) <- gsub("Anzahl_historischer_Abweichungen", "Historische\nAbweichungen", levels(plotdata_merged_sub$i), fixed = TRUE)
plotdata_merged_sub
pvalue <- plotdata_merged[17,length(plotdata_merged)] # p-wert aus Tabelle zuweisen
pvalue
dodge <- position_dodge(width=0.9)
hist_Abweichungen <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
  geom_col(position=dodge) +
  scale_fill_brewer(palette="Set1") +
  geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE, group=Group), position=dodge, width=0.25, size=0.25) +
  ggtitle("Anzahl historischer Abweichungen") + labs(x="", y = "Mittelwert") +
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="top", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"))+
  {if(pvalue < 0.05)geom_signif(annotations="*", xmin=0.77, xmax=1.22, y_position = 1.23, tip_length = 0.02, size=0.25, textsize = 2)}
hist_Abweichungen
ggsave("./Abbildungen/Zeitroman/hist_Abweichungen.pdf")
ggsave("./Abbildungen/Zeitroman/hist_Abweichungen.eps")
ggsave("./Abbildungen/Zeitroman/hist_Abweichungen.png", dpi=1200)



##################################
#########  Modelle  #############
#################################

options(scipen = 999) # deaktiviert exponentiale Notation

### ohne Interaktion ####
m1 <- glm(Exemplifikation.basal_Anteil_Figuren ~Werk+Genrezuordnung_formal,family=poisson(link="log"), data=d)
summary(m1)
m2 <- glm(Isomorphie.basal ~Werk+Genrezuordnung_formal,family=binomial(link="logit"),data=d)
summary(m2)
m3 <- glm(Zeitkritik ~Werk+Genrezuordnung_formal,family=binomial(link="logit"),data=d)
summary(m3)

# dispersion test auf overdispersion

# H0: mean=Varianz ist gegeben
# H1 bei "greater": overdispersion
dispersiontest(m1, alternative = "greater") # greater = overdispersion
# Ergebnis: m1 hat overdispersion

# Negativ binominal
summary(mod_nb1 <-glm.nb(Exemplifikation.basal_Anteil_Figuren ~Werk+Genrezuordnung_formal, data=d))

#Einseitiger Signifikanztest
res1 <- summary(mod_nb1)
res2 <- summary(m2)
res3 <- summary(m3)
res1

# For H1: beta > 0 (Koeffizient soll größer 0 sein)
p1 <- pt(coef(res1)[, 3], df.residual(mod_nb1), lower = FALSE)  # For H1: beta > 0 (Koeffizient soll größer 0 sein)
p2 <- pt(coef(res2)[, 3], df.residual(m2), lower = FALSE)  # For H1: beta > 0 (Koeffizient soll größer 0 sein)
p3 <- pt(coef(res3)[, 3], df.residual(m3), lower = FALSE)  # For H1: beta > 0 (Koeffizient soll größer 0 sein)
p1[5]

# For H1: beta < 0 (Koeffizient soll kleiner 0 sein)
#p3 <- pt(coef(res3)[, 3], df.residual(m3), lower = TRUE)  # For H1: beta > 0 (Koeffizient soll größer 0 sein)


# p-adjust
ps <- NULL
ps <- c(p1[5],p2[5], p3[5])
ps
ps_new <- p.adjust(ps, "fdr")
ps_new
ps_new[[3]][1]

# Stargazer M1 (p-Werte noch zu adjustieren) 
stargazer(mod_nb1, covariate.labels=c("Werk2 (Keller)","Werk3 (Müller-Guttenbrunn)", "Werk4 (Spielhagen)", "Genrezuordnung (ZR=1)"), 
          star.char = c("*", "**", "***"), star.cutoffs = c(0.05, 0.01, 0.001), report=('vcp*'), decimal.mark=",", 
          single.row=TRUE, type = "html", intercept.bottom = T, model.numbers = FALSE,
          out = "./Stargazer/Zeitroman/Negativ-binomiale-Regression(A3).htm")

#M2 und M3 (p-Werte noch zu adjustieren)
stargazer(m2, m3, covariate.labels=c("Werk2 (Keller)","Werk3 (Müller-Guttenbrunn)", "Werk4 (Spielhagen)", "Genrezuordnung (ZR=1)"), 
          star.char = c("*", "**", "***"), star.cutoffs = c(0.05, 0.01, 0.001), report=('vc*p'), decimal.mark=",", 
          single.row=TRUE, type = "html", intercept.bottom = T, model.numbers = FALSE,
          out = "./Stargazer/Zeitroman/Logistische-Regressionen(A4).htm")


#Effektstärken M2
# Funktion zur Konversion von odds ratios in d (nach Borenstein et al. 2009)
make_d <- function(x){log(x)*(sqrt(3)/pi)}
round( cbind( OR = exp( coef(m2)), d = make_d(exp(coef(m2))) ) , 3)
#Effektstärken M3
make_d <- function(x){log(x)*(sqrt(3)/pi)}
round( cbind( OR = exp( coef(m3)), d = make_d(exp(coef(m3))) ) , 3)




###############################################################
### Deskriptive Statistiken (Anhang) #########################
##############################################################

#count-data
countList = c("Objektbezug.basal",	
              "Objektbezug.basal_Anteil_Figuren",	
              "Objektbezug.basal_Anteil_Ereignisse",	
              "Objektbezug.basal_Anteil_Orte",	
              "Objektbezug.basal_unspezifisch_gesamt",	
              "Objektbezug.basal_Genese",	
              "Objektbezug.basal_Ähnlichkeit",	
              "Objektbezug.basal_Referenz",	
              "Exemplifikation.basal",	
              "Exemplifikation.basal_Anteil_Figuren",
              "Exemplifikation.basal_Anteil_Gesellschaft",	
              "Exemplifikation.basal_Anteil_Stimmung",	
              "Instanziierung.basal",
              "Klassenidentität.basal", 
              "Explizite.Wahrheit.basal_Anzahl", 
              "Implizite.Wahrheit.basal"
              ) # hier fallstudiensp. Variablen hinzufügen

countList
dflist <- data.frame(names = countList) %>% mutate(names = as.character(names))
dflist
dfnew <- select(d, one_of(dflist$names))
require(reporttools)
vars <- dfnew
group <- d[,c('Genrezuordnung_formal')]
mystats <- list("n", "mean", "s", "SE" = function(x){return((sd(x)/sqrt(length(x))))})
capture.output( TN <- tableContinuous(vars = dfnew, stats = mystats, group = group, prec = 2, longtable = TRUE),
                file="./Stargazer/Zeitroman/deskriptiv-count.tex")


#dichotome-data
varList = c("Bewertung_expliziter_Wahrheiten", 
            "Bewertung_impliziter_Wahrheiten",	
            "Arg_Auseinandersetzung_impl_expl_Wahrheit", 
            "Isomorphie.basal", 
            "Identifikation_Autor_Erzählerfigur", 
            "Impl_Identifikation_Autor_Erzähler",
            "Ästhetische_Wertung_dichotom", 
            "Absprechen_Literaturstatus", 
            "Universelles_Thema",
            "Epochenspezifisches_Thema",
            "Thematisierung_Sprache_Stil",	
            "Bewertung_Sprache_Stil", 
            "Kritik_anderer_Interpretationen",
            "Mangelnder_Realismus",
            "Lob_des_Realismus",
            "Zeitkritik")
varList
dflist <- data.frame(names = varList) %>% mutate(names = as.character(names))
dflist
dfnew <- select(d, one_of(dflist$names))
require(reporttools)
vars <- dfnew
group <- d[,c('Genrezuordnung_formal')]
mystats <- list("n", "mean")
capture.output( TN <- tableContinuous(vars = dfnew, stats = mystats, group = group, prec = 2, longtable = TRUE),
                file="./Stargazer/Zeitroman/deskriptiv-dichotom.tex")

#ordinale-data
d$Explizite.Wahrheit.basal_ordinal <- factor(d$Explizite.Wahrheit.basal_ordinal, levels = c(0,1,2,3,4), labels = c("nie", "selten", "gelegentlich", "oft", "immer"))
d$Explizite.Wahrheit.basal_ordinal
#d$Ästhetische_Wertung <- factor(d$Ästhetische_Wertung, levels = c(0,1,2,3,4), labels = c("sehr schlecht", "schlecht", "teils gut, teils schlecht", "gut", "sehr gut"))
#d$Ästhetische_Wertung
varList = c("Explizite.Wahrheit.basal",
            "Explizite.Wahrheit.basal_ordinal",            
            "Ästhetische_Wertung"
            
) # hier fallstudiensp. Variablen hinzufügen
varList
dflist <- data.frame(names = varList) %>% mutate(names = as.character(names))
dflist
dfnew <- select(d, one_of(dflist$names))
require(reporttools)
vars <- dfnew
group <- d[,c('Genrezuordnung_formal')]
mystats <- list("n", "mean", "s", "SE" = function(x){return((sd(x)/sqrt(length(x))))})
capture.output( TN <- tableNominal(vars = dfnew, cumsum = FALSE, stats = mystats, group = group, prec = 2, longtable = TRUE),
                file="./Stargazer/Zeitroman/deskriptiv-ordinal.tex")


#####################################
### Werkspezifische Unterschiede ####
#####################################

labelsWerk <- c("Hollaender", "Keller", "Müller-Guttenbrunn", "Spielhagen")

# Isomorphie nach Werk und Genre
d.sum <- summarySE(d, measurevar="Isomorphie.basal", groupvars=c("Genrezuordnung_formal","Werk"), na.rm = TRUE)
d.sum
pd <- position_dodge(0.1) # move them .05 to the left and right
posthoc <- ggplot(d.sum, aes(x=Genrezuordnung_formal, y=Isomorphie.basal, group=Werk, color=Werk, shape=Werk)) + 
  #geom_errorbar(aes(ymin=Immersion-se, ymax=Immersion+se), width=.1, position=pd) +
  geom_line() +
  geom_point() + 
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"),
        legend.box.margin=margin(-15,0,0,0)) +
  ggtitle("Isomorphie-basal nach Genrezuordnung und Werk") + labs(x="", y = "Mittelwert") + 
  scale_x_discrete(labels=c("R" = "Roman", "TR" = "Zeitroman"), expand=c(0, 0.5)) +
  scale_colour_brewer(name='Werk',labels=labelsWerk, type = "seq", palette = "Dark2", direction = 1,
                      aesthetics = "colour")+
  scale_shape_manual(name='Werk',labels=labelsWerk, values=c(16,15,17,25))
posthoc 
speichern("Isomorphie-basal")

# Exemplifikation-Anteil-Figuren
d$Exemplifikation.basal_Anteil_Figuren
d.sum <- summarySE(d, measurevar="Exemplifikation.basal_Anteil_Figuren", groupvars=c("Genrezuordnung_formal","Werk"), na.rm = TRUE)
d.sum
pd <- position_dodge(0.1) # move them .05 to the left and right
posthoc <- ggplot(d.sum, aes(x=Genrezuordnung_formal, y=Exemplifikation.basal_Anteil_Figuren, group=Werk, color=Werk, shape=Werk)) + 
  #geom_errorbar(aes(ymin=Immersion-se, ymax=Immersion+se), width=.1, position=pd) +
  geom_line() +
  geom_point() + 
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"),
        legend.box.margin=margin(-15,0,0,0)) +
  ggtitle("Exemplifikation-basal-Anteil-Figuren nach Genrezuordnung\nund Werk") + labs(x="", y = "Mittelwert") + 
  scale_x_discrete(labels=c("R" = "Roman", "TR" = "Zeitroman"), expand=c(0, 0.5)) +
  scale_colour_brewer(name='Werk',labels=labelsWerk, type = "seq", palette = "Dark2", direction = 1,
                      aesthetics = "colour")+
  scale_shape_manual(name='Werk',labels=labelsWerk, values=c(16,15,17,25))
posthoc 
speichern("Exemplifikation-basal_Anteil_Figuren (nach Werk)")


# Implizite Wahrheit
d.sum <- summarySE(d, measurevar="Implizite.Wahrheit.basal", groupvars=c("Genrezuordnung_formal","Werk"))
d.sum
pd <- position_dodge(0.1) # move them .05 to the left and right
posthoc <- ggplot(d.sum, aes(x=Genrezuordnung_formal, y=Implizite.Wahrheit.basal, group=Werk, color=Werk, shape=Werk)) + 
  #geom_errorbar(aes(ymin=Exemplifikation.basal-se, ymax=Exemplifikation.basal+se), width=.1, position=pd) +
  geom_line() +
  geom_point() + 
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"),
        legend.box.margin=margin(-15,0,0,0)) +
  ggtitle("Implizite-Wahrheit-basal nach Genrezuordnung und Werk") + labs(x="", y = "Mittelwert") + 
  scale_x_discrete(labels=c("R" = "Roman", "TR" = "Zeitroman"), expand=c(0, 0.5)) +
  scale_colour_brewer(name='Werk',labels=labelsWerk, type = "seq", palette = "Dark2", direction = 1,
                      aesthetics = "colour")+
  scale_shape_manual(name='Werk',labels=labelsWerk, values=c(16,15,17,25))
posthoc
speichern("Implizite-Wahrheit-basal nach Genrezuordnung und Werk")


# Zeitkritik
d.sum <- summarySE(d, measurevar="Zeitkritik", groupvars=c("Genrezuordnung_formal","Werk"))
d.sum
pd <- position_dodge(0.1) # move them .05 to the left and right
posthoc <- ggplot(d.sum, aes(x=Genrezuordnung_formal, y=Zeitkritik, group=Werk, color=Werk, shape=Werk)) + 
  #geom_errorbar(aes(ymin=Exemplifikation.basal-se, ymax=Exemplifikation.basal+se), width=.1, position=pd) +
  geom_line() +
  geom_point() + 
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"),
        legend.box.margin=margin(-15,0,0,0)) +
  ggtitle("Zeitkritik nach Genrezuordnung und Werk") + labs(x="", y = "Mittelwert") + 
  scale_x_discrete(labels=c("R" = "Roman", "TR" = "Zeitroman"), expand=c(0, 0.5)) +
  scale_colour_brewer(name='Werk',labels=labelsWerk, type = "seq", palette = "Dark2", direction = 1,
                      aesthetics = "colour")+
  scale_shape_manual(name='Werk',labels=labelsWerk, values=c(16,15,17,25))
posthoc
speichern("Zeitkritik nach Genrezuordnung und Werk")




# mangelnder Realismus
d.sum <- summarySE(d, measurevar="Mangelnder_Realismus", groupvars=c("Genrezuordnung_formal","Werk"), na.rm = TRUE)
d.sum
pd <- position_dodge(0.1) # move them .05 to the left and right
posthoc <- ggplot(d.sum, aes(x=Genrezuordnung_formal, y=Mangelnder_Realismus, group=Werk, color=Werk, shape=Werk)) + 
  #geom_errorbar(aes(ymin=Immersion-se, ymax=Immersion+se), width=.1, position=pd) +
  geom_line() +
  geom_point() + 
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"),
        legend.box.margin=margin(-15,0,0,0)) +
  ggtitle("Mangelnder-Realismus") + labs(x="", y = "Mittelwert") + 
  scale_x_discrete(labels=c("R" = "Roman", "TR" = "Zeitroman"), expand=c(0, 0.5)) +
  scale_colour_brewer(name='Werk',labels=labelsWerk, type = "seq", palette = "Dark2", direction = 1,
                      aesthetics = "colour")+
  scale_shape_manual(name='Werk',labels=labelsWerk, values=c(16,15,17,25))
posthoc 
speichern("Mangelnder-Realismus")


#Liniendiagramm Moralische Wertung nach referenzialisierten Figuren 
d.sum <- summarySE(d, measurevar="Moralische_Wertung_dichotom", groupvars=c("Objektbezug.basal_Referenz", "Moralische_Wertung_dichotom"))
d.sum
names(d.sum)[4]<-"new_name"

pd <- position_dodge(0.1) # move them .05 to the left and right
posthoc <- ggplot(d.sum, aes(x=Objektbezug.basal_Referenz, y=N, group=Moralische_Wertung_dichotom)) + 
  #geom_errorbar(aes(ymin=Exemplifikation.basal-se, ymax=Exemplifikation.basal+se), width=.1, position=pd) +
  geom_line() +
  geom_point() + 
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="top", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line")) +
  ggtitle("Exemplifikation-basal nach Genrezuordnung und Werk") + labs(x="", y = "Mittelwert") + 
  #scale_x_discrete(labels=c("R" = "Roman", "SR" = "Zeitroman"), expand=c(0, 0.5)) +
  scale_colour_brewer(name='Werk',labels=labelsWerk, type = "seq", palette = "Dark2", direction = 1,
                      aesthetics = "colour")
  #scale_shape_manual(name='Werk',labels=labelsWerk, values=c(16,15,17,25))
posthoc


# Post-hoc: Tage seit erster Rez. und Referenz
b1 <- ggplot(d[c(0:93, 95:119),], aes(x = Tage_seit_Erstrezension, y = Objektbezug.basal_Referenz, group=Werk, color=Werk))
b1 + geom_point(alpha = 0.3, aes(colour = Werk))+ 
  geom_smooth(method = "lm", alpha = 0.3, se = T)+
labs(x="Tage seit Erstrezension", y = "Referenz")+ 
ggtitle("Referenz nach Tage-seit-Erstrezension und Werk") + 
theme(plot.title = element_text(size=11), axis.title=element_text(size=8), legend.spacing.x=unit(0.15, 'cm'),
      legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
      legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
      legend.margin=margin(0,0,0,0), legend.box.margin=margin(-10,0,0,0)) #Abstand der Legende von Plot
speichern("Referenz nach Tagen")

b2 <- ggplot(d[c(0:93, 95:119),], aes(x = Tage_seit_Erstrezension, y = Moralische_Wertung_dichotom, group=Werk, color=Werk))
b2 + geom_point(alpha = 0.3, aes(colour = Werk))+ 
  geom_smooth(method = "glm", se = F, 
              method.args = list(family = "binomial"), alpha = 0.3)+
  #scale_fill_brewer(palette="Set1") + 
  labs(x="Tage seit Erstrezension", y = "Moralische Wertung (dichotom)")+ 
  ggtitle("Moralische Wertung nach Tage-seit-Erstrezension und Werk") + 
  theme(plot.title = element_text(size=11), axis.title=element_text(size=8), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
        legend.margin=margin(0,0,0,0), legend.box.margin=margin(-10,0,0,0)) #Abstand der Legende von Plot
speichern("Moralische Wertung (dichotom) nach Tagen")



