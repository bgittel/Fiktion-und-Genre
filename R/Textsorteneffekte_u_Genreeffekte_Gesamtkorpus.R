###############################################################
### Textsorteneffekte und Genreeffekte - Gesamtkorpus ########
###############################################################

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
install.packages("gghighlight")
install.packages("tidyverse")
install.packages("hrbrthemes")
install.packages("gtools")

library(hrbrthemes)
library(pscl) 
library(AER) 
library(Rmisc)
library("RColorBrewer")
library("lme4")
library("ordinal")
library("MASS")
library(ggplot2)
library(ggsignif)
library(ggpubr)
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
library(gtools)
library(grid)
library(gghighlight)


############################################
### Daten einlesen ZR ####
############################################
Sys.setlocale(locale = "German")
setwd("C:/Users/Benjamin/Dropbox/Wissenschaft/Fiktion und Genre/Habil/R/github/data")
d <- read.table("Annotation_ZEITROMAN.CSV",header=TRUE, sep=";",na.strings=c("NA","-"))
head(d)
d <- d[1:66,1:98]
str(d)

d$Werk <- factor(d$Werk)
d$Werk <- factor(d$Werk, levels = c(0,1,2,3), labels = c("Hollaender", "Keller", "Müller-Guttenbrunn", "Spielhagen"))
d$Werk
d$Genrezuordnung_formal <- factor(d$Genrezuordnung_formal)
d$Genrezuordnung_formal <- factor(d$Genrezuordnung_formal, levels = c(0,1), labels = c("Roman", "Einschlägiges Genre"))


d$Ästhetische_Wertung <- factor(d$Ästhetische_Wertung)
d$Ästhetische_Wertung_neu <- ifelse((d$Ästhetische_Wertung == 0), 1, ifelse((d$Ästhetische_Wertung ==1), 2,ifelse((d$Ästhetische_Wertung == 2), 3, ifelse((d$Ästhetische_Wertung == 3), 4, ifelse((d$Ästhetische_Wertung ==4), 5, NA)))))
d$Ästhetische_Wertung_neu <- factor(d$Ästhetische_Wertung_neu)
d$Ästhetische_Wertung <- d$Ästhetische_Wertung_neu

d$Ästhetische_Wertung <- factor(d$Ästhetische_Wertung, levels = c(1,2,3,4,5), labels = c("sehr schlecht", "schlecht", "teils gut, teils schlecht", "gut", "sehr gut"))
d$Ästhetische_Wertung

d$Ästhetische_Wertung_mitNA <- addNA(d$Ästhetische_Wertung)
d$Ästhetische_Wertung_mitNA


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
#Output: "Explizite.Wahrheit.basal" (eine ordinale Variable) u. "Explizite.Wahrheit.basal_metrisch" (eine ordinale, aber nicht als factor behandelt) 
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

########################
### ZR-df vorbereiten###
########################
d$Texttyp="ZR"

ZRdf <- d
ZRdf

############################################
### Daten einlesen TR ####
############################################
dTR <- read.table("Annotation_Tendenzroman.CSV",header=TRUE, sep=";",na.strings=c("NA","-"))
head(dTR)
dTR <- dTR[1:66,1:60]
str(dTR)


#Reihenfolge der Autoren geändert (Jordan, Kretzer, Mauthner, Suttner)
dTR$Werk <- ifelse((dTR$Werk == 0), 3, ifelse((dTR$Werk == 1), 2, ifelse((dTR$Werk == 2), 0, 
                                                                   ifelse((dTR$Werk == 3), 1, "NA"))))
dTR$Werk <- factor(dTR$Werk)
dTR$Werk <- factor(dTR$Werk, levels = c(0,1,2,3), labels = c("Jordan", "Kretzer", "Mauthner", "Suttner"))
dTR$Werk
dTR$Genrezuordnung_formal <- factor(dTR$Genrezuordnung_formal)
dTR$Genrezuordnung_formal <- factor(dTR$Genrezuordnung_formal, levels = c(0,1), labels = c("Roman", "Einschlägiges Genre"))


dTR$Ästhetische_Wertung <- factor(dTR$Ästhetische_Wertung)
dTR$Ästhetische_Wertung_neu <- ifelse((dTR$Ästhetische_Wertung == 0), 1, ifelse((dTR$Ästhetische_Wertung ==1), 2,ifelse((dTR$Ästhetische_Wertung == 2), 3, ifelse((dTR$Ästhetische_Wertung == 3), 4, ifelse((dTR$Ästhetische_Wertung ==4), 5, NA)))))
dTR$Ästhetische_Wertung_neu <- factor(dTR$Ästhetische_Wertung_neu)
dTR$Ästhetische_Wertung <- dTR$Ästhetische_Wertung_neu

dTR$Ästhetische_Wertung <- factor(dTR$Ästhetische_Wertung, levels = c(1,2,3,4,5), labels = c("sehr schlecht", "schlecht", "teils gut, teils schlecht", "gut", "sehr gut"))
dTR$Ästhetische_Wertung

dTR$Ästhetische_Wertung_mitNA <- addNA(dTR$Ästhetische_Wertung)
dTR$Ästhetische_Wertung_mitNA


### OUTLIERS
#generate z-scores for variable A using the scale() function
z <-- scale(dTR$Explizite.Wahrheit.basal_Anzahl, center = TRUE, scale = TRUE)
z
# Ergebnis TR-Rez47 ist ein outlier -> Korrektur nach Tabachnik nächstgrößerer Wert des zweitgrößten Werts zuweisen
dTR$Explizite.Wahrheit.basal_Anzahl[47]
dTR$Explizite.Wahrheit.basal_Anzahl[47] <- 15
max(dTR$Explizite.Wahrheit.basal_Anzahl, na.rm = TRUE)

# Skalentransformation explWahrheit (count -> ordinal) und dann die neue ordinale nur nehmen, wenn es noch keinen ordinalen Wert gab
# Skalentransformation-neu (cut-Funktion) Kategorie "nie" ist mit 0 besetzt
# Input ist "Explizite.Wahrheit.basal_Anzahl" und "Explizite.Wahrheit.basal_ordinal"
#Output: "Explizite.Wahrheit.basal" (eine ordinale Variable) u. "Explizite.Wahrheit.basal_metrisch" (eine ordinale, aber nicht als factor behandelt) 
max = 15 # maximale Anzahl Explizite_Wahrheit_Anzahl im Gesamtkorpus
dTR$Explizite.Wahrheit.basal_Anzahl
cut(dTR$Explizite.Wahrheit.basal_Anzahl, breaks=c(1, 1+(max-1)/4, 1+(max-1)/4*2, 1+(max-1)/4*3, max), ordered_result=FALSE, right=TRUE, include.lowest=TRUE)
dTR$Explizite_Wahrheit_temp_ordinal <- cut(dTR$Explizite.Wahrheit.basal_Anzahl, breaks=c(1, 1+(max-1)/4, 1+(max-1)/4*2, 1+(max-1)/4*3, max), labels=c("1", "2", "3", "4"), ordered_result=FALSE, right=TRUE, include.lowest=TRUE)
# NAs auf 0 setzen
dTR$Explizite_Wahrheit_temp_ordinal <- ifelse(is.na(dTR$Explizite_Wahrheit_temp_ordinal), "0",  dTR$Explizite_Wahrheit_temp_ordinal)
dTR$Explizite_Wahrheit_temp_ordinal
#Zusammenführen der Variablen
dTR$Explizite.Wahrheit.basal <- ifelse(is.na(dTR$Explizite.Wahrheit.basal_ordinal), as.numeric(as.character(dTR$Explizite_Wahrheit_temp_ordinal)),  dTR$Explizite.Wahrheit.basal_ordinal)
dTR$Explizite.Wahrheit.basal  
dTR$Explizite.Wahrheit.basal_metrisch <- dTR$Explizite.Wahrheit.basal #zum plotten als metrische Variable behalten
#dTR$Explizite.Wahrheit.basal <- factor(dTR$Explizite.Wahrheit.basal)
dTR$Explizite.Wahrheit.basal <- factor(dTR$Explizite.Wahrheit.basal, levels = c(0,1,2,3,4), labels = c("nie", "selten", "gelegentlich", "häufig", "immer"))
dTR$Explizite.Wahrheit.basal_metrisch
dTR$Explizite.Wahrheit.basal

########################
### TR-df vorbereiten###
########################
dTR$Texttyp="TR"

TRdf <- dTR
TRdf



############################################
### Daten einlesen HR ####
############################################
dHR <- read.table("Annotation_Historischer_Roman.CSV",header=TRUE, quote="", sep=";",na.strings=c("NA","-"))
head(dHR)
dHR <- dHR[1:83,1:55]
str(dHR)

dHR$Genrezuordnung_formal <- factor(dHR$Genrezuordnung_formal)
dHR$Genrezuordnung_formal <- factor(dHR$Genrezuordnung_formal, levels = c(0,1), labels = c("Roman", "Einschlägiges Genre"), ordered=FALSE)

dHR$Genrezuordnung_formal
#Reihenfolge der Autoren geändert (Ebers, Fontane, Gutzkow, Meyer)
dHR$Werk
dHR$Werk <- ifelse((dHR$Werk == 0), 1, ifelse((dHR$Werk == 1), 3, ifelse((dHR$Werk == 2), 0, 
                                                                   ifelse((dHR$Werk == 3), 2, "NA"))))
dHR$Werk <- factor(dHR$Werk)
dHR$Werk <- factor(dHR$Werk, levels = c(0,1,2,3), labels = c("Ebers", "Fontane", "Gutzkow", "C.F. Meyer"))
dHR$Werk

dHR$Ästhetische_Wertung <- factor(dHR$Ästhetische_Wertung)
dHR$Länge <- factor(dHR$Länge)
dHR$Ästhetische_Wertung

# Skalentransformation explWahrheit (count -> ordinal) und dann die neue ordinale nur nehmen, wenn es noch keinen ordinalen Wert gab
# Skalentransformation-neu (cut-Funktion) Kategorie "nie" ist mit 0 besetzt
# Input ist "Explizite.Wahrheit.basal_Anzahl" und "Explizite.Wahrheit.basal_ordinal"
#Output: "Explizite.Wahrheit.basal" (eine ordinale Variable) u. "Explizite.Wahrheit.basal_metrisch" (eine ordinale, aber nicht als factor behandelt) 
max = 15 # maximale Anzahl Explizite_Wahrheit_Anzahl im Gesamtkorpus
dHR$Explizite.Wahrheit.basal_Anzahl
cut(dHR$Explizite.Wahrheit.basal_Anzahl, breaks=c(1, 1+(max-1)/4, 1+(max-1)/4*2, 1+(max-1)/4*3, max), ordered_result=FALSE, right=TRUE, include.lowest=TRUE)
dHR$Explizite_Wahrheit_temp_ordinal <- cut(dHR$Explizite.Wahrheit.basal_Anzahl, breaks=c(1, 1+(max-1)/4, 1+(max-1)/4*2, 1+(max-1)/4*3, max), labels=c("1", "2", "3", "4"), ordered_result=FALSE, right=TRUE, include.lowest=TRUE)
# NAs auf 0 setzen
dHR$Explizite_Wahrheit_temp_ordinal <- ifelse(is.na(dHR$Explizite_Wahrheit_temp_ordinal), "0",  dHR$Explizite_Wahrheit_temp_ordinal)
dHR$Explizite_Wahrheit_temp_ordinal
#Zusammenführen der Variablen
dHR$Explizite.Wahrheit.basal <- ifelse(is.na(dHR$Explizite.Wahrheit.basal_ordinal), as.numeric(as.character(dHR$Explizite_Wahrheit_temp_ordinal)),  dHR$Explizite.Wahrheit.basal_ordinal)
dHR$Explizite.Wahrheit.basal  
dHR$Explizite.Wahrheit.basal_metrisch <- dHR$Explizite.Wahrheit.basal #zum plotten als metrische Variable behalten
dHR$Explizite.Wahrheit.basal <- factor(dHR$Explizite.Wahrheit.basal, levels = c(0,1,2,3,4), labels = c("nie", "selten", "gelegentlich", "häufig", "immer"))
dHR$Explizite.Wahrheit.basal_metrisch
dHR$Explizite.Wahrheit.basal

########################
### HR-df vorbereiten###
########################
dHR$Texttyp="HR"

HRdf <- dHR
HRdf


############################################
### Daten einlesen SR ####
############################################
setwd("C:/Users/Benjamin/Dropbox/Wissenschaft/Fiktion und Genre/Habil/R/github/data")
d <- read.table("Annotation_Schlüsselroman.csv",header=TRUE, sep=";",na.strings=c("NA","-"))
head(d)
#d <- d[1:120,1:44]
str(d)

d$Genrezuordnung_formal <- factor(d$Genrezuordnung_formal)
d$Genrezuordnung_formal <- factor(d$Genrezuordnung_formal, levels = c(0,1), labels = c("Roman", "Einschlägiges Genre"), ordered=FALSE)
d$Genrezuordnung_formal

#Reihenfolge der Autoren geändert (Ebers, Fontane, Gutzkow, Meyer)
#d$Werk <- ifelse((d$Werk == 0), 1, ifelse((d$Werk == 1), 3, ifelse((d$Werk == 2), 0, 
#                                                                   ifelse((d$Werk == 3), 2, "NA"))))
d$Werk <- factor(d$Werk)
d$Werk <- factor(d$Werk, levels = c(0,1,2), labels = c("Walser", "Biller", "Gstrein"))
d$Werk

# Ordinale Variablen als Faktor
d$Ästhetische_Wertung <- factor(d$Ästhetische_Wertung)
d$Länge <- factor(d$Länge)
d$Ästhetische_Wertung

# Skalentransformation explWahrheit (count -> ordinal) und dann die neue ordinale nur nehmen, wenn es noch keinen ordinalen Wert gab
# Skalentransformation-neu (cut-Funktion) Kategorie "nie" ist mit 0 besetzt
# Input ist "Explizite.Wahrheit.basal_Anzahl" und "Explizite.Wahrheit.basal_ordinal"
#Output: "Explizite.Wahrheit.basal" (eine ordinale Variable) u. "Explizite.Wahrheit.basal_metrisch" (eine ordinale, aber nicht als factor behandelt) 
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

########################
### SR-df vorbereiten###
########################
d$Texttyp="SR"

SRdf <- d
SRdf


###############################
### Zusammenführen der df's ###
###############################
library(gtools)
gesamt <- smartbind(ZRdf, TRdf, HRdf, SRdf)
gesamt
colnames(gesamt)[colSums(is.na(gesamt)) > 0] # Test auf NAs
gesamt$Texttyp <- factor(gesamt$Texttyp, levels = c("HR", "TR", "ZR", "SR"))

gesamt$Texttyp
gesamt$Genrezuordnung_formal
gesamt$Jahr
colnames(gesamt)

gesamt.sum <- summarySE(gesamt, measurevar="Isomorphie.basal", groupvars=c("Texttyp","Genrezuordnung_formal"), na.rm = TRUE)
gesamt.sum

# Variablenname ändern
#names(d)[names(d) == 'Referenz.basal_Anteil_Ort'] <- 'Referenz.basal_Anteil_Orte'


dvbinaryList = c("Bewertung_explizite_Wahrheit", 
            "Bewertung_implizite_Wahrheit",	
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
            "Kritik_anderer_Interpretationen")


dvordinalList = c("Explizite.Wahrheit.basal",
            "Explizite.Wahrheit.basal_ordinal",            
            "Ästhetische_Wertung")

dvcountList = c("Objektbezug.basal",	
              "Objektbezug.basal_Anteil_Figuren",	
              "Objektbezug.basal_Anteil_Ereignisse",	
              "Objektbezug.basal_Anteil_Orte",	
              "Objektbezug.basal_unspezifisch_gesamt",	
              "Objektbezug.basal_Genese",	
              "Objektbezug.basal_Ähnlichkeit",	
              "Objektbezug.basal_Referenz",	
              "Exemplifikation.basal",	
              "Exemplifikation.basal_Anteil_Figuren",	
              "Exemplifikation.basal_Anteil_Handlung",
              "Exemplifikation.basal_Anteil_Gesellschaft",	
              "Exemplifikation.basal_Anteil_Stimmung",	
              "Instanziierung.basal",
              "Klassenidentität.basal", 
              "Explizite.Wahrheit.basal_Anzahl",
              "Explizite.Wahrheit.basal_metrisch",
              "Implizite.Wahrheit.basal"
) # hier fallstudiensp. Variablen hinzufügen


###############################
### Visualisierung Korpus #####
###############################

dodge <- position_dodge(width=0.9)
farbig_theme <- function(titel) {
  list(geom_col(position=dodge), 
      scale_fill_brewer(palette="Set2"), 
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
       scale_fill_brewer(palette="Set2"), 
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
Pfad = "./Abbildungen/Texttypvergleich_neu_4Typen/"
speichern <- function(datei) {
  dateipfad <- paste(Pfad, datei, ".pdf", sep = "")
  ggsave(dateipfad)
  dateipfad <- paste(Pfad, datei, ".eps", sep = "")
  ggsave(dateipfad)
  dateipfad <- paste(Pfad, datei, ".png", sep = "")
  ggsave(dateipfad, dpi = 1200)
}
  
# Anzahl Rezensionen pro Werk
rez <- ggplot(d,aes(factor(Werk), group= Werk, fill=Werk)) + 
  geom_bar(show.legend = FALSE) + 
  scale_fill_brewer(palette="Paired") +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), position = position_stack(vjust = 1.15)) +
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


# Binäre Variablen ###########################################################################
y <- 0
plotdata_list = list()
plotdata_list = NULL
y <- 0 # counter
for (i in dvbinaryList) {
  print(i)
  Group <- c("Historischer Roman", "Tendenzroman", "Zeitroman")
  y <- y+1
  # dv <- eval(parse(text=paste("gesamt$", dvbinaryList[[y]], sep='')))
  # print(dv)
  summary <- summarySE(gesamt, measurevar=i, groupvars=c("Texttyp","Genrezuordnung_formal"), na.rm = TRUE)
  colnames(summary)[which(names(summary) == i)] <- "DvValue" # ersetzen des Variablennamens
  summary <- cbind(i=i, summary) # Variablenname in erste Spalte
  plotdata_list[[y]] <- summary
  
}
plotdata_list
dvbinaryList


### Count Variablen ##########################################################################
y <- 0
plotdata_list = list()
plotdata_list = NULL
y <- 0 # counter
for (i in dvcountList) {
  print(i)
  Group <- c("Historischer Roman", "Tendenzroman", "Zeitroman", "Schlüsselroman")
  y <- y+1
  # dv <- eval(parse(text=paste("gesamt$", dvbinaryList[[y]], sep='')))
  # print(dv)
  summary <- summarySE(gesamt, measurevar=i, groupvars=c("Texttyp","Genrezuordnung_formal"), na.rm = TRUE)
  colnames(summary)[which(names(summary) == i)] <- "DvValue" # ersetzen des Variablennamens
  summary <- cbind(i=i, summary) # Variablenname in erste Spalte
  plotdata_list[[y]] <- summary
  
}
dvcountList

### Visualisierung mehrerer Variablen in ein Diagramm ###
options(max.print = 9000)
#dev.new(width=5, height=8)
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(1:8,65:72,105:111,113:120),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
levels(plotdata_merged_sub$i) <- gsub(".", "\n", levels(plotdata_merged_sub$i), fixed = TRUE)
# Reihenfolge im Diagramm
plotdata_merged_sub$i <- factor(plotdata_merged_sub$i, levels = c('Objektbezug\nbasal', 'Exemplifikation\nbasal', 'Implizite\nWahrheit\nbasal', 'Klassenidentität\nbasal'))

# Mean für jede Variable (nur Roman-Leser) berechnen
plotdata_merged_sub
plotdata_merged_sub_sub <- plotdata_merged_sub[plotdata_merged_sub[, "Genrezuordnung_formal"] == "Roman",] # nur die Roman-Leser
Mean_Objektbezug <- mean(plotdata_merged_sub_sub$DvValue[1:4])
Mean_Exemplifikation <- mean(plotdata_merged_sub_sub$DvValue[5:8])
Mean_ImpliziteW <- mean(plotdata_merged_sub_sub$DvValue[9:12])
Mean_Klassenidentität <- mean(plotdata_merged_sub_sub$DvValue[13:16])
Mittelwerte <- data.frame(i = c("Objektbezug\nbasal", "Exemplifikation\nbasal", "Implizite\nWahrheit\nbasal", "Klassenidentität\nbasal"), 
                          GranMean = c(Mean_Objektbezug, Mean_Exemplifikation, Mean_ImpliziteW, Mean_Klassenidentität))
Mittelwerte

multiplot <- ggplot(data = plotdata_merged_sub, aes(x = Texttyp, y = DvValue, fill = Genrezuordnung_formal)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.5), alpha=1)  + 
  #gghighlight(data = plotdata_merged_sub, Texttyp == "HR", use_group_by = F) +
  facet_grid(~i, switch = 'x') +
  scale_fill_brewer(palette="Set1") +
  geom_hline(data = Mittelwerte, aes(yintercept = GranMean, group = i), colour = 'black', linetype = 3) +
  #geom_errorbar(aes(ymin = DvValue - se, ymax = DvValue + se, group=Texttyp), position=dodge, width=0.25, size=0.25) +
  #geom_signif(annotations="*", xmin=1.75, xmax=2.2, y_position = 2.8, tip_length = 0.02, size=0.25, textsize = 3) +
  ggtitle("Text-Welt-Verhältnisse") + labs(x="", y = "Mittelwert") +
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="top", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"),
        axis.text.x = element_text(angle = 0, size=8),
        panel.spacing.x=unit(0, "lines"),
        strip.placement = "outside",
        strip.text.x = element_text(size = 8, color = "black", margin = margin(0, 0, 0, 0, "cm")),
        strip.background = element_rect(
          color="white", fill="white", size=1, linetype="solid"
        ))
multiplot


  ### Speichern
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Text-Welt-Verhältnisse.pdf")
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Text-Welt-Verhältnisse.png", dpi = 1200)

multiplot2 <- multiplot + 
  ggtitle("Text-Welt-Verhältnisse: metrisch")
multiplot2
### Speichern
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Text-Welt-Verhältnisse(mit Signifikanz).eps")
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Text-Welt-Verhältnisse(mit Signifikanz).pdf")
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Text-Welt-Verhältnisse(mit Signifikanz).png", dpi = 1200)
#grau
multiplot2g <- multiplot2 + theme_bw() + scale_fill_grey(start = 0.2, end = .6) + theme(legend.position = 'bottom') +
theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
      legend.position="top", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
      legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"),
      axis.text.x = element_text(angle = 0, size=8),
      panel.spacing.x=unit(0, "lines"),
      strip.placement = "outside",
      strip.text.x = element_text(size = 8, color = "black", margin = margin(0, 0, 0, 0, "cm")),
      strip.background = element_rect(color="white", fill="white", size=1, linetype="solid"),
      panel.border = element_blank()
      )
multiplot2g
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Text-Welt-Verhältnisse(mit Signifikanz) (grau).eps")
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Text-Welt-Verhältnisse(mit Signifikanz)  (grau).pdf")
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Text-Welt-Verhältnisse(mit Signifikanz)  (grau).png", dpi = 1200)


#Nur Objektbezug für Textsorteneffekt vs. Genreeffekt
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(1:8),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
Mean_Objektbezug <- mean(c(plotdata_merged_sub$DvValue[[1]], plotdata_merged_sub$DvValue[[3]], plotdata_merged_sub$DvValue[[5]], plotdata_merged_sub$DvValue[[7]]))
Mean_Objektbezug

levels(plotdata_merged_sub$i) <- gsub(".", "\n", levels(plotdata_merged_sub$i), fixed = TRUE)
dodge <- position_dodge(width=0.9)
Objektbezug <- ggplot(plotdata_merged_sub, aes(x = Texttyp, y = DvValue, fill = Genrezuordnung_formal)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.5), alpha=1)  + facet_grid(~i, switch = 'x') +
  scale_fill_brewer(palette="Set1") +
  geom_hline(aes(yintercept = Mean_Objektbezug), colour = 'black', linetype = 3) +
  #geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE, group=Group), position=dodge, width=0.25, size=0.25) +
  ggtitle("Objektbezug-basal nach Textsorte und Genrezuordnung") + 
  labs(x="", y = "Mittelwert") +
  #scale_y_continuous(position = "left", name = "Mittelwert") +
  theme(plot.title = element_text(size=11), 
        legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8, margin = margin(r = 10, unit = "pt")), legend.justification=c(0, 0), legend.key.size = unit(0.75 ,"line"),        axis.text.x = element_text(angle = 0, size=8),
        legend.margin=margin(0,0,0,0), legend.box.margin=margin(-15,0,0,0),
        panel.spacing.x=unit(0, "lines"),
        strip.placement = "outside",
        strip.text.x = element_text(size = 8, color = "black", margin = margin(0, 0, 0, 0, "cm")),
        strip.background = element_rect(color="white", fill="white", size=1, linetype="solid"
        ))
Objektbezug
Objektbezug <- annotate_figure(Objektbezug,
                bottom = text_grob("HR - Historischer Roman | TR - Tendenzroman | ZR - Zeitroman | SR - Schlüsselroman", color = "black",
                                   hjust = 1.16, vjust = -0.93, x = 1, size = 8))
Objektbezug                
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Objektbezug.pdf")
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Objektbezug.png", dpi = 1200)



#grau
Objektbezugg <- Objektbezug + theme_bw() + scale_fill_grey(start = 0.2, end = .6) +theme(legend.position = "none") +
  theme(plot.title = element_text(size=11), axis.title=element_blank(), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"),
        legend.margin=margin(0,0,0,0), legend.box.margin=margin(-15,0,0,0),
        axis.text.x = element_text(angle = 0, size=8),
        panel.spacing.x=unit(0, "lines"),
        strip.placement = "outside",
        strip.text.x = element_text(size = 8, color = "black", margin = margin(0, 0, 0, 0, "cm")),
        strip.background = element_rect(color="white", fill="white", size=1, linetype="solid"),
        panel.border = element_blank()
  )
Objektbezugg
Objektbezugg <- annotate_figure(Objektbezugg,
                               bottom = text_grob("HR - Historischer Roman | TR - Tendenzroman | ZR - Zeitroman | SR - Schlüsselroman", color = "black",
                                                  hjust = 1.19, vjust = -0.93, x = 1, size = 8))
Objektbezugg                
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Objektbezug (grau).pdf")
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Objektbezug (grau).png", dpi = 1200)



#Isomorphie (erst Werte für binäre Variablen berechnen)
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(25:32),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
Mean_Isomorphie <- mean(c(plotdata_merged_sub$DvValue[[1]], plotdata_merged_sub$DvValue[[3]], plotdata_merged_sub$DvValue[[5]], plotdata_merged_sub$DvValue[[7]]))
Mean_Isomorphie


levels(plotdata_merged_sub$i) <- gsub(".", "\n", levels(plotdata_merged_sub$i), fixed = TRUE)
dodge <- position_dodge(width=0.9)
Isomorphie <- ggplot(plotdata_merged_sub, aes(x = Texttyp, y = DvValue, fill = Genrezuordnung_formal)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.5), alpha=1)  + facet_grid(~i, switch = 'x') +
  scale_fill_brewer(palette="Set1") +
  geom_hline(aes(yintercept = Mean_Isomorphie), colour = 'black', linetype = 3) +
  #geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE, group=Group), position=dodge, width=0.25, size=0.25) +
  ggtitle("dichotom") + 
  labs(x="", y = "Mittelwert") +
  scale_y_continuous(position = "right") +
  theme(plot.title = element_text(size=11,hjust = 0.5), axis.title=element_blank(), 
        legend.position="none", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"),
        axis.text.x = element_text(angle = 0, size=8),
        panel.spacing.x=unit(0, "lines"),
        strip.placement = "outside",
        strip.text.x = element_text(size = 8, color = "black", margin = margin(0, 0, 0, 0, "cm")),
        strip.background = element_rect(
          color="white", fill="white", size=1, linetype="solid"
        ))
Isomorphie
#grau
Isomorphieg <- Isomorphie + theme_bw() + scale_fill_grey(start = 0.2, end = .6) +theme(legend.position = "none") +
  theme(plot.title = element_text(size=11, hjust = 0.5), axis.title=element_blank(), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="none", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"),
        axis.text.x = element_text(angle = 0, size=8),
        panel.spacing.x=unit(0, "lines"),
        strip.placement = "outside",
        strip.text.x = element_text(size = 8, color = "black", margin = margin(0, 0, 0, 0, "cm")),
        strip.background = element_rect(color="white", fill="white", size=1, linetype="solid"),
        panel.border = element_blank()
  )
Isomorphieg

#explizite Wahrheit-basal (als metrisch behandelt)
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(129:136),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
Mean_Explizite <- mean(c(plotdata_merged_sub$DvValue[[1]], plotdata_merged_sub$DvValue[[3]], plotdata_merged_sub$DvValue[[5]], plotdata_merged_sub$DvValue[[7]]))
Mean_Explizite

levels(plotdata_merged_sub$i) <- gsub("Explizite.Wahrheit.basal_metrisch", "Explizite Wahrheit\nbasal", levels(plotdata_merged_sub$i), fixed = TRUE)
#levels(plotdata_merged_sub$i) <- gsub(".", "\n", levels(plotdata_merged_sub$i), fixed = TRUE)

explWahrheit <- ggplot(plotdata_merged_sub, aes(x = Texttyp, y = DvValue, fill = Genrezuordnung_formal)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.5), alpha=1)  + facet_grid(~i, switch = 'x') +
  geom_hline(aes(yintercept = Mean_Explizite), colour = 'black', linetype = 3) +
  scale_y_continuous(breaks=c(0,1,2), limits=c(0,2), label=c("nie", "selten", "gelegentlich"), position = "right")+
  scale_fill_brewer(palette="Set1") +
  #geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE, group=Group), position=dodge, width=0.25, size=0.25) +
  #geom_signif(annotations="*", xmin=0.75, xmax=1.2, y_position = 1.65, tip_length = 0.02, size=0.25, textsize = 3) +
  ggtitle("ordinal") + 
  labs(x="", y = "Mittelwert") +
  theme(plot.title = element_text(size=11,hjust = 0.5), axis.title=element_blank(), 
        legend.position="none", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"),
        axis.text.x = element_text(angle = 0, size=8),
        panel.spacing.x=unit(0, "lines"),
        strip.placement = "outside",
        strip.text.x = element_text(size = 8, color = "black", margin = margin(0, 0, 0, 0, "cm")),
        strip.background = element_rect(
          color="white", fill="white", size=1, linetype="solid"
        ))
explWahrheit

explWahrheitg <- explWahrheit + theme_bw() + scale_fill_grey(start = 0.2, end = .6) +theme(legend.position = "none")+
  theme(plot.title = element_text(size=11,hjust = 0.5), axis.title=element_blank(), 
        legend.position="non", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"),
        axis.text.x = element_text(angle = 0, size=8),
        panel.spacing.x=unit(0, "lines"),
        strip.placement = "outside",
        strip.text.x = element_text(size = 8, color = "black", margin = margin(0, 0, 0, 0, "cm")),
        strip.background = element_rect(color="white", fill="white", size=1, linetype="solid"),
        panel.border = element_blank()
    )
explWahrheitg

### 2 Diagramme nebeneinander ###
#dev.new(width=5, height=8)

ggarrange(
  multiplot2, explWahrheit, labels = c("A", "B"), ncol=2,
  common.legend = TRUE, legend = "bottom", align = "h", widths = 2:1)
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Text-Welt-Verhältnisse2.pdf")
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Text-Welt-Verhältnisse2.eps")
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Text-Welt-Verhältnisse2.png", dpi=1200)

### 3 Diagramme nebeneinander ###
TextWelt3 <-ggarrange(multiplot2, ggarrange(explWahrheit, Isomorphie, nrow=2, labels = c("B", "C"), align = "v"), 
  common.legend = TRUE, legend = "bottom", align = "no", widths = 2:1, ncol = 2, labels= "A")
TextWelt3
annotate_figure(TextWelt3,
              bottom = text_grob("HR - Historischer Roman | TR - Tendenzroman | ZR - Zeitroman | SR - Schlüsselroman", color = "black",
                                   hjust = 1.1, vjust = -2.5, x = 1, size = 8)
            )
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Text-Welt-Verhältnisse3.pdf", width =7 , height = 6)
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Text-Welt-Verhältnisse3.eps", width =7 , height = 6)
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Text-Welt-Verhältnisse3.png", width =7 , height = 6, dpi=1200)

### 3 Diagramme nebeneinander grau ###
TextWelt3g <- ggarrange(multiplot2g, ggarrange(explWahrheitg, Isomorphieg, nrow=2, labels = c("B", "C"), align = "v"), 
          common.legend = TRUE, legend = "bottom", align = "no", widths = 2:1, ncol = 2, labels= "A")
TextWelt3g
annotate_figure(TextWelt3g,
                bottom = text_grob("HR - Historischer Roman | TR - Tendenzroman | ZR - Zeitroman | SR - Schlüsselroman", color = "black",
                                   hjust = 1.1, vjust = -2.5, x = 1, size = 8)
)
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Text-Welt-Verhältnisse3 (grau).pdf", width =7 , height = 6)
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Text-Welt-Verhältnisse3 (grau).eps", width =7 , height = 6)
ggsave("./Abbildungen/Texttypvergleich_neu_4Typen/Text-Welt-Verhältnisse3 (grau).png", width =7 , height = 6, dpi=1200)

 


