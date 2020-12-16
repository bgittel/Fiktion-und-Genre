##################################################
### Textsorteneffekte grobkörnig (HR, TR, ZR) ####
##################################################

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
install.packages("stargazer")
install.packages("reporttools")
install.packages("Rmisc")
install.packages("sjstats")
install.packages("bucky")


library(pscl) 
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
library(bucky)
library(MASS)
library(gtools)



############################################
### Daten einlesen - Zeitroman #############
############################################
Sys.setlocale(locale = "German")
setwd("C:/Users/Benjamin/Dropbox/Wissenschaft/Fiktion und Genre/Habil/R/github/data")
d <- read.table("Annotation_Zeitroman.CSV",header=TRUE, sep=";",na.strings=c("NA","-"))
head(d)
d <- d[1:98,1:60]
str(d)

d$Werk <- factor(d$Werk)
#Reihenfolge der Autoren geändert (Jordan, Kretzer, Mauthner, Suttner)
d$Werk <- factor(d$Werk, levels = c(0,1,2,3), labels = c("Hollaender", "Keller", "Müller-Guttenbrunn", "Spielhagen"))
d$Werk
d$Genrezuordnung_formal <- factor(d$Genrezuordnung_formal)
d$Genrezuordnung_formal <- factor(d$Genrezuordnung_formal, levels = c(0,1), labels = c("Roman", "Zeitroman"))


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
d$Explizite.Wahrheit.basal <- factor(d$Explizite.Wahrheit.basal, levels = c(0,1,2,3,4), labels = c("nie", "selten", "gelegentlich", "häufig", "immer"))
d$Explizite.Wahrheit.basal_metrisch
d$Explizite.Wahrheit.basal

########################
### ZR-df vorbereiten###
########################
d$Texttyp="Zeitroman"

ZRdf <- d
ZRdf




############################################
### Daten einlesen - Tendenzroman ##########
############################################
setwd("C:/Users/Benjamin/Dropbox/Wissenschaft/Fiktion und Genre/Habil/R/github/data")
d <- read.table("Annotation_Tendenzroman.CSV",header=TRUE, sep=";",na.strings=c("NA","-"))
head(d)
d <- d[1:70,1:60]
str(d)

#Reihenfolge der Autoren geändert (Jordan, Kretzer, Mauthner, Suttner)
d$Werk <- ifelse((d$Werk == 0), 3, ifelse((d$Werk == 1), 2, ifelse((d$Werk == 2), 0, 
                                                                   ifelse((d$Werk == 3), 1, "NA"))))
d$Werk <- factor(d$Werk)
d$Werk <- factor(d$Werk, levels = c(0,1,2,3), labels = c("Jordan", "Kretzer", "Mauthner", "Suttner"))
d$Werk
d$Genrezuordnung_formal <- factor(d$Genrezuordnung_formal)
d$Genrezuordnung_formal <- factor(d$Genrezuordnung_formal, levels = c(0,1), labels = c("Roman", "Tendenzroman"))
d$Genrezuordnung_formal_weit <- factor(d$Genrezuordnung_formal_weit)
d$Genrezuordnung_formal_weit <- factor(d$Genrezuordnung_formal_weit, levels = c(0,1), labels = c("Roman", "Tendenzroman"))


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
# Ergebnis TR-Rez47 ist ein outlier -> Korrektur nach Tabachnik nächstgrößerer Wert des zweitgrößten Werts zuweisen
d$Explizite.Wahrheit.basal_Anzahl[47]
d$Explizite.Wahrheit.basal_Anzahl[47] <- 15
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
d$Explizite.Wahrheit.basal <- factor(d$Explizite.Wahrheit.basal, levels = c(0,1,2,3,4), labels = c("nie", "selten", "gelegentlich", "häufig", "immer"))
d$Explizite.Wahrheit.basal_metrisch
d$Explizite.Wahrheit.basal



########################
### TR-df vorbereiten###
########################
d$Texttyp="Tendenzroman"

TRdf <- d
TRdf



############################################
### Daten einlesen - Historischer Roman ####
############################################
setwd("C:/Users/Benjamin/Dropbox/Wissenschaft/Fiktion und Genre/Habil/R/github/data")
d <- read.table("Annotation_Historischer_Roman.CSV",header=TRUE, sep=";",na.strings=c("NA","-"))
head(d)
d <- d[1:82,1:44]
str(d)

d$Genrezuordnung_formal <- factor(d$Genrezuordnung_formal)
d$Genrezuordnung_formal <- factor(d$Genrezuordnung_formal, levels = c(0,1), labels = c("Roman", "historischer Roman"), ordered=FALSE)

#d$NrKritiker <- factor(d$NrKritiker)
d$Genrezuordnung_formal
#Reihenfolge der Autoren geändert (Ebers, Fontane, Gutzkow, Meyer)
d$Werk <- ifelse((d$Werk == 0), 1, ifelse((d$Werk == 1), 3, ifelse((d$Werk == 2), 0, 
                                                                   ifelse((d$Werk == 3), 2, "NA"))))
d$Werk <- factor(d$Werk)
d$Werk <- factor(d$Werk, levels = c(0,1,2,3), labels = c("Ebers", "Fontane", "Gutzkow", "C.F. Meyer"))
d$Werk


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
### HR-df vorbereiten###
########################
d$Texttyp="Historischer Roman"

HRdf <- d
HRdf

##############################
### Zusammenführen der dfs ###
##############################
library(gtools)
gesamt <- smartbind(ZRdf, TRdf, HRdf)
gesamt
colnames(gesamt)[colSums(is.na(gesamt)) > 0] # Test auf NAs
gesamt$Texttyp <- factor(gesamt$Texttyp)
gesamt$Texttyp

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
### Visualisierung  ###########
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
Pfad = "./Abbildungen/Texttyp-Vergleich/"
speichern <- function(datei) {
  dateipfad <- paste(Pfad, datei, ".pdf", sep = "")
  ggsave(dateipfad)
  dateipfad <- paste(Pfad, datei, ".eps", sep = "")
  ggsave(dateipfad)
  dateipfad <- paste(Pfad, datei, ".png", sep = "")
  ggsave(dateipfad, dpi = 1200)
}
  

###################################################
#### Daten generieren für Visualisierung ##########
##################################################

### Binäre Variablen ###########################################################################
y <- 0
plotdata_list = list()
plotdata_list = NULL
y <- 0 # counter
for (i in dvbinaryList) {
  Group <- c("Historischer Roman", "Tendenzroman", "Zeitroman")
  y <- y+1
  dv <- eval(parse(text=paste("gesamt$", dvbinaryList[[y]], sep=''))) 
  summary <- describeBy(dv, gesamt$Texttyp, na.rm=TRUE)
  summary
  SE <- unlist(c(summary[[1]][13], summary[[2]][13], summary[[3]][13]))
  SD <- unlist(c(summary[[1]][4], summary[[2]][13], summary[[3]][13]))
  DvValue <- unlist(c(summary[[1]][3], summary[[2]][3], summary[[3]][3])) # Mittelwert
  summary
  plotdata <- data.frame(Group, DvValue, SE, SD)
  plotdata_list[[y]] <-
    data.frame(i, Group, DvValue, SE) # für Visualisierung in einem Diagramm, s.u.
  plotdata_list[[y]]

  ## plot
  print(ggplot(plotdata, aes(x = Group, y = DvValue, fill = Group)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE), width = .2) +
      ggtitle(dvbinaryList[[y]]) + labs(y = "Mittelwert") +
      theme(plot.title = element_text(hjust = 0.5)))
}
dvbinaryList
#dvbinaryList <- dvbinaryList[-1]
#dvbinaryList <- dvbinaryList[-28:-30]



### Count Variablen ##########################################################################
y <- 0
plotdata_list = list()
plotdata_list = NULL
for (i in dvcountList) {
  ## create data
  Group <- c("Historischer Roman", "Tendenzroman", "Zeitroman")
  y <- y+1
  dv <- eval(parse(text=paste("gesamt$", dvcountList[[y]], sep=''))) 
  summary <- describeBy(dv, gesamt$Texttyp, na.rm=TRUE)
  summary
  SE <- unlist(c(summary[[1]][13], summary[[2]][13], summary[[3]][13]))
  SD <- unlist(c(summary[[1]][4], summary[[2]][13], summary[[3]][13]))
  DvValue <- unlist(c(summary[[1]][3], summary[[2]][3], summary[[3]][3])) # Mittelwert
  summary
  plotdata <- data.frame(Group, DvValue, SE, SD)
  plotdata_list[[y]] <-
    data.frame(i, Group, DvValue, SE) # für Visualisierung in einem Diagramm, s.u.
  plotdata_list[[y]]
  
  ## plot
  print(ggplot(plotdata, aes(x = Group, y = DvValue, fill = Group)) +
          geom_bar(stat = "identity") +
          geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE), width = .2) +
          ggtitle(dvcountList[[y]]) + labs(y = "Mittelwert") +
          theme(plot.title = element_text(hjust = 0.5)))
}
dvcountList

### Visualisierung mehrerer Variablen in ein Diagramm ###

#dev.new(width=5, height=8)
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(1:3,25:27, 52:54, 43:45, 40:42),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub

levels(plotdata_merged_sub$i) <- gsub(".", "\n", levels(plotdata_merged_sub$i), fixed = TRUE)
levels(plotdata_merged_sub$i) <- gsub("Klassenidentität\nbasal", "Klassen-\nidentität\nbasal", levels(plotdata_merged_sub$i), fixed = TRUE)

# Reihenfolge im Diagramm
plotdata_merged_sub$i <- factor(plotdata_merged_sub$i, levels = c('Objektbezug\nbasal', 'Exemplifikation\nbasal', 'Implizite\nWahrheit\nbasal', 'Klassen-\nidentität\nbasal', "Instanziierung\nbasal"))

dodge <- position_dodge(width=0.9)
multiplot <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
  geom_col(position=dodge) +
  scale_fill_brewer(palette="Set2") +
  geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE, group=Group), position=dodge, width=0.25, size=0.25) +
  #geom_signif(annotations="*", xmin=1.75, xmax=2.2, y_position = 2.8, tip_length = 0.02, size=0.25, textsize = 3) +
  ggtitle("Text-Welt-Verhältnisse") + labs(x="", y = "Mittelwert") +
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="top", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"))
multiplot
### Speichern
ggsave("./Abbildungen/Texttypvergleich/Text-Welt-Verhältnisse.pdf")
ggsave("./Abbildungen/Texttypvergleich/Text-Welt-Verhältnisse.png", dpi = 1200)

multiplot2 <- multiplot + 
  #geom_signif(annotations="***", xmin=0.77, xmax=1.2, y_position = 9.8, tip_length = 0.02, size=0.25, textsize = 2) +
  #geom_signif(annotations="*", xmin=1.75, xmax=2.2, y_position = 2.3, tip_length = 0.02, size=0.25, textsize = 2) +
  #geom_signif(annotations="**", xmin=3.7, xmax=4.2, y_position = 1.6, tip_length = 0.02, size=0.25, textsize = 2)
  ggtitle("Text-Welt-Verhältnisse: metrisch")
multiplot2
### Speichern
ggsave("./Abbildungen/Texttypvergleich/Text-Welt-Verhältnisse(mit Signifikanz).eps")
ggsave("./Abbildungen/Texttypvergleich/Text-Welt-Verhältnisse(mit Signifikanz).pdf")
ggsave("./Abbildungen/Texttypvergleich/Text-Welt-Verhältnisse(mit Signifikanz).png", dpi = 1200)
#grau
multiplot2g <- multiplot2 + theme_bw() + scale_fill_grey(start = 0.2, end = .6) + theme(legend.position = 'bottom') + theme(legend.title = element_blank())
ggsave("./Abbildungen/Texttypvergleich/Text-Welt-Verhältnisse(mit Signifikanz) (grau).eps")
ggsave("./Abbildungen/Texttypvergleich/Text-Welt-Verhältnisse(mit Signifikanz)  (grau).pdf")
ggsave("./Abbildungen/Texttypvergleich/Text-Welt-Verhältnisse(mit Signifikanz)  (grau).png", dpi = 1200)


#Isomorphie (erst Daten für binäre Variablen generieren)
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(10:12),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub

levels(plotdata_merged_sub$i) <- gsub(".", "\n", levels(plotdata_merged_sub$i), fixed = TRUE)
dodge <- position_dodge(width=0.9)
Isomorphie <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
  geom_col(position=dodge) +
  scale_fill_brewer(palette="Set2") +
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
plotdata_merged_sub <- plotdata_merged[c(49:51),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub

levels(plotdata_merged_sub$i) <- gsub(".", "\n", levels(plotdata_merged_sub$i), fixed = TRUE)

dodge <- position_dodge(width=0.9)
explWahrheit <- ggplot(plotdata_merged_sub, aes(x = "Explizite Wahrheit \n basal", y = DvValue, fill = Group)) +
  geom_col(position=dodge) +
  scale_y_continuous(breaks=c(0,1,2), limits=c(0,2), label=c("nie", "selten", "gelegentlich"), position = "right")+
  scale_fill_brewer(palette="Set2") +
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
ggsave("./Abbildungen/Texttypvergleich/Text-Welt-Verhältnisse2.pdf")
ggsave("./Abbildungen/Texttypvergleich/Text-Welt-Verhältnisse2.eps")
ggsave("./Abbildungen/Texttypvergleich/Text-Welt-Verhältnisse2.png", dpi=1200)

### 3 Diagramme nebeneinander ###
ggarrange(multiplot2, ggarrange(explWahrheit, Isomorphie, nrow=2, labels = c("B", "C"), align = "v"), 
  common.legend = TRUE, legend = "bottom", align = "no", widths = 2:1, ncol = 2, labels= "A")
ggsave("./Abbildungen/Texttypvergleich/Text-Welt-Verhältnisse3.pdf", width =7 , height = 6)
ggsave("./Abbildungen/Texttypvergleich/Text-Welt-Verhältnisse3.eps", width =7 , height = 6)
ggsave("./Abbildungen/Texttypvergleich/Text-Welt-Verhältnisse3.png", width =7 , height = 6, dpi=1200)

### 3 Diagramme nebeneinander grau ###
ggarrange(multiplot2g, ggarrange(explWahrheitg, Isomorphieg, nrow=2, labels = c("B", "C"), align = "v"), 
          common.legend = TRUE, legend = "bottom", align = "no", widths = 2:1, ncol = 2, labels= "A")
ggsave("./Abbildungen/Texttypvergleich/Text-Welt-Verhältnisse3 (grau).pdf", width =7 , height = 6)
ggsave("./Abbildungen/Texttypvergleich/Text-Welt-Verhältnisse3 (grau).eps", width =7 , height = 6)
ggsave("./Abbildungen/Texttypvergleich/Text-Welt-Verhältnisse3 (grau).png", width =7 , height = 6, dpi=1200)



######################################
### Exploration zeitlicher Effekte ###
######################################

Mean_Jahr <- summarySE(gesamt, measurevar="Objektbezug.basal", groupvars=c("Jahr", "Texttyp"), na.rm = TRUE)
#Mean_Jahr <- Mean_Jahr[c(-1,-2,-36),]
Mean_Jahr

Mean_Jahr2 <- summarySE(gesamt, measurevar="Exemplifikation.basal", groupvars=c("Jahr", "Texttyp"), na.rm = TRUE)
#Mean_Jahr2 <- Mean_Jahr2[c(-1,-2,-36),]
Mean_Jahr2


Objektbezug <- ggplot(Mean_Jahr, aes(x=Jahr, y=Objektbezug.basal)) +
  geom_point(aes(color = Texttyp), size = 1) + 
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = Texttyp), se= FALSE) +
  xlab("Jahr") +
  scale_color_manual(values = c("#CC0000", "#006600", "#669999", "#00CCCC")) +
  theme_minimal()
Objektbezug

Exemplifikation <- ggplot(Mean_Jahr2, aes(x=Jahr, y=Exemplifikation.basal)) +
  geom_point(aes(color = Texttyp), size = 1) + 
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = Texttyp), se= FALSE) +
  xlab("Jahr") +
  scale_color_manual(values = c("#CC0000", "#006600", "#669999", "#00CCCC")) +
  theme_minimal()
Exemplifikation

# mit allen Datenpunkten 
Objektbezug <- ggplot(gesamt, aes(x=Jahr, y=Objektbezug.basal)) +
  geom_point(aes(color = Texttyp), size = 1) + 
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = Texttyp), se= FALSE) +
  xlab("Jahr") +
  scale_color_manual(values = c("#CC0000", "#006600", "#669999", "#00CCCC", "blue")) +
  theme_minimal()
Objektbezug

Exemplifikation <- ggplot(gesamt, aes(x=Jahr, y=Exemplifikation.basal)) +
  geom_point(aes(color = Texttyp), size = 1) + 
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = Texttyp), se= FALSE) +
  xlab("Jahr") +
  scale_color_manual(values = c("#CC0000", "#006600", "#669999", "#00CCCC")) +
  theme_minimal()
Exemplifikation

Isomorphie <- ggplot(gesamt, aes(x=Jahr, y=Isomorphie.basal)) +
  geom_point(aes(color = Texttyp), size = 1) + 
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = Texttyp), se= FALSE) +
  xlab("Jahr") +
  scale_color_manual(values = c("#CC0000", "#006600", "#669999", "#00CCCC")) +
  theme_minimal()
Isomorphie

Implizite.Wahrheit <- ggplot(gesamt, aes(x=Jahr, y=Implizite.Wahrheit.basal)) +
  geom_point(aes(color = Texttyp), size = 1) + 
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = Texttyp), se= FALSE) +
  xlab("Jahr") +
  scale_color_manual(values = c("#CC0000", "#006600", "#669999", "#00CCCC")) +
  theme_minimal()
Implizite.Wahrheit

Mean_Textsorte_Objektbezug <- summarySE(gesamt, measurevar="Objektbezug.basal", groupvars=c("Texttyp"), na.rm = TRUE)
Mean_Textsorte_Objektbezug[1,3]
gesamt$Differenz_zu_Mean_Objektbezug <- ifelse((gesamt$Texttyp == "HR"), (gesamt$Objektbezug.basal - Mean_Textsorte_Objektbezug[1,3]), 
                                               ifelse((gesamt$Texttyp == "TR"), (gesamt$Objektbezug.basal - Mean_Textsorte_Objektbezug[2,3]), 
                                                      ifelse((gesamt$Texttyp == "ZR"), (gesamt$Objektbezug.basal - Mean_Textsorte_Objektbezug[3,3]),NA)))
gesamt$Differenz_zu_Mean_Objektbezug

Differenz_zu_Mean_Objektbezug <- ggplot(gesamt, aes(x=Jahr, y=Differenz_zu_Mean_Objektbezug)) +
  geom_point(aes(color = Texttyp), size = 1) + 
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = "polynomial"), se= FALSE) +
  #stat_smooth(method = 'lm', aes(colour = Texttyp), se= FALSE) +
  #stat_smooth(method = 'nls', formula = y ~ a * log(x) +b, aes(colour = "expotential"), se= FALSE, start = list(a=1,b=1)) +
  xlab("Jahr") +
  scale_color_manual(values = c("#CC0000", "#006600", "#669999", "#00CCCC")) +
  theme_minimal()
Differenz_zu_Mean_Objektbezug                                                                                                                                                                                                                                                               


Mean_Textsorte_Exemplifikation <- summarySE(gesamt, measurevar="Exemplifikation.basal", groupvars=c("Texttyp"), na.rm = TRUE)
Mean_Textsorte_Exemplifikation[1,3]
gesamt$Differenz_zu_Mean_Exemplifikation <- ifelse((gesamt$Texttyp == "HR"), (gesamt$Exemplifikation.basal - Mean_Textsorte_Exemplifikation[1,3]), 
                                                   ifelse((gesamt$Texttyp == "TR"), (gesamt$Exemplifikation.basal - Mean_Textsorte_Exemplifikation[2,3]), 
                                                          ifelse((gesamt$Texttyp == "ZR"), (gesamt$Exemplifikation.basal - Mean_Textsorte_Exemplifikation[3,3]),NA)))
gesamt$Differenz_zu_Mean_Exemplifikation

Differenz_zu_Mean_Exemplifikation <- ggplot(gesamt, aes(x=Jahr, y=Differenz_zu_Mean_Exemplifikation)) +
  geom_point(aes(color = Texttyp), size = 1) + 
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = "polynomial"), se= FALSE) +
  #stat_smooth(method = 'lm', aes(colour = Texttyp), se= FALSE) +
  #stat_smooth(method = 'nls', formula = y ~ a * log(x) +b, aes(colour = "expotential"), se= FALSE, start = list(a=1,b=1)) +
  xlab("Jahr") +
  scale_color_manual(values = c("#CC0000", "#006600", "#669999", "#00CCCC")) +
  theme_minimal()
Differenz_zu_Mean_Exemplifikation     