##################################
### Korpus Historischer Roman ####
##################################

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
library(AER)
library(MASS)
library(reshape2)



############################################
### Daten einlesen und Variablenformate ####
############################################

setwd("C:/Users/Benjamin/Dropbox/Wissenschaft/Fiktion und Genre/Habil/R")
d <- read.table("Historischer_Roman7.CSV",header=TRUE, sep=";",na.strings=c("NA","-"))
head(d)
d <- d[1:82,1:46]
str(d)

#Unabhängige Variablen als Faktor
#d$Werk <- factor(d$Werk)
#d$Werk <- factor(d$Werk, levels = c(0,1,2,3), labels = c("Fontane", "Meyer", "Ebers", "Gutzkow"))
#d <- within(d, Werk <- relevel(Werk, ref = 3)) # andere baseline
d$Genrezuordnung_formal <- factor(d$Genrezuordnung_formal)
d$Genrezuordnung_formal <- factor(d$Genrezuordnung_formal, levels = c(0,1), labels = c("Roman", "historischer Roman"), ordered=FALSE)
#d$Genrezuordnung_formal <- factor(d$Genrezuordnung_formal, levels = c(0,1), labels = c("Roman", "historischer Roman"), ordered=TRUE)

#d$NrKritiker <- factor(d$NrKritiker)
d$Genrezuordnung_formal
#Reihenfolge der Autoren geändert (Ebers, Fontane, Gutzkow, Meyer)
d$Werk <- ifelse((d$Werk == 0), 1, ifelse((d$Werk == 1), 3, ifelse((d$Werk == 2), 0, 
                                                                   ifelse((d$Werk == 3), 2, "NA"))))
d$Werk <- factor(d$Werk)
d$Werk <- factor(d$Werk, levels = c(0,1,2,3), labels = c("Ebers", "Fontane", "Gutzkow", "C.F. Meyer"))
d$Werk


# Ordinale Variablen als Faktor
#d$Moralische.Wertung <- factor(d$Moralische.Wertung)
d$Ästhetische_Wertung <- factor(d$Ästhetische_Wertung)
#d$Ästhetische_Wertung <- factor(d$Ästhetische_Wertung, levels = c(0,1,2,3,4), labels = c("sehr schlecht", "schlecht", "teils gut, teils schlecht", "gut", "sehr gut"))
d$Länge <- factor(d$Länge)
#d$Explizite.Wahrheit.basal_ordinal <- factor(d$Explizite.Wahrheit.basal_ordinal, levels = c(0,1,2,3,4), labels = c("nie", "selten", "gelegentlich", "oft", "immer"))
d$Ästhetische_Wertung

# Skalentransformation explWahrheit (count -> ordinal) und dann die neue ordinale nur nehmen, wenn es noch keinen ordinalen Wert gab
# Skalentransformation-neu (cut-Funktion) Kategorie "nie" ist mit 0 besetzt
# Input ist "Explizite.Wahrheit.basal_Anzahl" und "Explizite.Wahrheit.basal_ordinal"
#Output: "Explizite.Wahrheit.basal" (eine ordinale Variable) u. "Explizite.Wahrheit.basal_metrisch" (ordinale V., aber nicht als factor behandelt) 
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

#Berechnung konditionaler Variablen
d$Negative_Bewertung_Abweichungen
d$Negative_Bewertung_Abweichungen_kond <- ifelse((d$Anzahl_historischer_Abweichungen > 0), d$Negative_Bewertung_Abweichungen, NA)
d$Negative_Bewertung_Abweichungen_kond
describeBy(d$Negative_Bewertung_Abweichungen_kond, d$Genrezuordnung_formal)

d$Bewertung_explizite_Wahrheit_kond <- ifelse((d$Explizite.Wahrheit.basal != "nie"), d$Bewertung_explizite_Wahrheit, NA)
d$Bewertung_explizite_Wahrheit_kond
describeBy(d$Bewertung_explizite_Wahrheit, d$Genrezuordnung_formal)
describeBy(d$Bewertung_explizite_Wahrheit_kond, d$Genrezuordnung_formal)

d$Bewertung_implizite_Wahrheit_kond <- ifelse((d$Implizite.Wahrheit.basal > 0), d$Bewertung_implizite_Wahrheit, NA)
d$Bewertung_implizite_Wahrheit_kond
describeBy(d$Bewertung_impliziter_Wahrheiten, d$Genrezuordnung_formal)
describeBy(d$Bewertung_implizite_Wahrheit_kond, d$Genrezuordnung_formal)


d$Arg_Auseinandersetzung_impl_expl_Wahrheit_kond <- ifelse((d$Explizite.Wahrheit.basal != "nie" | d$Implizite.Wahrheit.basal > 0), d$Arg_Auseinandersetzung_impl_expl_Wahrheit, NA)
d$Arg_Auseinandersetzung_impl_expl_Wahrheit_kond
describeBy(d$Arg_Auseinandersetzung_impl_expl_Wahrheit, d$Genrezuordnung_formal)
describeBy(d$Arg_Auseinandersetzung_impl_expl_Wahrheit_kond, d$Genrezuordnung_formal)




# Variablenname ändern
#names(d)[names(d) == 'Referenz.basal_Anteil_Ort'] <- 'Referenz.basal_Anteil_Orte'

#binäre Variablen aus ästhetische Wertung und moralische Wertung
d$Ästhetische_Wertung_dichotom <- ifelse(!is.na(d$Ästhetische_Wertung), 1, 0) #Wert "1" wenn Wertung vorhanden
#d$Moralische_Wertung_dichotom <- ifelse(!is.na(d$Moralische_Wertung), 1, 0) #Wert "1" wenn Wertung vorhanden
as.integer(d$Ästhetische_Wertung_dichotom)
#as.integer(d$Moralische.Wertung.binary)
str(d)

##Variablennamen Ordinal
dvordinalList <- c("Ästhetische_Wertung") 
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

dvList <- names(d)[8:72] # hier alle abhängigen Variablen
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

#nicht erkannte Var hinzufügen
dvbinaryList <- c(dvbinaryList, "Bewertung_explizite_Wahrheit")

#Fälschlich binär erkannte Variablen rauswerfen
dvbinaryList <- dvbinaryList[-1:-11]
#dvbinaryList <- dvbinaryList[-c(3)]
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
dvcountList <- c(dvcountList, "Instanziierung.basal", "Klassenidentität.basal", "Explizite.Wahrheit.basal_metrisch", "Objektbezug.basal_Genese",	"Objektbezug.basal_Ähnlichkeit",	"Objektbezug.basal_Referenz", "Exemplifikation.basal_Anteil_Figuren",	"Exemplifikation.basal_Anteil_Handlung",	"Exemplifikation.basal_Anteil_Gesellschaft",	"Exemplifikation.basal_Anteil_Stimmung") ## adding elements
#dvcountList <- dvcountList[-10]
print(dvcountList)


###############################################################
### Infos über einzelne Variablen und Variablenberechnung #####
##############################################################

d$Anzahl_historischer_Abweichungen_dichotom <- ifelse(d$Anzahl_historischer_Abweichungen > 0, 1, 0) 
#d$Anzahl_historischer_Abweichungen_dichotom <- factor(d$Anzahl_historischer_Abweichungen_dichotom)
describeBy(d$Anzahl_historischer_Abweichungen_dichotom, d$Genrezuordnung_formal)
describeBy(d$Epochenspezifisches_Thema, d$Genrezuordnung_formal)

d$Anzahl_historischer_Abweichungen_dichotom

#Länge nach Genrezuordnung
d2 = data.frame(type=factor(d$Genrezuordnung_formal), group=factor(d$Länge))
d2
dat = dcast(d2, type ~ group, fun.aggregate = length)
dat.melt = melt(dat, id.vars = "type", measure.vars = c("0","1","2"))
dat.melt

describeBy(as.integer(d$Länge), d$Genrezuordnung_formal)
test = melt(data = d, id.vars = c("Genrezuordnung_formal", "Länge"))
test = count(d, 'Länge')
test

nrow(d[d$Ästhetische_Wertung_binary == "1", ])

#Berechnung konditionaler Variablen
d$Bewertung_explizite_Wahrheit_kond <- ifelse((d$Explizite.Wahrheit.basal != "nie"), d$Bewertung_explizite_Wahrheit, NA)
d$Bewertung_explizite_Wahrheit_kond
describeBy(d$Bewertung_explizite_Wahrheit_kond, d$Genrezuordnung_formal)

#Korrelation
cor(d$Universelles_Thema, d$Thematisierung_Sprache_Stil)


###############################
### Visualisierung ############
###############################
Group <- c("Roman", "historischer Roman")
Worklabels <- c("Ebers", "Fontane", "Gutzkow", "C.F. Meyer")
axistitles = c("Werk", "Anzahl")
#d$Genrezuordnung_formal_invertiert <- relevel(d$Genrezuordnung_formal, "HR") #macht historischer Roman zum ersten Level
#Group_invert = c("historischer Roman", "Roman") # invertierte Labels
#d$Genrezuordnung_formal_invertiert

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
Pfad = "./Abbildungen/Historischer_Roman/"
speichern <- function(datei) {
  dateipfad <- paste(Pfad, "HR-", datei, ".pdf", sep = "")
  ggsave(dateipfad)
  dateipfad <- paste(Pfad, "HR-", datei, ".eps", sep = "")
  ggsave(dateipfad)
  dateipfad <- paste(Pfad, "HR-", datei, ".png", sep = "")
  ggsave(dateipfad, dpi = 1200)
}

# Anzahl Rezensionen pro Werk
rez <- ggplot(d,aes(factor(Werk), group= Werk, fill=Werk)) + 
  geom_bar(show.legend = FALSE) + 
  scale_fill_brewer(palette="Paired") +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), position = position_stack(vjust = 0.9)) +
  ggtitle("Rezensionen je Werk") + 
  labs(x= "Werk", y = "Anzahl") + 
  scale_x_discrete(labels = c("0" = "Ebers", "1" = "Fontane", "2" = "Gutzkow", "3" = "C.F. Meyer")) +
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10))
rez 
speichern("Rezensionen je Werk")
rez + grau_theme()
speichern("Rezensionen je Werk (grau)")

# Genrezuordnung nach Werk
axistitles = c("Werk", "Rezensionen")
genrezu <- plot_grpfrq(d$Werk, d$Genrezuordnung_formal,
           title = "Genrezuordnung nach Werk",
           legend.title = "",
           axis.titles = axistitles,
           axis.labels = Worklabels,
           #legend.labels = Group_invert,
           show.na=FALSE, 
           show.axis.values=TRUE, 
           show.prc=FALSE,
           expand.grid =TRUE,
           geom.colors= "Set1")
genrezu + geom_col(position=dodge) + theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
                                           legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
                                           legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
                                           legend.margin=margin(0,0,0,0), legend.box.margin=margin(-13,0,0,0))
speichern("Genrezuordnung nach Werk")
genrezu + geom_col(position=dodge) + grau_theme()
speichern("Genrezuordnung nach Werk (grau)")

### Ex post Analysen - Plots
labelsmoral <- c("sehr verwerflich", "verwerflich", "moralisch neutral", "moralisch vorbildlich")

plot_grpfrq(d$Werk, d$Moralische.Wertung,
           title = "Moralische Wertung nach Werk",
           legend.title = "",
           axis.titles = axistitles,
           axis.labels = Worklabels,
           legend.labels = labelsmoral,
           show.na=FALSE, 
           show.axis.values=TRUE, 
           show.prc=FALSE,
           expand.grid =TRUE,
           geom.colors= "Paired")
ggsave("./Abbildungen/Moralische Wertung nach Werk.png", dpi = 400)


plot_grpfrq(d$Werk, d$Moralische.Wertung.binary,
           title = "Moralische Wertung (dichotomisiert) nach Werk",
           legend.title = "",
           axis.titles = axistitles,
           axis.labels = Worklabels,
           legend.labels = c("keine moralische Wertung", "moralische Wertung"),
           show.na=FALSE, 
           show.axis.values=TRUE, 
           show.prc=FALSE,
           expand.grid =TRUE,
           geom.colors= "Set1")
ggsave("./Abbildungen/Moralische Wertung (dichotom) nach Werk.png", dpi = 400)

plot_grpfrq(d$Werk, d$Referenzialisierte.Figuren,
           title = "Anzahl referenzialisierter Figuren nach Werk",
           legend.title = "",
           axis.titles = axistitles,
           axis.labels = Worklabels,
           #legend.labels = c("keine moralische Wertung", "moralische Wertung"),
           show.na=FALSE, 
           show.axis.values=TRUE, 
           show.prc=FALSE,
           expand.grid =TRUE,
           geom.colors= "Paired")
ggsave("./Abbildungen/Anzahl referenzialisierter Figuren nach Werk.png", dpi = 400)

plot_grpfrq(d$Referenzialisierte.Figuren, d$Moralische.Wertung.binary,
           title = "Moralische Wertung (dichotomisiert) nach Anzahl referenzialisierter Figuren",
           legend.title = "",
           axis.titles = axistitles,
           #axis.labels = c("keine moralische Wertung", "moralische Wertung"),
           legend.labels = c("keine moralische Wertung", "moralische Wertung"),
           show.na=FALSE, 
           show.axis.values=TRUE, 
           show.prc=FALSE,
           expand.grid =TRUE,
           geom.colors= "Set1")
ggsave("./Abbildungen/Moralische Wertung nach Anzahl referenzialisierter Figuren.png", dpi = 400)

### Rezensionen mit bestimmten Eigenschaften suchen
Auswahl <- d[which(d$Moralische.Wertung.binary=='1' & (d$Moralische.Wertung == '0' | d$Moralische.Wertung == '1')
              & d$Referenzialisierte.Figuren == 0), ]
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
  Group <- c("Roman", "historischer Roman")
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
  print(ggplot(plotdata, aes(x = Group, y = DvValue, fill = Group)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE), width = .2) +
      ggtitle(dvbinaryList[[y]]) + labs(y = "Mittelwert") +
      theme(plot.title = element_text(hjust = 0.5)))
}
dvbinaryList
dvbinaryList <- dvbinaryList[-19]
#dvbinaryList <- dvbinaryList[-28:-30]



## Immersion, Faktentreue, negative Bewertung
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(9,10,11,12,33:34),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
plotdata_merged_sub$Group <- factor (plotdata_merged_sub$Group,
                                     levels = c("Roman", "historischer Roman"),ordered = TRUE)
plotdata_merged_sub$i <- gsub("Negative_Bewertung_Abweichungen_kond", "Negative Bewertung\nhistorischer\nAbweichungen\n(n = 24)", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
#plotdata_merged_sub <- transform(plotdata_merged_sub,i=factor(i,levels=unique(i)))
#levels(plotdata_merged_sub$i) <- gsub(".", "\n", levels(plotdata_merged_sub$i), fixed = TRUE)

dodge <- position_dodge(width=0.9)
fallstudienspezifisch <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
  farbig_theme_minusSE("Fallstudienspezifische dichotome Variablen")
fallstudienspezifisch
speichern("Fallstudiensp_dichotome_Var")
fallstudienspezifisch + grau_theme()
speichern("Fallstudiensp_dichotome_Var (grau)")


# F-Praxis: Erzähler und Bewertung expliziter Wahrheiten
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(1,2, 17,18, 19,20),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
plotdata_merged_sub$Group <- factor (plotdata_merged_sub$Group,
                                     levels = c("Roman", "historischer Roman"),ordered = TRUE)
plotdata_merged_sub$i <- gsub("Bewertung_explizite_Wahrheit", "Bewertung\nexplizite Wahrheit", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Identifikation_Autor_Erzählerfigur", "Identifikation\nAutor-Erzählerfigur", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Impl_Identifikation_Autor_Erzähler", "Impl. Identifikation\nAutor-Erzähler", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames

dodge <- position_dodge(width=0.9)
f_praxis <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
  farbig_theme_minusSE("Spezifische Variablen Fiktionalitätspraxis")
f_praxis
speichern("F-Praxis")
f_praxis + grau_theme()
speichern("F-Praxis (grau)")

## Literaturstatus
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(39:40,19:20, 23:28, 21:22, 3:6,39,40, 31:32),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
plotdata_merged_sub$Group <- factor (plotdata_merged_sub$Group,
                                     levels = c("Roman", "historischer Roman"),ordered = TRUE)

plotdata_merged_sub$i <- gsub("Arg_Auseinandersetzung_impl_expl_Wahrheit", "Arg.\nAuseinander-\nsetzung", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Bewertung_implizite_Wahrheit", "Bew.\nimpl.\nWahrheiten", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Epochenspezifisches_Thema", "Epochensp.\nThema", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Thematisierung_Sprache_Stil", "Thematis.\nSprache\nStil", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Universelles_Thema", "Univ.\nThema", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Ästhetische_Wertung_dichotom", "Ästh.\nWertung\ndichotom", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Absprechen_Literaturstatus", "Absprechen\nLiteratur-\nstatus", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Kritik_anderer_Interpretationen", "Kritik\nanderer\nInterpr.", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("_", "\n", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("_", "\n", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames

L_Praxis <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) + 
  farbig_theme_minusSE("Literaturpraxis nach Genrezuordnung") +
  scale_x_discrete(limits=c("Ästh.\nWertung\ndichotom","Absprechen\nLiteratur-\nstatus", "Univ.\nThema", "Epochensp.\nThema", "Thematis.\nSprache\nStil", "Bew.\nimpl.\nWahrheiten", "Arg.\nAuseinander-\nsetzung", "Kritik\nanderer\nInterpr."))+
  theme(axis.text=element_text(size=7))
L_Praxis
speichern("L-Praxis")
L_Praxis + grau_theme()
speichern("L-Praxis (grau)")


plotdata_merged_sub$i <- gsub("Ästhetische_Wertung_binary", "Ästhetische\nWertung\ndichotom", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("Arg_Auseinandersetzung_impl_expl_Wahrheit", "Arg.\nAuseinandersetzung", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("_", "\n", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames

rownames[plotdata_merged_sub] <- seq(length=nrow(plotdata_merged_sub)) # renew index
rownames(plotdata_merged_sub) <- NULL 
plotdata_merged_sub <- plotdata_merged_sub[c(1,2,3,4,5,6,7,8,9,10,11,12),] # change order of rows
plotdata_merged_sub
rownames(plotdata_merged_sub) <- NULL 
#plotdata_merged_sub$i <- gsub(".", "\n", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub <- transform(plotdata_merged_sub,i=factor(i,levels=unique(i)))
#levels(plotdata_merged_sub$i) <- gsub(".", "\n", levels(plotdata_merged_sub$i), fixed = TRUE)

dodge <- position_dodge(width=0.9)
L_Praxis <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
  geom_col(position=dodge) +
  scale_fill_brewer(palette="Set1") +
  geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE, group=Group), position=dodge, width=0.25, size=0.25) +
  ggtitle("Literaturpraxis nach Genrezuordnung") + labs(x="", y = "Mittelwert") +
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), axis.text.x = element_text(size = 7), 
        legend.position="top", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"))
L_Praxis
### Speichern
ggsave("./Abbildungen/Historischer_Roman/l-praxis.pdf")
ggsave("./Abbildungen/Historischer_Roman/l-praxis.eps")
ggsave("./Abbildungen/Historischer_Roman/l-praxis.png", dpi=1200)



# Moralische vs. ästhetische (binär)
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub$Group <- factor (plotdata_merged_sub$Group,
                                     levels = c("Roman", "historischer Roman"),ordered = TRUE)
plotdata_merged_sub <- subset(plotdata_merged, i== "Aesthetische.Wertung.binary" |
                                i== "Moralische.Wertung.binary") # select subset of variables
#rownames[plotdata_merged_sub] <- seq(length=nrow(plotdata_merged_sub)) # renew index
#rownames(plotdata_merged_sub) <- NULL 
#plotdata_merged_sub <- plotdata_merged_sub[c(1,2,5,6,7,8,9,10,3,4),] # change order of rows
plotdata_merged_sub
#rownames(plotdata_merged_sub) <- NULL 
plotdata_merged_sub$i <- gsub(".", "\n", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames
plotdata_merged_sub$i <- gsub("binary", "", plotdata_merged_sub$i, fixed = TRUE) # line break for long varnames

plotdata_merged_sub <- transform(plotdata_merged_sub,i=factor(i,levels=unique(i)))
#levels(plotdata_merged_sub$i) <- gsub(".", "\n", levels(plotdata_merged_sub$i), fixed = TRUE)

dodge <- position_dodge(width=0.9)
multiplot <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
  geom_col(position=dodge) +
  scale_fill_brewer(palette="Set1") +
  geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE, group=Group), position=dodge, width=0.25, size=0.25) +
  ggtitle("Ästhetische und Moralische Wertung (dichotomisiert)") + labs(x="", y = "Mittelwert") +
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), axis.text.x = element_text(size = 7), 
        legend.position="top", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"))
multiplot
### Speichern
ggsave("./Abbildungen/Ästhetische und Moralische.png", dpi = 400)

multiplot2 <- multiplot + 
  #geom_signif(annotations="***", xmin=0.7, xmax=1.2, y_position = 1.6, tip_length = 0.02, size=0.25, textsize = 2)
  geom_signif(annotations="***", xmin=1.75, xmax=2.2, y_position = 1, tip_length = 0.02, size=0.25, textsize = 2)
#geom_signif(annotations="**", xmin=3.75, xmax=4.2, y_position = 0.65, tip_length = 0.02, size=0.25, textsize = 2)
multiplot2
### Speichern
ggsave("./Abbildungen/Ästhetische und Moralische (mit Signifikanz).png", dpi = 400)




# Ordinale Variablen ##########################################################################
set.seed(1234)
y <- 0
for (i in dvordinalList) {
  Group <- c("Roman", "historischer Roman")
  labelsmoral <- c("sehr verwerflich", "verwerflich", "moralisch neutral", "moralisch vorbildlich") # letzte Label fehlt, weil sonst Plot nicht funzt.
  labelsaesth <- c("schlecht", "teils gut, teils schlecht", "gut", "sehr gut") #erste Kategorie raus, sonst funzt ggplot nicht
  labelshäufig <- c("nie", "selten", "gelegentlich", "oft")
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


## plot model ExplWahrheit 
plot_grpfrq(d$Genrezuordnung_formal, d$Explizite.Wahrheit.basal, 
           title = gsub(".", " ", dvordinalList[[2]], fixed = TRUE),
           legend.title = "",
           legend.labels = labelshäufig,
           axis.labels = Group,
           axis.titles = "",
           show.na=FALSE, 
           show.axis.values=TRUE, 
           show.prc=FALSE,
           show.n = TRUE,
           geom.colors= "Paired"
)

#ggsave("./Abbildungen/Moralische Wertung.png", dpi = 400)

#aest Wertung mit xtab
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
d$Ästhetische_Wertung <- factor(d$Ästhetische_Wertung, levels = c(0,1,2,3,4), labels = c("sehr schlecht", "schlecht", "teils gut, teils schlecht", "gut", "sehr gut"))
d$Ästhetische_Wertung_mitNA <- addNA(d$Ästhetische_Wertung)
d$Ästhetische_Wertung_mitNA
aesth <- plot_xtab(d$Genrezuordnung_formal, d$Ästhetische_Wertung_mitNA, 
                  title = "Ästhetische Wertung",
                  legend.title = "",
                  margin = "row", 
                  show.total = FALSE, 
                  y.offset = 0.003, 
                  vjust = "center",
                  axis.titles = c("","Anteil Rezensionen"),
                  geom.colors= "Paired"
                  )
aesth + theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
              legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
              legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
              legend.margin=margin(0,0,0,0), legend.box.margin=margin(-5,0,0,0),
              axis.text.x= element_text(size=8),
              axis.text.y= element_text(size=8)) 
speichern("Ästhetische Wertung (mit NAs)")

??plot_xtab

## plot model Ästhetische Wertung (mit veränderter Reihenfolge, historischer Roman, Roman)
#d$Genrezuordnung_formal_invertiert <- relevel(d$Genrezuordnung_formal, "1") #macht historischer Roman zum ersten Level
#Group_invert = c("historischer Roman", "Roman") # invertierte Labels
plot_grpfrq(d$Genrezuordnung_formal_invertiert, d$Ästhetische_Wertung, 
           title = "Ästhetische Wertung",
           legend.title = "",
           legend.labels = labelsaesth,
           axis.labels = Group_invert,
           axis.titles = "",
           show.na=FALSE, 
           show.axis.values=TRUE, 
           show.prc=FALSE,
           show.n = TRUE,
           show.grpcnt = TRUE,
           geom.colors= "Paired"
           )
ggsave("./Abbildungen/Historischer_Roman/aesth_Wertung.pdf")
ggsave("./Abbildungen/Historischer_Roman/aesth_Wertung.eps")
ggsave("./Abbildungen/Historischer_Roman/aesth_Wertung.png", dpi=1200)


### Count Variablen ##########################################################################
set.seed(1234)
y <- 0
plotdata_list = list()
plotdata_list = NULL
for (i in dvcountList) {
  Group <- c("Roman", "historischer Roman")
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
plotdata_merged_sub <- plotdata_merged[c(1,2,11,12,17,18,25,26,23,24),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
plotdata_merged_sub$Group <- factor (plotdata_merged_sub$Group,
                                     levels = c("Roman", "historischer Roman"),ordered = TRUE)
levels(plotdata_merged_sub$i) <- gsub(".", "\n", levels(plotdata_merged_sub$i), fixed = TRUE)
levels(plotdata_merged_sub$i) <- gsub("Klassenidentität\nbasal", "Klassen-\nidentität\nbasal", levels(plotdata_merged_sub$i), fixed = TRUE)
levels(plotdata_merged_sub$i) <- gsub("Objektbezug\nbasal", "Objekt-\nbezug\nbasal", levels(plotdata_merged_sub$i), fixed = TRUE)
# Reihenfolge im Diagramm
plotdata_merged_sub$i <- factor(plotdata_merged_sub$i, levels = c('Objekt-\nbezug\nbasal', 'Exemplifikation\nbasal', 'Implizite\nWahrheit\nbasal', 'Klassen-\nidentität\nbasal', 'Instanziierung\nbasal'))


levels(plotdata_merged_sub$i) <- gsub(".", "\n", levels(plotdata_merged_sub$i), fixed = TRUE)
pvalue
dodge <- position_dodge(width=0.9)
multiplot <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
  geom_col(position=dodge) +
  scale_fill_brewer(palette="Set1") +
  geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE, group=Group), position=dodge, width=0.25, size=0.25) +
  ggtitle("Text-Welt-Verhältnisse") + labs(x="", y = "Mittelwert") +
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="top", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"))
multiplot
speichern("Text-Welt-Verhältnisse")

multiplot2 <- multiplot + 
  geom_signif(annotations="*", xmin=0.77, xmax=1.22, y_position = 10.2, tip_length = 0.02, size=0.25, textsize = 2) +
  geom_signif(annotations="*", xmin=1.75, xmax=2.2, y_position = 2.3, tip_length = 0.02, size=0.25, textsize = 2) +
  #geom_signif(annotations="**", xmin=3.7, xmax=4.2, y_position = 1.6, tip_length = 0.02, size=0.25, textsize = 2)
  ggtitle("Text-Welt-Verhältnisse: metrisch")
multiplot2
ggsave("./Abbildungen/Historischer_Roman/HR-Text-Welt-Verhältnisse(mit Signifikanz).eps")
ggsave("./Abbildungen/Historischer_Roman/HR-Text-Welt-Verhältnisse(mit Signifikanz).pdf")
ggsave("./Abbildungen/Historischer_Roman/HR-Text-Welt-Verhältnisse(mit Signifikanz).png", dpi = 1200)
#grau
multiplot2g <- multiplot2 + theme_bw() + scale_fill_grey(start = 0.2, end = .6) + theme(legend.position = 'bottom') + theme(legend.title = element_blank())
ggsave("./Abbildungen/Historischer_Roman/HR-Text-Welt-Verhältnisse(mit Signifikanz) (grau).eps")
ggsave("./Abbildungen/Historischer_Roman/HR-Text-Welt-Verhältnisse(mit Signifikanz)  (grau).pdf")
ggsave("./Abbildungen/Historischer_Roman/HR-Text-Welt-Verhältnisse(mit Signifikanz)  (grau).png", dpi = 1200)


#Isomorphie
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(7,8),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
plotdata_merged_sub$Group <- factor (plotdata_merged_sub$Group,
                                     levels = c("Roman", "historischer Roman"),ordered = TRUE)

levels(plotdata_merged_sub$i) <- gsub(".", "\n", levels(plotdata_merged_sub$i), fixed = TRUE)
pvalue
dodge <- position_dodge(width=0.9)
Isomorphie <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
  geom_col(position=dodge) +
  scale_fill_brewer(palette="Set1") +
  #geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE, group=Group), position=dodge, width=0.25, size=0.25) +
  ggtitle("dichotom") + 
  labs(x="", y = "Mittelwert") +
  scale_y_continuous(position = "right", limits = c(0, 0.4), breaks = c(0, 0.2, 0.4)) +
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
plotdata_merged_sub <- plotdata_merged[c(27,28),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
plotdata_merged_sub$Group <- factor (plotdata_merged_sub$Group,
                                     levels = c("Roman", "historischer Roman"),ordered = TRUE)

levels(plotdata_merged_sub$i) <- gsub(".", "\n", levels(plotdata_merged_sub$i), fixed = TRUE)
pvalue
dodge <- position_dodge(width=0.9)
explWahrheit <- ggplot(plotdata_merged_sub, aes(x = "Explizite Wahrheit \n basal", y = DvValue, fill = Group)) +
  geom_col(position=dodge) +
  scale_y_continuous(breaks=c(0,1,2), limits=c(0,2), label=c("nie","selten","gelegentlich"), position = "right")+
  scale_fill_brewer(palette="Set1") +
  #geom_errorbar(aes(ymin = DvValue - SE, ymax = DvValue + SE, group=Group), position=dodge, width=0.25, size=0.25) +
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
ggsave("./Abbildungen/Historischer_Roman/HR-Text-Welt-Verhältnisse2.pdf")
ggsave("./Abbildungen/Historischer_Roman/HR-Text-Welt-Verhältnisse2.eps")
ggsave("./Abbildungen/Historischer_Roman/HR-Text-Welt-Verhältnisse2.png", dpi=1200)


### 3 Diagramme nebeneinander ###
ggarrange(multiplot2, ggarrange(explWahrheit, Isomorphie, nrow=2, labels = c("B", "C"), align = "v"), 
  common.legend = TRUE, legend = "bottom", align = "no", widths = 2:1, ncol = 2, labels= "A")
ggsave("./Abbildungen/Historischer_Roman/HR-Text-Welt-Verhältnisse3.pdf", width =7 , height = 6)
ggsave("./Abbildungen/Historischer_Roman/HR-Text-Welt-Verhältnisse3.eps", width =7 , height = 6)
ggsave("./Abbildungen/Historischer_Roman/HR-Text-Welt-Verhältnisse3.png", width =7 , height = 6, dpi=1200)

### 3 Diagramme nebeneinander grau ###
ggarrange(multiplot2g, ggarrange(explWahrheitg, Isomorphieg, nrow=2, labels = c("B", "C"), align = "v"), 
          common.legend = TRUE, legend = "bottom", align = "no", widths = 2:1, ncol = 2, labels= "A")
ggsave("./Abbildungen/Historischer_Roman/HR-Text-Welt-Verhältnisse3 (grau).pdf", width =7 , height = 6)
ggsave("./Abbildungen/Historischer_Roman/HR-Text-Welt-Verhältnisse3 (grau).eps", width =7 , height = 6)
ggsave("./Abbildungen/Historischer_Roman/HR-Text-Welt-Verhältnisse3 (grau).png", width =7 , height = 6, dpi=1200)
#dev.off()

## Anteil referenzialisierter Entitäten
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(3:8),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
plotdata_merged_sub$Group <- factor (plotdata_merged_sub$Group,
                                     levels = c("Roman", "historischer Roman"),ordered = TRUE)
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
plotdata_merged_sub <- plotdata_merged[c(9:10,29:34),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
plotdata_merged_sub$Group <- factor (plotdata_merged_sub$Group,
                                     levels = c("Roman", "historischer Roman"),ordered = TRUE)
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

## Exemplifkation Entitäten
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(35,36,39,40,41,42),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
plotdata_merged_sub$Group <- factor (plotdata_merged_sub$Group,
                                     levels = c("Roman", "historischer Roman"),ordered = TRUE)
levels(plotdata_merged_sub$i) <- gsub("Exemplifikation.basal_Anteil_", "", levels(plotdata_merged_sub$i), fixed = TRUE)
levels(plotdata_merged_sub$i) <- gsub("Exemplifikation_basal_Anteil:", "", levels(plotdata_merged_sub$i), fixed = TRUE)
plotdata_merged_sub

dodge <- position_dodge(width=0.9)
Entitäten_Exempl <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
  farbig_theme("Exemplifikation-basal: Typen exemplifizierender Entitäten")
Entitäten_Exempl
speichern("Typen_exempl_Entitäten")
Entitäten_Exempl + grau_theme()
speichern("Typen_exempl_Entitäten (grau)")

## Anzahl historischer Abweichungen
plotdata_merged = do.call(rbind, plotdata_list)
plotdata_merged
plotdata_merged_sub <- plotdata_merged[c(19,20),1:length(plotdata_merged)] # select subset of variables
plotdata_merged_sub
plotdata_merged_sub$Group <- factor (plotdata_merged_sub$Group,
                                     levels = c("Roman", "historischer Roman"),ordered = TRUE)
levels(plotdata_merged_sub$i) <- gsub("Anzahl_historischer_Abweichungen", "Historische Abweichungen", levels(plotdata_merged_sub$i), fixed = TRUE)
plotdata_merged_sub
pvalue <- plotdata_merged[17,length(plotdata_merged)] # p-wert aus Tabelle zuweisen
pvalue

dodge <- position_dodge(width=0.9)
hist_Abweichungen <- ggplot(plotdata_merged_sub, aes(x = i, y = DvValue, fill = Group)) +
  farbig_theme("Historische Abweichungen nach Genrezuordnung") +
  geom_signif(annotations="*", xmin=0.77, xmax=1.22, y_position = 1.23, tip_length = 0.02, size=0.25, textsize = 2)
hist_Abweichungen
speichern("hist_Abweichungen")
hist_Abweichungen + grau_theme()
speichern("hist_Abweichungen (grau)")


##################################
#########  Modelle  #############
#################################

#Modelle ohne Interaktion
m1 <- glm(d$Objektbezug.basal ~Werk+Genrezuordnung_formal,family=poisson(link="log"), data=d)
summary(m1)
m2 <- glm(d$Anzahl_historischer_Abweichungen ~Werk+Genrezuordnung_formal,family=poisson(link="log"), data=d)
summary(m2)
m3 <- glm(d$Exemplifikation.basal ~Werk+Genrezuordnung_formal,family=poisson(link="log"), data=d)
summary(m3)

# dispersion test auf overdispersion
library(AER)
  # H0: mean=Varianz ist gegeben
  # H1 bei "greater": overdispersion
dispersiontest(m1, alternative = "greater") # greater = overdispersion
dispersiontest(m2, alternative = "greater") # greater = overdispersion
dispersiontest(m3, alternative = "greater") # greater = overdispersion
#Ergebnis: alle 3 Modelle haben overdispersion

# Negativ binominal
summary(mod_nb1 <-glm.nb(Objektbezug.basal ~Werk+Genrezuordnung_formal, data=d))
summary(mod_nb2 <-glm.nb(Anzahl_historischer_Abweichungen ~Werk+Genrezuordnung_formal, data=d))
summary(mod_nb3 <-glm.nb(Exemplifikation.basal ~Werk+Genrezuordnung_formal, data=d))

# einseitig
res1 <- summary(mod_nb1)
res2 <- summary(mod_nb2)
res3 <- summary(mod_nb3)
res2
# For H1: beta > 0 (Koeffizient soll größer 0 sein)
p1 <- pt(coef(res1)[, 3], df.residual(mod_nb1), lower = FALSE)  # For H1: beta > 0 (Koeffizient soll größer 0 sein)
p3 <- pt(coef(res3)[, 3], df.residual(mod_nb3), lower = FALSE)  # For H1: beta > 0 (Koeffizient soll größer 0 sein)
# For H1: beta < 0 (Koeffizient soll kleiner 0 sein)
p2 <- pt(coef(res2)[, 3], df.residual(mod_nb2), lower = TRUE)  # For H1: beta > 0 (Koeffizient soll größer 0 sein)


# p-adjust
p <- c(p1[5],p2[5],p3[5])
p
ps_new <- p.adjust(p, "fdr")
ps_new


# ACHTUNG: p-Werte noch zu adjustieren
stargazer(mod_nb1, mod_nb2, mod_nb3, covariate.labels=c("Werk2 (Fontane)","Werk3 (Gutzkow)", "Werk4 (Meyer)", "Genrezuordnung (HR=1)"), 
          star.char = c("*", "**", "***"), star.cutoffs = c(0.05, 0.01, 0.001), report=('vcp*'), decimal.mark=",", 
          single.row=TRUE, type = "html", intercept.bottom = T, model.numbers = FALSE,
          out = "./Stargazer/Historischer Roman/Negativ-binomiale-Regressionen(A3).htm")




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
              "Exemplifikation.basal_Anteil_Handlung",
              "Exemplifikation.basal_Anteil_Gesellschaft",	
              "Exemplifikation.basal_Anteil_Stimmung",	
              "Instanziierung.basal",
              "Klassenidentität.basal", 
              "Explizite.Wahrheit.basal_Anzahl", 
              "Implizite.Wahrheit.basal",
              "Anzahl_historischer_Abweichungen"
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
                file="./Stargazer/Historischer Roman/deskriptiv-count.tex")


#dichotome-data
varList = c("Bewertung_explizite_Wahrheit", 
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
            "Kritik_anderer_Interpretationen",
            "Immersion", 
            "Faktentreue", 
            "Negative_Bewertung_Abweichungen") 
varList
dflist <- data.frame(names = varList) %>% mutate(names = as.character(names))
dflist
dfnew <- select(d, one_of(dflist$names))
require(reporttools)
vars <- dfnew
group <- d[,c('Genrezuordnung_formal')]
mystats <- list("n", "mean")
capture.output( TN <- tableContinuous(vars = dfnew, stats = mystats, group = group, prec = 2, longtable = TRUE),
                file="./Stargazer/Historischer Roman/deskriptiv-dichotom.tex")

#dichotome-data
varList = c("Bewertung_explizite_Wahrheit", "Bewertung_implizite_Wahrheit",	"Arg_Auseinandersetzung_impl_expl_Wahrheit",	
              "Isomorphie.basal", "Identifikation_Autor_Erzählerfigur", "Impl_Identifikation_Autor_Erzähler", 
            "Ästhetische.Wertung_dichotom", "Absprechen_Literaturstatus", "Universelles_Thema", 
            "Thematisierung_Sprache_Stil",	"Bewertung_Sprache_Stil", "Kritik_anderer_Interpretationen", 
              "Immersion", "Faktentreue", "Negative_Bewertung_Abweichungen") # hier fallstudiensp. Variablen hinzufügen
varList
dflist <- data.frame(names = varList) %>% mutate(names = as.character(names))
dflist
dfnew <- select(d, one_of(dflist$names))
require(reporttools)
vars <- dfnew
group <- d[,c('Genrezuordnung_formal')]
mystats <- list("n", "mean", "s", "SE" = function(x){return((sd(x)/sqrt(length(x))))})
capture.output( TN <- tableContinuous(vars = dfnew, stats = mystats, group = group, prec = 2, longtable = TRUE),
                file="./Stargazer/Historischer Roman/deskriptiv-dichotom.tex")

#ordinale-data
d$Explizite.Wahrheit.basal_ordinal <- factor(d$Explizite.Wahrheit.basal_ordinal, levels = c(0,1,2,3,4), labels = c("nie", "selten", "gelegentlich", "oft", "immer"))
d$Explizite.Wahrheit.basal_ordinal
d$Ästhetische_Wertung <- factor(d$Ästhetische_Wertung, levels = c(0,1,2,3,4), labels = c("sehr schlecht", "schlecht", "teils gut, teils schlecht", "gut", "sehr gut"))
d$Ästhetische_Wertung
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
                file="./Stargazer/Historischer Roman/deskriptiv-ordinal.tex")


#Länge
varList = c("Länge") # hier fallstudiensp. Variablen hinzufügen
varList
dflist <- data.frame(names = varList) %>% mutate(names = as.character(names))
dflist
dfnew <- select(d, one_of(dflist$names))
require(reporttools)
vars <- dfnew
group <- d[,c('Genrezuordnung_formal')]
mystats <- list("n" = function(x){return((sd(x)/sqrt(length(x))))})
capture.output( TN <- tableNominal(vars = dfnew, cumsum = FALSE, stats = mystats, group = group, prec = 2, longtable = TRUE),
                file="./Stargazer/Historischer Roman/deskriptiv-Länge.tex")


#####################################
### Werkspezifische Unterschiede ####
####################################

# Immersion
d.sum <- summarySE(d, measurevar="Immersion", groupvars=c("Genrezuordnung_formal","Werk"))
d.sum
labelsWerk = c("Ebers", "Fontane", "Gutzkow", "C.F. Meyer")

pd <- position_dodge(0.1) # move them .05 to the left and right
posthoc <- ggplot(d.sum, aes(x=Genrezuordnung_formal, y=Immersion, group=Werk, color=Werk, shape=Werk)) + 
  #geom_errorbar(aes(ymin=Immersion-se, ymax=Immersion+se), width=.1, position=pd) +
  geom_line() +
  geom_point() + 
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"),
        legend.box.margin=margin(-20,0,0,0)) +
  ggtitle("Immersion nach Genrezuordnung und Werk") + labs(x="", y = "Mittelwert") + 
  scale_x_discrete(labels=c("R" = "Roman", "HR" = "Historischer Roman"), expand=c(0, 0.5)) +
  scale_colour_brewer(name='Werk',labels=labelsWerk, type = "seq", palette = "Dark2", direction = 1,
                      aesthetics = "colour")+
  scale_shape_manual(name='Werk',labels=labelsWerk, values=c(16,15,17,25))
posthoc
ggsave("./Abbildungen/Historischer_Roman/HR-Posthoc_Immersion.pdf")
ggsave("./Abbildungen/Historischer_Roman/HR-Posthoc_Immersion.eps")
ggsave("./Abbildungen/Historischer_Roman/HR-Posthoc_Immersion.png", dpi = 1200)

# Exemplifikation
d.sum <- summarySE(d, measurevar="Exemplifikation.basal", groupvars=c("Genrezuordnung_formal","Werk"))
d.sum
labelsWerk = c("Ebers", "Fontane", "Gutzkow", "C.F. Meyer")

describeBy(d$Exemplifikation.basal, d$Werk, d$Genrezuordnung_formal)

pd <- position_dodge(0.1) # move them .05 to the left and right
posthoc <- ggplot(d.sum, aes(x=Genrezuordnung_formal, y=Exemplifikation.basal, group=Werk, color=Werk, shape=Werk)) + 
  #geom_errorbar(aes(ymin=Exemplifikation.basal-se, ymax=Exemplifikation.basal+se), width=.1, position=pd) +
  geom_line() +
  geom_point() + 
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"),
        legend.box.margin=margin(-20,0,0,0)) +
  ggtitle("Exemplifikation-basal nach Genrezuordnung und Werk") + labs(x="", y = "Mittelwert") + 
  scale_x_discrete(labels=c("R" = "Roman", "HR" = "Historischer Roman"), expand=c(0, 0.5)) +
  scale_colour_brewer(name='Werk',labels=labelsWerk, type = "seq", palette = "Dark2", direction = 1,
                      aesthetics = "colour")+
  scale_shape_manual(name='Werk',labels=labelsWerk, values=c(16,15,17,25))
posthoc
ggsave("./Abbildungen/Historischer_Roman/HR-Posthoc_Exemplifikation.pdf")
ggsave("./Abbildungen/Historischer_Roman/HR-Posthoc_Exemplifikation.eps")
ggsave("./Abbildungen/Historischer_Roman/HR-Posthoc_Exemplifikation.png", dpi = 1200)
