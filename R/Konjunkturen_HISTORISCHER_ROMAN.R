##########################################
## Historische Romane - KONJUNKTUREN #####
##########################################

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



######################
### Daten einlesen ###
######################

setwd("C:/Users/Benjamin/Dropbox/Wissenschaft/Fiktion und Genre/Habil/R")
d <- read.table("Historischer_Roman_Konjunkturen.CSV",header=TRUE, sep=";",na.strings=c("NA",""))
head(d)
d <- d[1:163,1:46]
str(d)

d$Anteil.HR.an.schöner.Literatur <- as.numeric(as.character(d$Anteil.HR.an.schöner.Literatur))
d$Anteil.HR.an.schöner.Literatur


#Funktionen
Group <- c("Roman", "historischer Roman")
Worklabels <- c("Ebers", "Fontane", "Gutzkow", "C.F. Meyer")
axistitles = c("Werk", "Anzahl")
#d$Genrezuordnung_formal_invertiert <- relevel(d$Genrezuordnung_formal, "HR") #macht historischer Roman zum ersten Level
#Group_invert = c("historischer Roman", "Roman") # invertierte Labels
#d$Genrezuordnung_formal_invertiert

dodge <- position_dodge(width=0.9)
farbig_theme <- function(titel) {
  list(ggtitle(titel), 
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
Pfad = "./Abbildungen/Historischer_Roman_Konjunkturen/"
speichern <- function(datei) {
  dateipfad <- paste(Pfad, "HR-", datei, ".pdf", sep = "")
  ggsave(dateipfad)
  dateipfad <- paste(Pfad, "HR-", datei, ".eps", sep = "")
  ggsave(dateipfad)
  dateipfad <- paste(Pfad, "HR-", datei, ".png", sep = "")
  ggsave(dateipfad, dpi = 1200)
}




#####################
### Visualisierung###
#####################

#Anzahl HRs nach Mühlberger Habitzel
Anzahl_HR <- ggplot(d, aes(x=Jahr, y=Anzahl_HR, group = 1)) +
  geom_point(size = 1) + 
  #geom_line() +
  xlab("Jahr") +
  ylab("Historische Romane") +
  scale_x_continuous(limits = c(1800, 1945), breaks = (c(1800,1820,1840,1860,1880,1900,1920,1940)))+ 
  stat_smooth(color = "#FC4E07", fill = "#FC4E07",  method = "loess", formula = y ~ x, span = 0.2, alpha = 0.2)
Anzahl_HR + farbig_theme("Anzahl historischer Romane nach Mühlberger, Habitzel (2001)")
speichern("Anzahl_historischer_Romane")


#Prozent HRs nach Mühlberger Habitzel
Prozent_HR <- ggplot(d, aes(x=Jahr, y=Anteil.HR.an.schöner.Literatur, group = 1)) +
  geom_point(size = 1) + 
  #geom_line() +
  xlab("Jahr") +
  ylab("Anteil historischer Romane (%)") +
  #scale_y_continuous(limits = c(0,15)) +
  scale_x_continuous(limits = c(1800, 1920), breaks = (c(1800,1820,1840,1860,1880,1900,1920)))+ 
  stat_smooth(color = "#FC4E07", fill = "#FC4E07",  method = "loess", formula = y ~ x, span = 0.2, alpha = 0.2)+
  geom_vline(xintercept = 1847, linetype="dashed", 
             color = "darkgrey", size=0.6) + 
  geom_vline(xintercept = 1850, linetype="dashed", 
             color = "darkgrey", size=0.6)

Prozent_HR + farbig_theme("Anteil historischer Romane an der literarischen Produktion")
speichern("Anteil_historischer_Romane")



#Genrebegriffe als Titel oder Untertitel historischer Romane
d2 <- d %>% select(1, 12:17)
d2 <- melt(d2, id="Jahr")
d2$variable <- gsub(x = d2$variable, pattern = "Prozent_", replacement = "") 
d2$variable <- gsub(x = d2$variable, pattern = "\\.", replacement = " ") 
d2
d2$variable <- factor(d2$variable, levels = c("Erzählung", "Novelle", "Roman", "Historische Erzählung", "Geschichtlicher Roman", "Historischer Roman"))

Untertitel_HR <- ggplot(data = d2, aes(x=Jahr, y= value, fill=variable)) +
  geom_area()+
  xlab("Jahr")+
  ylab("Anteil der Gattungsbegriffe (%)") +
  #scale_y_continuous(limits = c(0,50)) +
  scale_x_continuous(limits = c(1820, 1920), breaks = (c(1800,1820,1840,1860,1880,1900, 1920)))+
  guides(fill=guide_legend(nrow=3))
Untertitel_HR + ggtitle("Einschlägige Gattungsbegriffe als Untertitel historischer Romane")+
  theme(plot.title = element_text(size=11), axis.title=element_text(size=10), legend.spacing.x=unit(0.15, 'cm'),
        legend.position="bottom", legend.title=element_blank(), # Titel zentrieren: plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=8), legend.justification=c(0, 0), legend.key.size = unit(0.75,"line"), 
        legend.margin=margin(0,0,0,0), legend.box.margin=margin(-12,0,0,0)) #Abstand der Legende von Plot
speichern("Untertitel_historischer_Romane")
