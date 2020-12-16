##################################
## Zeitromane - Konjunkturen #####
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


######################
### Daten einlesen ###
######################

Sys.setlocale(locale = "German")
setwd("C:/Users/Benjamin/Dropbox/Wissenschaft/Fiktion und Genre/Habil/R/Github")
d <- read.table("Untertitel_Zeitroman_Jahr.CSV",header=TRUE, sep=";",na.strings=c("NA","-"))
head(d)
d <- d[1:222,1:3]
str(d)

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
Pfad = "./"
speichern <- function(datei) {
  dateipfad <- paste(Pfad, "ZR-", datei, ".pdf", sep = "")
  ggsave(dateipfad)
  dateipfad <- paste(Pfad, "ZR-", datei, ".eps", sep = "")
  ggsave(dateipfad)
  dateipfad <- paste(Pfad, "ZR-", datei, ".png", sep = "")
  ggsave(dateipfad, dpi = 1200)
}


Anzahl_ZR <- ggplot(d, aes(x=Jahr, y=Untertitel.Zeitroman)) + coord_cartesian(ylim=c(0,5), xlim =c(1820, 1940)) +
  geom_point(size = 1) + 
  #geom_line() +
  xlab("Jahr") +
  ylab("Anzahl der Werke") +
  #scale_y_continuous(limits = c(0,5))+
  scale_x_continuous(limits = c(1820, 1940), breaks = (c(1820,1840,1860,1880,1900, 1920, 1940))) + 
  stat_smooth(fullrange=TRUE, color = "#FC4E07", fill = "#FC4E07",  method = "loess", formula = y ~ x, span = 0.3, alpha = 0.2)
Anzahl_ZR

Anzahl_ZR + farbig_theme("Auftreten des Untertitels \"Zeitroman\"")
speichern("Untertitel_Zeitroman")


#Barcode
d["Beding"] <- ifelse((d$Untertitel.Zeitroman > 0), 1, 0)
Anzahl_ZR <- ggplot(d, aes(x=Jahr, y=Untertitel.Zeitroman)) +
  #geom_point(size = 1) + 
  #geom_line() +
  xlab("Jahr") +
  ylab("Anzahl der Werke") +
  scale_x_continuous(limits = c(1850, 1935)) + 
  stat_smooth(color = "#FC4E07", fill = "#FC4E07",  method = "loess", formula = y ~ x, span = 0.4, alpha = 0.2)

Anzahl_ZR + geom_vline(aes(xintercept = Jahr),
             data = d %>% filter(Beding == 1) )



