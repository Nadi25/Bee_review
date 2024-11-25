############################
##
## Bees vs bees
#######################
## 04.04.2022
######################
getwd()
##############
library(ggplot2)
library(magrittr)
library(dplyr)
library(viridis)
library(hrbrthemes)
################

###############
## import data
################
competition <- read.csv2("competition_overview_studies_all.csv")
competition
#######################

######
## data analysis
###########
## all studies with influence of native range
#########
native <- ggplot(competition, aes(x=as.factor(impact), fill=as.factor(native_range) )) +  
  geom_bar( ) +
  scale_fill_manual(values = c("orange", "turquoise4") ) +
  theme(legend.position="right")+
  labs(x = "", fill = "Native range")
native
last_plot() + xlab("Effect")
last_plot() + ylab("Number of studies")
#######################

##

neutral <- sum(competition$effect==1) ##neutral  12
neutral
negative <- sum(competition$effect==2) ##negative  29
negative
mixed <- sum(competition$effect==3) ## mixed   18
mixed

sum_studies_total <- sum(neutral, negative, mixed)
sum_studies_total   ## 59

###########################
## influence of habitat
### wild vs agriculture
################
habitat <- ggplot(competition, aes(x=as.factor(impact),fill=as.factor(habitat) ) ) +
  geom_bar( ) +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position="right")+
  labs(x = "", fill = "Habitat")
habitat
last_plot() + xlab("Effect")
last_plot() + ylab("Number of studies")

#########
## pie chart with all countries
#######
circle <- ggplot(competition, aes(x="", y="", fill=country)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)
circle
## doesn´t make sense
###################

###############
## Year of publication
##############
competition
year <- ggplot(competition, aes(x=as.factor(year) ) ) +
  geom_bar( ) +
  scale_fill_manual(values =  "turquoise4" ) 
year  
last_plot() + xlab("Year")
last_plot() + ylab("Number of studies")

##################
## what is measured?
## abundance, visitation rates, reproduction, ...
################
measurement_variable <- ggplot(competition, aes(x=as.factor(impact),fill=as.factor(measurement_variable) ) ) +
  geom_bar( ) +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position="right")+
  labs(x = "", fill = "Habitat")
measurement_variable

#######################

library(openxlsx)
####
##################################
###############################
## read new table with data
######
competition_2 <- read.csv2("competition_overview_studies_all_measurement_variables.csv")
competition_2

###############
## native range
#########
native_2 <- ggplot(competition_2, aes(x=as.factor(impact), fill=as.factor(native_range) )) +  
  geom_bar( ) +
  scale_fill_manual(values = c("orange", "turquoise4") ) +
  theme(legend.position="right")+
  labs(x = "", fill = "Native range")
native_2
last_plot() + xlab("Effect")
last_plot() + ylab("Number of studies")
last_plot() + theme(axis.text.x = element_text(size = 14))
last_plot() + theme(axis.text.y = element_text(size = 14))
last_plot() + theme(axis.title.x = element_text(size =15))
last_plot() + theme(axis.title.y = element_text(size = 15, angle = 90))
last_plot() + theme(legend.title = element_text(size = 12, face="bold"))
last_plot() + theme(legend.text = element_text(size = 12))
##
##german
native_2 <- ggplot(competition_2, aes(x=as.factor(impact), fill=as.factor(native_range) )) +  
  geom_bar( ) +
  scale_fill_manual(values = c("orange", "turquoise4") ) +
  theme(legend.position="right")+
  labs(x = "", fill = "Native range")
native_2
last_plot() + xlab("Effekt")
last_plot() + ylab("Anzahl an Studien")
last_plot() + theme(axis.text.x = element_text(size = 14))
last_plot() + theme(axis.text.y = element_text(size = 14))
last_plot() + theme(axis.title.x = element_text(size =15))
last_plot() + theme(axis.title.y = element_text(size = 15, angle = 90))
last_plot() + theme(legend.title = element_text(size = 12, face="bold"))
last_plot() + theme(legend.text = element_text(size = 12))


neutral_2 <- sum(competition_2$effect==1) ##neutral  11
neutral_2
11/57 ## 0.19 -> 19 %
negative_2 <- sum(competition_2$effect==2) ##negative  29
negative_2
29/57 ## 0.51 -> 51 %
mixed_2 <- sum(competition_2$effect==3) ## mixed   17
mixed_2
17/57 ## 0.3 -> 30 %
0.19+0.51+0.3 ## 1 -> 100 %

sum_studies_total_2 <- sum(neutral_2, negative_2, mixed_2)
sum_studies_total_2   ## 57

###########################
## influence of habitat
### wild vs agriculture
################
habitat_2 <- ggplot(competition_2, aes(x=as.factor(impact),fill=as.factor(habitat) ) ) +
  geom_bar( ) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position="right")+
  labs(x = "", fill = "Habitat")
habitat_2
last_plot() + xlab("Effect")
last_plot() + ylab("Number of studies")
last_plot() + theme(axis.text.x = element_text(size = 14))
last_plot() + theme(axis.text.y = element_text(size = 14))
last_plot() + theme(axis.title.x = element_text(size =15))
last_plot() + theme(axis.title.y = element_text(size = 15, angle = 90))
last_plot() + theme(legend.title = element_text(size = 12, face="bold"))
last_plot() + theme(legend.text = element_text(size = 12))

###############
## Year of publication
##############
competition_2
year_2 <- ggplot(competition_2, aes(x=as.factor(year) ) ) +
  geom_bar( fill = "turquoise4") 
year_2  
last_plot() + xlab("Year")
last_plot() + ylab("Number of studies")

##################
## what is measured?
## abundance, visitation rates, reproduction, ...
################
measurement_variable_2 <- ggplot(competition_2, aes(x=as.factor(impact),fill=as.factor(measurement_variable_1) ) ) +
  geom_bar( ) +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position="right")+
  labs(x = "", fill = "Measurement variable")
measurement_variable_2
last_plot() + xlab("")
last_plot() + ylab("Number of studies")

###################
##
#######################
abundance <- ggplot(competition_2, aes(x=as.factor(abundance) ) ) +
  geom_bar( fill = "turquoise4") 
abundance 

visitation_rates <- ggplot(competition_2, aes(x=as.factor(visitation_rates.frequency) ) ) +
  geom_bar( fill = "turquoise4") 
visitation_rates 

##########################
## how many studies measure abundance, visitation rate, reproduction, ...
##############

competition_5 <- read.csv2("competition_parameter_0_1.csv")
competition_5

competition_5_transponiert <- t(competition_5)        # Tabelle transponieren
competition_5_transponiert
str(competition_5_transponiert)

number_studies <- rowSums(competition_5_transponiert)
number_studies

barplot(number_studies)

par(mar=c(11.5,4,4,4))
barplot(number_studies, ylab = "Number of studies", col = "darkred", ylim = c(0,38),
        names = c("Abundance", "Visitation rate / frequency", "Species richness / diversity", "Reproduction", "Interspecific interaction", "Density", "Visit duration", "Bee parameter", "Plant pollinator network"),
        main="Measurement parameter",
        cex.lab=1.3,cex.axis=1.1,  las=2, axis.lty = 1, cex.names= 1.0)
box()

par(mar=c(10,4,4,4))
barplot(number_studies, ylab = "Anzahl an Studien", col = "darkred", ylim = c(0,38),
        names = c("Abundanz", "Besuchsrate", "Artenreichtum", "Reproduktion", "Interspez. Interaktionen", "Dichte", "Besuchsdauer", "Bienen-Parameter", "Bestäuber-Netzwerk"),
        main="Untersuchte Parameter",
        cex.lab=1.3,cex.axis=1.1,  las=2, axis.lty = 1, cex.names= 1.0)
box()
