#INTERPOLATION PAR KRIGEAGE DES PROFONDEURS PAR RAPPORT AU ZÉRO DES CARTES

#Nettoyer l'espace de travail
rm(list=ls())

#Charger les librairies
library(sp)
library(gstat)
library(lattice)
library(parallel)

##===================##
#PRÉPARER LES DONNÉES##
##===================##

#Ouvrir les données d'échosondage
depth_J2012 <- read.table("./Corrected_data/DepthZC_LSPJ2012.csv",header=TRUE,sep=",")
#depth_A2012 <- read.table("./Corrected_data/DepthZC_LSPA2012.csv",header=TRUE,sep=",") #test avec seulement juin 2012
#depth_A2013 <- read.table("./Corrected_data/DepthZC_LSPA2013.csv",header=TRUE,sep=",")
#depth_A2014 <- read.table("./Corrected_data/DepthZC_LSPA2014.csv",header=TRUE,sep=",")
#depth_A2015 <- read.table("./Corrected_data/DepthZC_LSPA2015.csv",header=TRUE,sep=",")

#Pour limiter les calculs, faire la moyenne à toutes les 60 observations
n_rowmeans <- function(df, n){
  aggregate(x = df,
            by = list(gl(ceiling(nrow(df)/n),n)[1:nrow(df)]),
            FUN = mean)}
dJ12_ag <- n_rowmeans(depth_J2012[,c(2,3,8,9,10)],60)

#Définir le système de coordonnées des fichiers échosondage
CRS <- sp::CRS ("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

#Spatialiser les données
sp::coordinates(dJ12_ag) <- c("Long_UTM18N","Lat_UTM18N")
sp::proj4string(dJ12_ag) <- CRS

#Pour J2012, enlever les points hors de la zone d'étude
#plot(dJ12_ag)
#locator() #trouver quelles coordonnées enlever
dJ12_ag <- dJ12_ag[-which(coordinates(dJ12_ag)[,1]<660312 & coordinates(dJ12_ag)[,2]>5111756),]

##============================##
##VARIOGRAM ET CHOIX DU MODÈLE##
##============================##

# Travailler en parallele
# Calculer le nombre de coeur
no_cores <- parallel::detectCores() - 1
# Initier le cluster
cl <- parallel::makeCluster(no_cores)

#Visualiser les données
sp::spplot(dJ12_ag,"Z0", do.log = F, colorkey = TRUE) 

#Enlever les deux donnees aux profondeurs abberrantes
dJ12_ag <- dJ12_ag[-which(dJ12_ag$Z0 > 3),]

#Correlation selon la distance
hscat(Z0 ~ 1, dJ12_ag, (0:9) * 100)

#Examiner le Variogram pour détecter les valeurs 
v.cloud <- gstat::variogram(logZ0 ~ 1, dJ12_ag, cloud = TRUE)
plot(v.cloud)
sel <- plot(gstat::variogram(Z0 ~ 1, dJ12_ag, cloud = TRUE),digitize = TRUE)
plot(sel,dJ12_ag)

variogram <- gstat::variogram(Z0~1, data=depth_J2012, cutoff=500,.parallel=T)
plot(variogram,.progress="text")

#directional variogram
plot(variogram(log(Z0) ~ 1, eA2012_UTM, alpha = c(0, 45,+ 90, 135)))


##================##
##CROSS-VALIDATION##
##================##


##=============##
##INTERPOLATION##
##=============##