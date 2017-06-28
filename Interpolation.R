#INTERPOLATION PAR KRIGEAGE DES PROFONDEURS PAR RAPPORT AU ZÉRO DES CARTES

#Nettoyer l'espace de travail
rm(list=ls())

#Charger les packages
library(sp)
library(gstat)
library(lattice)

#Ouvrir les données d'échosondage
depth_J2012 <- read.table("./Corrected_data/DepthZC_LSPJ2012.csv",header=TRUE,sep=",")
depth_A2012 <- read.table("./Corrected_data/DepthZC_LSPA2012.csv",header=TRUE,sep=",")
depth_A2013 <- read.table("./Corrected_data/DepthZC_LSPA2013.csv",header=TRUE,sep=",")
depth_A2014 <- read.table("./Corrected_data/DepthZC_LSPA2014.csv",header=TRUE,sep=",")
depth_A2015 <- read.table("./Corrected_data/DepthZC_LSPA2015.csv",header=TRUE,sep=",")

#spatialiser
depth_J2012<- sp::SpatialPointsDataFrame(
              coords = cbind(depth_J2012$Long_UTM18N, depth_J2012$Lat_UTM18N), 
              proj4string =CRS ("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"), 
              data= depth_J2012)
depth_A2012<- sp::SpatialPointsDataFrame(
              coords = cbind(depth_A2012$Long_UTM18N, depth_A2012$Lat_UTM18N), 
              proj4string =CRS ("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"), 
              data= depth_A2012)
depth_A2013<- sp::SpatialPointsDataFrame(
              coords = cbind(depth_A2013$Long_UTM18N, depth_A2013$Lat_UTM18N), 
              proj4string =CRS ("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"), 
              data= depth_A2013)
depth_A2014<- sp::SpatialPointsDataFrame(
              coords = cbind(depth_A2014$Long_UTM18N, depth_A2014$Lat_UTM18N), 
              proj4string =CRS ("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"), 
              data= depth_A2014)
depth_A2015<- sp::SpatialPointsDataFrame(
              coords = cbind(depth_A2015$Long_UTM18N, depth_A2015$Lat_UTM18N), 
              proj4string =CRS ("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"), 
              data= depth_A2015)

##============================##
##VARIOGRAM ET CHOIX DU MODÈLE##
##============================##

#Visualiser les données
sp::spplot(depth_J2012, "Z0", do.log = T, colorkey = TRUE) #Il faut enlever les données profondes hors de la zone d'étude

#Pour Juin enlever les données hors de la zone d'étude

#Variogram
variogram_cloud <- gstat::variogram(Z0~1, data=eJ2012_UTM, cloud=TRUE)  
variogram <- gstat::variogram(Z0~1, data=eA2012_UTM, cutoff=500)
plot(variogram,.progress="text")

#directional variogram
plot(variogram(log(Z0) ~ 1, eA2012_UTM, alpha = c(0, 45,+ 90, 135)))

##================##
##CROSS-VALIDATION##
##================##


##=============##
##INTERPOLATION##
##=============##