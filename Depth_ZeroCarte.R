##CALCUL DES PROFONDEURS PAR RAPPORT AU ZÉRO DES CARTE (SELON IGLD 1985)##

#Nettoyer l'espace de travail
rm(list=ls())

#Charger les packages
library(sp)

##====================##
##PRÉPARER LES DONNÉES##
##====================##

#Ouvrir les données d'échosondage
echoJ2012 <- read.table("./Raw_data/EchoLSP_Juin_2012_R.csv",header=TRUE,sep=";")
echoA2012 <- read.table("./Raw_data/EchoLSP_Aout_2012_R.csv",header=TRUE,sep=";")
echo2013 <- read.table("./Raw_data/EchoLSP_Aout_2013_R.csv",header=TRUE,sep=";")
echo2014 <- read.table("./Raw_data/EchoLSP_Aout_2014_R.csv",header=TRUE,sep=";")
echo2015 <- read.table("./Raw_data/EchoLSP_Aout_2015_R.csv",header=TRUE,sep=";")

#Convertir la date et l'heure des fichiers d'échosondage (heure avancée de l'est en GMT) et sélectioner les variables d'intérêt 
eJ2012 <- data.frame(
  Date=as.Date(substr(as.character(echoJ2012$Time),1,10)),
  Time=format(as.POSIXct(substr(echoJ2012$Time,12,19),tz="America/Montreal",format="%H:%M:%S",usetz=TRUE),tz="GMT",format="%H:00"), 
  Latitude=echoJ2012$Latitude_deg,
  Longitude=echoJ2012$Longitude_deg,
  Zecho=abs(echoJ2012$BottomElevation_m)
) 
eA2012 <- data.frame(
  Date=as.Date(substr(as.character(echoA2012$Time),1,10)),
  Time=format(as.POSIXct(substr(echoA2012$Time,12,19),tz="America/Montreal",format="%H:%M:%S",usetz=TRUE),tz="GMT",format="%H:00"), 
  Latitude=echoA2012$Latitude_deg,
  Longitude=echoA2012$Longitude_deg,
  Zecho=abs(echoA2012$BottomElevation_m)
) 
eA2013 <- data.frame(
  Date=as.Date(substr(as.character(echo2013$Time),1,10)),
  Time=format(as.POSIXct(substr(echo2013$Time,12,19),tz="America/Montreal",format="%H:%M:%S",usetz=TRUE),tz="GMT",format="%H:00"), 
  Latitude=echo2013$Latitude_deg,
  Longitude=echo2013$Longitude_deg,
  Zecho=abs(echo2013$BottomElevation_m)
)
eA2014 <- data.frame(
  Date=as.Date(substr(as.character(echo2014$Time),1,10)),
  Time=format(as.POSIXct(substr(echo2014$Time,12,19),tz="America/Montreal",format="%H:%M:%S",usetz=TRUE),tz="GMT",format="%H:00"), 
  Latitude=echo2014$Latitude_deg,
  Longitude=echo2014$Longitude_deg,
  Zecho=abs(echo2014$BottomElevation_m)
)
eA2015 <- data.frame(
  Date=as.Date(substr(as.character(echo2015$Time),1,10)),
  Time=format(as.POSIXct(substr(echo2015$Time,12,19),tz="America/Montreal",format="%H:%M:%S",usetz=TRUE),tz="GMT",format="%H:00"), 
  Latitude=echo2015$Latitude_deg,
  Longitude=echo2015$Longitude_deg,
  Zecho=abs(echo2015$BottomElevation_m)
)

#Ouvrir les données de niveau d'eau
SLEV <- read.table("./Raw_data/Niveaux_horaires_15975.csv",header=TRUE,sep=";")
#Convertir les dates pour avoir le même format que le fichier d'échosondage
SLEV<-data.frame(
  Date=format(as.POSIXct(SLEV$Time,tz="GMT",format="%y-%m-%d %H:%M"),format="20%y-%m-%d"),
  Time=format(as.POSIXct(SLEV$Time,tz="GMT",format="%y-%m-%d %H:%M"),format="%H:%M"),
  SLEV=SLEV$SLEV
) 

##==========================##
##CORRIGER LES NIVEAUX D'EAU##
##==========================##

#Ajouter le niveau d'eau correspondant à la date et l'heure de l'échosondage
eJ2012_SLEV<-merge(eJ2012,SLEV,by=c("Date","Time"))
eA2012_SLEV<-merge(eA2012,SLEV,by=c("Date","Time"))
eA2013_SLEV<-merge(eA2013,SLEV,by=c("Date","Time"))
eA2014_SLEV<-merge(eA2014,SLEV,by=c("Date","Time"))
eA2015_SLEV<-merge(eA2015,SLEV,by=c("Date","Time"))

#Appliquer la correction (Z0 = Zecho - SLEVecho)
eJ2012_SLEV$Z0 <- eJ2012_SLEV$Zecho - eJ2012_SLEV$SLEV
eA2012_SLEV$Z0 <- eA2012_SLEV$Zecho - eA2012_SLEV$SLEV
eA2013_SLEV$Z0 <- eA2013_SLEV$Zecho - eA2013_SLEV$SLEV
eA2014_SLEV$Z0 <- eA2014_SLEV$Zecho - eA2014_SLEV$SLEV
eA2015_SLEV$Z0 <- eA2015_SLEV$Zecho - eA2015_SLEV$SLEV

##=====================================##
##SPATIALISER LES DONNÉES D'ÉCHOSONDAGE
##=====================================##

#Spatialiser 
eJ2012_SLEVs<- sp::SpatialPointsDataFrame(
  coords = cbind(eJ2012_SLEV$Longitude, eJ2012_SLEV$Latitude), 
  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), 
  data= eJ2012_SLEV)
eA2012_SLEVs<- sp::SpatialPointsDataFrame(
  coords = cbind(eA2012_SLEV$Longitude, eA2012_SLEV$Latitude), 
  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), 
  data= eA2012_SLEV)
eA2013_SLEVs<- sp::SpatialPointsDataFrame(
  coords = cbind(eA2013_SLEV$Longitude, eA2013_SLEV$Latitude), 
  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), 
  data= eA2013_SLEV)
eA2014_SLEVs<- sp::SpatialPointsDataFrame(
  coords = cbind(eA2014_SLEV$Longitude, eA2014_SLEV$Latitude), 
  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), 
  data= eA2014_SLEV)
eA2015_SLEVs<- sp::SpatialPointsDataFrame(
  coords = cbind(eA2015_SLEV$Longitude, eA2015_SLEV$Latitude), 
  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"), 
  data= eA2015_SLEV)

#Convertir en UTM zone 8
eJ2012_UTM <- sp::spTransform(eJ2012_SLEVs, CRS ("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
eA2012_UTM <- sp::spTransform(eA2012_SLEVs, CRS ("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
eA2013_UTM <- sp::spTransform(eA2013_SLEVs, CRS ("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
eA2014_UTM <- sp::spTransform(eA2014_SLEVs, CRS ("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
eA2015_UTM <- sp::spTransform(eA2015_SLEVs, CRS ("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

#Exporter les données
write.csv(data.frame(Long_UTM18N=eJ2012_UTM@coords[,1],Lat_UTM18N=eJ2012_UTM@coords[,2],eJ2012_SLEV),
          file="./Corrected_data/DepthZC_LSPJ2012.csv")
write.csv(data.frame(Long_UTM18N=eA2012_UTM@coords[,1],Lat_UTM18N=eA2012_UTM@coords[,2],eA2012_SLEV),
          file="./Corrected_data/DepthZC_LSPA2012.csv")
write.csv(data.frame(Long_UTM18N=eA2013_UTM@coords[,1],Lat_UTM18N=eA2013_UTM@coords[,2],eA2013_SLEV),
          file="./Corrected_data/DepthZC_LSPA2013.csv")
write.csv(data.frame(Long_UTM18N=eA2014_UTM@coords[,1],Lat_UTM18N=eA2014_UTM@coords[,2],eA2014_SLEV),
          file="./Corrected_data/DepthZC_LSPA2014.csv")
write.csv(data.frame(Long_UTM18N=eA2015_UTM@coords[,1],Lat_UTM18N=eA2015_UTM@coords[,2],eA2015_SLEV),
          file="./Corrected_data/DepthZC_LSPA2015.csv")

