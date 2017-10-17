#INTERPOLATION PAR KRIGEAGE DES PROFONDEURS PAR RAPPORT AU ZÉRO DES CARTES

#Nettoyer l'espace de travail
rm(list=ls())

#Charger les librairies
library(sp)
library(gstat)
library(lattice)
library(parallel)
library(raster)
library(rgdal)

##===================##
#PRÉPARER LES DONNÉES##
##===================##

#Ouvrir les données d'échosondage de juin 2012
depth_J2012 <- read.table("./Corrected_data/DepthPHOTO_LSPJ2012.csv",header=TRUE,sep=",")
depth_A2012 <- read.table("./Corrected_data/DepthPHOTO_LSPA2012.csv",header=TRUE,sep=",")
#depth_A2013 <- read.table("./Corrected_data/DepthPHOTO_LSPA2013.csv",header=TRUE,sep=",") #En attente de la date de photo
depth_A2014 <- read.table("./Corrected_data/DepthPHOTO_LSPA2014.csv",header=TRUE,sep=",")
depth_A2015 <- read.table("./Corrected_data/DepthPHOTO_LSPA2015.csv",header=TRUE,sep=",")

#Pour limiter les calculs, faire la moyenne à toutes les 60 observations
n_rowmeans <- function(df, n){
  aggregate(x = df,
            by = list(gl(ceiling(nrow(df)/n),n)[1:nrow(df)]),
            FUN = mean)}
dJ12 <- n_rowmeans(depth_J2012[,c(2,3,8,9,10)],25)
dA12 <- n_rowmeans(depth_A2012[,c(2,3,8,9,10)],25)
#dA13 <- n_rowmeans(depth_A2013[,c(2,3,8,9,10)],25)
dA14 <- n_rowmeans(depth_A2014[,c(2,3,8,9,10)],25)
dA15 <- n_rowmeans(depth_A2015[,c(2,3,8,9,10)],25)

#Définir le système de coordonnées des fichiers d'échosondage
CRS <- sp::CRS ("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

#Spatialiser les données
sp::coordinates(dJ12) <- c("Long_UTM18N","Lat_UTM18N")
sp::proj4string(dJ12) <- CRS
sp::coordinates(dA12) <- c("Long_UTM18N","Lat_UTM18N")
sp::proj4string(dA12) <- CRS
#sp::coordinates(dA13) <- c("Long_UTM18N","Lat_UTM18N")
#sp::proj4string(dA13) <- CRS
sp::coordinates(dA14) <- c("Long_UTM18N","Lat_UTM18N")
sp::proj4string(dA14) <- CRS
sp::coordinates(dA15) <- c("Long_UTM18N","Lat_UTM18N")
sp::proj4string(dA15) <- CRS

#Pour juin 2012, enlever les données hors de la zone d'étude
dJ12 <- dJ12[-which(coordinates(dJ12)[,1]<660312 & coordinates(dJ12)[,2]>5111756),]

##============================##
##VARIOGRAM ET CHOIX DU MODÈLE##
##============================##

# Travailler en parallèle
# Calculer le nombre de coeur
no_cores <- parallel::detectCores() - 1
# Initier le cluster
cl <- parallel::makeCluster(no_cores)

#Visualiser les données
p1 <- sp::spplot(dJ12,"Zphoto", do.log = F, colorkey = TRUE) 
p2 <- sp::spplot(dA12,"Zphoto", do.log = F, colorkey = TRUE) 
p3 <- sp::spplot(dA14,"Zphoto", do.log = F, colorkey = TRUE) 
p4 <- sp::spplot(dA15,"Zphoto", do.log = F, colorkey = TRUE) 
print(p1,position=c(0,0.5,0.5,1),more=T)
print(p2,position=c(0.5,0.5,1,1),more=T)
print(p3,position=c(0,0,0.5,0.5),more=T)
print(p4, position=c(0.5,0,1,0.5))
# sp::spplot(dA13,"Zphoto", do.log = F, colorkey = TRUE) 

#Enlever les donnees aux profondeurs abberrantes (chenal de vase)
dJ12<- dJ12[-which(dJ12$Zphoto > 2.5),]
dA12<- dA12[-which(dA12$Zphoto > 2.5),]
#dA13<- dA13[-which(dA13$Zphoto > 2.5),]
dA14<- dA14[-which(dA14$Zphoto > 2.5),]
dA15<- dA15[-which(dA15$Zphoto > 2.5),]

#Visualiser les données
p1 <- sp::spplot(dJ12,"Zphoto", do.log = F, colorkey = TRUE) 
p2 <- sp::spplot(dA12,"Zphoto", do.log = F, colorkey = TRUE) 
p3 <- sp::spplot(dA14,"Zphoto", do.log = F, colorkey = TRUE) 
p4 <- sp::spplot(dA15,"Zphoto", do.log = F, colorkey = TRUE) 
print(p1,position=c(0,0.5,0.5,1),more=T)
print(p2,position=c(0.5,0.5,1,1),more=T)
print(p3,position=c(0,0,0.5,0.5),more=T)
print(p4, position=c(0.5,0,1,0.5))
# sp::spplot(dA13,"Zphoto", do.log = F, colorkey = TRUE) 

#Correlation selon la distance
hscat(Zphoto ~ 1, dA12, (0:9) * 100)

#Examiner le variogramme pour détecter les valeurs abberrantes **JUIN 2012**
v.cloudJ12 <- gstat::variogram(Zphoto ~ 1, dJ12, cloud = TRUE)
plot(v.cloudJ12) 
#sel <- plot(gstat::variogram(Zphoto ~ 1, dJ12, cloud = TRUE),digitize = TRUE)
#plot(sel,dJ12)
#paste(sort(unique(sel[,1])),collapse = ",")
#Vecteur de la position des valeurs abberrantes
sel <- c(1,2,3,4,5,6,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,52,53,54,55,56,57,58,59,60,61,62,63,65,66,67,69,70,71,72,74,75,76,77,78,79,80,81,82,83,84,86,87,88,89,90,91,92,93,94,95,96,98,99,100,104,105,106,124,125,127,130,227,430,542,659,660,847)
#Enlever les outliers
dA12 <- dA12[-c(sel),]

#Examiner le variogramme pour détecter les valeurs abberrantes **AOUT 2012**
v.cloudA12 <- gstat::variogram(Zphoto ~ 1, dA12, cloud = TRUE)
plot(v.cloudA12) 
#sel <- plot(gstat::variogram(Zphoto ~ 1, dA12, cloud = TRUE),digitize = TRUE)
#plot(sel,dA12)
#paste(sort(unique(sel[,1])),collapse = ",")
#Vecteur de la position des valeurs abberrantes
sel <- c(1,2,3,4,5,6,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,52,53,54,55,56,57,58,59,60,61,62,63,65,66,67,69,70,71,72,74,75,76,77,78,79,80,81,82,83,84,86,87,88,89,90,91,92,93,94,95,96,98,99,100,104,105,106,124,125,127,130,227,430,542,659,660,847)
#Enlever les outliers
dA12 <- dA12[-c(sel),]

#Examiner les variogrammes à nouveau
v.cloud <- gstat::variogram(Z0 ~ 1, dJ12_ag, cloud = TRUE)
plot(v.cloud) #Le variogramme est magnifique!

#Inspecter le variogramme directionnel
v.dir <- variogram(Zphoto ~ 1, dA12, alpha = c(0, 45, 90, 135))
plot(v.dir) #Les différences entre les directions sont mineures

#Modifier le cutoff (distance pour laquelles les paires de points sont considérées) et le width (la largeur des "bins")
v.mod <- variogram(Zphoto ~ 1, dA12, cutoff = 3500, width = 60)
plot(v.mod) #range 3000,  partial sill 0.5, Gaussian model

#Variogram fitting
v.fit <- fit.variogram(v.mod, vgm(0.3,"Gau",3000,0.05))
plot(v.mod, v.fit)

#Obtenir le "minimised criterion - weighed sum of square errors from the non-linear regression"
attr(v.fit, "SSErr")

##=============##
##INTERPOLATION##
##=============##

#Créer un polygone autour des points (à partir de l'enveloppe convexe)
ch <- chull(dA12@coords[,1],dA12@coords[,2])
ch_poly<- coordinates(dA12)[c(ch, ch[1]), ] #Fermer le polygone
poly <- sp::SpatialPolygons(list(Polygons(list(Polygon(ch_poly)), ID=1)))

#Créer un raster pour enregistrer les résultats de l'interpolation
r <- setValues(raster::raster(ext=extent(poly),crs = CRS, resolution=60),0)
rm <- raster::mask(r, poly) 
grid <- as(rm, "SpatialGridDataFrame")

#Kriger
krig <- krige(Zphoto ~ 1, dA12, grid, model=v.fit)

##================##
##CROSS-VALIDATION##
##================##

crossval.Gau <- krige(Zphoto ~ 1, dA12[-1,], dA12[1,], model = v.fit) #, maxdist=200)
for (i in 2:nrow(dA12)){
  k <- krige(Zphoto ~ 1, dA12[-i,], dA12[i,], model = v.fit, maxdist=200)
  crossval.Gau=rbind(crossval.Gau,k);
}

plot(dA12$Zphoto,crossval.Gau$var1.pred,main="Gau")

#Coefficient de correlation de Pearson de la cross-validation
cor(dA12$Zphoto,crossval.Gau$var1.pred) #0.9913213


##==================##
##EXPORTER LE RASTER## 
##==================##

writeGDAL(krig["var1.pred"], fname = "Interpolated_maps/Depth_ZPHOTO_LSPA2012.tif", drivername = "GTiff")
writeGDAL(krig["var1.var"], fname = "Interpolated_maps/DepthError_ZPHOTO_LSPA2012.tif", drivername = "GTiff")




