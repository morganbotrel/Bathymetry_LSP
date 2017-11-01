#INTERPOLATION PAR KRIGEAGE DES PROFONDEURS LORS DE LA PRISE DE PHOTOAÉRIENNES

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

#Ouvrir les données d'échosondage de toutes les années
depth_J2012 <- read.table("./Corrected_data/DepthPHOTO_LSPJ2012.csv",header=TRUE,sep=",")
depth_A2012 <- read.table("./Corrected_data/DepthPHOTO_LSPA2012.csv",header=TRUE,sep=",")
#depth_A2013 <- read.table("./Corrected_data/DepthPHOTO_LSPA2013.csv",header=TRUE,sep=",") #En attente de la date de photo
depth_A2014 <- read.table("./Corrected_data/DepthPHOTO_LSPA2014.csv",header=TRUE,sep=",")
depth_A2015 <- read.table("./Corrected_data/DepthPHOTO_LSPA2015.csv",header=TRUE,sep=",")

#Pour limiter les calculs, faire la moyenne à toutes les 25 observations
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

##=================================##
##VARIOGRAM ET ENLEVER LES OUTLIERS##
##=================================##

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

#Visualiser de nouveau les données
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
selJ12 <- c(1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,478,479,3931,3932,3933,3934,3935,3936,3937,3938,3939,3940,3941,3942,3943,3944,3945,3946,3956,3957,3958,3959,3960,3961,3962,3963,3964,3965,3966,3967,3968,3970)
#Enlever les outliers
dJ12 <- dJ12[-c(selJ12),]
#Examiner les variogrammes à nouveau
v.cloudJ12 <- gstat::variogram(Zphoto ~ 1, dJ12, cloud = TRUE)
plot(v.cloudJ12) #Le variogramme est magnifique!


#Examiner le variogramme pour détecter les valeurs abberrantes **AOUT 2012**
v.cloudA12 <- gstat::variogram(Zphoto ~ 1, dA12, cloud = TRUE)
plot(v.cloudA12) 
#sel <- plot(gstat::variogram(Zphoto ~ 1, dA12, cloud = TRUE),digitize = TRUE)
#plot(sel,dA12)
#paste(sort(unique(sel[,1])),collapse = ",")
#Vecteur de la position des valeurs abberrantes
selA12 <- c(2,3,4,5,9,12,13,17,21,22,23,31,32,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,54,104,105,106,107,108,109,110,117,118,124,125,126,134,135,136,137,138,180,181)
#Enlever les outliers
dA12 <- dA12[-c(selA12),]
#Examiner les variogrammes à nouveau
v.cloudA12 <- gstat::variogram(Zphoto ~ 1, dA12, cloud = TRUE)
plot(v.cloudA12) 

#Examiner le variogramme pour détecter les valeurs abberrantes **AOUT 2014**
v.cloudA14 <- gstat::variogram(Zphoto ~ 1, dA14, cloud = TRUE)
plot(v.cloudA14) 
#Utiliser tel quel

#Examiner le variogramme pour détecter les valeurs abberrantes **AOUT 2012**
v.cloudA15 <- gstat::variogram(Zphoto ~ 1, dA15, cloud = TRUE)
plot(v.cloudA15) 
#sel <- plot(gstat::variogram(Zphoto ~ 1, dA15, cloud = TRUE),digitize = TRUE)
#plot(sel,dA15)
#paste(sort(unique(sel[,1])),collapse = ",")
#Vecteur de la position des valeurs abberrantes
selA15 <- c(1,2,3,4,5,6,7,8,18,20,21,22,23,24,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,49,430,431,432,556,557)
#Enlever les outliers
dA15 <- dA15[-c(selA15),]
#Examiner les variogrammes à nouveau
v.cloudA15 <- gstat::variogram(Zphoto ~ 1, dA15, cloud = TRUE)
plot(v.cloudA15) #Le variogramme est magnifique!


#Inspecter les variogrammes directionnels
v.dir <- variogram(Zphoto ~ 1, dA12, alpha = c(0, 45, 90, 135))
plot(v.dir) #Les différences entre les directions sont mineures

##================##
##CHOIX DU MODÈLE##
##===============##


#Modifier le cutoff (distance pour laquelles les paires de points sont considérées) et le width (la largeur des "bins")
v.modJ12 <- variogram(Zphoto ~ 1, dJ12, cutoff = 3500, width = 60)
v.modA12 <- variogram(Zphoto ~ 1, dA12, cutoff = 3500, width = 60)
#v.modA13 <- variogram(Zphoto ~ 1, dA13, cutoff = 3500, width = 60)
v.modA14 <- variogram(Zphoto ~ 1, dA14, cutoff = 3500, width = 60)
v.modA15 <- variogram(Zphoto ~ 1, dA15, cutoff = 3500, width = 60)
plot(v.modJ12) #range 3000,  partial sill 0.5, nugget 0.01,Gaussian model
plot(v.modA12) #range 3000,  partial sill 0.3, nugget 0.02, Gaussian model
plot(v.modA14) #range 3000,  partial sill 0.3, nugget 0.02, Gaussian model
plot(v.modA15) #range 2000,  partial sill 0.25, nugget 0.01, Gaussian model


#Variogram fitting
v.fitJ12 <- fit.variogram(v.modJ12, vgm(0.5,"Gau",3000,0.01))
plot(v.modJ12, v.fitJ12)
v.fitA12 <- fit.variogram(v.modA12, vgm(0.3,"Gau",3000,0.02))
plot(v.modA12, v.fitA12)
#v.fitA13 <- fit.variogram(v.modA13, vgm(0.3,"Gau",3000,0.02))
#plot(v.modA13, v.fitA13)
v.fitA14 <- fit.variogram(v.modA14, vgm(0.3,"Gau",3000,0.02))
plot(v.modA14, v.fitA14)
v.fitA15 <- fit.variogram(v.modA15, vgm(0.25,"Gau",2000,0.01))
plot(v.modA15, v.fitA15)

#Obtenir le "minimised criterion - weighed sum of square errors from the non-linear regression"
attr(v.fitJ12, "SSErr")
attr(v.fitA12, "SSErr")
#attr(v.fitA13, "SSErr")
attr(v.fitA14, "SSErr")
attr(v.fitA15, "SSErr")

##=============##
##INTERPOLATION##
##=============##

#Créer un polygone autour des points (à partir de l'enveloppe convexe) 
#**Juin 2012**
chJ12 <- chull(dJ12@coords[,1],dJ12@coords[,2])
ch_polyJ12<- coordinates(dJ12)[c(chJ12, chJ12[1]), ] #Fermer le polygone
polyJ12 <- sp::SpatialPolygons(list(Polygons(list(Polygon(ch_polyJ12)), ID=1)))
#**Août 2012**
chA12 <- chull(dA12@coords[,1],dA12@coords[,2])
ch_polyA12<- coordinates(dA12)[c(chA12, chA12[1]), ] #Fermer le polygone
polyA12 <- sp::SpatialPolygons(list(Polygons(list(Polygon(ch_polyA12)), ID=1)))
#**Août 2013**
#chA13 <- chull(dA13@coords[,1],dA13@coords[,2])
#ch_polyA13<- coordinates(dA13)[c(chA13, chA13[1]), ] #Fermer le polygone
#polyA13 <- sp::SpatialPolygons(list(Polygons(list(Polygon(ch_polyA13)), ID=1)))
#**Août 2014**
chA14 <- chull(dA14@coords[,1],dA14@coords[,2])
ch_polyA14<- coordinates(dA14)[c(chA14, chA14[1]), ] #Fermer le polygone
polyA14 <- sp::SpatialPolygons(list(Polygons(list(Polygon(ch_polyA14)), ID=1)))
#**Août 2015**
chA15 <- chull(dA15@coords[,1],dA15@coords[,2])
ch_polyA15<- coordinates(dA15)[c(chA15, chA15[1]), ] #Fermer le polygone
polyA15 <- sp::SpatialPolygons(list(Polygons(list(Polygon(ch_polyA15)), ID=1)))


#Créer un raster pour enregistrer les résultats de l'interpolation
#**Juin 2012**
rJ12 <- setValues(raster::raster(ext=extent(polyJ12),crs = CRS, resolution=60),0)
rmJ12 <- raster::mask(rJ12, polyJ12) 
gridJ12 <- as(rmJ12, "SpatialGridDataFrame")
#**Août 2012**
rA12 <- setValues(raster::raster(ext=extent(polyA12),crs = CRS, resolution=60),0)
rmA12 <- raster::mask(rA12, polyA12) 
gridA12 <- as(rmA12, "SpatialGridDataFrame")
#**Août 2013**
#rA13 <- setValues(raster::raster(ext=extent(polyA13),crs = CRS, resolution=60),0)
#rmA13 <- raster::mask(rA13, polyA13) 
#gridA13 <- as(rmA13, "SpatialGridDataFrame")
#**Août 2014**
rA14 <- setValues(raster::raster(ext=extent(polyA14),crs = CRS, resolution=60),0)
rmA14 <- raster::mask(rA14, polyA14) 
gridA14 <- as(rmA14, "SpatialGridDataFrame")
#**Août 2015**
rA15 <- setValues(raster::raster(ext=extent(polyA15),crs = CRS, resolution=60),0)
rmA15 <- raster::mask(rA15, polyA15) 
gridA15 <- as(rmA15, "SpatialGridDataFrame")

#Kriger
krigJ12 <- krige(Zphoto ~ 1, dJ12, gridJ12, model=v.fitJ12)
krigA12 <- krige(Zphoto ~ 1, dA12, gridA12, model=v.fitA12)
#krigA13 <- krige(Zphoto ~ 1, dA13, gridA13, model=v.fitA13)
krigA14 <- krige(Zphoto ~ 1, dA14, gridA14, model=v.fitA14)
krigA15 <- krige(Zphoto ~ 1, dA15, gridA15, model=v.fitA15)

##================##
##CROSS-VALIDATION##
##================##

#Juin 2012
crossval.GauJ12 <- krige(Zphoto ~ 1, dJ12[-1,], dJ12[1,], model = v.fitJ12) #, maxdist=200)
for (i in 2:nrow(dJ12)){
  kJ12 <- krige(Zphoto ~ 1, dJ12[-i,], dJ12[i,], model = v.fitJ12, maxdist=200)
  crossval.GauJ12=rbind(crossval.GauJ12,kJ12);
}
#Août 2012
crossval.GauA12 <- krige(Zphoto ~ 1, dA12[-1,], dA12[1,], model = v.fitA12) #, maxdist=200)
for (i in 2:nrow(dA12)){
  kA12 <- krige(Zphoto ~ 1, dA12[-i,], dA12[i,], model = v.fitA12, maxdist=200)
  crossval.GauA12=rbind(crossval.GauA12,kA12);
}
#Août 2013
#crossval.GauA13 <- krige(Zphoto ~ 1, dA13[-1,], dA13[1,], model = v.fitA13) #, maxdist=200)
#for (i in 2:nrow(dA13)){
  #kA13 <- krige(Zphoto ~ 1, dA13[-i,], dA13[i,], model = v.fitA13, maxdist=200)
  #crossval.GauA13=rbind(crossval.GauA13,kA13);
#}
#Août 2014
crossval.GauA14 <- krige(Zphoto ~ 1, dA14[-1,], dA14[1,], model = v.fitA14) #, maxdist=200)
for (i in 2:nrow(dA14)){
  kA14 <- krige(Zphoto ~ 1, dA14[-i,], dA14[i,], model = v.fitA14, maxdist=200)
  crossval.GauA14=rbind(crossval.GauA14,kA14);
}
#Août 2015
crossval.GauA15 <- krige(Zphoto ~ 1, dA15[-1,], dA15[1,], model = v.fitA15) #, maxdist=200)
for (i in 2:nrow(dA15)){
  kA15 <- krige(Zphoto ~ 1, dA15[-i,], dA15[i,], model = v.fitA15, maxdist=200)
  crossval.GauA15=rbind(crossval.GauA15,kA15);
}


#Coefficient de correlation de Pearson de la cross-validation
cor(dJ12$Zphoto,crossval.GauJ12$var1.pred,use = "complete.obs") #0.9923737
cor(dA12$Zphoto,crossval.GauA12$var1.pred) #0.9798116
#(dA13$Zphoto,crossval.GauA13$var1.pred)
cor(dA14$Zphoto,crossval.GauA14$var1.pred) #0.9800733
cor(dA15$Zphoto,crossval.GauA15$var1.pred) #0.9813889

#Graphique crossvalidation et exportation

pdf("Interpolated_maps/Zphoto/Cross_validation.pdf")
par(mfrow=c(2,2),mar=c(4,4,2,3),mgp=c(3,1,0))
plot(dJ12$Zphoto,crossval.GauJ12$var1.pred,main="Juin 2012",ylab="Predicted depth (m)", xlab="Measured depth (m)",las=1)
legend("topleft","r = 0.99",bty="n")
plot(dA12$Zphoto,crossval.GauA12$var1.pred,main="Août 2012",ylab="Predicted depth (m)", xlab="Measured depth (m)",las=1)
legend("topleft","r = 0.98",bty="n")
#plot(dA13$Zphoto,crossval.GauA13$var1.pred,main="Gau",ylab="Predicted depth (m)", xlab="Measured depth (m)",las=1)
#legend("topleft","r = NA",bty="n")
plot(dA14$Zphoto,crossval.GauA14$var1.pred,main="Août 2014",ylab="Predicted depth (m)", xlab="Measured depth (m)",las=1)
legend("topleft","r = 0.98",bty="n")
plot(dA15$Zphoto,crossval.GauA15$var1.pred,main="Août 2015",ylab="Predicted depth (m)", xlab="Measured depth (m)",las=1)
legend("topleft","r = 0.98",bty="n")
dev.off()


##==================##
##EXPORTER LE RASTER## 
##==================##

writeGDAL(krigJ12["var1.pred"], fname = "Interpolated_maps/Zphoto/Depth_ZPHOTO_LSPJ2012.tif", drivername = "GTiff")
writeGDAL(krigJ12["var1.var"], fname = "Interpolated_maps/Zphoto/DepthError_ZPHOTO_LSPJ2012.tif", drivername = "GTiff")
writeGDAL(krigA12["var1.pred"], fname = "Interpolated_maps/Zphoto/Depth_ZPHOTO_LSPA2012.tif", drivername = "GTiff")
writeGDAL(krigA12["var1.var"], fname = "Interpolated_maps/Zphoto/DepthError_ZPHOTO_LSPA2012.tif", drivername = "GTiff")
#writeGDAL(krigA13["var1.pred"], fname = "Interpolated_maps/Zphoto/Depth_ZPHOTO_LSPA2013.tif", drivername = "GTiff")
#writeGDAL(krigA13["var1.var"], fname = "Interpolated_maps/Zphoto/DepthError_ZPHOTO_LSPA2013.tif", drivername = "GTiff")
writeGDAL(krigA14["var1.pred"], fname = "Interpolated_maps/Zphoto/Depth_ZPHOTO_LSPA2014.tif", drivername = "GTiff")
writeGDAL(krigA14["var1.var"], fname = "Interpolated_maps/Zphoto/DepthError_ZPHOTO_LSPA2014.tif", drivername = "GTiff")
writeGDAL(krigA15["var1.pred"], fname = "Interpolated_maps/Zphoto/Depth_ZPHOTO_LSPA2015.tif", drivername = "GTiff")
writeGDAL(krigA15["var1.var"], fname = "Interpolated_maps/Zphoto/DepthError_ZPHOTO_LSPA2015.tif", drivername = "GTiff")



