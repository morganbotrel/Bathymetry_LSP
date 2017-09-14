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
depth_A2012 <- read.table("./Corrected_data/DepthPHOTO_LSPA2012.csv",header=TRUE,sep=",")

#Pour limiter les calculs, faire la moyenne à toutes les 60 observations
n_rowmeans <- function(df, n){
  aggregate(x = df,
            by = list(gl(ceiling(nrow(df)/n),n)[1:nrow(df)]),
            FUN = mean)}
dA12 <- n_rowmeans(depth_A2012[,c(2,3,8,9,10)],60)

#Définir le système de coordonnées des fichiers d'échosondage
CRS <- sp::CRS ("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

#Spatialiser les données
sp::coordinates(dA12) <- c("Long_UTM18N","Lat_UTM18N")
sp::proj4string(dA12) <- CRS

##============================##
##VARIOGRAM ET CHOIX DU MODÈLE##
##============================##

# Travailler en parallèle
# Calculer le nombre de coeur
no_cores <- parallel::detectCores() - 1
# Initier le cluster
cl <- parallel::makeCluster(no_cores)

#Visualiser les données
sp::spplot(dA12,"Zphoto", do.log = F, colorkey = TRUE) 

#Enlever les donnees aux profondeurs abberrantes (chenal de vase)
dA12<- dA12[-which(dA12$Zphoto > 2.5),]

#Visualiser les données
sp::spplot(dA12,"Zphoto", do.log = F, colorkey = TRUE)

#Correlation selon la distance
hscat(Zphoto ~ 1, dA12, (0:9) * 100)

#Examiner le Variogram pour détecter les valeurs abberrantes
v.cloud <- gstat::variogram(Zphoto ~ 1, dA12, cloud = TRUE)
plot(v.cloud) 
#sel <- plot(gstat::variogram(Zphoto ~ 1, dA12, cloud = TRUE),digitize = TRUE)
#plot(sel,dA12)
#paste(sort(unique(sel[,1])),collapse = ",")
#Vecteur de la position des valeurs abberrantes
sel <- c(1,2,3,4,5,6,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,52,53,54,55,56,57,58,59,60,61,62,63,65,66,67,69,70,71,72,74,75,76,77,78,79,80,81,82,83,84,86,87,88,89,90,91,92,93,94,95,96,98,99,100,104,105,106,124,125,127,130,227,430,542,659,660,847)
#Enlever les outliers
dA12 <- dA12[-c(sel),]

#Examiner le Variogram à nouveau
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

writeGDAL(krig["var1.pred"], fname = "Interpolated_maps/Depth_ZPHOTO_LSPJ2012.tif", drivername = "GTiff")
writeGDAL(krig["var1.var"], fname = "Interpolated_maps/DepthError_ZPHOTO_LSPJ2012.tif", drivername = "GTiff")




