#INTERPOLATION PAR KRIGEAGE DES PROFONDEURS PAR RAPPORT AU ZÉRO DES CARTES

#Nettoyer l'espace de travail
rm(list=ls())

#Charger les librairies
library(sp)
library(gstat)
library(lattice)
library(parallel)
library(raster)

##===================##
#PRÉPARER LES DONNÉES##
##===================##

#Ouvrir les données d'échosondage de juin 2012
depth_J2012 <- read.table("./Corrected_data/DepthZC_LSPJ2012.csv",header=TRUE,sep=",")

#Pour limiter les calculs, faire la moyenne à toutes les 60 observations
n_rowmeans <- function(df, n){
  aggregate(x = df,
            by = list(gl(ceiling(nrow(df)/n),n)[1:nrow(df)]),
            FUN = mean)}
dJ12_ag <- n_rowmeans(depth_J2012[,c(2,3,8,9,10)],60)

#Définir le système de coordonnées des fichiers d'échosondage
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

# Travailler en parallèle
# Calculer le nombre de coeur
no_cores <- parallel::detectCores() - 1
# Initier le cluster
cl <- parallel::makeCluster(no_cores)

#Visualiser les données
sp::spplot(dJ12_ag,"Z0", do.log = F, colorkey = TRUE) 

#Enlever les deux donnees aux profondeurs abberrantes
dJ12_ag <- dJ12_ag[-which(dJ12_ag$Z0 > 3),]

#Visualiser les données
sp::spplot(dJ12_ag,"Z0", do.log = F, colorkey = TRUE)

#Correlation selon la distance
hscat(Z0 ~ 1, dJ12_ag, (0:9) * 100)

#Examiner le Variogram pour détecter les valeurs abberrantes
v.cloud <- gstat::variogram(Z0 ~ 1, dJ12_ag, cloud = TRUE)
plot(v.cloud) 
#sel <- plot(gstat::variogram(log(Z0) ~ 1, dJ12_ag, cloud = TRUE),digitize = TRUE)
#plot(sel,dJ12_ag)
#paste(sort(unique(c(sel[,1],sel[,2]))),collapse = ",")
#Vecteur de la position des valeurs abberrantes
sel <- c(1,7,12,76,105,106,125,150,190,206,264,267,269,270,274,275,278,279,283,369,370,371,372,373,1473,1474,1476,1477,1487,1488,1489,1567,1582,1650,1655)
#Enlever les outliers
dJ12_ag <- dJ12_ag[-c(sel),]

#Examiner le Variogram à nouveau
v.cloud <- gstat::variogram(Z0 ~ 1, dJ12_ag, cloud = TRUE)
plot(v.cloud) #Le variogramme est magnifique!

#Inspecter le variogramme directionnel
v.dir <- variogram(Z0 ~ 1, dJ12_ag, alpha = c(0, 45, 90, 135))
plot(v.dir) #Les différences entre les directions sont mineures

#Modifier le cutoff (distance pour laquelles les paires de points sont considérées) et le width (la largeur des "bins")
v.mod <- variogram(Z0 ~ 1, dJ12_ag, cutoff = 5000, width = 60)
plot(v.mod) #range 3000,  partial sill 0.5, Gaussian model

#Variogram fitting
v.fit <- fit.variogram(v.mod, vgm(0.5,"Gau",3000))
plot(v.mod, v.fit)

#Obtenir le "minimised criterion - weighed sum of square errors from the non-linear regression"
attr(v.fit.gau, "SSErr")

##=============##
##INTERPOLATION##
##=============##

#Créer un raster pour enregistrer les résultats de l'interpolation
bb <- bbox(dJ12_ag)
cs <- c(60,60) #Cell size de 60m
cc <- bb[,1] +(cs/2)
cd <- ceiling(diff(t(bb))/cs)
int.grid <- SpatialGrid(GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd), proj4string = CRS)

#Kriger
int <- krige(Z0 ~ 1, dJ12_ag, int.grid, v.fit)

##================##
##CROSS-VALIDATION##
##================##

crossval.Gau <- krige(Z0 ~ 1, dJ12_ag[-1,], dJ12_ag[1,], model = v.fit) #, maxdist=200)
for (i in 2:nrow(dJ12_ag)){
  k <- krige(TN ~ 1, data[-i,], data[i,], model = v.fit, maxdist=200)
  crossval.Gau=rbind(crossval.Gau,k);
}

plot(dJ12_ag$Z0,crossval.Gau$var1.pred,main="Gau")

#Coefficient de correlation de Pearson de la cross-validation
cor(dJ12_ag$Z0,crossval.Gau$var1.pred) 


##==================##
##EXPORTER LE RASTER## 
##==================##

#Créer un polygone autour des points (à partir de l'enveloppe convexe)
ch <- chull(dJ12_ag@coords[,1],dJ12_ag@coords[,2])
ch_poly<- coordinates(dJ12_ag)[c(ch, ch[1]), ] #Fermer le polygone
poly <- sp::SpatialPolygons(list(Polygons(list(Polygon(ch_poly)), ID=1)))
poly_df <- SpatialPolygonsDataFrame(poly, data=data.frame(ID=1), proj4string = CRS)

#Clip 
cr <- crop(int.grid, bb, snap="out")
