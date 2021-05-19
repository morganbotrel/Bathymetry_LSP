#INTERPOLATION PAR KRIGEAGE DES PROFONDEURS LORS DE L'ÉCHOSONDAGE

#Nettoyer l'espace de travail
rm(list=ls())

#Charger les librairies
#Charger les librairies
library(sp)
library(ggplot2)
library(gridExtra)
library(viridis)
library(gstat)
library(raster)
library(rgdal)

#Open echo
echo <- read.csv("data/raw/echo_20pings_2012-2017")

#Create maps per campaign and sector
echo$secteur <- ifelse(echo$secteur == 'grille GRIL','S','N')
echo$group <- paste(echo$campagne,echo$secteur,sep='')
echo.camp <- split(echo, echo$group)

##====================##
##CARTES PRÉLIMINAIRES##
##====================##

#Define coordinate system
CRS <- CRS("+init=epsg:26918")

#Spatialize
for(i in 1:length(echo.camp)){
  coordinates(echo.camp[[i]]) <- c("x.utm","y.utm")
  sp::proj4string(echo.camp[[i]]) <- CRS
}

#Visualiser les données
map.prelim <- list()
for(i in 1:length(echo.camp)){
  map.prelim[[i]] <- spplot(echo.camp[[i]],"depth_m", 
                            do.log = F, colorkey = TRUE,
                            main = list(names(echo.camp)[i])) 
}
grid.arrange(map.prelim[[1]],map.prelim[[2]],map.prelim[[3]], map.prelim[[4]]
             ,map.prelim[[5]],map.prelim[[6]], map.prelim[[7]], map.prelim[[8]],
             nrow = 2)

##==========##
##VARIOGRAMS##
##==========##

#Examiner le variogramme pour détecter les valeurs abberrantes **JUIN 2012**
v.modJ12 <- variogram(depth_m ~ 1, echo.camp[[1]], cutoff = 3500, width = 20)
plot(v.modJ12)

v.modA12 <- variogram(depth_m ~ 1, echo.camp[[2]], cutoff = 3500, width = 20)
plot(v.modA12)

v.modA13 <- variogram(depth_m ~ 1, echo.camp[[3]], cutoff = 3500, width = 20)
plot(v.modA13)

##=====================================##
##CREATE SPATIAL GRID FOR INTERPOLATION##
##=====================================##

#Create convex hull polygons around points

poly <- list()
for(i in 1:length(echo.camp)){
  chull <- chull(echo.camp[[i]]@coords[,1],echo.camp[[i]]@coords[,2])
  chull_poly <- coordinates(echo.camp[[i]])[c(chull, chull[1]), ] #Close polygone
  poly[[i]] <- sp::SpatialPolygons(list(Polygons(list(Polygon(chull_poly)), ID=1)))
}
plot(poly[[7]])

#Transform to spatial grid
grid <- list()
for(i in 1:length(echo.camp)){
  rast <- setValues(raster::raster(ext=extent(poly[[i]]), crs = CRS, resolution=20),0)
  rast.mast <- raster::mask(rast, poly[[i]]) 
  grid[[i]] <- as(rast.mast, "SpatialGridDataFrame")
  
}


##==============##
##REMOVE OUTLIER##
##==============##

#June 2012
ggplot(echo.camp[[1]][!(echo.camp[[1]]$x.utm < 660900 & 
                          echo.camp[[1]]$y.utm > 5111850) &
                        echo.camp[[1]]$depth_m < 5,],
       aes(x = x.utm, y = y.utm)) +
  geom_point(aes(color = depth_m)) +
  scale_color_viridis_c()
echo.camp[[1]] <- echo.camp[[1]][!(echo.camp[[1]]$x.utm < 660900 & 
                                     echo.camp[[1]]$y.utm > 5111850) &
                                   echo.camp[[1]]$depth_m < 5,]

#August 2012
ggplot(echo.camp[[2]][echo.camp[[2]]$depth_m < 3,],
       aes(x = x.utm, y = y.utm)) +
  geom_point(aes(color = depth_m)) +
  scale_color_viridis_c()
echo.camp[[2]] <- echo.camp[[2]][echo.camp[[2]]$depth_m < 3,]

#August 2013
ggplot(echo.camp[[3]][echo.camp[[3]]$depth_m < 3,],
       aes(x = x.utm, y = y.utm)) +
  geom_point(aes(color = depth_m)) +
  scale_color_viridis_c()

#August 2014
#August 2015
#August 2016 south
##August 2016 North
#August 2017
