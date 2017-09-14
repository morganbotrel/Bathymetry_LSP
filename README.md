## Bathymetry_LSP ##
Mis à jour le 14/09/2017

## DESCRIPTION ##

Ce projet réalise des cartes bathymétriques dans la zone GRIL du lac Saint-Pierre 
à partir des transects d'échosondage.  

Deux types de cartes sont réalisées en réponse à deux objectifs distints:
1- Estimer la profondeur réelle lors de la prise des photographies aériennes 
2- Obtenir des cartes bathymétriques comparables entre les années afin de déterminer les 
zones d'érosion et d'accumulation des sédiments et utiliser pour le modèle hydrodynamique

Pour ce faire les profondeurs lors de l'échosondage et la prise de photographies aériennes
ont été corrigées selon le niveau d'eau. Les corrections apportées pour ces objectifs
sont les suivantes :
1- Zphoto = Zecho + (SLEVphoto - SLEVecho)
2- Z0 = Zecho - SLEVecho
où Zphoto est la profondeur réelle lors de la prise de photographie aérienne, 
Zecho est la profondeur mesurée lors de l'échosondage,
SLEVphoto est le niveau d'eau mesuré lors de la prise de photographie aérienne,
SLEVecho est le niveau d'eau mesuré lors de l'échosondage
et Z0 est le zéro des cartes.

Les profondeurs obtenues ont été interpolées sur la zone d'étude par krigeage. Ces cartes 
bathymétriques ont été exportées en geotiff.

## DONNÉES ET FICHIERS ##

# Le fichier Raw_data contient les données bruts pour les analyses

Les documents EchoLSP_Mois_2012_R.csv contient les données d'échosondage. 
Les échosondages ont été réalisés en juin 2012 et d'août 2013 à 2017 avec un  
transducteur Biosonic DT-X  à faisceau unique contrôlé par Visual Acquisition 6.06 
(angle du faisceau: 6.6°, fréquence: 430 kH, longueur et taux de l'impulsion 0.1 ms et 
5 ping/s). La résolution minimale des profondeurs (incrément de profondeur) est x cm.
Les données de positionnement ont été obtenues par un récepteur GNSS NovAtel 
Smart V1 utilisant le réseaux Omnistar VBS en 2012-2013 (précision 90 cm) et WAAS de 2014 
à 2017 (précision 65 cm). Les échogrammes ont été analysés sur Visual Habitat 1, le fond 
a été déterminé avec un rising edge threshold de -47 dB et un rising edge length de 10 cm. 
Les délinéations résultantes ont été corrigées manuellement et les fichiers exportés sur 
des cycles de 5 pings. 
Le document Niveaux_horaires_15975 contient les niveaux horaires 1er janvier 2012 au 1er 
décembre 2016 à la Station Lac Saint-Pierre (15975) du réseau de station canadiennes de 
Pêches et Océan Canada (http://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/twl-mne/index-fra.htm).
Le niveau d'eau est rapporté selon le SRIGL1985 (en anglais IGLD1985).
Le document Photo_date_heure.csv contient les informations sur le moment de prise des
photographies aériennes (date et heure) tiré des rapports d'envol transmis par la compagnie
Groupe Info Consult responsable de leur acquisition. 

# Le fichier Corrected_data contient les données avec les corrections de profondeurs selon 
le niveau d'eau

Les documents DepthPHOTO_LSPMonthYear.csv contiennent la correction pour l'objectif 1 et
les documents DepthZC_LSPMonthYear.csv contiennent la correction pour l'objectif 2.
Les coordonnées géographiques ont également été projetées et converties en UTM zone 8, 
NAD83.

# Le fichier Interpolated_maps contient les cartes bathymétriques interpolées en format 
geotiff

Pour chaque année et objectif (PHOTO ou Z0) deux types de cartes sont réalisées: 
la carte des profondeurs (Depth_OBJECTIF_LSPmoisannée.tif)
et la carte de la variance sur la prédiction (DepthError_OBJECTIF_LSPmoisannée.tif)

## SCRIPTS ##

# Le script Depth_Photo.R réalise la correction des profondeurs pour l'objectif 1.
# Le script Depth_ZeroCarte.R réalise la correction des profondeurs pour l'objectif 2
# Le script Interpolation_PHOTO réalise les cartes bathymétriques à partir des profondeurs 
corrigées correspondant à l'objectif 1.
# Le script Interpolation_Z0 réalise les cartes bathymétriques à partir des profondeurs 
corrigées correspondant à l'objectif 2.


## Crédit ##

Projet réalisé par Morgan Botrel
morganbotrel@gmail.com