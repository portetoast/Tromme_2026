#######################################################################
###       TO READ      ################################################
#######################################################################
#The goal of this work is to analyse data of two bat species in Switzerland: Myotis myotis and myotis blythii. 
#The 2 species are morphologically close and can live in sympatry as they have their own hunting place preferencies
# and don't fight for the same resources. 
#However, some studies revealed hybridization occuring, notably in the french alps. To be able to investigate if hybridization
#also happens within the swiss communities, it's important to look for places where both species can be present. 
#This work aims to analyse different dataset of occurence and ecological factors to determine possible sites
#where the 2 species can be present.


##COMMENT ON IMPORTED DATA#########
#After extracting data from Gbif and Inaturalist, I had aprrox. 1200 occurence points, but many observations where done at the 
#exact same location but at different dates, sometimes only 1 days between 2 observations. I conclueded it was the same individual 
#most of the time so I deleted the date of the curated data and suppressed the duplicates. At the end I had 216 data on Myotis myotis and 9 data from 
#Myotis blythii. Due to the low data on Myotis blythii, it will be difficult to perform strong ecological comparison between the 2 species
#and further analyses could be performed at larger scale by extracting European data for example. 


##COMMENT ON ECOSYSTEM################
#Analyses of both species ecosystem shows that Myotis myotis is found in the higher majority in Cool temparate moist climate and that
#Myotis blythii is found only in Cool temperate mosit climate (see barplot p2).
#Myotis myotis is found at different land types but in majority in croplands and Myotis blythii is also found in different types of 
#landcover buta bit more in forests (see barplot p3). 


##COMMENT ON ELEVATION################
## Comparison of elevation on the distribution of the 2 species shows similar elevation distribution 
#between the 2 species. The higher density where both species are present is at approximatively 800-900m high. 

##COMMENT ON SATELLITE (NDVI)################
#NDVI map of Switzerland was done using the quantile of 60 data from the year 2021.

##COMMENT ON SATELLITE (LANDCOVER)################
# Comparing the plot (see barplot pp)  with the ecosystem plot of the distribution of the 2 species (barplot p3),
# We can see that this one splits landcover types in more different categories and that 
# Myotis myotis is still predominantly found in croplands, but also in a new categorie, the savanna, and
# Myotis blythii is more present in woody savannas and in second position the mixed forests.
# It's interesting to see how much variations there are depending on the data you chose to work with.

##COMMENT ON CLIMATE################
##Climatic data were gathered after the intermediary project. 


###COMMENT ON DATA ANALYSES##########

#GRAPH A map of switzerland
#Analyses of occurence of Myotis myotis and Myotis blythii showed that most observations can be found 
#on the west side of Switerland, and all the observation points for Myotis blythii are in the Romandie region (JU,VD,NE)

#GRAPH B radar plot 
#Both species appear to inhabit a wide range of habitats in terms of elevation and precipitation
#Their strong presence in grasslands (high NDVI + high precipitation) is consistent with the known ecology of these bats, which hunt in open grassy areas
#Their presence in urban areas warrants attention—this may reflect roosting in buildings, a behavior typical of M. myotis
#Urban areas seems to have a high NDVI value, which is an odd result, but may find an explanation if observations of bats were in parks and green area of cities.

#Graph C 
#Myotis myotis and M. blythii primarily inhabit low- to mid-elevation habitats (cropland, settlements, grasslands), which is consistent with their known ecology. 
#Records from forested areas indicate a broader elevation tolerance. Sightings in high-elevation, vegetation-free areas are questionable and warrant further verification. 

#GRAPH D Temperature vs elevation by species
# There seems to be a strong similar correlation between both bat species considering temperature and elevation.
#Majority of observation are found between 200 and 1100 m. Myotis myotis is also found at higher altitude/lower temperature
#But it's hard to infer of this ecological trait as few data for Myotis blythii are available.

#PCA Biplot
#The PCA explains 74.9% + 17.2% = 92.1% of the total variance across the first two dimensions. The biplot captures almost all of the information.
#Les deux ellipses se chevauchent largement → pas de séparation nette des niches environnementales entre les deux espèces sur ces variables. 
#Cela peut s'expliquer par :
#Le très faible nombre d'observations de M. blythii (déséquilibre important)
#Une réelle similarité de niche entre les deux espèces, connue dans la littérature (elles cohabitent souvent dans les mêmes gîtes)
