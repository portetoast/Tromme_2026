source("./src/import_gbif_inat_data.r")

source("./src/ecosystem.r")

source("./src/elevation.r")

source("./src/Satellite.r")

source("./src/Satellite_landcover.r")

source("./src/matrix_full.r")


#The goal of this work is to analyse data of two bat species in Switzerland: Myotis myotis and myotis blythii. 
#The 2 species are morphologically close and can live in sympatry as they have their own hunting place preferencies
# and don't fight for the same resources. 
#However, some studies revealed hybridization occuring, notably in the french alps. To be able to investigate if hybridization
#also happens within the swiss communities, it's important to look for places where both species can be present. 
#This work aims to analyse different dataset of occurence and ecological factors to determine possible sites
#where the 2 species can be present.