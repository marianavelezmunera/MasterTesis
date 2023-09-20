# Rarefaccion

rarefaccion <- rarecurve(t(abundances(hongos_bien)), 
                         step = 50, label = T) #Vamos a usar el dato de QIIME y vamos a hacer la rarefacción a 	26919
hongos_rare<- rarefy_even_depth(hongos_bien, rngseed=1, sample.size = 26919, replace = F) 

