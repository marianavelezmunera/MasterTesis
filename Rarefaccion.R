# Rarefaccion

rarefaccion <- rarecurve(t(abundances(hongos_bien)), 
                         step = 50, label = T,tidy = TRUE) #Vamos a usar el dato de QIIME y vamos a hacer la rarefacción a 	26919
hongos_rare<- rarefy_even_depth(hongos_bien, rngseed=1, sample.size = 26919, replace = F) 

ggplot(data=rarefaccion,aes(x=Sample,y=Species,col=Site))+
  geom_line()+
  theme_pubclean()+
  theme(legend.position = "none")+
  xlab("Sequencing depth")+ylab("ASV number")+
  scale_color_manual(values = met.brewer("Klimt",34))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))
ggsave("rarefaccion.png",last_plot())
