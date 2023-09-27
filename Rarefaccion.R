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

unique(rarefaccion$Site)

nrow(subset(rarefaccion,Site=="HF1A"))+ nrow(subset(rarefaccion,Site=="HF1B"))+nrow(subset(rarefaccion,Site=="HF1C"))+
nrow(subset(rarefaccion,Site=="HF2A"))+nrow(subset(rarefaccion,Site=="HF2B"))+nrow(subset(rarefaccion,Site=="HF2C"))+
nrow(subset(rarefaccion,Site=="HF3A"))+nrow(subset(rarefaccion,Site=="HF3B"))+nrow(subset(rarefaccion,Site=="HF3C"))+
nrow(subset(rarefaccion,Site=="HF4A"))+nrow(subset(rarefaccion,Site=="HF4B"))+nrow(subset(rarefaccion,Site=="HF4C"))+
nrow(subset(rarefaccion,Site=="HF5A"))+nrow(subset(rarefaccion,Site=="HF5B"))+nrow(subset(rarefaccion,Site=="HF5C"))

nrow(subset(rarefaccion,Site=="HR1A"))+nrow(subset(rarefaccion,Site=="HR1B"))+nrow(subset(rarefaccion,Site=="HR1C"))+
nrow(subset(rarefaccion,Site=="HR2A"))+nrow(subset(rarefaccion,Site=="HR2B"))+nrow(subset(rarefaccion,Site=="HR2C"))+
nrow(subset(rarefaccion,Site=="HR3A"))+nrow(subset(rarefaccion,Site=="HR3B"))+nrow(subset(rarefaccion,Site=="HR3C"))+
nrow(subset(rarefaccion,Site=="HR4A"))+nrow(subset(rarefaccion,Site=="HR4B"))+nrow(subset(rarefaccion,Site=="HR4C"))+
nrow(subset(rarefaccion,Site=="HR5A"))+nrow(subset(rarefaccion,Site=="HR5B"))+nrow(subset(rarefaccion,Site=="HR5C"))

nrow(subset(rarefaccion,Site=="HS1"))+
nrow(subset(rarefaccion,Site=="HS2"))+
nrow(subset(rarefaccion,Site=="HS3"))+
nrow(subset(rarefaccion,Site=="HS4"))+
nrow(subset(rarefaccion,Site=="HS5"))

parcela<-c(rep("1",4923),rep("2",4963),rep("3",2678),rep("4",3216),rep("5",2289),rep("1",3776),rep("2",2617),rep("3",5449),rep("4",3702),rep("5",3673),rep("1",2227),rep("2",1617),rep("3",1703),rep("4",1009),rep("5",1728))

rarefaccion$parcela<-parcela

muestra<-c(rep("Filo",18069),rep("Rizo",19217),rep("Suelo",8284))

rarefaccion$muestra<-muestra
colores<-met.brewer("Klimt",5)

ggplot(data=rarefaccion,aes(x=Sample,y=Species,col=Site))+
  geom_line(aes(linetype=muestra))+
  theme_pubclean()+
  theme(legend.position = "none")+
  xlab("Sequencing depth")+ylab("ASV number")+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  facet_wrap(~parcela)+
  scale_color_manual(values = met.brewer("Klimt",34))
ggsave("rarefaccion2.png",last_plot())

rarefaccion$parcela<-as.factor(rarefaccion$parcela)
