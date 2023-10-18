# Análisis funcional

hongos_funcion_datos<-trans_func$new(hongos_meco)
hongos_funcion_datos$cal_spe_func(prok_database = "FAPROTAX")
View(hongos_funcion_datos$res_spe_func)

funciones_hongos<-as.data.frame(hongos_funcion_datos$res_spe_func)
funciones_hongos$total_fila<-rowSums(funciones_hongos,dims = 1)
funciones_hongos<-subset(funciones_hongos,total_fila>0)
funciones_hongos[4305,]<-c(colSums(funciones_hongos[,1:26],26,1),rep("NA",8))

taxonomy_rare<-as.data.frame(tax_table(hongos_rare))
taxonomy_rare$ASV<-rownames(taxonomy_rare)
taxanomy_funct<-subset(taxonomy_rare,ASV %in% rownames(funciones_hongos))
funciones_hongos

funciones_hongos<-cbind(funciones_hongos[-4305,],taxanomy_funct)
funciones_hongos<-funciones_hongos[,-35]

funciones_hongos<-gather(data=funciones_hongos,"Pathotroph","Saprotroph","Symbiotroph","Bryophyte Parasite","Dung Saprotroph",            "Ectomycorrhizal","Fungal Parasite","Leaf Saprotroph","Plant Parasite","Wood Saprotroph","Animal Pathogen","Endophyte","Plant Pathogen","Lichen Parasite","Litter Saprotroph","Soil Saprotroph","Plant Saprotroph","Epiphyte","Lichenized","Arbuscular Mycorrhizal","Endomycorrhizal","Ericoid Mycorrhizal","Orchid Mycorrhizal","Root Associated Biotroph","Clavicipitaceous Endophyte","Animal Endosymbiont",key="gremio",value="value")

quitar<-c("Litter Saprotroph","Lichen Parasite","Dung Saprotroph","Leaf Saprotroph","Plant Saprotroph","Lichenized","Bryophyte Parasite","Epiphyte","Orchid Mycorrhizal","Plant Parasite","Clavicipitaceous Endophyte","Arbuscular Mycorrhizal","Endomycorrhizal","Ericoid Mycorrhizal","Animal Endosymbiont","Root Associated Biotroph")

funciones_hongos<-sapply(funciones_hongos,function(x) replace(x, x %in% quitar, "Otros"))

funciones_hongos<-as.data.frame(funciones_hongos)
mode(funciones_hongos$value)<-"numeric"
ggplot(funciones_hongos,aes(x=reorder(gremio,-value),y=value,fill=Phylum))+
  geom_bar(stat = "identity")+
  ylab("Gremio funcional")+xlab("Número de ASVs")+
  scale_fill_manual(name="Filo",values=moma.colors("Warhol",9))+
  theme_pubclean()+
  theme(legend.position = "right")

