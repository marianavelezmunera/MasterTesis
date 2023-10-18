# Análisis funcional

# Todos los hongos
hongos_meco<-phyloseq2meco(hongos_rare)
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
ggsave("funcion_total.png",last_plot())

#Filosfera

hongos_filo_buenos<-prune_taxa(taxa_sums(hongos_filosfera)>1,hongos_filosfera)
hongos_meco_filo<-phyloseq2meco(hongos_filo_buenos)
filo_funcion_datos<-trans_func$new(hongos_meco_filo)
filo_funcion_datos$cal_spe_func(prok_database = "FAPROTAX")
funciones_filo<-as.data.frame(filo_funcion_datos$res_spe_func)
funciones_filo$total_fila<-rowSums(funciones_filo,dims = 1)
funciones_filo<-subset(funciones_filo,total_fila>0)

taxonomy_filo<-as.data.frame(tax_table(hongos_filo_buenos))
taxonomy_filo$ASV<-rownames(taxonomy_filo)
taxanomy_funct_filo<-subset(taxonomy_filo,ASV %in% rownames(funciones_filo))
funciones_filo<-cbind(funciones_filo,taxanomy_funct_filo)
funciones_filo<-funciones_filo[,-35]
funciones_filo<-gather(data=funciones_filo,"Pathotroph","Saprotroph","Symbiotroph","Bryophyte Parasite","Dung Saprotroph",            "Ectomycorrhizal","Fungal Parasite","Leaf Saprotroph","Plant Parasite","Wood Saprotroph","Animal Pathogen","Endophyte","Plant Pathogen","Lichen Parasite","Litter Saprotroph","Soil Saprotroph","Plant Saprotroph","Epiphyte","Lichenized","Arbuscular Mycorrhizal","Endomycorrhizal","Ericoid Mycorrhizal","Orchid Mycorrhizal","Root Associated Biotroph","Clavicipitaceous Endophyte","Animal Endosymbiont",key="gremio",value="value")

tablita<-table(funciones_filo[,9:10])
tablita<-as.data.frame.matrix(tablita)

colnames(tablita)<-c("A","P")
tablita<-arrange(tablita,desc(P))
quitar_filo<-rownames(tablita[11:26,])

funciones_filo<-sapply(funciones_filo,function(x) replace(x, x %in% quitar_filo, "Otros"))
funciones_filo<-as.data.frame(funciones_filo)
mode(funciones_filo$value)<-"numeric"

unique(funciones_filo$gremio)

ggplot(funciones_filo,aes(x=reorder(gremio,-value),y=value,fill=Phylum))+
  geom_bar(stat = "identity")+
  ylab("Gremio funcional")+xlab("Número de ASVs")+
  scale_fill_manual(name="Filo",values=moma.colors("Warhol",9))+
  theme_pubclean()+
  theme(legend.position = "right")
ggsave("funcion_filo.png",last_plot())

## Rizosfera

hongos_rizo_buenos<-prune_taxa(taxa_sums(hongos_rizosfera)>1,hongos_rizosfera)
hongos_meco_rizo<-phyloseq2meco(hongos_rizo_buenos)
rizo_funcion_datos<-trans_func$new(hongos_meco_rizo)
rizo_funcion_datos$cal_spe_func(prok_database = "FAPROTAX")
funciones_rizo<-as.data.frame(rizo_funcion_datos$res_spe_func)
funciones_rizo$total_fila<-rowSums(funciones_rizo,dims = 1)
funciones_rizo<-subset(funciones_rizo,total_fila>0)

taxonomy_rizo<-as.data.frame(tax_table(hongos_rizo_buenos))
taxonomy_rizo$ASV<-rownames(taxonomy_rizo)
taxanomy_funct_rizo<-subset(taxonomy_rizo,ASV %in% rownames(funciones_rizo))
funciones_rizo<-cbind(funciones_rizo,taxanomy_funct_rizo)
funciones_rizo<-funciones_rizo[,-35]
funciones_rizo<-gather(data=funciones_rizo,"Pathotroph","Saprotroph","Symbiotroph","Bryophyte Parasite","Dung Saprotroph",            "Ectomycorrhizal","Fungal Parasite","Leaf Saprotroph","Plant Parasite","Wood Saprotroph","Animal Pathogen","Endophyte","Plant Pathogen","Lichen Parasite","Litter Saprotroph","Soil Saprotroph","Plant Saprotroph","Epiphyte","Lichenized","Arbuscular Mycorrhizal","Endomycorrhizal","Ericoid Mycorrhizal","Orchid Mycorrhizal","Root Associated Biotroph","Clavicipitaceous Endophyte","Animal Endosymbiont",key="gremio",value="value")

tablita_rizo<-table(funciones_rizo[,9:10])
tablita_rizo<-as.data.frame.matrix(tablita_rizo)

colnames(tablita_rizo)<-c("A","P")
tablita_rizo<-arrange(tablita_rizo,desc(P))
quitar_rizo<-rownames(tablita[11:26,])

funciones_rizo<-sapply(funciones_rizo,function(x) replace(x, x %in% quitar_rizo, "Otros"))
funciones_rizo<-as.data.frame(funciones_rizo)
mode(funciones_rizo$value)<-"numeric"

unique(funciones_rizo$gremio)

ggplot(funciones_rizo,aes(x=reorder(gremio,-value),y=value,fill=Phylum))+
  geom_bar(stat = "identity")+
  ylab("Gremio funcional")+xlab("Número de ASVs")+
  scale_fill_manual(name="Filo",values=moma.colors("Warhol",9))+
  theme_pubclean()+
  theme(legend.position = "right")
ggsave("funcion_rizo.png",last_plot())
