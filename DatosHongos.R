# Objeto phyloseq 

hongos<-qza_to_phyloseq(features = "TablaDADA2HongosForward.qza",tree="ArbolRooted.qza",taxonomy = "TaxonomiaHongos.qza")

metadatos_hongos<-as.data.frame(metadatos_hongos)
rownames(metadatos_hongos)<-metadatos_hongos$id
rownames(metadatos_hongos)
metadatos_hongos$Parcela<-as.character(metadatos_hongos$Parcela)
metadatos_hongos$Altitud<-as.character(metadatos_hongos$Altitud)
sample_data(hongos)<-metadatos_hongos

# Objetos individuales

ASV<-as.data.frame(otu_table(hongos))
taxonomy<-as.data.frame(tax_table(hongos))

# Mirar si hay que filtrar
unique(taxonomy$Phylum)
unique(taxonomy$Kingdom)

# Reemplazar "Fungi_phy_Incertae_sedis" por NA

hongos@tax_table[hongos@tax_table=="Fungi_phy_Incertae_sedis"]<-NA


# Objeto sin controles

hongos_bien<-subset_samples(hongos,ID_individuo!="Control")
hongos_bien
