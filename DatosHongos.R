# Objeto phyloseq 

hongos<-qza_to_phyloseq(features = "TablaDADA2HongosForward.qza",tree="ArbolRooted.qza",taxonomy = "TaxonomiaHongos.qza")
library(readr)
metadatos_hongos <- read_delim("metadatos_hongos.txt", 
                               delim = "\t", escape_double = FALSE, 
                               trim_ws = TRUE)
View(metadatos_hongos)
metadatos_hongos<-as.data.frame(metadatos_hongos)
rownames(metadatos_hongos)<-metadatos_hongos$id
rownames(metadatos_hongos)
metadatos_hongos$Parcela<-as.character(metadatos_hongos$Parcela)
metadatos_hongos$Altitud<-as.character(metadatos_hongos$Altitud)
metadatos_hongos[3,25]<-10
colnames(metadatos_hongos)[25]<-"MO"
metadatos_hongos[22,25]<-9
colnames(metadatos_hongos)[35]<-"NO3"
colnames(metadatos_hongos)[36]<-"NH4"
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

# Objetos por tipo de muestra

hongos_filosfera<-subset_samples(hongos_rare,Tipo_muestra=="Filosfera")
hongos_suelo<-subset_samples(hongos_rare,Tipo_muestra=="Suelo")
hongos_rizosfera<-subset_samples(hongos_rare,Tipo_muestra=="Rizosfera")

