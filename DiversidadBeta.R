#Diversidad beta
# PCoAs

pcoa.bray <- ordinate(hongos_rare, "PCoA", "bray")
muestra_bray <- plot_ordination(hongos_rare, pcoa.bray, color= "Tipo_muestra")
altitud_bray<-plot_ordination(hongos_rare, pcoa.bray, color= "Altitud")
muestra_bray
altitud_bray

pcoa.unifrac<-ordinate(hongos_rare,"PCoA","unifrac")
muestra_uni<-plot_ordination(hongos_rare,pcoa.unifrac,color = "Tipo_muestra")
muestra_uni
altitud_uni<-plot_ordination(hongos_rare, pcoa.unifrac, color= "Altitud")
altitud_uni

#PERMANOVA

# Por parcela por tipo de muestra

perma_muestra<-adonis2(t(hongos_rare@otu_table)~Tipo_muestra,data=metadatos_hongos[1:34,],method = "bray")
perma_muestra
perma_parce<-adonis2(t(hongos_rare@otu_table)~Parcela,data = metadatos_hongos[1:34,],method = "bray")
perma_parce

# Desagreguemos por muestra para la de las parcelas

hongos_filosfera<-subset_samples(hongos_rare,Tipo_muestra=="Filosfera")
hongos_suelo<-subset_samples(hongos_rare,Tipo_muestra=="Suelo")
hongos_rizosfera<-subset_samples(hongos_rare,Tipo_muestra=="Rizosfera")

perma_filo<-adonis2(t(hongos_filosfera@otu_table)~Parcela,data = subset(metadatos_hongos,Tipo_muestra=="Filosfera"),method = "bray",permutations = 999)

perma_filo

perma_suelo<-adonis2(t(hongos_suelo@otu_table)~Parcela,data = subset(metadatos_hongos,Tipo_muestra=="Suelo"),method = "bray",permutations = 999)
perma_suelo

perma_rizos<-adonis2(t(hongos_rizosfera@otu_table)~Parcela,data = subset(metadatos_hongos,Tipo_muestra=="Rizosfera"),method = "bray",permutations = 999)
perma_rizos # No sé qué hacer

# Ordenacion de cada tipo de muestra

# Filosfera
pcoa.bray.filo<-ordinate(hongos_filosfera,"PCoA","bray")
pcoa.unifrac.filo<-ordinate(hongos_filosfera,"PCoA","unifrac")

bray.filo<-plot_ordination(hongos_filosfera,pcoa.bray.filo,color="Parcela")
bray.filo
unifrac.filo<-plot_ordination(hongos_filosfera,pcoa.unifrac.filo,color = "Parcela")
unifrac.filo

# Suelo
pcoa.bray.suelo<-ordinate(hongos_suelo,"PCoA","bray")
pcoa.unifrac.suelo<-ordinate(hongos_suelo,"PCoA","unifrac")

bray.suelo<-plot_ordination(hongos_suelo,pcoa.bray.suelo,color="Parcela")
bray.suelo
unifrac.suelo<-plot_ordination(hongos_suelo,pcoa.unifrac.suelo,color = "Parcela")
unifrac.suelo

# Rizosfera
pcoa.bray.rizo<-ordinate(hongos_rizosfera,"PCoA","bray")
pcoa.unifrac.rizo<-ordinate(hongos_rizosfera,"PCoA","unifrac")

bray.rizo<-plot_ordination(hongos_rizosfera,pcoa.bray.rizo,color="Parcela")
bray.rizo

unifrac.rizo<-plot_ordination(hongos_rizosfera,pcoa.unifrac.rizo,color = "Parcela")
unifrac.rizo

# ANOSIMs 

ANOSIM_muestra<-anosim(t(hongos_rare@otu_table), metadatos_hongos[1:34,]$Tipo_muestra, distance="bray",permutations=9999)
ANOSIM_muestra

ANOSIM_filo<-anosim(t(hongos_filosfera@otu_table),subset(metadatos_hongos,Tipo_muestra=="Filosfera")$Parcela,distance = "bray",permutations = 9999)
ANOSIM_filo

ANOSIM_rizo<-anosim(t(hongos_rizosfera@otu_table),subset(metadatos_hongos,Tipo_muestra=="Rizosfera")$Parcela,distance = "bray",permutations = 9999)
ANOSIM_rizo

