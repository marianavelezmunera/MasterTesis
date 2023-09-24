#Diversidad beta
# PCoAs

pcoa.bray <- ordinate(hongos_rare, "PCoA", "bray")
muestra_bray <- plot_ordination(hongos_rare, pcoa.bray, color= "Tipo_muestra")+
  theme_pubr()+
  theme(legend.position = "right")+
  scale_color_manual(name="Sample type",values=met.brewer("Klimt",3),labels=c("Phyllosphere","Rhizosphere","Bulk soil"))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=20))+
  theme(legend.text = element_text(family = "Rubik",size=16))
muestra_bray
ggsave("bray_muestra.png",last_plot())

pcoa.unifrac<-ordinate(hongos_rare,"PCoA","unifrac")
muestra_uni<-plot_ordination(hongos_rare,pcoa.unifrac,color = "Tipo_muestra")+
  theme_pubr()+
  theme(legend.position = "right")+
  scale_color_manual(name="Sample type",values=met.brewer("Klimt",3),labels=c("Phyllosphere","Rhizosphere","Bulk soil"))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=20))+
  theme(legend.text = element_text(family = "Rubik",size=16))
muestra_uni
ggsave("uni_muestra.png",last_plot())

#PERMANOVA

# Por parcela por tipo de muestra

perma_muestra<-adonis2(t(hongos_rare@otu_table)~Tipo_muestra,data=metadatos_hongos[1:34,],method = "bray")
perma_muestra
perma_parce<-adonis2(t(hongos_rare@otu_table)~Altitud,data = metadatos_hongos[1:34,],method = "bray")
perma_parce

# Desagreguemos por muestra para la de las parcelas

hongos_filosfera<-subset_samples(hongos_rare,Tipo_muestra=="Filosfera")
hongos_suelo<-subset_samples(hongos_rare,Tipo_muestra=="Suelo")
hongos_rizosfera<-subset_samples(hongos_rare,Tipo_muestra=="Rizosfera")

perma_filo<-adonis2(t(hongos_filosfera@otu_table)~Altitud,data = subset(metadatos_hongos,Tipo_muestra=="Filosfera"),method = "bray",permutations = 999)

perma_filo

perma_suelo<-adonis2(t(hongos_suelo@otu_table)~Altitud,data = subset(metadatos_hongos,Tipo_muestra=="Suelo"),method = "bray",permutations = 999)
perma_suelo

perma_rizos<-adonis2(t(hongos_rizosfera@otu_table)~Altitud,data = subset(metadatos_hongos,Tipo_muestra=="Rizosfera"),method = "bray",permutations = 999)
perma_rizos # No sé qué hacer

# Ordenacion de cada tipo de muestra

# Filosfera
pcoa.bray.filo<-ordinate(hongos_filosfera,"PCoA","bray")
pcoa.unifrac.filo<-ordinate(hongos_filosfera,"PCoA","unifrac")

bray.filo<-plot_ordination(hongos_filosfera,pcoa.bray.filo,color="Altitud")+
  theme_pubr()+
  theme(legend.position = "right")+
  scale_color_manual(name="Elevation",values=met.brewer("Klimt",5))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=20))+
  theme(legend.text = element_text(family = "Rubik",size=16))
bray.filo
ggsave("bray_filo.png",last_plot())

unifrac.filo<-plot_ordination(hongos_filosfera,pcoa.unifrac.filo,color = "Altitud")+
  theme_pubr()+
  theme(legend.position = "right")+
  scale_color_manual(name="Elevation",values=met.brewer("Klimt",5))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=20))+
  theme(legend.text = element_text(family = "Rubik",size=16))
unifrac.filo
ggsave("uni_filo.png",last_plot())

# Rizosfera
pcoa.bray.rizo<-ordinate(hongos_rizosfera,"PCoA","bray")
pcoa.unifrac.rizo<-ordinate(hongos_rizosfera,"PCoA","unifrac")

bray.rizo<-plot_ordination(hongos_rizosfera,pcoa.bray.rizo,color="Altitud")+
  theme_pubr()+
  theme(legend.position = "right")+
  scale_color_manual(name="Elevation",values=met.brewer("Klimt",5))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=20))+
  theme(legend.text = element_text(family = "Rubik",size=16))
bray.rizo
ggsave("bray_rizo.png",last_plot())

unifrac.rizo<-plot_ordination(hongos_rizosfera,pcoa.unifrac.rizo,color = "Altitud")+
  theme_pubr()+
  theme(legend.position = "right")+
  scale_color_manual(name="Elevation",values=met.brewer("Klimt",5))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=20))+
  theme(legend.text = element_text(family = "Rubik",size=16))
unifrac.rizo
ggsave("uni_rizo.png",last_plot())

# ANOSIMs 

ANOSIM_muestra<-anosim(t(hongos_rare@otu_table), metadatos_hongos[1:34,]$Tipo_muestra, distance="bray",permutations=9999)
ANOSIM_muestra

ANOSIM_elevacion<-anosim(t(hongos_rare@otu_table),metadatos_hongos[1:34,]$Altitud,distance = "bray",permutations = 9999)
ANOSIM_elevacion

ANOSIM_filo<-anosim(t(hongos_filosfera@otu_table),subset(metadatos_hongos,Tipo_muestra=="Filosfera")$Altitud,distance = "bray",permutations = 9999)
ANOSIM_filo

ANOSIM_rizo<-anosim(t(hongos_rizosfera@otu_table),subset(metadatos_hongos,Tipo_muestra=="Rizosfera")$Altitud,distance = "bray",permutations = 9999)
ANOSIM_rizo

# Tabla permanova

modelo_perma<-c("All ~ Sample type","All ~ Elevation","Phyllosphere ~ Elevation","Rhizosphere ~ Elevation")
p_perma<-c(0.001,0.085,0.001,0.001)
f_perma<-c(2.2413,1.1736,1.4538,2.4178)
tabla_perma<-data.frame(modelo_perma,f_perma,p_perma)%>%
  gt()%>%
  cols_label(modelo_perma="Model",f_perma="Pseudo-F",p_perma="p-value")%>%
  gt_theme_pff()
tabla_perma
gtsave(tabla_perma,"tabla_permanova.png")

#Tabla ANCOM

R_ancom<-c(0.4212,0.1378,0.6193,0.9428)
p_ancom<-c("<0.001","0.0334","<0.001","<0.001")
tabla_ANCOM<-data.frame(modelo_perma,R_ancom,p_ancom)%>%
  gt()%>%
  cols_label(modelo_perma="Model",R_ancom="R",p_ancom="p-value")%>%
  gt_theme_pff()
tabla_ANCOM
gtsave(tabla_ANCOM,"tabla_ANCOM.png")
