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
muestra_uni<-plot_ordination(hongos_rare,pcoa.unifrac,shape = "Tipo_muestra",color="Altitud")+
  theme_pubr()+
  theme(legend.position = "right")+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=20))+
  theme(legend.text = element_text(family = "Rubik",size=16))+
  scale_color_manual(name="Elevación",values=moma.colors("Warhol",5))+
  scale_shape(name="Tipo de muestra",labels=c("Filósfera","Rizósfera","Suelo"))
muestra_uni
ggsave("uni_muestra_total.png",last_plot())

#PERMANOVA

# Por parcela por tipo de muestra

perma_muestra<-adonis2(t(hongos_rare@otu_table)~Tipo_muestra,data=metadatos_hongos[1:34,],method = "bray")
perma_muestra
perma_parce<-adonis2(t(hongos_rare@otu_table)~Altitud,data = metadatos_hongos[1:34,],method = "bray")
perma_parce

perma_total<-adonis2(t(hongos_rare@otu_table)~Tipo_muestra+Altitud,data = metadatos_hongos[1:34,],method = "bray")
perma_total
# Desagreguemos por muestra para la de las parcelas


perma_filo<-adonis2(t(hongos_filosfera@otu_table)~Altitud,data = subset(metadatos_hongos,Tipo_muestra=="Filosfera"),method = "bray",permutations = 999)

perma_filo

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

modelo_perma<-c("Tipo de muestra", "Elevacion","Phyllosphere ~ Elevation","Rhizosphere ~ Elevation")
p_perma<-c(0.001,0.085,0.001,0.001)
f_perma<-c(2.2413,1.1736,1.4538,2.4178)
tabla_perma<-data.frame(modelo_perma,f_perma,p_perma)%>%
  gt()%>%
  cols_label(modelo_perma="Model",f_perma="Pseudo-F",p_perma="p-value")%>%
  gt_theme_pff()
tabla_perma
gtsave(tabla_perma,"tabla_permanova.png")

#Tabla ANOSIM

R_anosim<-c(0.4212,0.1378,0.6193,0.9428)
p_anosim<-c("<0.001","0.0334","<0.001","<0.001")
tabla_ANOSIM<-data.frame(modelo_perma,R_anosim,p_anosim)%>%
  gt()%>%
  cols_label(modelo_perma="Model",R_anosim="R",p_anosim="p-value")
tabla_ANOSIM
gtsave(tabla_ANOSIM,"tabla_ANOSIM.png")
rownames(perma_total)<-c("Tipo de muestra","Elevación","Residual","Total")

tabla_perma_completa<-perma_total[1:2,-3]%>%
  gt(rownames_to_stub = TRUE)%>%
  tab_options(table.font.size = px(20)) |>
  opt_table_font(
    font = list(
      google_font(name = "Rubik")))

tabla_perma_completa<-sub_missing(tabla_perma_completa,missing_text = "-")
tabla_perma_completa

tabla_perma_completa<-tabla_perma_completa%>%
  tab_options(column_labels.font.weight = 'bold',
              stub.font.weight = "bold")

tabla_perma_completa<-tabla_perma_completa%>%
  tab_options(column_labels.border.top.color = "black",
              column_labels.border.bottom.color = "black",
              column_labels.border.lr.color = "black")

tabla_perma_completa<-tabla_perma_completa%>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "all", weight = px(2),color="black")))%>%
  tab_style(locations = cells_stubhead(),
            style=cell_borders(sides=c("left","top"),color="transparent",weight = px(2)))%>%
  tab_style(locations = cells_stubhead(),
            style = cell_borders(sides="right",color="transparent"))
tabla_perma_completa

tabla_perma_completa<-tabla_perma_completa%>%
  tab_style(
  style = cell_borders(
    sides = "all",
    weight = px(2),color="black"
  ),
  locations = cells_body()
)

tabla_perma_completa<-tabla_perma_completa%>%
  tab_style(
    style = cell_borders(sides="all",color="black",weight = px(2)),
    locations = cells_stub()
  )

tabla_perma_completa<-tabla_perma_completa%>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(cell_text(color="black"),cell_fill(color=moma.colors("Warhol",10)[2])))%>%
  tab_style(style = list(cell_text(color="black"),cell_fill(color=moma.colors("Warhol",10)[2])),
            locations = cells_stub())
tabla_perma_completa

tabla_perma_completa<-tabla_perma_completa%>%
  tab_source_note(
    source_note = "Tabla 2. Resultados del PERMANOVA"
  )%>%
  tab_style(locations = cells_source_notes(),
            style = cell_borders("all",color="white"))%>%
  opt_table_lines(extent = "none")
gtsave(tabla_perma_completa,"tabla_perma_completa.png")

pagedown::chrome_print("Poster/Poster.Rmd")

