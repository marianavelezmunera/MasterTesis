# PCA
# PCA filo
pca_filo<-prcomp(na.omit(subset(metadatos_hongos,Tipo_muestra=="Filosfera")[,c(17,19,21,23:37)]),scale. = TRUE)

pca_filo_tabla<-data.frame(na.omit(subset(metadatos_hongos,Tipo_muestra=="Filosfera")[,c(1:5,17,19,21,23:37)])[1:5],pca_filo$x[,1:2])
pca_filo_loadings<-data.frame(Variables=rownames(pca_filo$rotation),pca_filo$rotation)

#PCA rizo
pca_rizo<-prcomp(na.omit(subset(metadatos_hongos,Tipo_muestra=="Rizosfera")[,c(17,19,21,23:37)]),scale. = TRUE)
pca_rizo_tabla<-data.frame(na.omit(subset(metadatos_hongos,Tipo_muestra=="Rizosfera")[,c(1:5,17,19,21,23:37)])[1:5],pca_rizo$x[,1:2])
pca_rizo_loadings<-data.frame(Variables=rownames(pca_rizo$rotation),pca_rizo$rotation)

# Plots

pca_filosfera<-ggplot(pca_filo_tabla,aes(x=PC1,y=PC2,colour=Altitud))+
  geom_point()+
  geom_segment(data = pca_filo_loadings, aes(x = 0, y = 0, xend = (PC1*20),yend = (PC2*20)), arrow = arrow(length = unit(1/2, "picas")),color = "black",linewidth=0.1)+
  annotate("text", x = (pca_filo_loadings$PC1*22), y = (pca_filo_loadings$PC2*23),
           label = c("Temperatura (C°)","Intensidad de luz (lux)","Humedad relativa","pH","CE","Materia orgánica","N","Ctotal","Ptotal","P","K","Ca","Mg","Na","CIC","NO3","NH4","Fe"),family="Rubik")+
  ylab("PC2 (19.74%)")+xlab("PC1 (41.9%)")+
  scale_colour_manual(name="Elevación",values=moma.colors("Warhol",5))+
  theme_pubr()+
  theme(legend.position = "right")+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=20))+
  theme(legend.text = element_text(family = "Rubik",size=16))+
  theme()
ggsave("pca_filo2.png",last_plot())
pca_filosfera

pca_rizo<-ggplot(pca_rizo_tabla,aes(x=PC1,y=PC2,colour=Altitud))+
  geom_point()+
  geom_segment(data = pca_rizo_loadings, aes(x = 0, y = 0, xend = (PC1*20),yend = (PC2*20)), arrow = arrow(length = unit(1/2, "picas")),color = "black",linewidth=0.1)+
  annotate("text", x = (pca_filo_loadings$PC1*22), y = (pca_filo_loadings$PC2*23),
           label = c("Temperatura (C°)","Intensidad de luz (lux)","Humedad relativa","pH","CE","Materia orgánica","N","Ctotal","Ptotal","P","K","Ca","Mg","Na","CIC","NO3","NH4","Fe"),family="Rubik")+
  ylab("PC2 (20.46%)")+xlab("PC1 (41.07%)")+
  scale_colour_manual(name="Elevación",values=moma.colors("Warhol",5))+
  theme_pubr()+
  theme(legend.position = "right")+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=20))+
  theme(legend.text = element_text(family = "Rubik",size=16))+
  theme()+
  ggtitle("Rizósfera")+theme(plot.title = element_text(family="Rubik",size=16,face = "bold"))


pca_completo
