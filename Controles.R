library(readr)
metadatos_control <- read_delim("control/metadatos_hongos_qiime_int1.txt", 
                                delim = "\t", escape_double = FALSE, 
                                trim_ws = TRUE)
View(metadatos_control)
metadatos_control<-as.data.frame(metadatos_control)
rownames(metadatos_control)<-metadatos_control$id
rownames(metadatos_control)
metadatos_control$Parcela<-as.character(metadatos_control$Parcela)
metadatos_control$Altitud<-as.character(metadatos_control$Altitud)

metadatos_control<-metadatos_control[-1,]
metadatos_control[metadatos_control=="RNA later"]<-"Control"
sample_data(hongos_control)<-metadatos_control
hongos_control@tax_table[hongos_control@tax_table=="Fungi_phy_Incertae_sedis"]<-NA

# Barras

barras_control<-microbiome::transform(aggregate_top_taxa2(hongos_control,"Order",top=10),"compositional")

barras_controls<-plot_composition(barras_control,group_by = "Tipo_muestra")+
  theme_pubclean()+
  theme(legend.position = "right")+
  ylab("Abundancia")+xlab("Muestra")+
  scale_fill_manual(name="Orden",values=moma.colors("Warhol",11))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16,angle = 90))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=18))+
  theme(legend.text = element_text(family = "Rubik",size=14))+
  theme(legend.key.size = unit(0.5,"cm"))
barras_controls
ggsave("control_barras.png",last_plot())
