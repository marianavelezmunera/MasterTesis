# Taxonomia 

# Nivel: Filo
# Con los NA
barras_filo_datos<-microbiome::transform(aggregate_top_taxa2(hongos_filosfera,"Phylum",top=10),"compositional")

barras_filo<-plot_composition(barras_filo_datos)+
  theme_pubclean()+
  theme(legend.position = "right")+
  ylab("Abundancia")+xlab("Muestra")+
  scale_fill_manual(name="Filo",values=met.brewer("Klimt",11))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=18))+
  theme(legend.text = element_text(family = "Rubik",size=14))+
  theme(legend.key.size = unit(0.5,"cm"))
barras_filo
ggsave("taxonomia_filo.png",last_plot())

barras_rizo_datos<-transform(aggregate_top_taxa2(hongos_rizosfera,"Phylum",top=10),"compositional")
barras_rizo<-plot_composition(barras_rizo_datos)+
  theme_pubclean()+
  theme(legend.position = "right")+
  scale_fill_manual(name="Phylum",values=met.brewer("Klimt",11))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=18))+
  theme(legend.text = element_text(family = "Rubik",size=14))+
  theme(legend.key.size = unit(0.5,"cm"))
barras_rizo
ggsave("taxonomia_rizo.png",last_plot())

barras_suelo_datos<-microbiome::transform(aggregate_top_taxa2(hongos_suelo,"Phylum",top=10),"compositional")
barras_suelo<-plot_composition(barras_suelo_datos)+
  theme_pubclean()+
  theme(legend.position = "right")+
  scale_fill_manual(name="Phylum",values=met.brewer("Klimt",11))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=18))+
  theme(legend.text = element_text(family = "Rubik",size=14))+
  theme(legend.key.size = unit(0.5,"cm"))
barras_suelo
ggsave("taxonomia_suelo.png",last_plot())

#Quitando los NA


filo_NA<-microbiome::transform(aggregate_top_taxa2(subset_taxa(hongos_filosfera,!is.na(Phylum)),"Phylum",top=5),"compositional")
filo_NA_barras<-plot_composition(filo_NA)+
  theme_pubclean()+
  theme(legend.position = "right")+
  scale_fill_manual(name="Phylum",values=met.brewer("Klimt",11))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=18))+
  theme(legend.text = element_text(family = "Rubik",size=14))+
  theme(legend.key.size = unit(0.5,"cm"))
filo_NA_barras
ggsave("filo_na_taxo.png",last_plot())

suelo_NA<-microbiome::transform(aggregate_top_taxa2(subset_taxa(hongos_suelo,!is.na(Phylum)),"Phylum",top=5),"compositional")
suelo_NA_barras<-plot_composition(suelo_NA)+
  theme_pubclean()+
  theme(legend.position = "right")+
  scale_fill_manual(name="Phylum",values=met.brewer("Klimt",11))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=18))+
  theme(legend.text = element_text(family = "Rubik",size=14))+
  theme(legend.key.size = unit(0.5,"cm"))
suelo_NA_barras
ggsave("suelo_na_taxo.png",last_plot())

rizo_NA<-microbiome::transform(aggregate_top_taxa2(subset_taxa(hongos_rizosfera,!is.na(Phylum)),"Phylum",top=5),"compositional")
rizo_NA_barras<-plot_composition(rizo_NA)+
  theme_pubclean()+
  theme(legend.position = "right")+
  scale_fill_manual(name="Phylum",values=met.brewer("Klimt",11))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=18))+
  theme(legend.text = element_text(family = "Rubik",size=14))+
  theme(legend.key.size = unit(0.5,"cm"))
rizo_NA_barras
ggsave("rizo_na_taxo.png",last_plot())
#Nivel: Orden

orden_filo_datos<-microbiome::transform(aggregate_top_taxa2(hongos_filosfera,"Order",top=10),"compositional")
orden_filo<-plot_composition(orden_filo_datos)+
  theme_pubclean()+
  theme(legend.position = "right")+
  scale_fill_manual(name="Phylum",values=met.brewer("Klimt",11))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=18))+
  theme(legend.text = element_text(family = "Rubik",size=14))+
  theme(legend.key.size = unit(0.5,"cm"))
orden_filo
orden_rizo_datos<-microbiome::transform(aggregate_top_taxa2(hongos_rizosfera,"Order",top=10),"compositional")
orden_rizo<-plot_composition(orden_rizo_datos)+
  theme_pubclean()+
  theme(legend.position = "right")+
  scale_fill_manual(name="Phylum",values=met.brewer("Klimt",11))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=18))+
  theme(legend.text = element_text(family = "Rubik",size=14))+
  theme(legend.key.size = unit(0.5,"cm"))
orden_rizo

orden_suelo_datos<-microbiome::transform(aggregate_top_taxa2(hongos_suelo,"Order",top=10),"compositional")
orden_suelo<-plot_composition(orden_suelo_datos)+
  theme_pubclean()+
  theme(legend.position = "right")+
  scale_fill_manual(name="Phylum",values=met.brewer("Klimt",11))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=18))+
  theme(legend.text = element_text(family = "Rubik",size=14))+
  theme(legend.key.size = unit(0.5,"cm"))
orden_suelo

#Quitando los NA


orden_filo_NA<-microbiome::transform(aggregate_top_taxa2(subset_taxa(hongos_filosfera,!is.na(Order)),"Order",top=10),"compositional")
filo_NA_orden<-plot_composition(orden_filo_NA)+
  theme_pubclean()+
  theme(legend.position = "right")+
  scale_fill_manual(name="Order",values=met.brewer("Klimt",11))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=18))+
  theme(legend.text = element_text(family = "Rubik",size=14))+
  theme(legend.key.size = unit(0.5,"cm"))
filo_NA_orden
ggsave("filo_na_orden.png",last_plot())

hongos_suelo@tax_table[hongos_suelo@tax_table=="Rozellomycota_ord_Incertae_sedis"]<-NA
orden_suelo_NA<-microbiome::transform(aggregate_top_taxa2(subset_taxa(hongos_suelo,!is.na(Order)),"Order",top=10),"compositional")
suelo_NA_orden<-plot_composition(orden_suelo_NA)+
  theme_pubclean()+
  theme(legend.position = "right")+
  scale_fill_manual(name="Order",values=met.brewer("Klimt",11))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=18))+
  theme(legend.text = element_text(family = "Rubik",size=14))+
  theme(legend.key.size = unit(0.5,"cm"))
suelo_NA_orden
ggsave("suelo_na_orden.png",last_plot())

hongos_rizosfera@tax_table[hongos_rizosfera@tax_table=="Rozellomycota_ord_Incertae_sedis"]<-NA
orden_rizo_NA<-microbiome::transform(aggregate_top_taxa2(subset_taxa(hongos_rizosfera,!is.na(Order)),"Order",top=10),"compositional")
rizo_NA_orden<-plot_composition(orden_rizo_NA)+
  theme_pubclean()+
  theme(legend.position = "right")+
  scale_fill_manual(name="Order",values=met.brewer("Klimt",11))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=18))+
  theme(legend.text = element_text(family = "Rubik",size=14))+
  theme(legend.key.size = unit(0.5,"cm"))
rizo_NA_orden
ggsave("rizo_na_orden.png",last_plot())

total_NA<-microbiome::transform(aggregate_top_taxa2(subset_taxa(hongos_rare,!is.na(Order)),"Order",top=10),"compositional")
NA_barras<-plot_composition(total_NA,group_by = "Tipo_muestra")+
  theme_pubclean()+
  theme(legend.position = "right")+
  scale_fill_manual(name="Orden",values=moma.colors("Warhol",11))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=12,angle = 90))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=18))+
  theme(legend.text = element_text(family = "Rubik",size=14))+
  theme(legend.key.size = unit(0.5,"cm"))+
  ylab("Abundancia")+
  xlab("Tipo de muestra")+
  labs(caption = "Figura 2. Abundancia relativa de los diferentes órdenes presentes en las muestras")+
  theme(plot.caption = element_text(family = "Rubik",size = 16,hjust = 0))

ggsave("barras_todo.png",last_plot())
NA_barras

