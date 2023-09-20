# Taxonomia 

# Nivel: Filo
# Con los NA
barras_filo_datos<-transform(aggregate_top_taxa2(hongos_filosfera,"Phylum",top=10),"compositional")
barras_filo<-plot_composition(barras_filo_datos)
 
barras_rizo_datos<-transform(aggregate_top_taxa2(hongos_rizosfera,"Phylum",top=10),"compositional")
barras_rizo<-plot_composition(barras_rizo_datos)
barras_rizo

barras_suelo_datos<-transform(aggregate_top_taxa2(hongos_suelo,"Phylum",top=10),"compositional")
barras_suelo<-plot_composition(barras_suelo_datos)
barras_suelo

#Quitando los NA


filo_NA<-transform(aggregate_top_taxa2(subset_taxa(hongos_filosfera,!is.na(Phylum)),"Phylum",top=10),"compositional")
filo_NA_barras<-plot_composition(filo_NA)
filo_NA_barras

suelo_NA<-transform(aggregate_top_taxa2(subset_taxa(hongos_suelo,!is.na(Phylum)),"Phylum",top=10),"compositional")
suelo_NA_barras<-plot_composition(suelo_NA)
suelo_NA_barras

rizo_NA<-transform(aggregate_top_taxa2(subset_taxa(hongos_rizosfera,!is.na(Phylum)),"Phylum",top=10),"compositional")
rizo_NA_barras<-plot_composition(rizo_NA)
rizo_NA_barras

#Nivel: Orden

orden_filo_datos<-transform(aggregate_top_taxa2(hongos_filosfera,"Order",top=10),"compositional")
orden_filo<-plot_composition(orden_filo_datos)
orden_filo
orden_rizo_datos<-transform(aggregate_top_taxa2(hongos_rizosfera,"Order",top=10),"compositional")
orden_rizo<-plot_composition(orden_rizo_datos)
orden_rizo

orden_suelo_datos<-transform(aggregate_top_taxa2(hongos_suelo,"Order",top=10),"compositional")
orden_suelo<-plot_composition(orden_suelo_datos)
orden_suelo

#Quitando los NA


orden_filo_NA<-transform(aggregate_top_taxa2(subset_taxa(hongos_filosfera,!is.na(Order)),"Order",top=10),"compositional")
filo_NA_orden<-plot_composition(orden_filo_NA)
filo_NA_orden

hongos_suelo@tax_table[hongos_suelo@tax_table=="Rozellomycota_ord_Incertae_sedis"]<-NA
orden_suelo_NA<-transform(aggregate_top_taxa2(subset_taxa(hongos_suelo,!is.na(Order)),"Order",top=10),"compositional")
suelo_NA_orden<-plot_composition(orden_suelo_NA)
suelo_NA_orden

hongos_rizosfera@tax_table[hongos_rizosfera@tax_table=="Rozellomycota_ord_Incertae_sedis"]<-NA
orden_rizo_NA<-transform(aggregate_top_taxa2(subset_taxa(hongos_rizosfera,!is.na(Order)),"Order",top=10),"compositional")
rizo_NA_orden<-plot_composition(orden_rizo_NA)
rizo_NA_orden
