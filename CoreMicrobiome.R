# Core microbiome

hongos_meco<-phyloseq2meco(hongos_rare)
venn_hongos_datos<-hongos_meco$merge_samples(use_group = "Altitud")
venn_hongos<- trans_venn$new(venn_hongos_datos, ratio = NULL)
venn_hongos$plot_venn(color_circle = moma.colors("Warhol",5),linesize = 2)

compartido<-venn_hongos$data_details$`2210&1978&2178&2007&2018`
compartido
venn_hongos$tax_table$asv<-rownames(venn_hongos$tax_table)
identidad_core<-subset(venn_hongos$tax_table,asv%in%compartido)
View(venn_hongos$tax_table)
View(identidad_core)                       
table(identidad_core)
unique(identidad_core$Class)
ggplot(data = identidad_core,aes(x=fct_infreq(Class),fill=Class))+
  geom_bar()+
  theme_pubclean()+
  theme(legend.position = "right")+
  theme(axis.text.x = element_blank())+
  theme(axis.title.x =element_blank())+
  ylab("Número de ASVs")+
  scale_fill_manual(values = moma.colors("Warhol",15),name="Clase",labels=c("NA","Agaricomycetes","Arthoniomycetes","Dothideomycetes","Eurotiomycetes","Lecanoromycetes","Leotiomycetes","Malasseziomycetes","Microbotryomycetes","Mortierellomycetes","Orbiliomycetes","Saccharomycetes","Sordariomycetes","Taphrinomycetes","Tremellomycetes"))+
  theme(axis.title.y = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text.y = element_text(family = "Rubik",size=12,))+
  theme(legend.title = element_text(family = "Rubik",face="bold",size=18))+
  theme(legend.text = element_text(family = "Rubik",size=12))+
  theme(legend.key.size =unit(0.25,"cm") )
ggsave("taxa_core.png",last_plot())
