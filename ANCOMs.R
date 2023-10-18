#ANCOMs
ANCOM_filo<-ancombc2(hongos_rare,assay_name="counts",tax_level="Phylum",fix_formula = "Altitud",rand_formula = "Tipo_muestra",pseudo = 1,group = "Altitud",alpha = 0.05)

ANCOM_order<-ancombc2(hongos_rare,assay_name="counts",tax_level="Order",fix_formula = "Altitud",rand_formula = "Tipo_muestra",pseudo = 1,group = "Altitud",alpha = 0.05)

ANCOM_family<-ancombc2(hongos_rare,assay_name="counts",tax_level="Family",fix_formula = "Altitud",rand_formula = "Tipo_muestra",pseudo = 1,group = "Altitud",alpha = 0.01)

ANCOM_genero<-ancombc2(hongos_rare,assay_name="counts",tax_level="Genus",fix_formula = "Altitud",rand_formula = "Tipo_muestra",pseudo = 1,group = "Altitud",alpha = 0.05)

View(ANCOM_filo_family$res)
View(ANCOM_filo$beta_data)
View(ANCOM_filo$p_data)
View(ANCOM_family$res)
View(ANCOM_order$res)
View(ANCOM_genero$res)

# ANCOMs filosfera

ANCOM_filo_total<-ancombc2(hongos_filosfera,assay_name="counts",tax_level="Phylum",pseudo = 1,fix_formula="Altitud",group = "Altitud",alpha = 0.05)
ANCOM_filo_order<-ancombc2(hongos_filosfera,assay_name="counts",tax_level="Order",pseudo = 1,fix_formula="Altitud",group = "Altitud",alpha = 0.05)
ANCOM_filo_family<-ancombc2(hongos_filosfera,assay_name="counts",tax_level="Family",pseudo = 1,fix_formula="Altitud",group = "Altitud",alpha = 0.05)

# Plot
ANCOM_subset_filo<-ANCOM_filo_family$res
ANCOM_subset_filo<-subset(ANCOM_subset_filo,diff_Altitud2007!="FALSE"| diff_Altitud2018!="FALSE"|diff_Altitud2178!="FALSE"|diff_Altitud2210!="FALSE"|`diff_(Intercept)`!="FALSE")

ANCOM_subset_filo<-ANCOM_subset_filo[,1:6]
ANCOM_subset_filo<-gather(ANCOM_subset_filo,`lfc_(Intercept)`,lfc_Altitud2007,lfc_Altitud2018,lfc_Altitud2178,lfc_Altitud2210,key="Elevation",value="LFC")

ggplot(data=ANCOM_subset_filo,aes(x=taxon,y=LFC,fill=taxon))+
  geom_col()+
  facet_wrap(~Elevation,labeller = labeller(Elevation=c(`lfc_(Intercept)`="1978","lfc_Altitud2007"="2007","lfc_Altitud2018" = "2018","lfc_Altitud2178"="2178","lfc_Altitud2210"="2210")
))+
  theme_pubclean()+
  theme(legend.position = "none")+
  scale_x_discrete(label=c("Cerrenaceae","Cryptococcaceae","Microsporomyceteaceae","Ploettnerulaceae","Mycosphaerellales"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust=0.5,family = "Rubik",face = "bold",size=12))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=20,color = "black"))+
  scale_fill_manual(values=moma.colors("Warhol",5))+
  xlab("Taxon")+
  theme(strip.text = element_text(face = "bold",family = "Rubik",size = 20))
ggsave("ANCOM_filo.png",last_plot())

# ANCOMs rizosfera

ANCOM_rizo_total<-ancombc2(hongos_rizosfera,assay_name="counts",tax_level="Phylum",pseudo = 1,fix_formula="Altitud",group = "Altitud",alpha = 0.05)
ANCOM_rizo_order<-ancombc2(hongos_rizosfera,assay_name="counts",tax_level="Order",pseudo = 1,fix_formula="Altitud",group = "Altitud",alpha = 0.05)
ANCOM_rizo_family<-ancombc2(hongos_rizosfera,assay_name="counts",tax_level="Family",pseudo = 1,fix_formula="Altitud",group = "Altitud",alpha = 0.05)

# Plot

ANCOM_subset_rizo<-ANCOM_rizo_family$res
ANCOM_subset_rizo<-subset(ANCOM_subset_rizo,diff_Altitud2007!="FALSE"| diff_Altitud2018!="FALSE"|diff_Altitud2178!="FALSE"|diff_Altitud2210!="FALSE"|`diff_(Intercept)`!="FALSE")

ANCOM_subset_rizo<-ANCOM_subset_rizo[,1:6]
ANCOM_subset_rizo<-gather(ANCOM_subset_rizo,`lfc_(Intercept)`,lfc_Altitud2007,lfc_Altitud2018,lfc_Altitud2178,lfc_Altitud2210,key="Elevation",value="LFC")
ANCOM_subset_rizo<-subset(ANCOM_subset_rizo,
                          taxon!="Family:Glomeromycota_fam_Incertae_sedis"&taxon!="Family:Sordariomycetes_fam_Incertae_sedis"&taxon!="Family:Leotiomycetes_fam_Incertae_sedis"&taxon!="Family:Myrmecridiales_fam_Incertae_sedis"&taxon!="Family:Atractosporales_fam_Incertae_sedis")


ggplot(data=ANCOM_subset_rizo,aes(x=taxon,y=LFC,fill=taxon))+
  geom_col()+
  facet_wrap(~Elevation,labeller = labeller(Elevation=c(`lfc_(Intercept)`="1978","lfc_Altitud2007"="2007","lfc_Altitud2018" = "2018","lfc_Altitud2178"="2178","lfc_Altitud2210"="2210")
  ))+
  theme_pubclean()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust=0.5))+
  scale_fill_manual(values=moma.colors("Warhol",14))+
  scale_x_discrete(label=c("Chaetomiaceae","Melanconidaceae","Mortierellaceae","Orbiliaceae","Physalacriaceae","Piskurozymaceae
","Pyronemataceae","Ramicandelaberaceae","Auriculariales"))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=20,color = "black"))+
  xlab("Taxon")+
  theme(strip.text = element_text(face = "bold",family = "Rubik",size = 20))
ggsave("ANCOM_rizo.png",last_plot())

ancom_genero<-ancombc2(hongos_rare,assay_name="counts",tax_level="Genus",fix_formula = "Altitud",rand_formula = "Tipo_muestra",pseudo = 1,group = "Altitud",alpha = 0.05)
