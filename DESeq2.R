#DESeq2

# Todo
hongos_DESeq<-phyloseq_to_deseq2(hongos_rare, ~ Altitud+Tipo_muestra)

diagdds= DESeq(hongos_DESeq, fitType="local",sfType = "poscounts")

resultados_DESeq<-results(diagdds,cooksCutoff = FALSE)
sig_table<-resultados_DESeq[which(resultados_DESeq$padj<0.05),]
sig_table<-cbind(as(sig_table,"data.frame"),as(tax_table(hongos_rare)[rownames(sig_table), ], "matrix"))

#Filosfera

filo_DESeq<-phyloseq_to_deseq2(hongos_filosfera, ~ Altitud)

diagdds_filo = DESeq(diagdds_filo, fitType="local",sfType = "poscounts")

resultados_DESeq_filo<-results(diagdds_filo,cooksCutoff = FALSE)
sig_table_filo<-resultados_DESeq_filo[which(resultados_DESeq_filo$padj<0.05),]
sig_table_filo<-cbind(as(sig_table_filo,"data.frame"),as(tax_table(hongos_filosfera)[rownames(sig_table_filo), ], "matrix"))

sig_table_filo<-subset(sig_table_filo,!is.na(Family))

sig_table_filo[sig_table_filo=="Capnodiales_fam_Incertae_sedis"]<-NA
sig_table_filo[sig_table_filo=="Orbiliales_fam_Incertae_sedis"]<-NA
sig_table_filo[sig_table_filo=="Auriculariales_fam_Incertae_sedis"]<-NA


x= tapply(sig_table_filo$log2FoldChange, sig_table_filo$Order, function(x) max(x))
x = sort(x, TRUE)
sig_table_filo$Order = factor(as.character(sig_table_filo$Order), levels=names(x))
# Family order
x = tapply(sig_table_filo$log2FoldChange, sig_table_filo$Family, function(x) max(x))
x = sort(x, TRUE)
sig_table_filo$Family = factor(as.character(sig_table_filo$Family), levels=names(x))

ggplot(sig_table_filo, aes(x=Family, y=log2FoldChange, color=Class)) + geom_point(size=2) + 
  theme_pubclean()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust=0.5))+
  scale_color_manual(values=moma.colors("Warhol",11))+
  theme(legend.position = "right")+
  ylab("LFC")+
  theme(axis.title = element_text(family="Rubik",face="bold",size=20))+
  theme(axis.text = element_text(family="Rubik",size = 12))+
  theme(legend.text = element_text(family="Rubik"))+
  theme(legend.title = element_text(family="Rubik",face="bold",size=16))+
  theme(legend.key.size = unit(0.5,"cm"))
  
ggsave("DESeq_filo.png",last_plot())

#Solo rizosfera

rizo_DESeq<-phyloseq_to_deseq2(hongos_rizosfera, ~ Altitud)

diagdds_rizo = DESeq(diagdds_rizo, fitType="local",sfType = "poscounts")

resultados_DESeq_rizo<-results(diagdds_rizo,cooksCutoff = FALSE)
sig_table_rizo<-resultados_DESeq_rizo[which(resultados_DESeq_rizo$padj<0.05),]
sig_table_rizo<-cbind(as(sig_table_rizo,"data.frame"),as(tax_table(hongos_rizosfera)[rownames(sig_table_rizo), ], "matrix"))

sig_table_rizo<-subset(sig_table_rizo,!is.na(Family))
unique(sig_table_rizo$Family)
sig_table_rizo[sig_table_rizo=="Agaricomycetes_fam_Incertae_sedis"]<-NA
sig_table_rizo[sig_table_rizo=="Rozellomycota_fam_Incertae_sedis"]<-NA
sig_table_rizo[sig_table_rizo=="Archaeorhizomycetales_fam_Incertae_sedis"]<-NA
sig_table_rizo[sig_table_rizo=="Acrospermales_fam_Incertae_sedis"]<-NA
sig_table_rizo[sig_table_rizo=="Venturiales_fam_Incertae_sedis"]<-NA
sig_table_rizo[sig_table_rizo=="Capnodiales_fam_Incertae_sedis"]<-NA
sig_table_rizo[sig_table_rizo=="Leotiomycetes_fam_Incertae_sedis"]<-NA
sig_table_rizo[sig_table_rizo=="Jobellisiales_fam_Incertae_sedis"]<-NA



x= tapply(sig_table_rizo$log2FoldChange, sig_table_rizo$Order, function(x) max(x))
x = sort(x, TRUE)
sig_table_rizo$Order = factor(as.character(sig_table_rizo$Order), levels=names(x))
# Family order
x = tapply(sig_table_rizo$log2FoldChange, sig_table_rizo$Family, function(x) max(x))
x = sort(x, TRUE)
sig_table_rizo$Family = factor(as.character(sig_table_rizo$Family), levels=names(x))

ggplot(sig_table_rizo, aes(x=Family, y=log2FoldChange, color=Class)) + geom_point(size=2) + 
  theme_pubclean()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust=0.5))+
  scale_color_manual(values=moma.colors("Warhol",10))+
  theme(legend.position = "right")+
  ylab("LFC")+
  theme(axis.title = element_text(family="Rubik",face="bold",size=20))+
  theme(axis.text = element_text(family="Rubik",size = 12))+
  theme(legend.text = element_text(family="Rubik"))+
  theme(legend.title = element_text(family="Rubik",face="bold",size=16))+
  theme(legend.key.size = unit(0.5,"cm"))

ggsave("DESeq_rizo.png",last_plot())


