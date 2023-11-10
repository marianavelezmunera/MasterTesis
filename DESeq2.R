#DESeq2

# Todo
hongos_DESeq<-phyloseq_to_deseq2(hongos_rare, ~ Altitud+Tipo_muestra)

diagdds= DESeq(hongos_DESeq, fitType="local",sfType = "poscounts")

resultados_DESeq<-results(diagdds,cooksCutoff = FALSE)
sig_table<-resultados_DESeq[which(resultados_DESeq$padj<0.05),]
sig_table<-cbind(as(sig_table,"data.frame"),as(tax_table(hongos_rare)[rownames(sig_table), ], "matrix"))

#Filosfera

filo_DESeq<-phyloseq_to_deseq2(hongos_filosfera, ~ Altitud)

diagdds_filo = DESeq(filo_DESeq, fitType="local",sfType = "poscounts")

resultados_DESeq_filo<-results(diagdds_filo,cooksCutoff = FALSE)
sig_table_filo<-resultados_DESeq_filo[which(resultados_DESeq_filo$padj<0.05),]
sig_table_filo<-cbind(as(sig_table_filo,"data.frame"),as(tax_table(hongos_filosfera)[rownames(sig_table_filo), ], "matrix"))

sig_table_filo<-subset(sig_table_filo,!is.na(Family))

sig_table_filo[sig_table_filo=="Capnodiales_fam_Incertae_sedis"]<-NA
sig_table_filo[sig_table_filo=="Orbiliales_fam_Incertae_sedis"]<-NA
sig_table_filo[sig_table_filo=="Auriculariales_fam_Incertae_sedis"]<-NA


x_filo= tapply(sig_table_filo$log2FoldChange, sig_table_filo$Order, function(x) max(x))
x_filo = sort(x_filo, TRUE)
sig_table_filo$Order = factor(as.character(sig_table_filo$Order), levels=names(x_filo))

# Family order
x_filo = tapply(sig_table_filo$log2FoldChange, sig_table_filo$Family, function(x) max(x))
x_filo = sort(x_filo, TRUE)
sig_table_filo$Family = factor(as.character(sig_table_filo$Family), levels=names(x_filo))

ggplot(sig_table_filo, aes(x=Family, y=log2FoldChange, color=Family)) + geom_point(size=2) + 
  theme_pubclean()+
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust=0.5))+
  scale_color_manual(values=moma.colors("Warhol",15))+
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

## Heatmap general

sig_table1<-sig_table[order(sig_table$padj,na.last = NA),]
sig_table1<-sig_table1[1:20,]
taxa_sig<-rownames(sig_table1)
ps.taxa.rel <- transform_sample_counts(hongos_rare, function(x) x/sum(x)*100)
ps.taxa.rel.sig <- prune_taxa(taxa_sig, ps.taxa.rel)
matrix <- as.matrix(data.frame(otu_table(ps.taxa.rel.sig)))
rownames(matrix) <- as.character(tax_table(ps.taxa.rel.sig)[, "Species"])
metadata_sub <- data.frame(sample_data(ps.taxa.rel.sig))

annotation_col = data.frame(
  Elevation = as.factor(metadata_sub$Altitud), 
  `Sample type` = as.factor(metadata_sub$Tipo_muestra), 
  check.names = FALSE
)
rownames(annotation_col) = rownames(metadata_sub)
annotation_row = data.frame(
  Phylum = as.factor(tax_table(ps.taxa.rel.sig)[, "Phylum"])
)
phylum_col = RColorBrewer::brewer.pal(length(levels(annotation_row$Phylum)), "Paired")
names(phylum_col) = levels(annotation_row$Phylum)
ann_colors = list(
  Elevation = c("1978"=colores[1],"2007"=colores[2],"2018"=colores[3],"2178"=colores[4],"2210"=colores[5]),
  `Sample type` = c(Filosfera = colores2[1], Rizosfera = colores2[2],Suelo=colores2[3])
)

colores<-moma.colors("Flash",5)
colores2<-moma.colors("Picabia",3)
pheatmap(matrix, scale= "row", 
         annotation_col = annotation_col,
         annotation_colors = ann_colors,
         color = moma.colors("Kippenberger",type="continuous"),
         cellwidth = 10,
         filename = "DESeq2_heatmap.png")



## Heatmap rizosfera

sig_table_rizo1<-sig_table_rizo[order(sig_table_rizo$padj,na.last = NA),]
sig_table_rizo1<-sig_table_rizo1[1:20,]
taxa_sig_rizo<-rownames(sig_table_rizo1)
ps.taxa.rel_rizo <- transform_sample_counts(hongos_rizosfera, function(x) x/sum(x)*100)
ps.taxa.rel.sig_rizo <- prune_taxa(taxa_sig_rizo, ps.taxa.rel_rizo)
matrix_rizo <- as.matrix(data.frame(otu_table(ps.taxa.rel.sig_rizo)))
rownames(matrix_rizo) <- as.character(tax_table(ps.taxa.rel.sig_rizo)[, "Species"])
metadata_sub_rizo <- data.frame(sample_data(ps.taxa.rel.sig_rizo))

annotation_col_rizo = data.frame(
  Elevation = as.factor(metadata_sub_rizo$Altitud), 
  check.names = FALSE
)
rownames(annotation_col_rizo) <- rownames(metadata_sub_rizo)
annotation_row_rizo = data.frame(
  Phylum = as.factor(tax_table(ps.taxa.rel.sig_rizo)[, "Phylum"])
)
phylum_col_rizo = RColorBrewer::brewer.pal(length(levels(annotation_row_rizo$Phylum)), "Paired")
names(phylum_col_rizo) = levels(annotation_row_rizo$Phylum)
ann_colors_rizo = list(
  Elevation = c("1978"=colores[1],"2007"=colores[2],"2018"=colores[3],"2178"=colores[4],"2210"=colores[5])
)

colores<-moma.colors("Flash",5)
colores2<-moma.colors("Picabia",3)
pheatmap(matrix_rizo, scale= "row", 
         annotation_col = annotation_col_rizo,
         annotation_colors = ann_colors_rizo,
         color = moma.colors("Kippenberger",type="continuous"),
         cellwidth = 10,
         filename = "DESeq2_heatmap_rizo.png")

## Heatmap filosfera

sig_table_filo1<-sig_table_filo[order(sig_table_filo$padj,na.last = NA),]
sig_table_filo1<-sig_table_filo[1:20,]
taxa_sig_filo<-rownames(sig_table_filo)
ps.taxa.rel_filo <- transform_sample_counts(hongos_filosfera, function(x) x/sum(x)*100)
ps.taxa.rel.sig_filo <- prune_taxa(taxa_sig_filo, ps.taxa.rel_filo)
matrix_filo <- as.matrix(data.frame(otu_table(ps.taxa.rel.sig_filo)))
rownames(matrix_filo) <- as.character(tax_table(ps.taxa.rel.sig_filo)[, "Species"])
metadata_sub_filo <- data.frame(sample_data(ps.taxa.rel.sig_filo))

annotation_col_filo = data.frame(
  Elevation = as.factor(metadata_sub_filo$Altitud), 
  check.names = FALSE
)
rownames(annotation_col_filo) <- rownames(metadata_sub_filo)
annotation_row_filo = data.frame(
  Phylum = as.factor(tax_table(ps.taxa.rel.sig_filo)[, "Phylum"])
)
phylum_col_filo = RColorBrewer::brewer.pal(length(levels(annotation_row_filo$Phylum)), "Paired")
names(phylum_col_filo) = levels(annotation_row_filo$Phylum)
ann_colors_filo = list(
  Elevation = c("1978"=colores[1],"2007"=colores[2],"2018"=colores[3],"2178"=colores[4],"2210"=colores[5])
)

colores<-moma.colors("Flash",5)
colores2<-moma.colors("Picabia",3)
pheatmap(matrix_filo, scale= "row", 
         annotation_col = annotation_col_filo,
         annotation_colors = ann_colors_filo,
         color = moma.colors("Kippenberger",type="continuous"),
         cellwidth = 10,
         filename = "DESeq2_heatmap_filo.png")
