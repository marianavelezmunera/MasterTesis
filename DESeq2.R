#DESeq2
View(hongos_rare@sam_data)
hongos_DESeq<-phyloseq_to_deseq2(hongos_rare, ~ Altitud+Tipo_muestra)

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

geoMeans = apply(counts(hongos_DESeq), 1, gm_mean)
diagdds = estimateSizeFactors(hongos_DESeq, geoMeans = geoMeans)
diagdds = DESeq(diagdds, fitType="local")

resultados_DESeq<-results(diagdds,cooksCutoff = FALSE)
sig_table<-resultados_DESeq[which(resultados_DESeq$padj<0.05),]
sig_table<-cbind(as(sig_table,"data.frame"),as(tax_table(hongos_rare)[rownames(sig_table), ], "matrix"))
hongos_rare
#Plot

x= tapply(sig_table$log2FoldChange, sig_table$Phylum, function(x) max(x))
x = sort(x, TRUE)
sig_table$Phylum = factor(as.character(sig_table$Phylum), levels=names(x))
# Genus order
x = tapply(sig_table$log2FoldChange, sig_table$Family, function(x) max(x))
x = sort(x, TRUE)
sig_table$Family = factor(as.character(sig_table$Family), levels=names(x))

ggplot(sig_table, aes(x=Family, y=log2FoldChange, color=Phylum)) + geom_point(size=6) + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust=0.5))
