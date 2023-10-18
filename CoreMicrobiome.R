# Core microbiome

hongos_meco<-phyloseq2meco(hongos_rare)
venn_hongos_datos<-hongos_meco$merge_samples(use_group = "Altitud")
venn_hongos<- trans_venn$new(venn_hongos_datos, ratio = NULL)
venn_hongos$plot_venn()


