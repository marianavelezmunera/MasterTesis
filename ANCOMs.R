#ANCOMs
ANCOM_filo<-ancom(hongos_rare,assay_name="counts",tax_level="Phylum",main_var = "Altitud",adj_formula = "Tipo_muestra")

ANCOM_order<-ancom(hongos_rare,assay_name="counts",tax_level="Order",main_var = "Altitud",adj_formula = "Tipo_muestra")

ANCOM_family<-ancom(hongos_rare,assay_name="counts",tax_level="Family",main_var = "Altitud",adj_formula = "Tipo_muestra")

View(ANCOM_filo$res)
View(ANCOM_filo$beta_data)
View(ANCOM_filo$p_data)
View(ANCOM_family$res)
View(ANCOM_order$res)
