#ANCOMs
ANCOM_filo<-ancombc2(hongos_rare,assay_name="counts",tax_level="Phylum",fix_formula = "Altitud",rand_formula = "Tipo_muestra",pseudo = 1,group = "Altitud",alpha = 0.05)

ANCOM_order<-ancombc2(hongos_rare,assay_name="counts",tax_level="Order",fix_formula = "Altitud",rand_formula = "Tipo_muestra",pseudo = 1,group = "Altitud",alpha = 0.05)

ANCOM_family<-ancombc2(hongos_rare,assay_name="counts",tax_level="Family",fix_formula = "Altitud",rand_formula = "Tipo_muestra",pseudo = 1,group = "Altitud",alpha = 0.01)

View(ANCOM_filo$res)
View(ANCOM_filo$beta_data)
View(ANCOM_filo$p_data)
View(ANCOM_family$res)
View(ANCOM_order$res)
