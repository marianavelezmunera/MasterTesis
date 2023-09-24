## CCA
# CCA total

order.sum = tapply(taxa_sums(hongos_rare), tax_table(hongos_rare)[, "Order"], sum, na.rm=TRUE)
top10order = names(sort(order.sum, TRUE))[1:10]
hongos_cca = prune_taxa((tax_table(hongos_rare)[, "Order"] %in% top10order), hongos_rare)

cca_total<-cca(t(hongos_cca@otu_table)~T_prom_jul+Luz_prom_jul+HR_prom_jul+pH+MO+CE+N+Ctotal+Ptotal+P+K+Ca+Mg+Na+CIC+NO3+NH4+Fe,data=metadatos_hongos[c(1:34),])

hongos_cca

cca_total_data<-as.data.frame(cca_total$CCA$biplot)
cca_total_data2<-as.data.frame(cca_total$CCA$v)
cca_total_data$env<-rownames(cca_total_data)

tax_cca<-as.data.frame(tax_table(hongos_cca))
cca_order<-tax_cca$Order
cca_total_data2$Order<-cca_order
unique(cca_total_data2$Order)

#plot

ggplot()+
  geom_point(data = cca_total_data2,aes(x=CCA1,y=CCA2,colour=Order),size=0.5)+
  geom_segment(data = cca_total_data,aes(x = 0,y = 0, xend = CCA1*5,yend = CCA2*5),arrow = arrow(length = unit(0.25, "cm")),linewidth=0.25)+
  annotate("text", x = (cca_total_data$CCA1*6), y = (cca_total_data$CCA2*6), label = c("Temperature (C°)","Light intensity (lux)","Relative humidity","pH","Organic matter","CE","N","Ctotal","Ptotal","P","K","Ca","Mg","Na","CIC","NO3","NH4","Fe"),family="Rubik",size=7)+
  theme_pubr()+
  theme(legend.position = "right")+
  theme(axis.title = element_text(family="Rubik",face="bold",size=24))+
  theme(axis.text = element_text(family="Rubik",size = 16))+
  theme(legend.title = element_text(family = "Rubik",size = 20,face="bold"))+
  theme(legend.text = element_text(family = "Rubik",size = 16))+
  theme(legend.key.size = unit(0.25,"cm"))+
  scale_colour_manual(values = met.brewer("Klimt",10))
ggsave("CCA_total.png",last_plot())

## CCA filo

order.sum.filo = tapply(taxa_sums(hongos_filosfera), tax_table(hongos_filosfera)[, "Order"], sum, na.rm=TRUE)
top10order.filo = names(sort(order.sum.filo, TRUE))[1:10]
hongos_cca_filo = prune_taxa((tax_table(hongos_filosfera)[, "Order"] %in% top10order.filo), hongos_filosfera)

cca_filo<-cca(t(hongos_cca_filo@otu_table)~T_prom_jul+Luz_prom_jul+HR_prom_jul+pH+MO+CE+N+Ctotal+Ptotal+P+K+Ca+Mg+Na+CIC+NO3+NH4+Fe,data=subset(metadatos_hongos,Tipo_muestra=="Filosfera"))
cca_filo$CCA$biplot

cca_filo_data<-as.data.frame(cca_filo$CCA$biplot)
cca_filo_data$env<-row.names(cca_filo_data)

cca_filo_data2<-as.data.frame(cca_filo$CCA$v)

taxa_filo<-as.data.frame(tax_table(hongos_cca_filo))
taxa_filo$nombre<-row.names(taxa_filo)

taxa_filo2<-subset(taxa_filo,nombre %in% rownames(cca_filo_data2))
cca_filo_data2$Order<-taxa_filo2$Order

ggplot()+
  geom_point(data = cca_filo_data2,aes(x=CCA1,y=CCA2,colour=Order),size=0.5)+
  geom_segment(data = cca_filo_data,aes(x = 0,y = 0, xend = CCA1*3,yend = CCA2*3),arrow = arrow(length = unit(0.25, "cm")),linewidth=0.25)+
  annotate("text", x = (cca_filo_data$CCA1*3.5), y = (cca_filo_data$CCA2*3.5), label = c("Temperature (C°)","Light intensity (lux)","Relative humidity","pH","Organic matter","CE","N","Ctotal","Ptotal","P","K","Ca","Mg","Na"),family="Rubik")+
  theme_pubr()+
  theme(legend.position = "right")+
  theme(axis.title = element_text(family="Rubik",face="bold",size=24))+
  theme(axis.text = element_text(family="Rubik",size = 16))+
  theme(legend.title = element_text(family = "Rubik",size = 20,face="bold"))+
  theme(legend.text = element_text(family = "Rubik",size = 16))+
  theme(legend.key.size = unit(0.25,"cm"))+
  scale_colour_manual(values = met.brewer("Klimt",10))
ggsave("CCA_filo.png",last_plot())


## CCA rizo

order.sum.rizo = tapply(taxa_sums(hongos_rizosfera), tax_table(hongos_rizosfera)[, "Order"], sum, na.rm=TRUE)
top10order.rizo = names(sort(order.sum.rizo, TRUE))[1:10]
hongos_cca_rizo = prune_taxa((tax_table(hongos_rizosfera)[, "Order"] %in% top10order.rizo), hongos_rizosfera)

cca_rizo<-cca(t(hongos_cca_rizo@otu_table)~T_prom_jul+Luz_prom_jul+HR_prom_jul+pH+MO+CE+N+Ctotal+Ptotal+P+K+Ca+Mg+Na+CIC+NO3+NH4,data=subset(metadatos_hongos,Tipo_muestra=="Rizosfera"))

cca_rizo_data<-as.data.frame(cca_rizo$CCA$biplot)
cca_rizo_data$env<-row.names(cca_rizo_data)

cca_rizo_data2<-as.data.frame(cca_rizo$CCA$v)

taxa_rizo<-as.data.frame(tax_table(hongos_cca_rizo))
taxa_rizo$nombre<-row.names(taxa_rizo)

taxa_rizo2<-subset(taxa_rizo,nombre %in% rownames(cca_rizo_data2))
cca_rizo_data2$Order<-taxa_rizo2$Order

ggplot()+
  geom_point(data = cca_rizo_data2,aes(x=CCA1,y=CCA2,colour=Order),size=0.5)+
  geom_segment(data = cca_rizo_data,aes(x = 0,y = 0, xend = CCA1*3,yend = CCA2*3),arrow = arrow(length = unit(0.25, "cm")),linewidth=0.25)+
  annotate("text", x = (cca_rizo_data$CCA1*3.5), y = (cca_rizo_data$CCA2*3.5), label = c("Temperature (C°)","Light intensity (lux)","Relative humidity","pH","Organic matter","CE","N","Ctotal","Ptotal","P","K","Ca","Mg"),family="Rubik")+
  theme_pubr()+
  theme(legend.position = "right")+
  theme(axis.title = element_text(family="Rubik",face="bold",size=24))+
  theme(axis.text = element_text(family="Rubik",size = 16))+
  theme(legend.title = element_text(family = "Rubik",size = 20,face="bold"))+
  theme(legend.text = element_text(family = "Rubik",size = 16))+
  theme(legend.key.size = unit(0.25,"cm"))+
  scale_colour_manual(values = met.brewer("Klimt",10))
ggsave("CCA_rizo.png",last_plot())

