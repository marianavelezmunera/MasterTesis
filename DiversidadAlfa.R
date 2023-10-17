# Diversidad alfa

diversidad_alfa<-alpha(hongos_rare,index = "all") 

#No tiene el Faith's PD entonces lo vamos a traer del qza

faith_pd<-read_qza("faith_pd_vector.qza")
FaithsPD<-as.data.frame(faith_pd$data)
diversidad_alfa<-cbind(diversidad_alfa,FaithsPD)

diversidad_alfa<-cbind(metadatos_hongos[1:34,1:5],diversidad_alfa)
colnames(diversidad_alfa)


# Plots
# Observed
ggplot(data = diversidad_alfa,aes(x=Tipo_muestra,y=observed,fill=Tipo_muestra))+
  geom_boxplot(color="black")+
  theme_pubclean()+
  xlab("Sample type")+ylab("Richness")+
  theme(legend.position = "none")+
  scale_fill_manual(values=met.brewer("Klimt",3))+
  scale_x_discrete(labels=c("Phyllosphere","Rhizosphere","Bulk soil"))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))
ggsave("observed_sample.png",last_plot())

ggplot(data = diversidad_alfa,aes(x=Altitud, y=observed,fill=Altitud))+
  geom_boxplot(color="black")+
  theme_pubclean()+
  xlab("Elevation")+ylab("Richness")+
  theme(legend.position = "none")+
  scale_fill_manual(values=met.brewer("Klimt",5,override.order = FALSE))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))
ggsave("observed_elevation.png",last_plot())

#Shannon

ggplot(data = diversidad_alfa,aes(x=Tipo_muestra,y=diversity_shannon,fill=Tipo_muestra))+
  geom_boxplot(color="black")+
  theme_pubclean()+
  xlab("Sample type")+ylab("Shannon index")+
  theme(legend.position = "none")+
  scale_fill_manual(values=met.brewer("Klimt",3))+
  scale_x_discrete(labels=c("Phyllosphere","Rhizosphere","Bulk soil"))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))
ggsave("shannon_sample.png",last_plot())
  
ggplot(data = diversidad_alfa,aes(x=Altitud, y=diversity_shannon,fill=Altitud))+
  geom_boxplot(color="black")+
  theme_pubclean()+
  xlab("Elevation")+ylab("Shannon Index") +
  theme(legend.position = "none")+
  scale_fill_manual(values=met.brewer("Klimt",5))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))
ggsave("shannon_elevation.png",last_plot())

# Faiths PD

ggplot(data = diversidad_alfa,aes(x=Tipo_muestra,y=faith_pd,fill=Tipo_muestra))+
  geom_boxplot(color="black")+
  theme_pubclean()+
  xlab("Sample type")+ylab("Faith's PD")+
  theme(legend.position = "none")+
  scale_fill_manual(values=met.brewer("Klimt",3))+
  scale_x_discrete(labels=c("Phyllosphere","Rhizosphere","Bulk soil"))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))
ggsave("faith_sample.png",last_plot())

ggplot(data = diversidad_alfa,aes(x=Altitud, y=faith_pd,fill=Altitud))+
  geom_boxplot(color="black")+
  theme_pubclean()+
  xlab("Elevation")+ylab("Faith's PD") +
  theme(legend.position = "none")+
  scale_fill_manual(values=met.brewer("Klimt",5))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))
ggsave("faith_elevation.png",last_plot())

## Análisis estadísticos

#Parcela vs observed

hist(diversidad_alfa$observed)
shapiro.test(diversidad_alfa$observed)
parcela_obs<-aov(data=diversidad_alfa,observed~Altitud)
summary(parcela_obs)
TukeyHSD(parcela_obs)
leveneTest(diversidad_alfa$observed,diversidad_alfa$Parcela)

# Parcela vs faith

hist(diversidad_alfa$faith_pd)
shapiro.test(diversidad_alfa$faith_pd)
parcela_faith<-aov(data=diversidad_alfa,faith_pd~Altitud)
summary(parcela_faith)
TukeyHSD(parcela_faith)
leveneTest(diversidad_alfa$faith_pd,diversidad_alfa$Parcela)

# Parcela vs shannon

hist(diversidad_alfa$diversity_shannon)
shapiro.test(diversidad_alfa$diversity_shannon)
parcela_shannon<-aov(data=diversidad_alfa,diversity_shannon~Altitud)
summary(parcela_shannon)
TukeyHSD(parcela_shannon)
leveneTest(diversidad_alfa$diversity_shannon,diversidad_alfa$Parcela)

# Tipo de muestra vs observed

muestra_obs<-aov(data=diversidad_alfa,observed~Tipo_muestra)
summary(muestra_obs)
TukeyHSD(muestra_obs)
leveneTest(diversidad_alfa$observed,diversidad_alfa$Tipo_muestra)
# Tipo de muestra vs faith

muestra_faith<-aov(data=diversidad_alfa,faith_pd~Tipo_muestra)
summary(muestra_faith)
TukeyHSD(muestra_faith)
leveneTest(diversidad_alfa$faith_pd,diversidad_alfa$Tipo_muestra)

# Tipo de muestra vs shannon

muestra_shannon<-aov(data=diversidad_alfa,diversity_shannon~Tipo_muestra)
summary(muestra_shannon)
TukeyHSD(muestra_shannon)
leveneTest(diversidad_alfa$diversity_shannon,diversidad_alfa$Tipo_muestra)

modelos<-c("Richness ~ Elevation","Shannon ~ Elevation","Faith's PD ~ Elevation","Richness ~ Sample type","Shannon ~ Sample type","Faith's PD ~ Sample type")
valor_F<-c(2.23,1.603,1.176,1.5,4.611,0.671)
valor_P<-c(0.0902,0.2,0.342,0.239,0.0177,0.518)

tabla_anova<-data.frame(modelos,valor_F,valor_P)
colnames(tabla_anova)<-c("Model","F","p-value")

tabla_anova<-tabla_anova %>%
  gt()%>%
  gtExtras::gt_theme_pff()
gtsave(tabla_anova,"tabla_anova.png")
tabla_anova

ggplot(data = diversidad_alfa,aes(x=Tipo_muestra,y=faith_pd,fill=Parcela))+
  geom_boxplot(color="black")+
  theme_pubclean()+
  xlab("Sample type")+ylab("Faith's PD")+
  theme(legend.position = "none")+
  scale_fill_manual(values=met.brewer("Klimt",5))+
  scale_x_discrete(labels=c("Phyllosphere","Rhizosphere","Bulk soil"))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=24))+
  theme(axis.text = element_text(family = "Rubik",size=16))
ggsave("faith_sample.png",last_plot())

unique(diversidad_alfa$Parcela)

ggplot(data = diversidad_alfa,aes(x=Altitud,y=diversity_shannon,fill=Tipo_muestra))+
  geom_boxplot(color="black")+
  theme_pubclean()+
  xlab("Elevación")+ylab("Índice de Shannon")+
  theme(legend.position = "right")+
  scale_fill_manual(name="Tipo de muestra", label=c("Filósfera","Rizósfera","Suelo"),values=moma.colors("Warhol",3))+
  theme(axis.title = element_text(family = "Rubik",face = "bold",size=32))+
  theme(axis.text = element_text(family = "Rubik",size=24))+
  theme(legend.title = element_text(family = "Rubik",face = "bold",size = 24))+
  theme(legend.text = element_text(family = "Rubik",size = 16))

ggsave("boxplot_todo.png",last_plot())


anova_completo<-aov(diversity_shannon~Tipo_muestra+Parcela,data = diversidad_alfa)
summary(anova_completo)

anova_datos<-data.frame(Df=c(2,4,27),Sum_sq=c(2.16,1.79,5.47),F_val=c(5.33,2.21,NA),p=c(0.001,0.09,NA))
colnames(anova_datos)<-c("Df","SumOfSqs","F","Pr(>F)")
rownames(anova_datos)<-c("Tipo de muestra","Elevación","")
anova_datos<-anova_datos[-3,]

tabla_anova_completa<-anova_datos%>%
  gt(rownames_to_stub = TRUE)%>%
  tab_options(table.font.size = px(20)) |>
  opt_table_font(
    font = list(
      google_font(name = "Rubik")))

tabla_anova_completa<-sub_missing(tabla_anova_completa,missing_text = "-")%>%
  tab_options(column_labels.font.weight = 'bold',
              stub.font.weight = "bold",
              column_labels.border.top.color = "black",
              column_labels.border.bottom.color = "black",
              column_labels.border.lr.color = "black")%>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(cell_borders(sides = "all", weight = px(2),color="black")))

tabla_anova_completa<-tabla_anova_completa%>%
  tab_style(locations = cells_stubhead(),
            style=cell_borders(sides=c("left","top"),color="transparent",weight = px(2)))%>%
  tab_style(locations = cells_stubhead(),
            style = cell_borders(sides="right",color="transparent"))%>%
  tab_style(
    style = cell_borders(
      sides = "all",
      weight = px(2),color="black"
    ),
    locations = cells_body()
  )

tabla_anova_completa<-tabla_anova_completa%>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(cell_text(color="black"),cell_fill(color=moma.colors("Warhol",10)[1])))%>%
  tab_style(style = list(cell_text(color="black"),cell_fill(color=moma.colors("Warhol",10)[1])),
            locations = cells_stub())
tabla_anova_completa

tabla_anova_completa<-tabla_anova_completa%>%
  tab_source_note(
    source_note = "Tabla 1. Resultados del ANOVA"
  )%>%
  tab_style(locations = cells_source_notes(),
            style = cell_borders("all",color="white"))%>%
  opt_table_lines(extent = "none")
tabla_anova_completa

tabla_anova_completa<-tabla_anova_completa%>%
  tab_style(style=cell_borders("all",color="black",weight = px(2)),
            locations = cells_stub())
gtsave(tabla_anova_completa,"tabla_anova_completa.png")


