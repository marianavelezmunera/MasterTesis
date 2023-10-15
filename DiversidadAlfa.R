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


