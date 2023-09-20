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
ggplot(data = diversidad_alfa,aes(x=Tipo_muestra,y=observed))+
  geom_boxplot()

ggplot(data = diversidad_alfa,aes(x=Parcela, y=observed))+
  geom_boxplot()

ggplot(data = diversidad_alfa,aes(x=Altitud, y=observed))+
  geom_boxplot()

#Shannon
ggplot(data = diversidad_alfa,aes(x=Tipo_muestra,y=diversity_shannon))+
  geom_boxplot()

ggplot(data = diversidad_alfa,aes(x=Parcela, y=diversity_shannon))+
  geom_boxplot()

ggplot(data = diversidad_alfa,aes(x=Altitud, y=diversity_shannon))+
  geom_boxplot()

# Faiths PD

ggplot(data = diversidad_alfa,aes(x=Tipo_muestra,y=faith_pd))+
  geom_boxplot()

ggplot(data = diversidad_alfa,aes(x=Parcela, y=faith_pd))+
  geom_boxplot()

ggplot(data = diversidad_alfa,aes(x=Altitud, y=faith_pd))+
  geom_boxplot()

## Análisis estadísticos

#Parcela vs observed
hist(diversidad_alfa$observed)
shapiro.test(diversidad_alfa$observed)
parcela_obs<-aov(data=diversidad_alfa,observed~Parcela)
summary(parcela_obs)

# Parcela vs faith

hist(diversidad_alfa$faith_pd)
shapiro.test(diversidad_alfa$faith_pd)
parcela_faith<-aov(data=diversidad_alfa,faith_pd~Parcela)
summary(parcela_faith)

# Parcela vs shannon

hist(diversidad_alfa$diversity_shannon)
shapiro.test(diversidad_alfa$diversity_shannon)
parcela_shannon<-aov(data=diversidad_alfa,diversity_shannon~Parcela)
summary(parcela_shannon)

# Tipo de muestra vs observed

muestra_obs<-aov(data=diversidad_alfa,observed~Tipo_muestra)
summary(muestra_obs)

# Tipo de muestra vs faith

muestra_faith<-aov(data=diversidad_alfa,faith_pd~Tipo_muestra)
summary(muestra_faith)


# Tipo de muestra vs shannon
muestra_shannon<-aov(data=diversidad_alfa,diversity_shannon~Tipo_muestra)
summary(muestra_shannon)



