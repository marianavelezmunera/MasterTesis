#Predict NA
m1<-lm(MO~Parcela+Tipo_muestra,data = metadatos_hongos)
predict(m1)
metadatos_hongos[3,25]<-10

metadatos_hongos[22,25]<-9
