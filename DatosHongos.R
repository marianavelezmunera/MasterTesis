# Data hongos

# Tabla de ASVs
# Tabla de taxonomía
# Árbol

# Metadatos 

metadatos_hongos <- read_delim("~/Cosas de la maestría/TESIS/Datos/metadatos_hongos.txt",delim = "\t", escape_double = FALSE,col_types = cols(Transecto = col_skip(),Individuo = col_skip(), Distribucion_Cm = col_skip(),Distribucion_grados = col_skip(),Distribucion_ind = col_skip(), Fecha = col_skip(),HOBO_correspondiente = col_skip()),trim_ws = TRUE)

metadatos_hongos<-metadatos_hongos[-c(31,36,37),]
