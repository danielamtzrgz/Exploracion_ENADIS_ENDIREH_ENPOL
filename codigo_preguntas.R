# Segunda fase INTERSECTA 

  # Paquetes 

options(scipen = 999)
library(pacman)
pacman::p_load(tidyverse, readr, dplyr, janitor, stringr,fixest)

#################### Pregunta 1: Indigenas privados de su libertad  ##########################

enpol21s <- list.files('Bases de datos/01_bd_enpol_2021_csv')
enpol21 <- lapply(enpol21s, function(x) read.csv(paste('Bases de datos/01_bd_enpol_2021_csv',x,sep="/")))

names(enpol21) <- gsub("\\.csv$", "", enpol21s)

#################### Pregunta 2: Diversidad Religiosa #####################################
enadis22s <- list.files('Bases de datos/02_bd_enadis_2022_csv')
enadis22 <- lapply(enadis22s, function(x) read.csv(paste('Bases de datos/02_bd_enadis_2022_csv',x,sep="/")))
names(enadis22) <- gsub("\\.csv$", "", enadis22s)

religion <- enadis22$TRELIGION

# (PM4_1) En su opinión, en el país, ¿los derechos de las personas con su religión se respetan: mucho (1), algo, poco o nada?
frecuencia_PM4_1 <- table(religion$PM4_1)
print(frecuencia)
# Fac expa
resultados_PM4_1 <- aggregate(FAC_REL ~ PM4_1, data = religion, FUN = function(x) sum(abs(x)))
print(resultados_PM4_1)

# (PM4_2) De las siguientes opciones, ¿cuál considera que es el principal problema para las personas con su religión en el país, hoy en día?
      # 1 trabajo o escuela 2 Son rechazadas o aisladas por la sociedad 3 Falta de oportunidades para mostrar sus creencias a la sociedad 4 Falta de respeto a sus costumbres y tradiciones 5 El gobierno apoya más a la comunidad católica 6 Otro 
frecuencia_PM4_2 <- table(religion$PM4_2)
porcentaje_PM4_2 <- prop.table(frecuencia_PM4_2) * 100
print(porcentaje_PM4_2)
print(frecuencia_PM4_2)

#Fac expa 
resultados_PM4_2 <- aggregate(FAC_REL ~ PM4_2, data = religion, FUN = function(x) sum(abs(x)))
print(resultados_PM4_2)

# (PM4_3_1) 1: sí Dígame si está de acuerdo con las siguientes frases: Las personas de su religión son más confiables que el resto de la población

frecuencia_PM4_3_1 <- table(religion$PM4_3_1)
print(frecuencia_PM4_3_1)

#Fac expa 
resultados_PM4_3_1 <- aggregate(FAC_REL ~ PM4_3_1, data = religion, FUN = function(x) sum(abs(x)))
print(resultados_PM4_3_1)

#(PM4_3_2) Dígame si está de acuerdo con las siguientes frases: Las personas de su religión son rechazadas por la mayoría de la gente
frecuencia_PM4_3_2 <- table(religion$PM4_3_2)
print(frecuencia_PM4_3_2)

#Fac expa 
resultados_PM4_3_2 <- aggregate(FAC_REL ~ PM4_3_2, data = religion, FUN = function(x) sum(abs(x)))
print(resultados_PM4_3_2)


# (PM4_3_3) Dígame si está de acuerdo con las siguientes frases: Las personas de su religión son consideradas fanáticas o tercas por la mayoría de la gente

frecuencia_PM4_3_3 <- table(religion$PM4_3_3)
print(frecuencia_PM4_3_3)

# Fac expa
resultados_PM4_3_3 <- aggregate(FAC_REL ~ PM4_3_3, data = religion, FUN = function(x) sum(abs(x)))
print(resultados_PM4_3_3)


# (PM4_3_4) Dígame si está de acuerdo con las siguientes frases: Mientras más religiones se permitan en el país, habrá más conflictos sociales
frecuencia_PM4_3_4 <- table(religion$PM4_3_4)
print(frecuencia_PM4_3_4)

# Fac expa
resultados_PM4_3_4 <- aggregate(FAC_REL ~ PM4_3_4, data = religion, FUN = function(x) sum(abs(x)))
print(resultados_PM4_3_4)


# (PM4_4_1) ¿las personas con su religión son discriminadas cuando van a las oficinas o servicios de gobierno 1: mucho, 2 algo, poco, nada
frecuencia_PM4_4_1 <- table(religion$PM4_4_1)
print(frecuencia_PM4_4_1)
aaaaa <- prop.table(frecuencia_PM4_4_1) * 100
print(aaaaa)

# Fac expa 
resultados_PM4_4_1 <- aggregate(FAC_REL ~ PM4_4_1, data = religion, FUN = function(x) sum(abs(x)))
print(resultados_PM4_4_1)
resultados_PM4_4_1 <- prop.table(resultados_PM4_4_1) * 100


# (PM4_4_2) ¿las personas con su religión son discriminadas cuando van a los tribunales o juzgados
frecuencia_PM4_4_2 <- table(religion$PM4_4_2)
print(frecuencia_PM4_4_2)


# Fac expa
resultados_PM4_4_2 <- aggregate(FAC_REL ~ PM4_4_2, data = religion, FUN = function(x) sum(abs(x)))
print(resultados_PM4_4_2)


# (PM4_4_3) ¿las personas con su religión son discriminadas cuando van a los servicios de salud
frecuencia_PM4_4_3 <- table(religion$PM4_4_3)
print(frecuencia_PM4_4_3)


# Fac expa
resultados_PM4_4_3 <- aggregate(FAC_REL ~ PM4_4_3, data = religion, FUN = function(x) sum(abs(x)))
print(resultados_PM4_4_3)


# (PM4_4_4) ¿las personas con su religión son discriminadas cuando van a las escuelas
frecuencia_PM4_4_4 <- table(religion$PM4_4_4)
print(frecuencia_PM4_4_4)


# Fac expa 
resultados_PM4_4_4 <- aggregate(FAC_REL ~ PM4_4_4, data = religion, FUN = function(x) sum(abs(x)))
print(resultados_PM4_4_4)


# (PM4_4_5) ¿las personas con su religión son discriminadas cuando van a los negocios (tiendas, restaurantes)
frecuencia_PM4_4_5 <- table(religion$PM4_4_5)
print(frecuencia_PM4_4_5)

#Fac expa
resultados_PM4_4_5 <- aggregate(FAC_REL ~ PM4_4_5, data = religion, FUN = function(x) sum(abs(x)))
print(resultados_PM4_4_5)


# (PM4_4_6) ¿las personas con su religión son discriminadas cuando van a buscar empleo
frecuencia_PM4_4_6 <- table(religion$PM4_4_6)
print(frecuencia_PM4_4_6)
aaaaa <- prop.table(frecuencia_PM4_4_6) * 100
print(aaaaa)


# Fac expa 
resultados_PM4_4_6 <- aggregate(FAC_REL ~ PM4_4_6, data = religion, FUN = function(x) sum(abs(x)))
print(resultados_PM4_4_6)



# (PM4_5) ¿La libertad que tiene para expresar sus creencias, cultos o ritos en su comunidad es mucha, alguna, poca o ninguna? 1. Mucha, alguna, poca, ninguna 

frecuencia_PM4_5 <- table(religion$PM4_5)
print(frecuencia_PM4_5)

aaaaa <- prop.table(frecuencia_PM4_5) * 100
print(aaaaa)
# Fac expa
resultados_PM4_5 <- aggregate(FAC_REL ~ PM4_5, data = religion, FUN = function(x) sum(abs(x)))
print(resultados_PM4_5)




################### Pregunta 3: Violencia Obstetrica #####################################

endireh21s <- list.files('Bases de datos/03_bd_endireh_2021_csv')
endireh21 <- lapply(endireh21s, function(x) read.csv(paste('Bases de datos/03_bd_endireh_2021_csv',x,sep="/")))
names(endireh21) <- gsub("\\.csv$", "", endireh21s)


  # Colapsar bases importantes
endireh_im <- Reduce(function(x, y) merge(x, y, by = 'ID_PER', all = FALSE), endireh21)

#Filtrar a las que tuvieron un hijo en los ultimos 5 años (encuesta 21')
endireh_obste <- endireh_im[!is.na(endireh_im$P10_2) & endireh_im$P10_2 == 1, ]
endireh_obste <- endireh_obste[!is.na(endireh_obste$ID_PER), ]
# Hay 20947 onservaciones 

# Parte 1: Situación general, violencia durante el parto 
# Reportaron algún tipo de violencia, al menos un tipo de violencia 

violencia_endireh <- endireh_obste[rowSums(endireh_obste[ c('P10_8_1', 'P10_8_2', 'P10_8_3', 'P10_8_4', 'P10_8_5', 'P10_8_6', 'P10_8_7', 'P10_8_8', 'P10_8_9', 'P10_8_10', 'P10_8_11')] == 1, na.rm = TRUE) > 0, ]
violencia_endireh <- violencia_endireh[!is.na(violencia_endireh$ID_PER), ] #Hay 5282 observaciones 

#Nivel Nacional, utilizando el factor de expansión 
proporcion_nac <- sum(violencia_endireh$FAC_MUJ.y, na.rm = TRUE) / sum(endireh_obste$FAC_MUJ.y, na.rm = TRUE)
#  0.2552686

# Nivel Estatal: Loop estatal proporciones_estados <- list()

# Estados
proporciones_estados <- list()
nombres_estados <- character()

for (estado in 1:32) {
  endireh_obste <- endireh_im[!is.na(endireh_im$P10_2) & endireh_im$P10_2 == 1 & endireh_im$CVE_ENT.x == estado, ]
  endireh_obste <- endireh_obste[!is.na(endireh_obste$ID_PER), ]
  violencia_parto <- c('P10_8_1', 'P10_8_2', 'P10_8_3', 'P10_8_4', 'P10_8_5', 'P10_8_6', 'P10_8_7', 'P10_8_8', 'P10_8_9', 'P10_8_10', 'P10_8_11')
  violencia_endireh <- endireh_obste[rowSums(endireh_obste[violencia_parto] == 1, na.rm = TRUE) > 0 | endireh_obste$P10_8_13 == 2 | endireh_obste$P10_8_14 == 2, ]
  violencia_endireh <- violencia_endireh[!is.na(violencia_endireh$ID_PER), ]
  proporcion_nac <- sum(violencia_endireh$FAC_MUJ.y, na.rm = TRUE) /sum(endireh_obste$FAC_MUJ.y, na.rm = TRUE)
  proporciones_estados[[estado]] <- proporcion_nac
  
  nombre_estado <- endireh_obste$NOM_ENT.x[1]
  nombres_estados[[estado]] <- nombre_estado
}

# Hacer el cuadro de resultados 
lista_estados <- data.frame(NombreEstado = unlist(nombres_estados),
                            ProporcionNac = unlist(proporciones_estados))


# Argumento 1: De las mujeres que sufrieron violencia cuantas en centros públicos y cuantas en privados
# Centros publicos P10_7 == (1-5)
vio_pub <-  subset(violencia_endireh, P10_7 >= 1 & P10_7 <= 5)
prop_pub <-sum(vio_pub$FAC_MUJ.y, na.rm = TRUE)/ sum(violencia_endireh$FAC_MUJ.y, na.rm = TRUE)


# Centros privados P10_7 == (6-8)
vio_priv <- subset(violencia_endireh, P10_7 >= 6 & P10_7 <= 8)
prop_priv <-  sum(vio_priv$FAC_MUJ.y, na.rm = TRUE) / sum(violencia_endireh$FAC_MUJ.y, na.rm = TRUE)


# Argumento 2: De las mujeres que sufrieron violencia "violencia_endireh" cuantas tuvieron otro tipo de violencia en el ámbito familiar

#Violencia en el ambito familia p11 (1-20)
violencia_fami <- subset(violencia_endireh, 
                            P11_1_1 %in% c(1, 2, 3) | 
                              P11_1_2 %in% c(1, 2, 3) |
                              P11_1_3 %in% c(1, 2, 3) |
                              P11_1_4 %in% c(1, 2, 3) |
                              P11_1_5 %in% c(1, 2, 3) |
                              P11_1_6 %in% c(1, 2, 3) |
                              P11_1_7 %in% c(1, 2, 3) |
                              P11_1_8 %in% c(1, 2, 3) |
                              P11_1_9 %in% c(1, 2, 3) |
                              P11_1_10 %in% c(1, 2, 3) |
                              P11_1_11 %in% c(1, 2, 3) |
                              P11_1_12 %in% c(1, 2, 3) |
                              P11_1_13 %in% c(1, 2, 3) |
                              P11_1_14 %in% c(1, 2, 3) |
                              P11_1_15 %in% c(1, 2, 3) |
                              P11_1_16 %in% c(1, 2, 3) |
                              P11_1_17 %in% c(1, 2, 3) |
                              P11_1_18 %in% c(1, 2, 3) |
                              P11_1_19 %in% c(1, 2, 3) |
                              P11_1_20 %in% c(1, 2, 3)
)


prop_fam <-  sum(violencia_fami$FAC_MUJ.y, na.rm = TRUE) / sum(violencia_endireh$FAC_MUJ.y, na.rm = TRUE)
# 0.1818152

# Argumento 3: De las mujeres indigenas que tuvieron hijos, cuantas tuvieron violencia obstetrica

mamas_indigenas <- endireh_im[!is.na(endireh_im$P10_2) & (endireh_im$P10_2 == 1) & (endireh_im$P2_10 %in% c(1, 2)), ]
mamas_indigenas <- mamas_indigenas[!is.na(mamas_indigenas$ID_PER), ]

#Violencia indigena 
violencia_ind <- mamas_indigenas[rowSums(mamas_indigenas[violencia_parto] == 1, na.rm = TRUE) > 0 | mamas_indigenas$P10_8_13 == 2 | mamas_indigenas$P10_8_14 == 2, ]
violencia_ind <- violencia_ind[!is.na(violencia_ind$ID_PER), ]

#Proporcion de violencia indigena, obstetrica 

prop_ind <-  sum(violencia_ind$FAC_MUJ.y, na.rm = TRUE) / sum(mamas_indigenas$FAC_MUJ.y, na.rm = TRUE)
# 0.2832439

