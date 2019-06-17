# Análisis de datos de muestreo por distancia para Atelopus spurrelli del PNN Utría a cargo de 
# personal del parque con apoyo del Grupo de Investigación y Asesoría en Estadística de la Universidad del
# Quindío, representado por el investigador Diego A. Gómez Hoyos. 
# El código y los datos de este proyecto estarán liberados en Github, pero estamos interesados en colaboraciones
# agradecemos entonces contactarnos en al correo: biodiego88@gmail.com

# Llamamos los paquetes unmarked y AICcmodavg luego de ser instalados 

library(unmarked)

library(AICcmodavg)

library(ggplot2)

# Establecemos la ruta de la carpeta donde se encuentran nuestros archivos
setwd("C:/RAnalysis/Unmarked/distsamp/Atelopus_spurrelli")


# le damos el nombre de "dists" al archivo que contiene nuestros datos
dists <- read.csv("Atelopus_obs.csv", header=TRUE)

head(dists)

# Darle formato de factor a la columna que contiene las etiquetas de nuestras unidades de muestreo 
dists$transect <- as.factor(dists$transect)


levels(dists$transect) <- c(levels(dists$transect))

# se convierten los datos de frecuencias de distancias a rangos de distancias, en este caso cada 0.5 metros

cp = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5)

# organizamos el formato de nuestros datos con la funcion formatDistData

yData = formatDistData(dists, "distm", "transect", cp)

class(yData)

hist(yData)

# importamos los datos de las covariables desde el archivo "Atelopus_cov.csv", de manera que coincidan con la organización de los datos de los conteos en cada unidad de 
# muestreo por cada rango de distancia 

covs <- read.csv("Atelopus_cov.csv", header=TRUE)

head(covs)

# con la funcion unmarkedFrameDS organizamos nuestros datos para correrlos con la función distamp

umf <- unmarkedFrameDS(y=as.matrix(yData), siteCovs=covs, survey="line", 
                       dist.breaks=c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5), 
                       tlength = covs$long, unitsIn="m")

umf

# procedemos a ajustar nuestros datos a los modelos, iniciando con un modelo nulo
# con las diferentes funciones de distribución halfnormal, hazard, exp y uniforme

hn_Null <- distsamp(~1~1, umf, keyfun="halfnorm", output="density", unitsOut="ha")

hz_Null <- distsamp(~1~1, umf, keyfun="hazard", output="density", unitsOut="ha")

exp_Null <- distsamp(~1~1, umf, keyfun="exp", output="density", unitsOut="ha")

unf_Null <- distsamp(~1~1, umf, keyfun="uniform", output="density", unitsOut="ha")

# a continuación probamos los modelos en los que la probabilidad de detección sin covariables y la densidad explicada por la quebrada
# con cada función de distribución

hn_Nullqda <- distsamp(~1~Qda, umf, keyfun="halfnorm", output="density", unitsOut="ha")

hz_Nullqda <- distsamp(~1~Qda, umf, keyfun="hazard", output="density", unitsOut="ha")

exp_Nullqda <- distsamp(~1~Qda, umf, keyfun="exp", output="density", unitsOut="ha")

unf_Nullqda <- distsamp(~1~Qda, umf, keyfun="uniform", output="density", unitsOut="ha")

# ajustamos los modelos en los que la probabilidad de detección este explicada por tipo de quebrada 
# y la densidad sin covariables con cada función de distribución

hn_qdaNull <- distsamp(~Qda~1, umf, keyfun="halfnorm", output="density", unitsOut="ha")

hz_qdaNull <- distsamp(~Qda~1, umf, keyfun="hazard", output="density", unitsOut="ha")

exp_qdaNull <- distsamp(~Qda~1, umf, keyfun="exp", output="density", unitsOut="ha")

unf_qdaNull <- distsamp(~Qda~1, umf, keyfun="uniform", output="density", unitsOut="ha")

# ajustamos los modelos en los que la probabilidad de detección y 
# la densidad esten explicadas por tipo de quebrada con cada función de distribución

hn_qdaqda <- distsamp(~Qda~Qda, umf, keyfun="halfnorm", output="density", unitsOut="ha")

hz_qdaqda <- distsamp(~Qda~Qda, umf, keyfun="hazard", output="density", unitsOut="ha")

exp_qdaqda <- distsamp(~Qda~Qda, umf, keyfun="exp", output="density", unitsOut="ha")

unf_qdaqda <- distsamp(~Qda~Qda, umf, keyfun="uniform", output="density", unitsOut="ha")



# ajuste y selección del modelo

#listar los modelos
Cand.models <- list(hn_Null, hz_Null, exp_Null, unf_Null, hn_Nullqda, hz_Nullqda, exp_Nullqda, 
                    unf_Nullqda, hn_qdaNull, hz_qdaNull, exp_qdaNull, unf_qdaNull, hn_qdaqda, 
                    hz_qdaqda, exp_qdaqda, unf_qdaqda)

#nombrar los modelos
Modnames <- c("hn_Null", "hz_Null", "exp_Null", "unf_Null", "hn_Nullqda", "hz_Nullqda", "exp_Nullqda", 
              "unf_Nullqda", "hn_qdaNull", "hz_qdaNull", "exp_qdaNull", "unf_qdaNull", "hn_qdaqda", 
              "hz_qdaqda", "exp_qdaqda", "unf_qdaqda")

aictab(cand.set = Cand.models, modnames = Modnames,
       second.ord = TRUE, nobs = NULL, sort = TRUE)

########### Resultados ##############################
            K   AICc Delta_AICc AICcWt Cum.Wt      LL
hz_qdaqda   7  60.94       0.00      1      1  -79.47
hz_Null     3 192.17     131.23      0      1  -89.09
exp_Null    2 192.68     131.74      0      1  -92.84
exp_qdaNull 4 201.00     140.07      0      1  -86.50
exp_Nullqda 4 204.19     143.25      0      1  -88.10
hn_Null     2 225.63     164.69      0      1 -109.32
hn_qdaNull  4 231.96     171.02      0      1 -101.98
hz_qdaNull  5 232.11     171.17      0      1  -81.05
hn_Nullqda  4 237.15     176.21      0      1 -104.57
hz_Nullqda  5 238.68     177.75      0      1  -84.34
unf_qdaNull 1 558.64     497.70      0      1 -277.92
unf_Null    1 558.64     497.70      0      1 -277.92
unf_qdaqda  3 560.36     499.42      0      1 -273.18
unf_Nullqda 3 560.36     499.42      0      1 -273.18
exp_qdaqda  6    Inf        Inf      0      1  -84.56
hn_qdaqda   6    Inf        Inf      0      1  -97.91       
######################################################

# Realizamos la predicción incluyendo la transformación del antilogaritmo con la función predict. 
# Para esto creamos un formato de datos de las variables "nuevo" y 
# luego se calcula con un nivel de 0.90 (IC 95%)

nuevo = data.frame(Qda = c("Cocalito", "Aguada", "Guachalito"))

denshz_qdaqda= predict(hz_qdaqda, type = "state", newdata = nuevo, appendData = T, level = 0.90)
denshz_qdaqda
denshz_qdaqda/10000

#Resultados
Predicted       SE     lower    upper        Qda
1  940.1043 147.0323  726.8613 1215.907   Cocalito
2 1300.1653 245.4987  953.0464 1773.712     Aguada
3 1387.8859 262.8034 1016.4541 1895.046 Guachalito

Predicted         SE      lower     upper Qda
1 0.09401043 0.01470323 0.07268613 0.1215907  NA
2 0.13001653 0.02454987 0.09530464 0.1773712  NA
3 0.13878859 0.02628034 0.10164541 0.1895046  NA

dethz_qdaqda= predict(hz_qdaqda, type = "det", newdata = nuevo, appendData = T, level = 0.90)
dethz_qdaqda

# Resultados
Predicted         SE     lower     upper        Qda
1 0.7525293 0.09917507 0.6058706 0.9346887   Cocalito
2 0.4886760 0.08001327 0.3732991 0.6397129     Aguada
3 0.8284084 0.11267804 0.6623389 1.0361168 Guachalito


# Representamos los resultados a través de gráficas construidas en ggplot

ggplot(denshz_qdaqda, aes(x=Qda, y=Predicted)) +  
  geom_bar(position=position_dodge(), stat="identity", width=.5, colour = 'gray', fill = 'light gray') +
  geom_errorbar(aes(ymin=Predicted-SE, ymax=Predicted+SE), width=.2, colour = 'black') +
  scale_x_discrete(name = expression(bold('Quebrada'))) +
  scale_y_continuous(name = expression(bold(paste("Densidad ", individuos / hectárea)))) +
  theme_bw()

ggplot(dethz_qdaqda, aes(x=Qda, y=Predicted)) +  
  geom_bar(position=position_dodge(), stat="identity", width=.5, colour = 'gray', fill = 'light gray') +
  geom_errorbar(aes(ymin=Predicted-SE, ymax=Predicted+SE), width=.2, colour = 'black') +
  scale_x_discrete(name = expression(bold('Quebrada'))) +
  scale_y_continuous(name = expression(bold(paste("Probabilidad de detección")))) +
  theme_bw()

