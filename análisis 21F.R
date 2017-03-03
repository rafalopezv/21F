
# Establecer un encoding compatible con las bases de datos (español)
Sys.getlocale() # saber cuál es el encoding actual
Sys.setlocale(locale = "es_ES.UTF-8") # cambio al encoding UTF-8 en español. Paso omitible si el locale ya trabaja en español

setwd("/Users/rafalopezv/Dropbox/R/analisis.electoral/base.de.datos.editada.nivel.municipal/")

# Cargado de paquetes necesarios
pkgs <- c("rio", "magrittr", "xlsx", "readxl", "stringi", "gdata",
          "gsubfn", "dplyr", "plyr", "leaflet", "plotly")
lapply(pkgs, function(x) require(x, character.only = TRUE))
rm(pkgs)

# Cargando bases 
na.ex <- import("2014.nacionales/2014.nacionales.exterior.csv")
re.ex <- import("2016.referendum.reeleccion/2016.referendum.reelecion.exterior.csv")
na.bo <- import("2014.nacionales/2014.nacionales.ejecutivo.csv")
re.bo <- import("2016.referendum.reeleccion/2016.referedum.reeleccion.csv")

# Cálculo de agregados ambos procesos
agregado <- matrix()
agregado <- matrix(agregado, nrow = 6, ncol = 4)
colnames(agregado)  <- c("INSCRITOS", "EMITIDOS", "VÁLIDOS", "EVO")
rownames(agregado) <- c("ELECCIÓN NACIONAL VOTO BOLIVIA", "RE-ELECCIÓN VOTO BOLIVIA",  
                        "ELECCIÓN NACIONAL VOTO EXTERIOR", "RE-ELECCIÓN VOTO EXTERIOR",
                        "TOTAL ELECCIÓN NACIONAL", "TOTAL RE-ELECCIÓN")

agregado[1, ] <- na.bo %>% select(-DEPARTAMENTO, -PROVINCIA, -MUNICIPIO) %>%
  select(INSCRITOS, EMITIDOS, VÁLIDOS, `MAS-IPSP`) %>% colSums()
agregado[2, ] <- re.bo %>%   select(-DEPARTAMENTO, -PROVINCIA, -MUNICIPIO) %>% select(INSCRITOS, EMITIDOS, VÁLIDOS, SI) %>% colSums()
agregado[3, ] <- na.ex %>% select(-PAÍS, -CIUDAD) %>% select(INSCRITOS, EMITIDOS, VÁLIDOS, `MAS-IPSP`) %>% colSums()
agregado[4, ] <- re.ex %>% select(-PAÍS, -CIUDAD) %>% select(INSCRITOS, EMITIDOS, VÁLIDOS, SI) %>% colSums()
agregado[5, ] <- agregado[1, ] + agregado[3, ] 
agregado[6, ] <- agregado[2, ] + agregado[4, ] 
agregado <- as.data.frame(agregado)
agregado$PROCESO <- rownames(agregado)
rownames(agregado) <- NULL
agregado <- agregado[, c(5,1,2,3,4)]

# Cálculo porcentual
agregado.1 <- agregado 
colnames(agregado.1) <- c("PROCESO", "INSCRITOS", "EMITIDOS RESPECTO INSCRITOS", 
                          "VÁLIDOS RESPECTO EMITIDOS", "EVO RESPECTO VÁLIDOS")
agregado.1[, 3] <- round((agregado[, 3] / agregado[, 2]) * 100, 2) 
agregado.1[, 4] <- round((agregado[, 4] / agregado[, 3]) * 100, 2) 
agregado.1[, 5] <- round((agregado[, 5] / agregado[, 4]) * 100, 2) 

# Cálculo de variaciones porcentualales
agregado.2 <- agregado[1:3, ]

agregado.2$PROCESO <- c("VARIACION:RE-ELECCIÓN SOBRE ELECCIÓN NACIONAL VOTO BOLIVIA", 
                        "VARIACIÓN: RE-ELECCIÓN SOBRE ELECCIÓN NACIONAL VOTO EXTERIOR", 
                        "VARIACION:RE-ELECCIÓN SOBRE ELECCIÓN NACIONAL TOTAL")
agregado.2[1, 2:ncol(agregado.2)] <- round(((agregado[2,2:ncol(agregado)] - agregado[1, 2:ncol(agregado)]) / agregado[2,2:ncol(agregado)])*100, 2)
agregado.2[2, 2:ncol(agregado.2)] <- round(((agregado[4,2:ncol(agregado)] - agregado[3, 2:ncol(agregado)]) / agregado[4,2:ncol(agregado)])*100, 2)
agregado.2[3, 2:ncol(agregado.2)] <- round(((agregado[6,2:ncol(agregado)] - agregado[5, 2:ncol(agregado)]) / agregado[6,2:ncol(agregado)])*100, 2)



# Cálculo a nivel municipal
municipal <- re.bo %>% select(DEPARTAMENTO, PROVINCIA, MUNICIPIO, INSCRITOS, EMITIDOS, VÁLIDOS, SI)
colnames(municipal)[4:7] <- c("INSCRITOS RE-ELECCIÓN", "EMITIDOS RE-ELECCIÓN", "VÁLIDOS RE-ELECCIÓN",
                              "EVO RE-ELECCIÓN")
municipal <- merge(municipal, na.bo %>% select(DEPARTAMENTO, PROVINCIA, MUNICIPIO, INSCRITOS, EMITIDOS, VÁLIDOS, `MAS-IPSP`))
colnames(na.bo)
colnames(municipal)[8:11] <- c("INSCRITOS ELECCIÓN NACIONAL", "EMITIDOS ELECCIÓN NACIONAL", "VÁLIDOS ELECCIÓN NACIONAL",
                               "EVO ELECCIÓN NACIONAL")
municipal[, "DIFERENCIA INSCRITOS"] <- municipal$`INSCRITOS RE-ELECCIÓN` - municipal$`INSCRITOS ELECCIÓN NACIONAL`
municipal[, "DIFERENCIA INSCRITOS %"] <- round(municipal$`DIFERENCIA INSCRITOS`/municipal$`INSCRITOS RE-ELECCIÓN` *100, 2)
municipal[, "DIFERENCIA EMITIDOS"] <- municipal$`EMITIDOS RE-ELECCIÓN`-municipal$`EMITIDOS ELECCIÓN NACIONAL`
municipal[, "DIFERENCIA EMITIDOS %"] <- round(municipal$`DIFERENCIA EMITIDOS`/municipal$`EMITIDOS RE-ELECCIÓN`*100, 2)
municipal[, "DIFERENCIA VÁLIDOS"] <- municipal$`VÁLIDOS RE-ELECCIÓN`-municipal$`VÁLIDOS ELECCIÓN NACIONAL`
municipal[, "DIFERENCIA VÁLIDOS %"] <- round(municipal$`DIFERENCIA VÁLIDOS`/municipal$`VÁLIDOS RE-ELECCIÓN`*100,2)
municipal[, "DIFERENCIA VÁLIDOS %"] <- round(municipal$`DIFERENCIA VÁLIDOS`/municipal$`VÁLIDOS RE-ELECCIÓN`*100,2)
municipal[, "DIFERENCIA EVO"] <- municipal$`EVO RE-ELECCIÓN` -municipal$`EVO ELECCIÓN NACIONAL`
municipal[, "DIFERENCIA EVO % EN VOTOS"] <- round(municipal$`DIFERENCIA EVO`/municipal$`EVO RE-ELECCIÓN`*100, 2)
municipal[, "EVO RE-ELECCIÓN %"] <- round(municipal$`EVO RE-ELECCIÓN`/municipal$`VÁLIDOS RE-ELECCIÓN`*100, 2)
municipal[, "EVO ELECCIÓN NACIONAL %"] <- round(municipal$`EVO ELECCIÓN NACIONAL`/municipal$`VÁLIDOS ELECCIÓN NACIONAL`*100, 2)
municipal[, "DIFERENCIA EVO % EN PORCENTAJES"] <- municipal$`EVO RE-ELECCIÓN %`-municipal$`EVO ELECCIÓN NACIONAL %`
municipal <- municipal[,c(1:3,4,8,12,13,5,9,14,15,6,10,16,17,7,11,18,21,19,20,22)]

# crear una fórmula para los crear las siguientes columnas
y <- function(x) {
  if(x < 0) {
    print("PERDIÓ APOYO")
  } else {
    print("GANÓ APOYO")
  }
}

municipal$`PERDIDA O GANANCIA` <- lapply(municipal$`DIFERENCIA EVO % EN PORCENTAJES`, y) %>%
  unlist

z <- function(x) {
  if(x == 0 | x > 0) {
    print("0 % a 13 %")
  } else if(x < 0 & x > -15) {
    print("-1 % a -15 %")
  } else {
    print("-15.01 % a -42 %")
  }
}

municipal$MAGNITUD <- lapply(municipal$`DIFERENCIA EVO % EN PORCENTAJES`, z) %>% 
  unlist
rm(y, z)
# fusionar con base del mapa a ser creado
a <- import("/Users/rafalopezv/Dropbox/R/analisis.electoral/21F/339.municipios.vicepresidencia/municipal.dbf")
a$no <- 1:344
id <- import("/Users/rafalopezv/Dropbox/R/analisis.electoral/21F/id.xlsx")
m1 <- merge(id, municipal, all.x = T)
temp <- m1$DEPARTAMENTO == "LAGO" | m1$DEPARTAMENTO == "SALAR"
m1[temp, c("PERDIDA O GANANCIA", "MAGNITUD")] <- "LAGO/SALAR"
a <- merge(a, m1, by = "id")
a <- plyr::arrange(a, a$no)
rio::export(a, "/Users/rafalopezv/Dropbox/R/analisis.electoral/21F/339.municipios.vicepresidencia/municipal.dbf")

# Cálculo voto en el exterior
exterior <- re.ex %>% select (-BLANCOS, -NULOS, -NO)
colnames(exterior)[3:6] <- c("INSCRITOS RE-ELECCIÓN", "EVO RE-ELECCIÓN", "VÁLIDOS RE-ELECCIÓN", "EMITIDOS RE-ELECCIÓN")
exterior <- merge(exterior, na.ex %>% select(-PDC, -PVB, -MSM, -UD, -BLANCOS, -NULOS), all.x = T)
exterior[, "DIFERENCIA INSCRITOS"] <- exterior$`INSCRITOS RE-ELECCIÓN`-exterior$INSCRITOS
exterior[, "DIFERENCIA INSCRITOS %"] <- round(exterior$`DIFERENCIA INSCRITOS`/exterior$`INSCRITOS RE-ELECCIÓN`*100,2)
exterior[, "DIFERENCIA EMITIDOS"] <- exterior$`EMITIDOS RE-ELECCIÓN`-exterior$EMITIDOS
exterior[, "DIFERENCIA EMITIDOS %"] <- round(exterior$`DIFERENCIA EMITIDOS`/exterior$`EMITIDOS RE-ELECCIÓN`*100,2)
exterior[, "DIFERENCIA VÁLIDOS"] <- exterior$`VÁLIDOS RE-ELECCIÓN`-exterior$VÁLIDOS
exterior[, "DIFERENCIA VÁLIDOS %"] <- round(exterior$`DIFERENCIA VÁLIDOS`/exterior$`VÁLIDOS RE-ELECCIÓN`*100,2) 
exterior[, "DIFERENCIA EVO"] <- exterior$`EVO RE-ELECCIÓN`-exterior$`MAS-IPSP`
exterior[, "DIFERENCIA EVO % EN VOTOS"] <- round(exterior$`DIFERENCIA EVO`/exterior$`EVO RE-ELECCIÓN`*100,2)
exterior[, "EVO RE-ELECCIÓN %"] <- round(exterior$`EVO RE-ELECCIÓN`/exterior$`VÁLIDOS RE-ELECCIÓN`*100,2)
exterior[, "EVO ELECCIÓN NACIONAL %"] <- round(exterior$`MAS-IPSP`/exterior$VÁLIDOS*100,2)
exterior[, "DIFERENCIA EVO % EN PORCENTAJES"] <- exterior$`EVO RE-ELECCIÓN %`-exterior$`EVO ELECCIÓN NACIONAL %`
exterior <- exterior[,c(1,2,3,10,11,12,6,9,13,14,5,8,15,16,4,7,17,18,19,20,21)]
colnames(exterior)[c(4,8,12,16)] <- c("INSCRITOS ELECCIÓN NACIONAL", "EMITIDOS ELECCIÓN NACIONAL", "VÁLIDOS ELECCIÓN NACIONAL",
                                      "EVO ELECCIÓN NACIONAL")
temp <- exterior$CIUDAD == "COMODORO RIVADAVIA"
temp1 <- colnames(exterior) == "DIFERENCIA INSCRITOS" | colnames(exterior) == "DIFERENCIA EMITIDOS" |
  colnames(exterior) == "DIFERENCIA VÁLIDOS" | colnames(exterior) == "DIFERENCIA EVO" |
  colnames(exterior) == "DIFERENCIA EVO % EN PORCENTAJES"
exterior[temp, temp1] <- c(537,360,324,286,88.27)
temp <- exterior$CIUDAD == "HOUSTON"
exterior[temp, temp1] <- c(172,137,135,22,16.3)
temp <- exterior
temp1 <- exterior$CIUDAD == "COMODORO RIVADAVIA"
temp2 <- is.na(exterior[temp1,])
temp[temp1, temp2] <- 0
temp1 <- exterior$CIUDAD == "HOUSTON"
temp2 <- is.na(exterior[temp1,])
temp[temp1, temp2] <- 0

exterior.paises <- exterior %>% select(-CIUDAD) %>% group_by(PAÍS) %>% summarise_each(funs(sum(., na.rm =T)))
exterior.paises$`DIFERENCIA INSCRITOS %` <- round(exterior.paises$`DIFERENCIA INSCRITOS`/exterior.paises$`INSCRITOS RE-ELECCIÓN`*100,2)
exterior.paises$`DIFERENCIA EMITIDOS %` <- round(exterior.paises$`DIFERENCIA EMITIDOS`/exterior.paises$`EMITIDOS RE-ELECCIÓN`*100,2)
exterior.paises$`DIFERENCIA VÁLIDOS %` <- round(exterior.paises$`DIFERENCIA VÁLIDOS`/exterior.paises$`VÁLIDOS RE-ELECCIÓN`*100,2)
exterior.paises$`DIFERENCIA EVO % EN VOTOS` <- round(exterior.paises$`DIFERENCIA EVO`/exterior.paises$`EVO RE-ELECCIÓN`*100,2)
exterior.paises$`EVO RE-ELECCIÓN %` <- round(exterior.paises$`EVO RE-ELECCIÓN`/exterior.paises$`VÁLIDOS RE-ELECCIÓN`*100,2)
exterior.paises$`EVO ELECCIÓN NACIONAL %` <- round(exterior.paises$`EVO ELECCIÓN NACIONAL`/exterior.paises$`VÁLIDOS ELECCIÓN NACIONAL`*100,2)
exterior.paises$`DIFERENCIA EVO % EN PORCENTAJES` <- exterior.paises$`EVO RE-ELECCIÓN %`-exterior.paises$`EVO ELECCIÓN NACIONAL %`
exterior.paises$`DIFERENCIA EMITIDOS EVO POSITIVO`<-exterior.paises$`EMITIDOS ELECCIÓN NACIONAL`-exterior.paises$`EMITIDOS RE-ELECCIÓN` 

# Para gráficos 

agregado.3 <- agregado[c(3, 4), 1:3] # baja de emitidos exterior

colnames(agregado.3) <- c("PROCESO", "PORCENTAJE EMITIOS", "PORCENTAJE ABSTECIÓN")
agregado.3[1, 2] <- round(agregado[3, 3]/ agregado[3, 2] *100, 2)
agregado.3[1, 3] <- 100- agregado.3[1, 2]
agregado.3[2, 2] <- round(agregado[4, 3]/ agregado[4, 2] *100, 2)
agregado.3[2, 3] <- 100- agregado.3[2, 2]
agregado.3[1, 1] <- "ELECCIÓN NACIONAL 2014"
agregado.3[2, 1] <- "REFERENDUM RE-ELECCIÓN 2016"
rio::export(agregado.3, "/Users/rafalopezv/Dropbox/R/analisis.electoral/21F/bases/agregado.3.csv")

# peso porcentual de la baja por país
caida.emitidos.exterior <- exterior.paises %>% select(PAÍS, `DIFERENCIA EMITIDOS EVO POSITIVO`)
temp <- caida.emitidos.exterior$PAÍS == "ARGENTINA" |
  caida.emitidos.exterior$PAÍS == "ESPAÑA" |
  caida.emitidos.exterior$PAÍS == "BRASIL" |
  caida.emitidos.exterior$PAÍS == "CHILE" |
  caida.emitidos.exterior$PAÍS == "ITALIA" |
  caida.emitidos.exterior$PAÍS == "ESTADOS UNIDOS"
caida.emitidos.exterior <- caida.emitidos.exterior[temp, ]
caida.emitidos.exterior[nrow(caida.emitidos.exterior)+1 , "PAÍS"] <- "DEMÁS 27 PAÍSES"
caida.emitidos.exterior[nrow(caida.emitidos.exterior) , 2] <- sum(exterior.paises$`DIFERENCIA EMITIDOS EVO POSITIVO`, na.rm = T) - sum(caida.emitidos.exterior[1:6, 2])
rio::export(caida.emitidos.exterior, "/Users/rafalopezv/Dropbox/R/analisis.electoral/21F/bases/caida.emitidos.exterior.csv")

# pérdida de apoyo evo en porentaje exterior
perdida.porcentaje.exterior <- exterior.paises %>% select(PAÍS, `EVO RE-ELECCIÓN`, 
                                                          `EVO ELECCIÓN NACIONAL`,
                                                          `DIFERENCIA EVO`,
                                                          `VÁLIDOS RE-ELECCIÓN`,
                                                          `VÁLIDOS ELECCIÓN NACIONAL`,
                                                          `EVO RE-ELECCIÓN %`,
                                                          `EVO ELECCIÓN NACIONAL %`)
perdida.porcentaje.exterior[nrow(perdida.porcentaje.exterior)+1, "PAÍS"] <- "TOTAL"
perdida.porcentaje.exterior[34, 2:8] <- colSums(perdida.porcentaje.exterior[,2:ncol(perdida.porcentaje.exterior)], na.rm = T)
perdida.porcentaje.exterior[34, "EVO RE-ELECCIÓN %"] <- round(perdida.porcentaje.exterior[34, "EVO RE-ELECCIÓN"]/
                                                                perdida.porcentaje.exterior[34, "VÁLIDOS RE-ELECCIÓN"] *
                                                                100,2)
perdida.porcentaje.exterior[34, "EVO ELECCIÓN NACIONAL %"] <- round(perdida.porcentaje.exterior[34, "EVO ELECCIÓN NACIONAL"]/
                                                                      perdida.porcentaje.exterior[34, "VÁLIDOS ELECCIÓN NACIONAL"] *
                                                                      100,2)

perdida.porcentaje.exterior[, "PÉRDIDA PORCENTAJES"] <- perdida.porcentaje.exterior[,7]- perdida.porcentaje.exterior[,8]
perdida.total.porcentaje <- perdida.porcentaje.exterior[34, "PÉRDIDA PORCENTAJES"]
colnames(perdida.total.porcentaje) <- "PERDIDA DE APOYO EN EL EXTERIOR"
rio::export(perdida.total.porcentaje, "/Users/rafalopezv/Dropbox/R/analisis.electoral/21F/bases/perdida.total.porcentaje.csv")

# PERDIDA DE APOYO EN EL EXTERIOR EN PORCENTAJE
temp <- perdida.porcentaje.exterior$PAÍS == "TOTAL"
perdida.afuera <- perdida.porcentaje.exterior[temp, c(7, 8)]
perdida.afuera <- perdida.afuera[, c(2,1)]
PROCESO <- c("ELECCIÓN NACIONAL", "REFERENDUM RE-ELECCIÓN")
APOYO <- c(72.29, 51.37)
perdida.afuera <- data.frame(PROCESO, APOYO)
rio::export(perdida.afuera, "/Users/rafalopezv/Dropbox/R/analisis.electoral/21F/bases/perdida.afuera.csv" )

# DIFERENCIA DE VOTOS EVO EN EL EXTERIOR
temp <- perdida.porcentaje.exterior$PAÍS == "TOTAL"
perdida.porcentaje.exterior1 <- perdida.porcentaje.exterior
perdida.porcentaje.exterior1[temp, "PAÍS"] <- "RESTO DE 27 PAÍSES"
temp1 <- perdida.porcentaje.exterior1$PAÍS != "ARGENTINA" & 
  perdida.porcentaje.exterior1$PAÍS != "BRASIL" &
  perdida.porcentaje.exterior1$PAÍS != "ESPAÑA" &
  perdida.porcentaje.exterior1$PAÍS != "ESTADOS UNIDOS" &
  perdida.porcentaje.exterior1$PAÍS != "ITALIA" &
  perdida.porcentaje.exterior1$PAÍS != "CHILE" &
  perdida.porcentaje.exterior1$PAÍS != "RESTO DE 27 PAÍSES" 
perdida.porcentaje.exterior1[34, 2:9] <- colSums(perdida.porcentaje.exterior1[temp1, 2:9])
temp2 <- perdida.porcentaje.exterior1$PAÍS == "ARGENTINA" |
  perdida.porcentaje.exterior1$PAÍS == "BRASIL" |
  perdida.porcentaje.exterior1$PAÍS == "ESPAÑA" |
  perdida.porcentaje.exterior1$PAÍS == "ESTADOS UNIDOS" |
  perdida.porcentaje.exterior1$PAÍS == "ITALIA" |
  perdida.porcentaje.exterior1$PAÍS == "CHILE" |
  perdida.porcentaje.exterior1$PAÍS == "RESTO DE 27 PAÍSES" 
perdida.porcentaje.exterior1 <- perdida.porcentaje.exterior1[temp2, ]
rio::export(perdida.porcentaje.exterior1, "/Users/rafalopezv/Dropbox/R/analisis.electoral/21F/bases/perdida.porcentaje.exterior1.csv")

# DIFERENCIA DE PORCENTAJE VOTOS EVO EN EL EXTERIOR
perdida.porcentaje.exterior2 <- perdida.porcentaje.exterior
temp <- perdida.porcentaje.exterior$PAÍS != "TOTAL"
perdida.porcentaje.exterior2 <- perdida.porcentaje.exterior2[temp,]
rio::export(perdida.porcentaje.exterior2, "/Users/rafalopezv/Dropbox/R/analisis.electoral/21F/bases/perdida.porcentaje.exterior2.csv")

# CAIDA en lugares estratégicos
clave <- municipal
temp <- clave$MUNICIPIO == "EL ALTO"|clave$MUNICIPIO == "POTOSÍ"|clave$PROVINCIA == "CHAPARE"
clave <- clave[temp, c(3,12,13,16, 17, 19, 21)]
clave[nrow(clave)+1, 1] <- "CHAPARE"
clave[nrow(clave), 2] <- sum(clave[c(1:3), 2])
clave[nrow(clave), 3] <- sum(clave[c(1:3), 3])
clave[nrow(clave), 4] <- sum(clave[c(1:3), 4])
clave[nrow(clave), 5] <- sum(clave[c(1:3), 5])
clave[nrow(clave), 7] <- round(clave[nrow(clave), 4]/clave[nrow(clave), 2]*100,2) 
clave[nrow(clave), 6] <- round(clave[nrow(clave), 5]/clave[nrow(clave), 3]*100,2) 
clave <- subset(clave, clave$MUNICIPIO == "EL ALTO" | clave$MUNICIPIO == "POTOSÍ"| clave$MUNICIPIO == "CHAPARE")
clave[, c(2,3)] <- NULL

a <- rio::import("/Users/rafalopezv/Dropbox/R/analisis.electoral/base.de.datos.editada.nivel.municipal/2009.nacionales/2009.nacionales.ejecutivo.csv")
temp <- a$MUNICIPIO == "EL ALTO"|a$MUNICIPIO == "POTOSÍ"|a$PROVINCIA == "CHAPARE"
a <- a[temp, c(3, 5, 11)]
a[nrow(a)+1, 1] <- "CHAPARE"
temp <- !is.na(a$MUNICIPIO)
a <- a[temp,]
a[6, 2] <- sum(a[c(1,2,3),2]) 
a[6, 3] <- sum(a[c(1,2,3),3]) 
a$`ELECCIÓN 2009%` <- round(a$`MAS-IPSP`/a$VÁLIDOS*100,2)
temp <- a$MUNICIPIO == "EL ALTO"|a$MUNICIPIO == "POTOSÍ"|a$MUNICIPIO == "CHAPARE"
a <- a[temp,]
a$VÁLIDOS <- NULL
clave <- merge(clave, a)
clave <- clave[, c(1,6, 3, 2, 7, 4, 5)]

colnames(clave) <- c("MUNICIPIO","ELECCIÓN NACIONAL 2009", "ELECCIÓN NACIONAL 2014", "REFERENDUM RE-ELECCIÓN 2016",
                     "ELECCIÓN NACIONAL 2009%", "ELECCIÓN NACIONAL 2014%", "REFERENDUM RE-ELECCIÓN 2016%")

clave1 <- t(clave) %>% as.data.frame()
clave1$PROCESO <- rownames(clave1)
rownames(clave1) <- NULL
temp <- clave1$V1 != "CHAPARE"
clave1 <- clave1[temp,]
nombres <- clave1[1,] %>% as.character()
clave1 <- clave1[,c(4,1,2,3)]
colnames(clave1) <- c("PROCESO", "CHAPARE", "EL ALTO", "POTOSÍ")
lapply(clave1, class)
clave2 <- clave1[c(1,2,3),]
clave3 <- clave1[c(4,5,6),]
clave2$CHAPARE <- c(87663, 102065, 84056) 
clave2$`EL ALTO` <- c(398682,363276,310114)
clave2$POTOSÍ <- c(53698,44671,15984)
clave2$PROCESO <- c("ELECCIÓN 2009","ELECCIÓN 2014","REFERENDUM 2016")

clave3$CHAPARE <- c(76.14,73.41,57.89)
clave3$`EL ALTO` <- c(87.42,72.01,58.10)
clave3$POTOSÍ <- c(56.51,42.36,14.13)
clave3$PROCESO <- c("ELECCIÓN 2009","ELECCIÓN 2014","REFERENDUM 2016")
rio::export(clave2, "/Users/rafalopezv/Dropbox/R/analisis.electoral/21F/bases/clave2.csv")
rio::export(clave3, "/Users/rafalopezv/Dropbox/R/analisis.electoral/21F/bases/clave3.csv")

# EXPORTAR LAS BASES DE DATOS
rio::export(agregado, "/Users/rafalopezv/Dropbox/R/analisis.electoral/21F/bases/agregado.csv")
rio::export(agregado.1, "/Users/rafalopezv/Dropbox/R/analisis.electoral/21F/bases/agregado1.csv")
rio::export(agregado.2, "/Users/rafalopezv/Dropbox/R/analisis.electoral/21F/bases/agregado2.csv")
rio::export(exterior, "/Users/rafalopezv/Dropbox/R/analisis.electoral/21F/bases/exterior.csv")
rio::export(exterior.paises, "/Users/rafalopezv/Dropbox/R/analisis.electoral/21F/bases/exterior.paises.csv")
rio::export(municipal, "/Users/rafalopezv/Dropbox/R/analisis.electoral/21F/bases/municipal.csv")


