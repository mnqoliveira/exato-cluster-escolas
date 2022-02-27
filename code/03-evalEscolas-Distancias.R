# Identificar as distancias entre as escolas da regi?o
# Extraido daqui:
# https://gist.github.com/josecarlosgonz/6417633

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Bibliotecas -------------------------------------------------------------
# install.packages(c("RCurl", "RJSONIO", "plyr", "sp", "rgdal"))
# Google Maps
library(RCurl)
library(RJSONIO)
library(plyr)

# Conversão de coordenadas
library(sp)
library(rgdal)


# Funcao ------------------------------------------------------------------

url <- function(address, return.call = "json", sensor = "false") {
  # root <- "http://maps.google.com/maps/api/geocode/"
  # u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  # return(URLencode(u))
  
  root <- "https://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, 
             
             # "&key=INSERT_KEY", 
             sep = "")
  #"&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if (verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if (x$status == "OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type  <- x$results[[1]]$geometry$location_type
    formatted_address  <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
    Sys.sleep(0.5)
  } else {
    return(c(NA,NA,NA, NA))
  }
}

# Caminhos ----------------------------------------------------------------
caminhoDados <- "../data/"
caminhoOut <- "../outputs/"

# Arquivos ----------------------------------------------------------------
load(paste0(caminhoDados,"cadastroEscolas.RData"))

# Etapas ------------------------------------------------------------------

enderecos <- paste(escolas$endComp, "SP - Brazil", sep = ", ")
coordenadas  <- ldply(enderecos, function(x) geoCode(x))
names(coordenadas)  <- c("lat","lon","location_type", "formatted")

coordCompleto <- cbind(escolas$nomeFormat, escolas$mun, escolas$alem,
                       coordenadas)
colnames(coordCompleto)  <- c("nome","cidade", "alEM",
                              "lat","lon","location_type", "formatted")
coordCompleto[is.na(coordCompleto["lat"]),1]

condicao <- coordCompleto$nome == "Parque Dos Servidores"
coordCompleto$lat[condicao] <- -22.78597
coordCompleto$lon[condicao] <- -47.17849

save(coordCompleto, file = paste0(caminhoDados, "coordEscolas.RData"))

load(paste0(caminhoOut, "coordEscolas.RData"))

# Clusterizacao -----------------------------------------------------------

# Remover colunas desnecessarias
coordCompleto <- coordCompleto[!is.na(coordCompleto[,"lat"]),]
remover <- c("location_type", "formatted")
remover <- match(remover, colnames(coordCompleto))
coordCompleto <- coordCompleto[,-remover]

# Remover a escola Barao Geraldo de Rezende porque nela nos nao vamos
condicao <- coordCompleto$nome != "Barao Geraldo De Rezende"
coordCompleto <- coordCompleto[condicao,]

# Manter as rotas apenas em Campinas e Paulina
condicao <- coordCompleto$cidade == "Campinas" | coordCompleto$cidade == "Paulinia"
coordCompleto <- coordCompleto[condicao,]

# Conversao das coordenadas
coords <- data.frame(ID = coordCompleto$nome, 
                     X = as.numeric(coordCompleto$lon), 
                     Y = as.numeric(coordCompleto$lat))
coordinates(coords) <- c("X", "Y")
proj4string(coords) <- CRS("+proj=longlat +datum=WGS84")
res <- spTransform(coords, CRS("+proj=utm +zone=23 ellps=WGS84"))

coordCompleto$latUTM <- res@coords[,2]
coordCompleto$lonUTM <- res@coords[,1]

clusters <- kmeans(coordCompleto[,c("latUTM", "lonUTM")], centers = 50)

coordCompleto$rota <- clusters$cluster

# rotasMultiplas <- coordCompleto$rota[duplicated(coordCompleto$rota)]
# rotas <- coordCompleto[coordCompleto$rota %in% rotasMultiplas,]

rotas <- coordCompleto

# Selecao pela distancia ate Unicamp --------------------------------------
# Converter em UTM para calculo da distancia
# Unicamp
latUni <- -22.8170106
longUni <- -47.0700645

coords <- data.frame(ID = "unicamp", X = longUni, Y = latUni)
coordinates(coords) <- c("X", "Y")
proj4string(coords) <- CRS("+proj=longlat +datum=WGS84")
res <- spTransform(coords, CRS("+proj=utm +zone=23 ellps=WGS84"))

latUniUTM <- res@coords[,2]
longUniUTM <- res@coords[,1]

rotas$dist <- sqrt((rotas$latUTM - latUniUTM)^2 + (rotas$lonUTM - longUniUTM)^2)
condicao <- ((rotas$dist < 15000 & rotas$cidade == "Paulinia") | 
               (rotas$dist < 10000))
rotasSel <- rotas$rota[condicao]

subRotas <- rotas[rotas$rota %in% rotasSel,]

write.csv(subRotas, paste0(caminhoOut, "coordEscolas.csv"))
