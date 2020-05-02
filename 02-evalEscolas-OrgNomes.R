# Identificar as escolas da região

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Bibliotecas -------------------------------------------------------------
#install.packages("readxl")
library(readxl)
library(stringr)

# Função ------------------------------------------------------------------
primMaiusc <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep = "", collapse = " ")
}

movePalavra <- function(x) {
  listaPalavras <- c("professor", "doutor", "dona", "general", "pastor", 
                     "vereador", "prefeito", "padre", "barao", "conselheiro",
                     "jornalista", "escritor", "dom",
                     "conego", "deputado", "reverendo", "dr",
                     "coronel", "monsenhor", "governador", "prof")
  
  listaPalavras <- c(listaPalavras, paste0(listaPalavras,"a"))
  
  palavras <- unlist(strsplit(x, split = " "))
  
  condicao <- sum(palavras %in% listaPalavras) == 1
  if (condicao) {
    
    novaSeq <- c(length(palavras), 1:(length(palavras) - 1))
    palavras <- palavras[novaSeq]
    x <- paste(palavras, sep = "", collapse = " ")
    
  } else if (sum(palavras %in% listaPalavras) > 1) {
    # Caso de professor doutor
    
    novaSeq <- c((length(palavras) - 1), length(palavras), 
                 1:(length(palavras) - 2))
    palavras <- palavras[novaSeq]
    x <- paste(palavras, sep = "", collapse = " ")
    
  } else {
    x <- x
  }
}

orgSigla <- function(x) {
  
  palavras <- unlist(strsplit(x, " "))
  posicao <- grep("adolesc", palavras)
  completo <- "centro atend socio-educ adolescente"
  palavras <- paste(palavras[(posicao + 1):length(palavras)],
                    sep = "", collapse = " ")
  x <- paste(completo, palavras, collapse = " ")
  
}

# Caminhos ----------------------------------------------------------------
caminhoDados <- "../dados/"

# Read data ---------------------------------------------------------------
# Escolas em SP
sheets <- list.files(path = caminhoDados, pattern = "*.xlsx", full.names = TRUE)

for (sheet in sheets) {
  
  if (sheet == sheets[1]) {
    escolas <- read_excel(sheet, sheet = 1)
    
  } else {
    tempVar <- read_excel(sheet, sheet = 1)
    
    escolas <- rbind(escolas, tempVar)
    rm(tempVar)
  }
  
}

colnames(escolas) <- tolower(colnames(escolas))
head(escolas)

# clem é o código para o número de turmas do EM
condicao <- (escolas$clem > 0) | (escolas$clejaem > 0) | (escolas$clteleem  > 0)
escolas <- escolas[condicao,]

escolas <- escolas[,c("mun", "tipoesc", "nomesc", "endesc", "numesc", 
                      "complend", "baiesc", "codcep", "compcep", "ddd",
                      "fone1", "fone2", "email", "alem", "alprof",
                      "alejaem", "alteleem")]

escolas <- apply(escolas, 2, tolower)
escolas <- apply(escolas, 2, chartr, old = 'çáãâíóõôéêú', new = 'caaaioooeeu')

escolas <- as.data.frame(escolas)

# Selecionar escolas dos municipios da regiao

municipios <- c("campinas", "paulinia" , "jaguariuna", "valinhos", "vinhedo",
                "sumare", "hortolandia", "nova odessa", "monte mor")

escolas <- escolas[escolas$mun %in% municipios,]
row.names(escolas) <- NULL
# Renomear ----------------------------------------------------------------
# Remover escolas técnicas
condicao <- str_trim(escolas$alprof) == "0"
escolas <- escolas[condicao,]
escolas$alprof <- NULL

unique(escolas$tipoesc)
unique(escolas$mun)

# Escolas tipo 08 - EE Escola Estadual
condicao <- escolas$tipoesc == "08"
#sort(escolas$nomesc[condicao])
#escolas$nomesc[condicao]

escolas$nomeFormat[condicao] <- sapply(escolas$nomesc[condicao], movePalavra)
escolas$nomeFormat[condicao] <- sapply(escolas$nomeFormat[condicao], primMaiusc)

condicao <- escolas$nomesc == "dom barreto"
palavras <- unlist(strsplit(escolas$nomeFormat[condicao], split = " "))
palavras <- palavras[c(2,1)]
escolas$nomeFormat[condicao] <- paste(palavras, sep = "", collapse = " ")

# Escolas tipo 34 - Fundação Casa
condicao <- escolas$tipoesc == "34"
#sort(escolas$nomesc[condicao])

escolas$nomeFormat[condicao] <- sapply(escolas$nomesc[condicao], orgSigla)
escolas$nomeFormat[condicao] <- sapply(escolas$nomeFormat[condicao], primMaiusc)

# Escolas tipo 00 - Desconhecido --------- PAREI AQUI
condicao <- escolas$tipoesc == "00"
#sort(escolas$nomesc[condicao])

# Mais simples editar manualmente
condicao2 <- escolas$nomesc == "andre franco montoro professor em"
escolas$nomeFormat[condicao2] <- "professor andre franco montoro (EM)"

condicao2 <- escolas$nomesc == "angelo corassa filho vereador emefm"
escolas$nomeFormat[condicao2] <- "vereador angelo corassa filho (EMEFM)"

condicao2 <- escolas$nomesc == "jose de anchieta escola municipal"
escolas$nomeFormat[condicao2] <- "jose de anchieta (EM)"

condicao2 <- escolas$nomesc == "leandro franceschini doutor escola municipal"
escolas$nomeFormat[condicao2] <- "doutor leandro franceschini (EM)"

condicao2 <- escolas$nomesc == "marcelino pietrobom maestro emefm"
escolas$nomeFormat[condicao2] <- "maestro marcelino pietrobom (EMEFM)"

condicao2 <- escolas$nomesc == "vinhedo centro municipal de ensino supletivo fund e medio"
escolas$nomeFormat[condicao2] <- "centro mun de ens supletivo fund e med vinhedo"

escolas$nomeFormat[condicao] <- sapply(escolas$nomeFormat[condicao], primMaiusc)

# Municipio
escolas$mun <- sapply(escolas$mun, primMaiusc)

# Municipio + Escola
escolas$cidNome <- paste(escolas$mun, escolas$nomeFormat, sep = " - ")

# Arrumar CEP -------------------------------------------------------------

escolas$cep <- paste(escolas$codcep, escolas$compcep, sep = "-")
escolas$codcep <- NULL
escolas$compcep <- NULL

# Endereço completo -------------------------------------------------------

escolas$endComp <- paste(escolas$endesc, escolas$numesc, escolas$complend,
                         escolas$mun, sep = ", ")

# Salvar tabela -----------------------------------------------------------

write.csv(escolas, '../dados/cadastroEscolas.csv')

# Endereço completo -------------------------------------------------------

escolas$endComp <- paste(escolas$nomeFormat, escolas$endesc, escolas$mun,
                         sep = ", ")

# Salvar ------------------------------------------------------------------

save(escolas, file = '../dados/cadastroEscolas.RData')