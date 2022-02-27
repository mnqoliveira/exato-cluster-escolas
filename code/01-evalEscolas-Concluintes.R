# Identificar de onde sao as escolas em que estudaram os alunos que concluíram
# o ano de 2016
# Dados oficiais utilizados para uniformizar os nomes

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------
#install.packages("readxl")
#install.packages("reshape2")
#install.packages("lubridate")
library(readxl)


# Caminhos ----------------------------------------------------------------
pathSheets <- "../data/"

# Read data ---------------------------------------------------------------
# Escolas em SP
sheets <- list.files(path = pathSheets, pattern = "*.xlsx", full.names = TRUE)

for (sheet in sheets) {
  
  if (sheet == sheets[1]) {
    escolas <- read_excel(sheet, sheet = 1)
    
  } else {
    tempVar <- read_excel(sheet, sheet = 1)
    
    escolas <- rbind(escolas, tempVar)
  }
  
}

colnames(escolas) <- tolower(colnames(escolas))
head(escolas)
tail(escolas)
escolas <- escolas[,c("mun", "tipoesc", "nomesc", "endesc", "numesc", 
                      "complend", "baiesc", "codcep", "compcep", "ddd",
                      "fone1", "fone2", "email")]

escolas$nomesc <- tolower(escolas$nomesc)
escolas$nomesc <- chartr('çáãâíóõôéêú', 'caaaioooeeu', escolas$nomesc)

# Escolas de origem dos alunos do Exato
escolasEvasao <- read.csv(paste0(pathSheets,"escolasConcluintes2016.csv"))
escolasEvasao <- escolasEvasao[,-1]
colnames(escolasEvasao) <- c("nome", "permanencia", "abandono")

escolasEvasao$nome <- tolower(escolasEvasao$nome)
escolasEvasao$nome <- chartr('çáãâíóõôéêú', 'caaaioooeeu', escolasEvasao$nome)

escolasEvasao$nomesc <- sapply(escolasEvasao$nome, 
                               function(x) grep(x, escolas$nomesc,
                                                value = TRUE)[1])

# Regiao das escolas ------------------------------------------------------
escCidade <- merge(escolasEvasao, escolas, by = "nomesc", all.x = TRUE)


# Organização final -------------------------------------------------------
escCidade <- escCidade[escCidade$mun != "RIBEIRAO PRETO",]
escCidade <- escCidade[,-match("nomesc", colnames(escCidade))]
escCidade <- escCidade[!is.na(escCidade$nome),]

write.csv(escCidade, '../outputs/escolasConcluintes_org.csv')
