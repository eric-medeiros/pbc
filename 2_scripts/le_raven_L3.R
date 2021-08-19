le_raven_L3 <- function(pasta_L3)
{
  library(purrr)
  library(xlsx)
  library(stringr)
  library(lubridate)
  library(data.table)
  library(forcats)

  le_arquivo_raven <- function(arquivo_raven)
  {
    dados_raven <- read.delim(arquivo_raven,
                              sep = "\t")
    
    arrumado <- dados_raven[c(TRUE, FALSE, FALSE),]
    
    FI <- dados_raven[c(FALSE, TRUE, FALSE),"Low.Freq..Hz."]
    FF <- dados_raven[c(FALSE, FALSE, TRUE),"Low.Freq..Hz."]
    
    arrumado$FI <- FI
    arrumado$FF <- FF
    arrumado$arquivo_wav <- str_sub(arquivo_raven, -15, -5)
    arrumado$saida <- str_sub(arquivo_raven, -30, -28)
    arrumado$data <- as.character.Date(ymd(str_sub(arquivo_raven, -26, -17)))
    
    names(arrumado)[3] <- "canal"
    names(arrumado)[4] <- "BT"
    names(arrumado)[5] <- "ET"
    names(arrumado)[6] <- "LF"
    names(arrumado)[7] <- "HF"
    names(arrumado)[8] <- "MF"
    names(arrumado)[9] <- "CF"
    names(arrumado)[10] <- "APD"
    names(arrumado)[11] <- "DT"
    names(arrumado)[12] <- "DF"  
    names(arrumado)[13] <- "PF"
    names(arrumado)[14] <- "PT"
    names(arrumado)[15] <- "OBS"
    names(arrumado)[16] <- "PF_check"
    names(arrumado)[17] <- "modulacao"
    names(arrumado)[18] <- "PI"
    names(arrumado)[19] <- "sobreposicao"
    
    arrumado$PI <- str_replace_all(arrumado$PI, "0", "NA")
    
    arrumado <- arrumado[,c(23,24,22,3:14,20,21,16:19,15)]
    
    arrumado$canal <- as.character(arrumado$canal)
    arrumado$BT <- as.character(arrumado$BT)
    arrumado$ET <- as.character(arrumado$ET)
    arrumado$LF <- as.character(arrumado$LF)
    arrumado$HF <- as.character(arrumado$HF)
    arrumado$MF <- as.character(arrumado$MF)
    arrumado$CF <- as.character(arrumado$CF)
    arrumado$APD <- as.character(arrumado$APD)
    arrumado$DT <- as.character(arrumado$DT)
    arrumado$DF <- as.character(arrumado$DF)
    arrumado$PF <- as.character(arrumado$PF)
    arrumado$PT <- as.character(arrumado$PT)
    arrumado$FI <- as.character(arrumado$FI)
    arrumado$FF <- as.character(arrumado$FF)
    
    return(arrumado)
    
  }
  
  lista_arquivos_raven <- list.files(path = paste0(pasta_L3, "/03_ANALISES"),
                                     full.names = TRUE,
                                     pattern = "txt",
                                     recursive = TRUE)

  dados_raven <- map_dfr(lista_arquivos_raven, le_arquivo_raven)
  
  dados_raven$saida <- as.character(as.numeric(dados_raven$saida))
  dados_raven$data <- ymd(dados_raven$data)
  dados_raven$PF_check <- as_factor(dados_raven$PF_check)
  dados_raven$modulacao <- as_factor(dados_raven$modulacao)
  dados_raven$PI <- as.integer(dados_raven$PI)
  dados_raven$sobreposicao <- as_factor(dados_raven$sobreposicao)
  dados_raven$assobios <- row.names(dados_raven)
  
  dados_raven$canal <- as.integer(dados_raven$canal)
  dados_raven$BT <- as.double(dados_raven$BT)
  dados_raven$ET <- as.double(dados_raven$ET)
  dados_raven$LF <- as.double(dados_raven$LF)
  dados_raven$HF <- as.double(dados_raven$HF)
  dados_raven$MF <- as.double(dados_raven$MF)
  dados_raven$CF <- as.double(dados_raven$CF)
  dados_raven$APD <- as.double(dados_raven$APD)
  dados_raven$DT <- as.double(dados_raven$DT)
  dados_raven$DF <- as.double(dados_raven$DF)
  dados_raven$PF <- as.double(dados_raven$PF)
  dados_raven$PT <- as.double(dados_raven$PT)
  dados_raven$FI <- as.double(dados_raven$FI)
  dados_raven$FF <- as.double(dados_raven$FF)
  
  return(dados_raven)
  
}

