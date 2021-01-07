# Função para abrir todas abas do arquivo *.xls de campo

le_planilha_L3 <- function(pasta_L3) {
  library(readxl)
  library(stringr)
  library(lubridate)
  
  arquivo_campo_L3 <- paste(pasta_L3, "/01_CAMPO/02_EXCEL/acustica_PBC.xlsx", sep = "")
  
  saidas <- read_excel(arquivo_campo_L3,
                       sheet = 1, 
                       col_types = c("text", "date", "text", "date", "date",
                                     "text", "text", "text", "text", "text",
                                     "text", "text")
  )
  
  clima <- read_excel(arquivo_campo_L3,
                      sheet = 2, 
                      col_types = c("text", "date", "date","date", "text", 
                                    "text", "text", "text", "text", "text",
                                    "text", "text", "text", "text")
  )
  
  estacoes <- read_excel(arquivo_campo_L3,
                         sheet = 3,
                         col_types = c("text", "date", "text", "date", "date",
                                       "text", "text", "text")
                         )
  
  gravacoes <- read_excel(arquivo_campo_L3,
                          sheet = 4, 
                          col_types = c("text", "date", "text", "text", "text",
                                        "text", "text")
  )
  
  embarcacoes <- read_excel(arquivo_campo_L3,
                            sheet = 5,
                            col_types = c("text", "date", "text", "text", "text",
                                          "text", "text", "text", "text", "text",
                                          "text")
  )
  
  assobios <- read_excel(arquivo_campo_L3,
                         sheet = 6, 
                         col_types = c("text", "date", "text", "text", "text",
                                       "text", "text", "text", "text", "text",
                                       "text", "text", "text", "text", "text",
                                       "text", "skip", "text", "text", "skip",
                                       "text", "text", "text", "text")
  )
  
  WP_extras <- read_excel(arquivo_campo_L3,
                          sheet = 7,
                          col_types = c("text", "date", "text", "date", "text",
                                        "text", "text", "text")
  )
  
  
  saidas$saida <- as.character(saidas$saida)
  saidas$data <- ymd(saidas$data)
  saidas$barco <- as.factor(saidas$barco)
  saidas$hora_I <- hms(str_sub(saidas$hora_I, 12, 19))
  saidas$hora_F <- hms(str_sub(saidas$hora_F, 12, 19))
  saidas$WP_I <- str_pad(saidas$WP_I, 3, "left", "0")
  saidas$WP_F <- str_pad(saidas$WP_F, 3, "left", "0")
  saidas$periodo <- as.factor(saidas$periodo)
  saidas$equipe <- as.character(saidas$equipe)
  saidas$barqueiro <- as.factor(saidas$barqueiro)
  saidas$litros_consumidos <- as.double(saidas$litros_consumidos)

  clima$saida <- as.factor(clima$saida)
  clima$data <- ymd(clima$data)
  clima$hora_I <- hms(str_sub(clima$hora_I, 12, 19))
  clima$hora_F <- hms(str_sub(clima$hora_F, 12, 19))
  clima$WP_I <- str_pad(clima$WP_I, 3, "left", "0")
  clima$WP_F <- str_pad(clima$WP_F, 3, "left", "0")
  clima$dir_vento <- as.factor(clima$dir_vento)
  clima$veloc_vento <- as.double(clima$veloc_vento)
  clima$beaufort <- as.factor(clima$beaufort)
  clima$cobert_nuvens <- as.factor(clima$cobert_nuvens)
  clima$visibilidade <- as.factor(clima$visibilidade)
  clima$reflexo <- as.factor(clima$reflexo)
  clima$coloracao_agua <- as.factor(clima$coloracao_agua)
  
  estacoes$saida <- as.factor(estacoes$saida)
  estacoes$data <- ymd(estacoes$data)
  estacoes$estacao <- as.factor(estacoes$estacao)
  estacoes$hora_I <- hms(str_sub(estacoes$hora_I, 12, 19))
  estacoes$hora_F <- hms(str_sub(estacoes$hora_F, 12, 19))
  estacoes$WP_I <- str_pad(estacoes$WP_I, 3, "left", "0")
  estacoes$WP_F <- str_pad(estacoes$WP_F, 3, "left", "0")

  gravacoes$saida <- as.factor(gravacoes$saida) 
  gravacoes$data <- ymd(gravacoes$data)
  gravacoes$estacao <- as.factor(gravacoes$estacao)
  gravacoes$area <- as.factor(gravacoes$area)
  gravacoes$arquivo_wav <- str_c(gravacoes$arquivo_wav, ".wav", sep = "")
  gravacoes$profund_m <- as.double(gravacoes$profund_m)
  
  embarcacoes$saida <- as.factor(embarcacoes$saida) 
  embarcacoes$data <- ymd(embarcacoes$data)
  embarcacoes$estacao <- as.factor(embarcacoes$estacao)
  embarcacoes$arquivo_wav <- str_c(embarcacoes$arquivo_wav, ".wav", sep = "")
  embarcacoes$tipo_emb <- as.factor(embarcacoes$tipo_emb)
  embarcacoes$finalid_emb <- as.factor(embarcacoes$finalid_emb)
  embarcacoes$potencia <- as.integer(embarcacoes$potencia)
  embarcacoes$tipo_motor <- as.factor(embarcacoes$tipo_motor)
  embarcacoes$distancia_m <- as.factor(embarcacoes$distancia_m)
  embarcacoes$registro_na_gravacao <- ms(embarcacoes$registro_na_gravacao)
  
  assobios$saida <- as.factor(assobios$saida)
  assobios$data <- ymd(assobios$data)
  assobios$estacao <- as.factor(assobios$estacao)
  assobios$arquivo_wav <- str_c(assobios$arquivo_wav, ".wav", sep = "")
  assobios$assobios <- as.factor(assobios$assobios)
  assobios$canal <- as.integer(assobios$canal)  
  assobios$BT <- assobios$BT
  assobios$ET <- assobios$ET
  assobios$LF <- as.double(assobios$LF)
  assobios$HF <- as.double(assobios$HF)
  assobios$MF <- as.double(assobios$MF)
  assobios$CF <- as.double(assobios$CF)
  assobios$APD <- as.double(assobios$APD)
  assobios$DT <- as.double(assobios$DT)
  assobios$DF <- as.double(assobios$DF)
  assobios$PF <- as.double(assobios$PF)
  assobios$FI <- as.double(assobios$FI)
  assobios$FF <- as.double(assobios$FF)
  assobios$Modulacao <- as.factor(assobios$Modulacao)
  assobios$PI <- as.integer(assobios$PI)
  assobios$Sobreposicao <- (as.factor(assobios$Sobreposicao))

  WP_extras$saida <- as.character(WP_extras$saida)
  WP_extras$data <- ymd(WP_extras$data)
  WP_extras$aba <- as.factor(WP_extras$aba)
  WP_extras$datahora_extra <- ymd_hm(paste(WP_extras$data, str_sub(WP_extras$hora_extra, 12, 16))) + hours(3)
  WP_extras$hora_extra <- NULL
  WP_extras$WP_extra <- str_pad(WP_extras$WP_extra, 3, "left", "0")
  WP_extras$lng_extra <- as.numeric(WP_extras$lng_extra)
  WP_extras$lat_extra <- as.numeric(WP_extras$lat_extra)
  WP_extras <- WP_extras %>%
    select(c(1:4, 8, 5:7))
  
  
  #Junção de todas tablas em uma lista
  lista_L3 <- list(saidas = saidas,
                   clima = clima,
                   estacoes = estacoes,
                   gravacoes = gravacoes,
                   embarcacoes = embarcacoes,
                   assobios = assobios,
                   WP_extras = WP_extras)
  
  invisible(lista_L3)
  
  return(lista_L3)
}
