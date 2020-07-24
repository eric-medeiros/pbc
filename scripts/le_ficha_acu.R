# Função para abrir todas abas do arquivo *.xls de campo

le_ficha_acu <- function(pasta_EXCEL_acu) {
  library(readxl)
  library(stringr)
  library(lubridate)
  
  arquivo_campo_acustica <- paste(pasta_EXCEL_acu, "/acustica_PBC.xlsx", sep = "")
  
  saidas <- read_excel(arquivo_campo_acustica,
                       sheet = 1, 
                       col_types = c("text", "date", "text", "date", "date",
                                     "text", "text", "text", "text", "text",
                                     "text")
  )
  
  clima <- read_excel(arquivo_campo_acustica,
                      sheet = 2, 
                      col_types = c("text", "date", "date","date", "text", 
                                    "text", "text", "text", "text", "text",
                                    "text", "text", "text", "text")
  )
  
  estacoes <- read_excel(arquivo_campo_acustica,
                         sheet = 3,
                         col_types = c("text", "date", "text", "date", "date",
                                       "text", "text", "text")
                         )
  
  gravacoes <- read_excel(arquivo_campo_acustica,
                          sheet = 4, 
                          col_types = c("text", "date", "text", "text", "text",
                                        "text", "text", "text", "text")
  )
  
  grupos <- read_excel(arquivo_campo_acustica,
                       sheet = 5,
                       col_types = c("text", "date", "text", "text", "text",
                                     "date", "date", "text", "text", "text",
                                     "text", "text", "text", "text", "text",
                                     "text", "text", "text", "text")
  )
  
  embarcacoes <- read_excel(arquivo_campo_acustica,
                            sheet = 6,
                            col_types = c("text", "date", "text", "text", "text",
                                          "text", "date", "text", "text", "text",
                                          "text", "text", "text", "text", "text",
                                          "text")
  )
  
  assobios <- read_excel(arquivo_campo_acustica,
                         sheet = 7, 
                         col_types = c("text", "date", "text", "text", "text",
                                       "text", "text", "text", "text", "text",
                                       "text", "text", "text", "text", "text",
                                       "text", "text", "text", "text", "text",
                                       "text", "text", "text", "text")
  )
  
  saidas$saida <- as.factor(saidas$saida)
  saidas$data <- ymd(saidas$data)
  saidas$barco <- as.factor(saidas$barco)
  saidas$hora_I_saida <- hms(str_sub(saidas$hora_I_saida, 12, 19))
  saidas$hora_F_saida <- hms(str_sub(saidas$hora_F_saida, 12, 19))
  saidas$WP_I_saida <- str_pad(saidas$WP_I_saida, 3, "left", "0")
  saidas$WP_F_saida <- str_pad(saidas$WP_F_saida, 3, "left", "0")
  saidas$periodo <- as.factor(saidas$periodo)
  saidas$equipe <- as.factor(saidas$equipe)
  saidas$litros_abastecidos <- as.double(saidas$litros_abastecidos)

  clima$saida <- as.factor(clima$saida)
  clima$data <- ymd(clima$data)
  clima$hora_I_clima <- hms(str_sub(clima$hora_I_clima, 12, 19))
  clima$hora_F_clima <- hms(str_sub(clima$hora_F_clima, 12, 19))
  clima$WP_I_clima <- str_pad(clima$WP_I_clima, 3, "left", "0")
  clima$WP_F_clima <- str_pad(clima$WP_F_clima, 3, "left", "0")
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
  estacoes$hora_I_estacao <- hms(str_sub(estacoes$hora_I_estacao, 12, 19))
  estacoes$hora_F_estacao <- hms(str_sub(estacoes$hora_F_estacao, 12, 19))
  estacoes$WP_I_estacao <- str_pad(estacoes$WP_I_estacao, 3, "left", "0")
  estacoes$WP_F_estacao <- str_pad(estacoes$WP_F_estacao, 3, "left", "0")

  gravacoes$saida <- as.factor(gravacoes$saida) 
  gravacoes$data <- ymd(gravacoes$data)
  gravacoes$estacao <- as.factor(gravacoes$estacao)
  gravacoes$arq <- as.factor(gravacoes$arq)
  gravacoes$area <- as.factor(gravacoes$area)
  gravacoes$WP_arq <- str_pad(gravacoes$WP_arq, 3, "left", "0")
  gravacoes$arq_wav <- str_c(gravacoes$arq_wav, ".wav", sep = "")
  gravacoes$profund_m <- as.double(gravacoes$profund_m)
  
  grupos$saida <- as.factor(grupos$saida)
  grupos$data <- ymd(grupos$data)
  grupos$estacao <- as.factor(grupos$estacao)
  grupos$arq_wav <- str_c(grupos$arq_wav, ".wav", sep = "")
  grupos$grupo <- as.factor(grupos$grupo)
  grupos$hora_I_G <- hms(str_sub(grupos$hora_I_G, 12, 19))
  grupos$hora_F_G <- hms(str_sub(grupos$hora_F_G, 12, 19))
  grupos$WP_I_G <- str_pad(grupos$WP_I_G, 3, "left", "0")
  grupos$WP_F_G <- str_pad(grupos$WP_F_G, 3, "left", "0")
  grupos$estado <- as.factor(grupos$estado)
  grupos$coesao <- as.factor(grupos$coesao)
  grupos$tam_grupo  <- as.integer(grupos$tam_grupo)
  grupos$tam_min <- as.integer(grupos$tam_min)
  grupos$tam_max <- as.integer(grupos$tam_max)
  grupos$n_neonatos <- as.integer(grupos$n_neonatos)
  grupos$n_infantes <- as.integer(grupos$n_infantes)
  grupos$n_juvenis <- as.integer(grupos$n_juvenis)
  grupos$n_adultos <- as.integer(grupos$n_adultos)
  
  embarcacoes$saida <- as.factor(embarcacoes$saida) 
  embarcacoes$data <- ymd(embarcacoes$data)
  embarcacoes$estacao <- as.factor(embarcacoes$estacao)
  embarcacoes$arq <- as.factor(embarcacoes$arq)
  embarcacoes$emb <- as.factor(embarcacoes$emb)
  embarcacoes$WP_emb <- str_pad(embarcacoes$WP_emb, 3, "left", "0")
  embarcacoes$hora_emb <- hms(str_sub(embarcacoes$hora_emb, 12, 19)) 
  embarcacoes$ID_emb <- as.factor(embarcacoes$ID_emb)
  embarcacoes$tipo_emb <- as.factor(embarcacoes$tipo_emb)
  embarcacoes$finalid_emb <- as.factor(embarcacoes$finalid_emb)
  embarcacoes$quant_emb <- as.integer(embarcacoes$quant_emb)
  embarcacoes$potenc_hp_motor <- as.integer(embarcacoes$potenc_hp_motor)
  embarcacoes$tipo_motor <- as.factor(embarcacoes$tipo_motor)
  embarcacoes$registro_na_gravacao <- ms(embarcacoes$registro_na_gravacao)
  
  assobios$saida <- as.factor(assobios$saida)
  assobios$data <- ymd(assobios$data)
  assobios$estacao <- as.factor(assobios$estacao)
  assobios$pasta <- as.factor(assobios$pasta)
  assobios$arq_wav <- as.factor(assobios$arq_wav)
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
  assobios$MT <- as.double(assobios$MT)
  assobios$FI <- as.double(assobios$FI)
  assobios$FF <- as.double(assobios$FF)
  assobios$Modulacao <- as.factor(assobios$Modulacao)
  assobios$PI <- as.integer(assobios$PI)
  assobios$Sobreposicao <- as.logical(as.numeric(assobios$Sobreposicao))

  
  
  #Junção de todas tablas em uma lista
  list_acu <- list(saidas = saidas,
                   clima = clima,
                   estacoes = estacoes,
                   gravacoes = gravacoes,
                   grupos = grupos,
                   embarcacoes = embarcacoes,
                   assobios = assobios)
  
  invisible(list_acu)
  
  return(list_acu)
}
