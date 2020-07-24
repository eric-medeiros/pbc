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
  
  gravacoes <- read_excel(arquivo_campo_acustica,
                          sheet = 3, 
                          col_types = c("text", "date", "text", "date", "date",
                                        "text", "text", "text", "text", "text",
                                        "text", "text", "text", "text", "text",
                                        "text", "text", "text", "text", "text",
                                        "text")
  )
  
  assobios <- read_excel(arquivo_campo_acustica,
                          sheet = 4, 
                          col_types = c("text", "date", "text", "text", "text",
                                        "text", "text", "text", "text", "text",
                                        "text", "text", "text", "text", "text",
                                        "text", "text", "text", "text", "text",
                                        "text", "text", "text")
  )
  

  saidas$saida <- as.factor(saidas$saida)
  saidas$data <- ymd(saidas$data)
  saidas$barco <- as.factor(saidas$barco)
  saidas$WP_I_saida <- str_pad(saidas$WP_I_saida, 3, "left", "0")
  saidas$WP_F_saida <- str_pad(saidas$WP_F_saida, 3, "left", "0")
  saidas$area <- as.factor(saidas$area)
  saidas$equipe <- as.factor(saidas$equipe)
  saidas$litros_abastecidos <- as.double(saidas$litros_abastecidos)

  clima$saida <- as.factor(clima$saida)
  clima$data <- ymd(clima$data)
  clima$WP_I_clima <- str_pad(clima$WP_I_clima, 3, "left", "0")
  clima$WP_F_clima <- str_pad(clima$WP_F_clima, 3, "left", "0")
  clima$dir_vento <- as.factor(clima$dir_vento)
  clima$veloc_vento <- as.double(clima$veloc_vento)
  clima$beaufort <- as.factor(clima$beaufort)
  clima$cobert_nuvens <- as.factor(clima$cobert_nuvens)
  clima$visibilidade <- as.factor(clima$visibilidade)
  clima$reflexo <- as.factor(clima$reflexo)
  clima$coloracao_agua <- as.factor(clima$coloracao_agua)

  gravacoes$saida <- as.factor(gravacoes$saida) 
  gravacoes$data <- ymd(gravacoes$data)
  gravacoes$grupo <- as.factor(gravacoes$grupo)
  gravacoes$WP_I_grupo <- str_pad(gravacoes$WP_I_grupo, 3, "left", "0")
  gravacoes$WP_F_grupo <- str_pad(gravacoes$WP_F_grupo, 3, "left", "0")
  gravacoes$profund_m <- as.double(gravacoes$profund_m)
  gravacoes$estado <- as.factor(gravacoes$estado)
  gravacoes$coesao <- as.factor(gravacoes$coesao)
  gravacoes$tam_grupo  <- as.integer(gravacoes$tam_grupo)
  gravacoes$tam_min <- as.integer(gravacoes$tam_min)
  gravacoes$tam_max <- as.integer(gravacoes$tam_max)
  gravacoes$n_neonatos <- as.integer(gravacoes$n_neonatos)
  gravacoes$n_infantes <- as.integer(gravacoes$n_infantes)
  gravacoes$n_juvenis <- as.integer(gravacoes$n_juvenis)
  gravacoes$n_adultos <- as.integer(gravacoes$n_adultos)
  gravacoes$quant_embarc <- as.integer(gravacoes$quant_embarc)
  gravacoes$tipo_motor<- as.integer(gravacoes$tipo_motor)

  assobios$saida <- as.factor(assobios$saida)
  assobios$data <- ymd(assobios$data)
  assobios$grupo <- as.factor(assobios$grupo)
  assobios$assobios <- assobios$assobios
  assobios$AP <- as.double(assobios$AP)
  assobios$BT <- assobios$BT
  assobios$ET <- assobios$ET
  assobios$DT <- assobios$DT
  assobios$MT <- assobios$MT
  assobios$CF <- as.double(assobios$CF)
  assobios$LF <- as.double(assobios$LF)
  assobios$HF <- as.double(assobios$HF)
  assobios$FI <- as.double(assobios$FI)
  assobios$FF <- as.double(assobios$FF)
  assobios$DF <- as.double(assobios$DF)
  assobios$MF <- as.double(assobios$MF)
  assobios$PI <- as.integer(assobios$PI)
  assobios$Sobreposicao <- as.logical(assobios$Sobreposicao)
  assobios$Modulacao <- as.factor(assobios$Modulacao)
  assobios$Canal <- as.integer(assobios$Canal)
  
  
  #Junção de todas tablas em uma lista
  list_acu <- list(saidas = saidas,
                   clima = clima,
                   gravacoes = gravacoes,
                   assobios = assobios)
  
  invisible(list_acu)
  
  return(list_acu)
}
