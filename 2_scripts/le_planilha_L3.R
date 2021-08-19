# Função para abrir todas abas do arquivo *.xls de campo

le_planilha_L3 <- function(pasta_L3) {
  
  library(readxl)
  library(dplyr)
  library(stringr)
  library(lubridate)
  
  arquivo_campo_L3 <- paste(pasta_L3, "/01_CAMPO/02_EXCEL/acustica_PBC.xlsx", sep = "")
  
  saidas <- read_excel(arquivo_campo_L3, sheet = 1, 
                       col_types = c("text", "date", "text", "skip", "skip",
                                     "text", "text", "text", "text", "text",
                                     "text", "text"))
  
  clima <- read_excel(arquivo_campo_L3, sheet = 2, 
                      col_types = c("text", "date", "skip", "skip", "text", 
                                    "text", "text", "text", "text", "text",
                                    "text", "text", "text", "text"))
  
  estacoes <- read_excel(arquivo_campo_L3, sheet = 3,
                         col_types = c("text", "date", "text", "skip", "skip",
                                       "text", "text", "text"))
  
  gravacoes <- read_excel(arquivo_campo_L3, sheet = 4, 
                          col_types = c("text", "date", "text", "text", "text",
                                        "text", "text"))
  
  embarcacoes <- read_excel(arquivo_campo_L3, sheet = 5,
                            col_types = c("text", "date", "text", "text", "text",
                                          "text", "text", "text", "text", "text",
                                          "text"))
  
  WP_extras <- read_excel(arquivo_campo_L3, sheet = 6,
                          col_types = c("text", "date", "text", "text", "text",
                                        "text", "text", "text"))
  
  
  saidas$saida <- as.character(saidas$saida)
  saidas$data <- ymd(saidas$data)
  saidas$barco <- as.character(saidas$barco)
  saidas$WP_I <- str_pad(saidas$WP_I, 3, "left", "0")
  saidas$WP_F <- str_pad(saidas$WP_F, 3, "left", "0")
  saidas$periodo <- as.character(saidas$periodo)
  saidas$equipe <- as.character(saidas$equipe)
  saidas$barqueiro <- as.character(saidas$barqueiro)
  saidas$litros_consumidos <- as.double(saidas$litros_consumidos)
  
  clima$saida <- as.character(clima$saida)
  clima$data <- ymd(clima$data)
  clima$WP_I <- str_pad(clima$WP_I, 3, "left", "0")
  clima$WP_F <- str_pad(clima$WP_F, 3, "left", "0")
  clima$dir_vento <- as.character(clima$dir_vento)
  clima$veloc_vento <- as.double(clima$veloc_vento)
  clima$beaufort <- as.integer(clima$beaufort)
  clima$cobert_nuvens <- as.integer(clima$cobert_nuvens)
  clima$visibilidade <- as.integer(clima$visibilidade)
  clima$reflexo <- as.integer(clima$reflexo)
  clima$coloracao_agua <- as.character(clima$coloracao_agua)
  
  estacoes$saida <- as.character(estacoes$saida)
  estacoes$data <- ymd(estacoes$data)
  estacoes$estacao <- as.integer(estacoes$estacao)
  estacoes$WP_I <- str_pad(estacoes$WP_I, 3, "left", "0")
  estacoes$WP_F <- str_pad(estacoes$WP_F, 3, "left", "0")
  
  gravacoes$saida <- as.character(gravacoes$saida) 
  gravacoes$data <- ymd(gravacoes$data)
  gravacoes$estacao <- as.integer(gravacoes$estacao)
  gravacoes$area <- as.integer(gravacoes$area)
  gravacoes$arquivo_wav <- as.character(gravacoes$arquivo_wav)
  gravacoes$profund_m <- as.double(gravacoes$profund_m)
  
  embarcacoes$saida <- as.character(embarcacoes$saida) 
  embarcacoes$data <- ymd(embarcacoes$data)
  embarcacoes$estacao <- as.integer(embarcacoes$estacao)
  embarcacoes$arquivo_wav <- str_c(embarcacoes$arquivo_wav, ".wav", sep = "")
  embarcacoes$tipo_emb <- as.character(embarcacoes$tipo_emb)
  embarcacoes$finalid_emb <- as.character(embarcacoes$finalid_emb)
  embarcacoes$potencia <- as.integer(embarcacoes$potencia)
  embarcacoes$tipo_motor <- as.character(embarcacoes$tipo_motor)
  embarcacoes$distancia_m <- as.numeric(embarcacoes$distancia_m)
  embarcacoes$registro_na_gravacao <- ms(embarcacoes$registro_na_gravacao)
  
  WP_extras$saida <- as.character(WP_extras$saida)
  WP_extras$data <- ymd(WP_extras$data)
  WP_extras$aba <- as.character(WP_extras$aba)
  WP_extras$datahora_extra <- ymd_hm(paste(WP_extras$data, WP_extras$hora_extra)) + hours(3)
  WP_extras$hora_extra <- NULL
  WP_extras$WP_extra <- str_pad(WP_extras$WP_extra, 3, "left", "0")
  WP_extras$lng_extra <- as.numeric(WP_extras$lng_extra)
  WP_extras$lat_extra <- as.numeric(WP_extras$lat_extra)
  WP_extras <- WP_extras %>%
    dplyr::select(c(1:4, 8, 5:7))
  
  
  #Junção de todas tablas em uma lista
  lista_L3 <- list(saidas = saidas,
                   clima = clima,
                   estacoes = estacoes,
                   gravacoes = gravacoes,
                   embarcacoes = embarcacoes,
                   WP_extras = WP_extras)
  
  invisible(lista_L3)
  
  return(lista_L3)
}
