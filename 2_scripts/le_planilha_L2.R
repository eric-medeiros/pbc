# Função para abrir todas abas do arquivo *.xls de campo

le_planilha_L2 <- function(pasta_L2) {
  library(readxl)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(data.table)
  
  arquivo_campo_L2 <- paste(pasta_L2, "/01_CAMPO/02_EXCEL/comportamento_PBC.xlsx", sep = "")
  
  saidas <- read_excel(arquivo_campo_L2, sheet = 1, 
                       col_types = c("text", "text", "text", "text", "text", 
                                     "text", "text", "text", "text", "text"))
  
  amostragens <- read_excel(arquivo_campo_L2, sheet = 2, 
                            col_types = c("text", "text", "text", "text", "text", 
                                          "text"))
  
  clima <- read_excel(arquivo_campo_L2, sheet = 3, 
                      col_types = c("text", "text", "text", "text", "text", 
                                    "text", "text", "text", "text", "text", 
                                    "text"))
  
  avistagens <- read_excel(arquivo_campo_L2, sheet = 4, 
                           col_types = c("text", "text", "text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "text", "text", "text"))
  
  embarcacoes <- read_excel(arquivo_campo_L2, sheet = 5, 
                              col_types = c("text", "text", "text", "text", "text",
                                            "text", "text", "text", "text", "text",
                                            "text", "text", "text", "text", "text"))
  
  comportamento <- read_excel(arquivo_campo_L2, sheet = 6, 
                              col_types = c("text", "text", "text", "text", "text",
                                            "text", "text", "text", "text", "text",
                                            "text", "text", "text"))
  
  WP_extras <- read_excel(arquivo_campo_L2, sheet = 7,
                          col_types = c("text", "text", "text", "text", "text",
                                        "text", "text", "text"))
  
  
  saidas$saida <- as.character(saidas$saida)
  saidas$data <- dmy(saidas$data)
  saidas$barco <- as.character(saidas$barco)
  saidas$WP_I <- str_pad(saidas$WP_I, 3, "left", "0")
  saidas$WP_F <- str_pad(saidas$WP_F, 3, "left", "0")
  saidas$rota <- as.character(saidas$rota)
  saidas$equipe <- as.character(saidas$equipe)
  saidas$barqueiro <- as.character(saidas$barqueiro)
  saidas$litros_consumidos <- as.double(saidas$litros_consumidos)
  
  amostragens$saida <- as.character(amostragens$saida)
  amostragens$data <- dmy(amostragens$data)
  amostragens$rota <- as.character(amostragens$rota)
  amostragens$WP_I <- str_pad(amostragens$WP_I, 3, "left", "0")
  amostragens$WP_F <- str_pad(amostragens$WP_F, 3, "left", "0")
  
  clima$saida <- as.character(clima$saida)
  clima$data <- dmy(clima$data)
  clima$WP_I <- str_pad(clima$WP_I, 3, "left", "0")
  clima$WP_F <- str_pad(clima$WP_F, 3, "left", "0")
  clima$dir_vento <- as.character(clima$dir_vento)
  clima$veloc_vento <- as.double(clima$veloc_vento)
  clima$beaufort <- as.numeric(clima$beaufort)
  clima$cobert_nuvens <- as.numeric(clima$cobert_nuvens)
  clima$visibilidade <- as.numeric(clima$visibilidade)
  clima$reflexo <- as.numeric(clima$reflexo)
  
  avistagens$saida <- as.character(avistagens$saida)
  avistagens$data <- dmy(avistagens$data)
  avistagens$grupo <- as.integer(avistagens$grupo)
  avistagens$agrupamento <- as.logical(nafill(as.numeric(avistagens$agrupamento), fill = 0))
  avistagens$agrupamento_embarcacao <- as.logical(nafill(as.numeric(avistagens$agrupamento_embarcacao), fill = 0))
  avistagens$WP_I <- str_pad(avistagens$WP_I, 3, "left", "0")
  avistagens$WP_F <- str_pad(avistagens$WP_F, 3, "left", "0")
  avistagens$fotoID <- as.character(avistagens$fotoID)
  avistagens$coesao_grupo <- as.character(avistagens$coesao_grupo)
  avistagens$coesao_af <- as.character(avistagens$coesao_af)
  avistagens$tam_grupo <- as.integer(avistagens$tam_grupo)
  avistagens$tam_min <- as.integer(avistagens$tam_min)
  avistagens$tam_max <- as.integer(avistagens$tam_max)
  avistagens$n_neonatos <- as.integer(avistagens$n_neonatos)
  avistagens$n_infantes <- as.integer(avistagens$n_infantes)
  avistagens$n_juvenis <- as.integer(avistagens$n_juvenis)
  avistagens$n_adultos <- as.integer(avistagens$n_adultos)
  
  embarcacoes$saida <- as.character(embarcacoes$saida)
  embarcacoes$data <- dmy(embarcacoes$data)
  embarcacoes$grupo <- as.integer(embarcacoes$grupo)
  embarcacoes$barco <- as.integer(embarcacoes$barco)
  embarcacoes$motor <- as.character(embarcacoes$motor)
  embarcacoes$tipo <- as.character(embarcacoes$tipo)
  embarcacoes$tempo <- as.character(embarcacoes$tempo)
  embarcacoes$distancia <- as.numeric(embarcacoes$distancia)
  embarcacoes$velocidade <- as.character(embarcacoes$tempo)
  embarcacoes$mud_dir <- as.integer(embarcacoes$mud_dir)
  embarcacoes$angulacao <- as.integer(embarcacoes$angulacao)
  embarcacoes$mud_coe_af <- as.integer(embarcacoes$mud_coe_af)
  embarcacoes$mud_tam_gru <- as.integer(embarcacoes$mud_tam_gru)
  embarcacoes$mud_comp_gru <- as.integer(embarcacoes$mud_comp_gru)
  
  comportamento$saida <- as.character(comportamento$saida)
  comportamento$data <- dmy(comportamento$data)
  comportamento$grupo <- as.integer(comportamento$grupo)
  comportamento$na <- as.integer(comportamento$na)
  comportamento$dois_grupos <- as.integer(comportamento$dois_grupos)
  comportamento$varios_grupos <- as.integer(comportamento$varios_grupos)
  comportamento$cre <- as.integer(comportamento$cre)
  comportamento$esc <- as.integer(comportamento$esc)
  comportamento$cond <- as.integer(comportamento$cond)
  comportamento$rvz <- as.integer(comportamento$rvz)
  comportamento$int <- as.integer(comportamento$int)
  comportamento$bar <- as.integer(comportamento$bar)

  WP_extras$saida <- as.character(WP_extras$saida)
  WP_extras$data <- dmy(WP_extras$data)
  WP_extras$aba <- as.character(WP_extras$aba)
  WP_extras$datahora_extra <- ymd_hm(paste(WP_extras$data, WP_extras$hora_extra)) + hours(3)
  WP_extras$hora_extra <- NULL
  WP_extras$WP_extra <- str_pad(WP_extras$WP_extra, 3, "left", "0")
  WP_extras$lng_extra <- as.numeric(WP_extras$lng_extra)
  WP_extras$lat_extra <- as.numeric(WP_extras$lat_extra)
  WP_extras <- WP_extras %>%
    dplyr::select(c(1:4, 8, 5:7))
  
  
  #Junção de todas tablas em uma lista
  lista_L2 <- list(saidas = saidas,
                   amostragens = amostragens,
                   clima = clima,
                   avistagens = avistagens,
                   embarcacoes = embarcacoes,
                   comportamento = comportamento,
                   WP_extras = WP_extras)
  
  invisible(lista_L2)
  
  return(lista_L2)
  
}
