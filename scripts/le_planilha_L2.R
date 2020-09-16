# Função para abrir todas abas do arquivo *.xls de campo

le_planilha_L2 <- function(pasta_L2) {
  library(readxl)
  library(stringr)
  library(lubridate)
  library(data.table)
  
  arquivo_campo_L2 <- paste(pasta_L2, "/01_CAMPO/02_EXCEL/comportamento_PBC.xlsx", sep = "")
  
  saidas <- read_excel(arquivo_campo_L2, 
                        sheet = 1, 
                        col_types = c("text", "date", "text", "text", "text", 
                                      "text", "text", "text", "text", "text")
  )
  
  amostragens <- read_excel(arquivo_campo_L2, 
                           sheet = 2, 
                           col_types = c("text", "date", "text", "text", "text", 
                                         "text")
  )
  
  clima <- read_excel(arquivo_campo_L2,
                       sheet = 3, 
                       col_types = c("text", "date", "text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text")
  )
  
  avistagens <- read_excel(arquivo_campo_L2,
                            sheet = 4, 
                            col_types = c("text", "date", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text")
  )
  
  comportamento <- read_excel(arquivo_campo_L2,
                               sheet = 5, 
                               col_types = c("text", "date", "text", "text", "text",
                                             "text", "text", "text", "text", "text",
                                             "text", "text", "text", "text", "text", 
                                             "text", "text")
  )
  
  WP_extras <- read_excel(arquivo_campo_L2,
                          sheet = 6,
                          col_types = c("text", "date", "text", "date", "text",
                                        "text", "text", "text")
  )
  
  
  saidas$saida <- as.factor(saidas$saida)
  saidas$data <- ymd(saidas$data)
  saidas$barco <- as.factor(saidas$barco)
  saidas$WP_I <- str_pad(saidas$WP_I, 3, "left", "0")
  saidas$WP_F <- str_pad(saidas$WP_F, 3, "left", "0")
  saidas$rota <- as.factor(saidas$rota)
  saidas$equipe <- as.factor(saidas$equipe)
  saidas$barqueiro <- as.factor(saidas$barqueiro)
  saidas$litros_consumidos <- as.double(saidas$litros_consumidos)
  
  amostragens$saida <- as.factor(amostragens$saida)
  amostragens$data <- ymd(amostragens$data)
  amostragens$rota <- as.factor(amostragens$rota)
  amostragens$WP_I <- str_pad(amostragens$WP_I, 3, "left", "0")
  amostragens$WP_F <- str_pad(amostragens$WP_F, 3, "left", "0")

  clima$saida <- as.factor(clima$saida)
  clima$data <- ymd(clima$data)
  clima$WP_I <- str_pad(clima$WP_I, 3, "left", "0")
  clima$WP_F <- str_pad(clima$WP_F, 3, "left", "0")
  clima$dir_vento <- as.factor(clima$dir_vento)
  clima$veloc_vento <- as.double(clima$veloc_vento)
  clima$beaufort <- as.factor(clima$beaufort)
  clima$cobert_nuvens <- as.factor(clima$cobert_nuvens)
  clima$visibilidade <- as.factor(clima$visibilidade)
  clima$reflexo <- as.factor(clima$reflexo)

  avistagens$saida <- as.factor(avistagens$saida)
  avistagens$data <- ymd(avistagens$data)
  avistagens$grupo <- as.factor(avistagens$grupo)
  avistagens$agrupamento <- as.logical(nafill(as.numeric(avistagens$agrupamento), fill = 0))
  avistagens$agrupamento_embarcacao <- as.logical(nafill(as.numeric(avistagens$agrupamento_embarcacao), fill = 0))
  avistagens$WP_I <- str_pad(avistagens$WP_I, 3, "left", "0")
  avistagens$WP_F <- str_pad(avistagens$WP_F, 3, "left", "0")
  avistagens$motor <- as.factor(avistagens$motor)
  avistagens$tipo <- as.factor(avistagens$tipo)
  avistagens$tempo <- as.factor(avistagens$tempo)
  avistagens$distancia <- as.factor(avistagens$distancia)
  avistagens$velocidade <- as.factor(avistagens$tempo)
  avistagens$fotoID <- as.factor(avistagens$fotoID)
  avistagens$coesao_grupo <- as.factor(avistagens$coesao_grupo)
  avistagens$coesao_af <- as.factor(avistagens$coesao_af)
  avistagens$tam_grupo <- as.integer(avistagens$tam_grupo)
  avistagens$tam_min <- as.integer(avistagens$tam_min)
  avistagens$tam_max <- as.integer(avistagens$tam_max)
  avistagens$n_neonatos <- as.factor(avistagens$n_neonatos)
  avistagens$n_infantes <- as.factor(avistagens$n_infantes)
  avistagens$n_juvenis <- as.factor(avistagens$n_juvenis)
  avistagens$n_adultos <- as.factor(avistagens$n_adultos)

  comportamento$saida <- as.factor(comportamento$saida)
  comportamento$data <- ymd(comportamento$data)
  comportamento$grupo <- as.factor(comportamento$grupo)
  comportamento$na <- as.integer(comportamento$na)
  comportamento$dois_grupos <- as.integer(comportamento$dois_grupos)
  comportamento$varios_grupos <- as.integer(comportamento$varios_grupos)
  comportamento$cre <- as.integer(comportamento$cre)
  comportamento$esc <- as.integer(comportamento$cre)
  comportamento$cond <- as.integer(comportamento$cre)
  comportamento$rvz <- as.integer(comportamento$cre)
  comportamento$int <- as.integer(comportamento$cre)
  comportamento$bar <- as.integer(comportamento$cre)
  comportamento$mud_dir <- as.integer(comportamento$mud_dir)
  comportamento$angulacao <- as.integer(comportamento$angulacao)
  comportamento$mud_coe_af <- as.integer(comportamento$mud_coe_af)
  comportamento$mud_tam_gru <- as.integer(comportamento$mud_tam_gru)  
  
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
  lista_L2 <- list(saidas = saidas,
                   amostragens = amostragens,
                   clima = clima,
                   avistagens = avistagens,
                   comportamento = comportamento,
                   WP_extras = WP_extras)
  
  invisible(lista_L2)
  
  return(lista_L2)
  
}
