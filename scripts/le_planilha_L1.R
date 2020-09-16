# Função para abrir todas abas do arquivo *.xls de campo

le_planilha_L1 <- function(pasta_L1) {
  library(readxl)
  library(stringr)
  library(lubridate)

  arquivo_campo_L1 <- paste(pasta_L1, "/01_CAMPO/02_EXCEL/populacional_PBC.xlsx", sep = "")
  
  saidas <- read_excel(arquivo_campo_L1,
                       sheet = 1, 
                       col_types = c("text", "date", "text", "text", "text",
                                     "text", "text", "text", "text", "text")
  )
  
  amostragens <- read_excel(arquivo_campo_L1, 
                            sheet = 2, 
                            col_types = c("text", "date", "text", "text", "text", 
                                          "text")
  )
  
  clima <- read_excel(arquivo_campo_L1,
                      sheet = 3, 
                      col_types = c("text", "date", "text", "text", "text", 
                                    "text", "text", "text", "text", "text", 
                                    "text")
  )
  
  avistagens <- read_excel(arquivo_campo_L1,
                           sheet = 4, 
                           col_types = c("text", "date", "text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "text")
  )
  
  pausas <- read_excel(arquivo_campo_L1,
                      sheet = 5,
                      col_types = c("text", "date", "text", "text", "text")
  )
  
  WP_extras <- read_excel(arquivo_campo_L1,
                          sheet = 6,
                          col_types = c("text", "date", "text", "date", "text",
                                        "text", "text", "text")
  )
  
  fotos <- read_excel(arquivo_campo_L1,
                      sheet = 7, 
                      col_types = c("text", "date", "text", "text", "text", 
                                    "text", "text", "text", "text", "text")
  )
  
  saidas$saida <- as.character(saidas$saida)
  saidas$data <- ymd(saidas$data)
  saidas$barco <- as.factor(saidas$barco)
  saidas$WP_I <- str_pad(saidas$WP_I, 3, "left", "0")
  saidas$WP_F <- str_pad(saidas$WP_F, 3, "left", "0")
  saidas$rota <- as.factor(saidas$rota)
  saidas$equipe <- as.character(saidas$equipe)
  saidas$barqueiro <- as.factor(saidas$barqueiro)
  saidas$litros_consumidos <- as.numeric(saidas$litros_consumidos)
  
  amostragens$saida <- as.character(amostragens$saida)
  amostragens$data <- ymd(amostragens$data)
  amostragens$rota <- as.factor(amostragens$rota)
  amostragens$WP_I <- str_pad(amostragens$WP_I, 3, "left", "0")
  amostragens$WP_F <- str_pad(amostragens$WP_F, 3, "left", "0")
  
  clima$saida <- as.character(clima$saida)
  clima$data <- ymd(clima$data)
  clima$WP_I <- str_pad(clima$WP_I, 3, "left", "0")
  clima$WP_F <- str_pad(clima$WP_F, 3, "left", "0")
  clima$dir_vento <- as.factor(clima$dir_vento)
  clima$veloc_vento <- as.double(clima$veloc_vento)
  clima$beaufort <- as.factor(clima$beaufort)
  clima$cobert_nuvens <- as.factor(clima$cobert_nuvens)
  clima$visibilidade <- as.factor(clima$visibilidade)
  clima$reflexo <- as.factor(clima$reflexo)
  
  avistagens$saida <- as.character(avistagens$saida)
  avistagens$data <- ymd(avistagens$data)
  avistagens$grupo <- as.factor(avistagens$grupo)
  avistagens$num_fotos <- as.integer(avistagens$num_fotos)
  avistagens$WP_I <- str_pad(avistagens$WP_I, 3, "left", "0")
  avistagens$WP_F <- str_pad(avistagens$WP_F, 3, "left", "0")
  avistagens$coesao <- as.factor(avistagens$coesao)
  avistagens$estado <- as.factor(avistagens$estado)
  avistagens$tam_grupo <- as.integer(avistagens$tam_grupo)
  avistagens$tam_min <- as.integer(avistagens$tam_min)
  avistagens$tam_max <- as.integer(avistagens$tam_max)
  avistagens$n_neonatos <- as.factor(avistagens$n_neonatos)
  avistagens$n_infantes <- as.factor(avistagens$n_infantes)
  avistagens$n_juvenis <- as.factor(avistagens$n_juvenis)
  avistagens$n_adultos <- as.factor(avistagens$n_adultos)
  
  pausas$saida <- as.character(pausas$saida)
  pausas$data <- ymd(pausas$data)
  pausas$WP_I <- str_pad(pausas$WP_I, 3, "left", "0")
  pausas$WP_F <- str_pad(pausas$WP_F, 3, "left", "0")
  
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
  
  fotos$saida <- as.factor(fotos$saida)
  fotos$datahora <- ymd_hms(fotos$data)
  fotos$grupo <- as.factor(fotos$grupo)
  fotos$pasta <- as.factor(fotos$pasta)
  fotos$arquivo <- as.factor(fotos$arquivo)
  fotos$ID <- as.factor(fotos$ID)
  fotos$quali_F <- as.factor(fotos$quali_F)
  fotos$quali_M <- as.factor(fotos$quali_M)
  fotos$lado <- as.factor(fotos$lado)


  #Junção de todas tabelas em uma lista
  lista_L1 <- list(saidas = saidas,
                   amostragens = amostragens,
                   clima = clima,
                   avistagens = avistagens,
                   pausas = pausas,
                   WP_extras = WP_extras,
                   fotos = fotos)
  
  invisible(lista_L1)
  
  return(lista_L1)
}

