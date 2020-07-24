# Função para abrir todas abas do arquivo *.xls de campo
le_ficha_pop <- function(pasta_EXCEL_pop) {
  library(readxl)
  library(stringr)
  library(lubridate)
  
  arquivo_campo_pop <- paste(pasta_EXCEL_pop, "/populacional_PBC.xlsx", sep = "")
  
  saidas <- read_excel(arquivo_campo_pop,
                       sheet = 1, 
                       col_types = c("text", "date", "text", "text", "text",
                                     "text", "text", "text", "text")
  )
  
  clima <- read_excel(arquivo_campo_pop,
                      sheet = 2, 
                      col_types = c("text", "date", "text", "text", "text", 
                                    "text", "text", "text", "text", "text", 
                                    "text")
  )
  
  avistagens <- read_excel(arquivo_campo_pop,
                           sheet = 3, 
                           col_types = c("text", "date", "text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "text", "text", "text", "text", "text", 
                                         "text")
  )
  
  fotos <- read_excel(arquivo_campo_pop,
                      sheet = 4, 
                      col_types = c("text", "date", "text", "text", "text", 
                                     "text", "text", "text", "text", "text",
                                     "text", "text")
  )
  
  saidas$saida <- as.factor(saidas$saida)
  saidas$data <- ymd(saidas$data)
  saidas$barco <- as.factor(saidas$barco)
  saidas$WP_I_saida <- str_pad(saidas$WP_I_saida, 3, "left", "0")
  saidas$WP_F_saida <- str_pad(saidas$WP_F_saida, 3, "left", "0")
  saidas$rota <- as.factor(saidas$rota)
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
  
  avistagens$saida <- as.factor(avistagens$saida)
  avistagens$data <- ymd(avistagens$data)
  avistagens$grupo <- as.factor(avistagens$grupo)
  avistagens$num_fotos <- as.factor(avistagens$num_fotos)
  avistagens$WP_I_grupo <- str_pad(avistagens$WP_I_grupo, 3, "left", "0")
  avistagens$WP_F_grupo <- str_pad(avistagens$WP_F_grupo, 3, "left", "0")
  avistagens$coesao <- as.factor(avistagens$coesao)
  avistagens$estado <- as.factor(avistagens$estado)
  avistagens$tam_grupo <- as.factor(avistagens$tam_grupo)
  avistagens$tam_min <- as.factor(avistagens$tam_min)
  avistagens$tam_max <- as.factor(avistagens$tam_max)
  avistagens$n_neonatos <- as.factor(avistagens$n_neonatos)
  avistagens$n_infantes <- as.factor(avistagens$n_infantes)
  avistagens$n_juvenis <- as.factor(avistagens$n_juvenis)
  avistagens$n_adultos <- as.factor(avistagens$n_adultos)
  
  fotos$saida <- as.factor(fotos$saida)
  fotos$grupo <- as.factor(fotos$grupo)
  fotos$datahora_foto <- ymd_hms(fotos$datahora_foto)
  fotos$ID <- as.factor(fotos$ID)
  fotos$lado <- as.factor(fotos$lado)
  fotos$quali_F <- as.factor(fotos$quali_F)
  fotos$quali_M <- as.factor(fotos$quali_M)
  fotos$lng_foto <- as.double(fotos$lng_foto)
  fotos$lat_foto <- as.double(fotos$lat_foto)
  
  #Junção de todas tabelas em uma lista
  list_pop <- list(saidas = saidas,
                   clima = clima,
                   avistagens = avistagens,
                   fotos = fotos)
  
  invisible(list_pop)
  
  return(list_pop)
}

