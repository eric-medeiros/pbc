# Função para abrir todas abas do arquivo *.xls de campo

le_ficha_com <- function(pasta_EXCEL_com) {
  library(readxl)
  library(stringr)
  library(lubridate)
  
  arquivo_campo_comportamento <- paste(pasta_EXCEL_com, "/comportamento_PBC.xlsx", sep = "")
  
  saidas <- read_excel(arquivo_campo_comportamento, 
                        sheet = 1, 
                        col_types = c("text", "date", "text", "text", "text", 
                                      "text", "text", "text", "text")
  )
  
  clima <- read_excel(arquivo_campo_comportamento,
                       sheet = 2, 
                       col_types = c("text", "date", "text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text")
  )
  
  avistagens <- read_excel(arquivo_campo_comportamento,
                            sheet = 3, 
                            col_types = c("text", "date", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text")
  )
  
  comportamento <- read_excel(arquivo_campo_comportamento,
                               sheet = 4, 
                               col_types = c("text", "date", "text", "text", "text",
                                             "text", "text")
  )
  
  saidas$saida <- as.factor(saidas$saida)
  saidas$data <- ydm(saidas$data)
  saidas$barco <- as.factor(saidas$barco)
  saidas$WP_I_saida <- str_pad(saidas$WP_I_saida, 3, "left", "0")
  saidas$WP_F_saida <- str_pad(saidas$WP_F_saida, 3, "left", "0")
  saidas$rota <- as.factor(saidas$rota)
  saidas$equipe <- as.factor(saidas$equipe)
  saidas$litros_abastecidos <- as.double(saidas$litros_abastecidos)
  
  clima$saida <- as.factor(clima$saida)
  clima$data <- ydm(clima$data)
  clima$WP_I_clima <- str_pad(clima$WP_I_clima, 3, "left", "0")
  clima$WP_F_clima <- str_pad(clima$WP_F_clima, 3, "left", "0")
  clima$dir_vento <- as.factor(clima$dir_vento)
  clima$veloc_vento <- as.double(clima$veloc_vento)
  clima$beaufort <- as.factor(clima$beaufort)
  clima$cobert_nuvens <- as.factor(clima$cobert_nuvens)
  clima$visibilidade <- as.factor(clima$visibilidade)
  clima$reflexo <- as.factor(clima$reflexo)

  avistagens$saida <- as.factor(avistagens$saida)
  avistagens$data <- ydm(avistagens$data)
  avistagens$grupo <- as.factor(avistagens$grupo)
  avistagens$agrupamento <- as.logical(avistagens$agrupamento)
  avistagens$embarcacao <- as.logical(avistagens$embarcacao)
  avistagens$agrupamento_embarcacao <- as.logical(avistagens$agrupamento_embarcacao)
  avistagens$WP_I <- str_pad(avistagens$WP_I, 3, "left", "0")
  avistagens$WP_F <- str_pad(avistagens$WP_F, 3, "left", "0")
  avistagens$motor <- as.factor(avistagens$motor)
  avistagens$tipo <- as.factor(avistagens$tipo)
  avistagens$tempo <- as.factor(avistagens$tempo)
  avistagens$distancia <- as.factor(avistagens$distancia)
  avistagens$velocidade <- as.factor(avistagens$tempo)
  avistagens$fotoID <- as.factor(avistagens$fotoID)
  avistagens$coesao <- as.factor(avistagens$coesao)
  avistagens$estado <- as.factor(avistagens$estado)
  avistagens$tam_grupo <- as.factor(avistagens$tam_grupo)
  avistagens$tam_min <- as.factor(avistagens$tam_min)
  avistagens$tam_max <- as.factor(avistagens$tam_max)
  avistagens$n_neonatos <- as.factor(avistagens$n_neonatos)
  avistagens$n_infantes <- as.factor(avistagens$n_infantes)
  avistagens$n_juvenis <- as.factor(avistagens$n_juvenis)
  avistagens$n_adultos <- as.factor(avistagens$n_adultos)

  comportamento$saida <- as.factor(comportamento$saida)
  comportamento$data <- ydm(comportamento$data)
  comportamento$grupo <- as.factor(comportamento$grupo)
  comportamento$resposta_positiva <- as.factor(comportamento$resposta_positiva)
  comportamento$resposta_neutra <- as.factor(comportamento$resposta_neutra)
  comportamento$resposta_negativa <- as.factor(comportamento$resposta_negativa)
  
  #Junção de todas tablas em uma lista
  list_com <- list(saidas = saidas,
                   clima = clima,
                   avistagens = avistagens,
                   comportamento = comportamento)
  
  invisible(list_com)
  
  return(list_com)
  
}