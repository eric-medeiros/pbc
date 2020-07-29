gera_lista_com <- function ( arquivo_EXCEL_comportamento, pasta_GPS, pasta_SONDA ) {

  source("scripts/le_ficha_com.R")
  source("scripts/le_WPs.R")
  source("scripts/le_rota_pontos.R")
  source("scripts/le_sonda.R")
  source("scripts/junta_WP_com.R")
  source("scripts/junta_rota.R")
  source("scripts/junta_sonda.R")
  
  list_com <- le_ficha_com(pasta_EXCEL_com)
  dados_WP  <- le_WPs(pasta_GPS)
  dados_rotas  <- le_rota_pontos(pasta_GPS)
  dados_sonda  <- le_sonda(pasta_SONDA)  

  lista_comportamental <- list_com %>%
    junta_WP_com(dados_WP) %>%
    junta_rota(dados_rotas) %>%
    junta_sonda(dados_sonda)
  
  invisible(lista_comportamental)
  
  return(lista_comportamental)
}

