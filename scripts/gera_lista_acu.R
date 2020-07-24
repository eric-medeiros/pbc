gera_lista_acu <- function (pasta_EXCEL_acu, pasta_GPS, pasta_SONDA) {
  
  source("../scripts/le_ficha_acu.R")
  source("../scripts/le_WPs.R")
  source("../scripts/le_rota_pontos.R")
  source("../scripts/le_sonda.R")
  source("../scripts/junta_WP_acu.R")
  source("../scripts/junta_rota.R")
  source("../scripts/junta_sonda.R")
  
  list_acu <- le_ficha_acu(pasta_EXCEL_acu)
  dados_WP  <- le_WPs(pasta_GPS)
  dados_rotas  <- le_rota_pontos(pasta_GPS)
  dados_sonda  <- le_sonda(pasta_SONDA)  
  
  lista_acustica <- list_acu %>%
    junta_WP_acu(dados_WP) %>%
    junta_rota(dados_rotas) %>%
    junta_sonda(dados_sonda)
  
  invisible(lista_acustica)
  
  return(lista_acustica)
}
