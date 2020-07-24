gera_lista_pop <- function (pasta_EXCEL_pop, pasta_GPS, pasta_SONDA, pasta_CAT) {

  source("../scripts/le_ficha_pop.R")
  source("../scripts/le_WPs.R")
  source("../scripts/le_exif.R")
  source("../scripts/le_rota_pontos.R")
  source("../scripts/le_sonda.R")
  source("../scripts/junta_WP_pop.R")
  source("../scripts/junta_exif_pop.R")
  source("../scripts/junta_rota.R")
  source("../scripts/junta_sonda.R")
  
  list_pop <- le_ficha_pop(pasta_EXCEL_pop)
  dados_WP  <- le_WPs(pasta_GPS)
  dados_fotos  <- le_exif(pasta_CAT)
  dados_rotas  <- le_rota_pontos(pasta_GPS)
  dados_sonda  <- le_sonda(pasta_SONDA)  

  lista_populacional <- list_pop %>%
    junta_WP_pop(dados_WP) %>%
    junta_exif_pop(dados_fotos) %>%
    junta_rota(dados_rotas) %>%
    junta_sonda(dados_sonda)
  
  invisible(lista_populacional)
  
  return(lista_populacional)
}
