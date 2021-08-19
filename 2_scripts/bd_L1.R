bd_L1 <- function ( pasta_L1 ) {
  library(magrittr)
  
  source("2_scripts/sub_WP_L1.R")
  source("2_scripts/le_sonda_L.R")
  source("2_scripts/junta_sonda_L.R")
  source("2_scripts/le_rota_L.R")
  source("2_scripts/junta_rota_L.R")

  bd_L1 <- sub_WP_L1(pasta_L1) %>%
    junta_sonda_L(dados_sonda = le_sonda_L(pasta_L1)) %>%
    junta_rota_L(dados_rotas = le_rota_L(pasta_L1))
  
  invisible(bd_L1)
  
  return(bd_L1)
}
