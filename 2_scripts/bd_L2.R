bd_L2 <- function ( pasta_L2 ) {
library(magrittr)
  
  source("2_scripts/sub_WP_L2.R")
  source("2_scripts/le_sonda_L.R")
  source("2_scripts/junta_sonda_L.R")
  source("2_scripts/le_rota_L.R")
  source("2_scripts/junta_rota_L.R")
  
  bd_L2 <- sub_WP_L2(pasta_L2) %>%
    junta_sonda_L(dados_sonda = le_sonda_L(pasta_L2)) %>%
    junta_rota_L(dados_rota = le_rota_L(pasta_L2))
  
  invisible(bd_L2)
  
  return(bd_L2)
}