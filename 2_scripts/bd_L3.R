bd_L3 <- function ( pasta_L3 ) {
  library(magrittr)
  
  source("2_scripts/sub_WP_L3.R")
  source("2_scripts/le_sonda_L.R")
  source("2_scripts/junta_sonda_L.R")
  source("2_scripts/le_rota_L.R")
  source("2_scripts/junta_rota_L.R")
  source("2_scripts/le_raven_L3.R")
  source("2_scripts/junta_raven_L3.R")
  source("2_scripts/le_audios_L3.R")
  source("2_scripts/junta_audios_L3.R")
  
  bd_L3 <- sub_WP_L3(pasta_L3) %>%
    junta_sonda_L(dados_sonda = le_sonda_L(pasta_L3)) %>%
    junta_rota_L(dados_rota = le_rota_L(pasta_L3)) %>%
    junta_raven_L3(dados_raven = le_raven_L3(pasta_L3)) %>%
    junta_audios_L3(dados_audios = le_audios_L3(pasta_L3))
  
  invisible(bd_L3)  
  
  return(bd_L3)
}