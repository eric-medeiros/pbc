bd_L3 <- function ( pasta_L3 ) {
  
  source("scripts/sub_WP_L3.R")
  source("scripts/le_sonda_L.R")
  source("scripts/junta_sonda_L.R")
  source("scripts/le_rota_L.R")
  source("scripts/junta_rota_L.R")
  
  bd_L3 <- sub_WP_L3(pasta_L3) %>%
    junta_sonda_L(dados_sonda = le_sonda_L(pasta_L3)) %>%
    junta_rota_L(dados_rota = le_rota_L(pasta_L3))
  
  bd_L3$saidas <- bd_L3$gravacoes %>%
    group_by(saida) %>%
    summarise(SAIDA = first(saida),
              DATA = first(data),
              RECS = n()) %>%
    right_join(bd_L3$saidas, by = "saida") %>%
    select(1, 5:19, 2:4)
  
  bd_L3$saidas <- bd_L3$estacoes %>%
    group_by(saida) %>%
    summarise(T_REC = seconds_to_period(sum(period_to_seconds(as.period(datahora_F - datahora_I))))) %>%
    right_join(bd_L3$saidas, by = "saida") %>%
    select(1, 3:20, 2)
  
  bd_L3$saidas <- bd_L3$assobios %>%
    group_by(saida) %>%
    summarise(ASSOBIOS = n()) %>%
    right_join(bd_L3$saidas, by = "saida") %>%
    select(1, 3:21, 2)
  
  bd_L3$saidas <- bd_L3$rota[c(2, 6, 7)] %>%
    right_join(bd_L3$saidas, by = "saida") %>%
    select(1, 4:23, 2, 3)
  
  return(bd_L3)
}