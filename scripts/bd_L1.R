bd_L1 <- function ( pasta_L1 ) {
  
  source("scripts/sub_WP_L1.R")
  source("scripts/le_sonda_L.R")
  source("scripts/junta_sonda_L.R")
  source("scripts/le_rota_L.R")
  source("scripts/junta_rota_L.R")

  bd_L1 <- sub_WP_L1(pasta_L1) %>%
    junta_sonda_L(dados_sonda = le_sonda_L(pasta_L1)) %>%
    junta_rota_L(dados_rota = le_rota_L(pasta_L1))
    
  bd_L1$avistagens <- bd_L1$avistagens %>%
    mutate(tempo_grupo = bd_L1$avistagens$datahora_F - bd_L1$avistagens$datahora_I) %>%
    rowwise()%>%
    mutate(tam = nafill(tam_grupo, fill = 0L) + nafill(mean(c(tam_min, tam_max), na.rm = TRUE), fill = 0L, nan = NA))
  
  
  bd_L1$saidas <- bd_L1$avistagens %>%
    select(1:4, 23, 24) %>%
    group_by(saida) %>%
    summarise(SAIDA = first(saida),
              DATA = first(data),
              BOTOS = round(sum(tam), 0),
              T_BOTO = seconds_to_period(sum(tempo_grupo, na.rm = TRUE)),
              GRUPOS = n(),
              FOTOS = sum(num_fotos)) %>%
    left_join(bd_L1$saidas, by = "saida") %>%
    select(1, 8:22, 2:7)
    
  bd_L1$saidas <- bd_L1$rota[c(2, 6, 7)] %>%
    left_join(bd_L1$saidas, by = "saida") %>%
    select(1, 4:18, 19:24, 2, 3)

  return(bd_L1)
}
