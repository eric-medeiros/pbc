bd_L2 <- function ( pasta_L2 ) {

  source("scripts/sub_WP_L2.R")
  source("scripts/le_sonda_L.R")
  source("scripts/junta_sonda_L.R")
  source("scripts/le_rota_L.R")
  source("scripts/junta_rota_L.R")
  
  bd_L2 <- sub_WP_L2(pasta_L2) %>%
    junta_sonda_L(dados_sonda = le_sonda_L(pasta_L2)) %>%
    junta_rota_L(dados_rota = le_rota_L(pasta_L2))
  
  bd_L2$avistagens <- bd_L2$avistagens %>%
    mutate(tempo_grupo = bd_L2$avistagens$datahora_F - bd_L2$avistagens$datahora_I) %>%
    rowwise()%>%
    mutate(tam = nafill(tam_grupo, fill = 0L) + nafill(mean(c(tam_min, tam_max), na.rm = TRUE), fill = 0L, nan = NA))
  
  bd_L2$saidas <- bd_L2$avistagens %>%
    select(1:4, 30, 31) %>%
    group_by(saida) %>%
    summarise(SAIDA = first(saida),
              DATA = first(data),
              BOTOS = round(sum(tam), 0),
              T_BOTO = seconds_to_period(sum(tempo_grupo, na.rm = TRUE)),
              GRUPOS = n()) %>%
    left_join(bd_L2$saidas, by = "saida") %>%
    select(1, 7:21, 2:6)
  
  bd_L2$saidas <- bd_L2$comportamento %>%
    rowwise() %>%
    mutate(comps = sum(na, dois_grupos, varios_grupos, cre, esc, cond, rvz, int, bar, mud_dir, mud_coe_af, mud_tam_gru, na.rm = TRUE)) %>%
    group_by(saida) %>%
    summarise(COMPS = sum(comps)) %>%
    left_join(bd_L2$saidas, by = "saida") %>%
    select(1, 3:22, 2)
  
  bd_L2$saidas <- bd_L2$rota[c(2, 6, 7)] %>%
    left_join(bd_L2$saidas, by = "saida") %>%
    select(1, 4:24, 2, 3)
  
  invisible(bd_L2)
  
  return(bd_L2)
}