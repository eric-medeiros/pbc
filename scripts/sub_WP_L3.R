sub_WP_L3 <- function(pasta_L3) {
  
  source("scripts/le_planilha_L3.R")
  source("scripts/le_WPs_L.R")
  
  lista_L3 <- le_planilha_L3(pasta_L3)
  dados_WP <- le_WPs_L(pasta_L3)
  
  WP_sub <- lista_L3$WP_extras %>%
    transmute("registro_PONTOS" = "extra",
              "saida" = saida,
              "datahora" = datahora_extra,
              "WP" = WP_extra,
              "lng" = lng_extra,
              "lat" = lat_extra) %>%
    bind_rows(dados_WP)
  
  lista_L3[[1]] <- lista_L3[[1]] %>%
    left_join(WP_sub, by = c(saida = "saida", WP_I = "WP")) %>%
    select(c(1:3, 6 , "datahora_I" = 14, "lng_I" = 15, "lat_I" = 16, 7:12)) %>%
    left_join(WP_sub, by = c(saida = "saida", WP_F = "WP")) %>%
    select(c(1:8, "datahora_F" = 15, "lng_F" = 16, "lat_F" = 17, 9:13))
  
  lista_L3[[2]] <- lista_L3[[2]] %>%
    left_join(WP_sub, by = c(saida = "saida", WP_I = "WP")) %>%
    select(c(1, 2, 5, "datahora_I" = 16, "lng_I" = 17, "lat_I" = 18, 6:14)) %>%
    left_join(WP_sub, by = c(saida = "saida", WP_F = "WP")) %>%
    select(c(1:7, "datahora_F" = 17, "lng_F" = 18, "lat_F" = 18, 8:15))
  
  lista_L3[[3]] <- lista_L3[[3]] %>%
    left_join(WP_sub, by = c(saida = "saida", WP_I = "WP")) %>%
    select(c(1:3, 6, "datahora_I" = 10, "lng_I" = 11, "lat_I" = 12, 7, 8)) %>%
    left_join(WP_sub, by = c(saida = "saida", WP_F = "WP")) %>%
    select(c(1:8, "datahora_F" = 11, "lng_F" = 12, "lat_F" = 13, 9, 10))
  
  lista_L3$WP_extras <- NULL
  
  invisible(lista_L3)
  
  return(lista_L3)  
}
