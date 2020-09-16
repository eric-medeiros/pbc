sub_WP_L1 <- function(pasta_L1) {
  
  source("scripts/le_planilha_L1.R")
  source("scripts/le_WPs_L.R")
  
  lista_L1 <- le_planilha_L1(pasta_L1)
  dados_WP <- le_WPs_L(pasta_L1)
  
  WP_sub <- lista_L1$WP_extras %>%
    transmute("registro_PONTOS" = "extra",
              "saida" = saida,
              "datahora" = datahora_extra,
              "WP" = WP_extra,
              "lng" = lng_extra,
              "lat" = lat_extra) %>%
    bind_rows(dados_WP)
  
  lista_L1[[1]] <- lista_L1[[1]] %>%
    left_join(WP_sub, by = c(saida = "saida", WP_I = "WP")) %>%
    select(c(1:4, "datahora_I" = 12, "lng_I" = 13, "lat_I" = 14, 5:10)) %>%
    left_join(WP_sub, by = c(saida = "saida", WP_F = "WP")) %>%
    select(c(1:8, "datahora_F" = 15, "lng_F" = 16, "lat_F" = 17, 9:13))
  
  lista_L1[[2]] <- lista_L1[[2]] %>%
    left_join(WP_sub, by = c(saida = "saida", WP_I = "WP")) %>%
    select(c(1:4, "datahora_I" = 8, "lng_I" = 9, "lat_I" = 10, 5:6)) %>%
    left_join(WP_sub, by = c(saida = "saida", WP_F = "WP")) %>%
    select(c(1:8, "datahora_F" = 11, "lng_F" = 12, "lat_F" = 13, 9))
  
  lista_L1[[3]] <- lista_L1[[3]] %>%
    left_join(WP_sub, by = c(saida = "saida", WP_I = "WP")) %>%
    select(c(1:3, "datahora_I" = 13, "lng_I" = 14, "lat_I" = 15, 4:11)) %>%
    left_join(WP_sub, by = c(saida = "saida", WP_F = "WP")) %>%
    select(c(1:7, "datahora_F" = 16, "lng_F" = 17, "lat_F" = 18, 8:14))
  
  lista_L1[[4]] <- lista_L1[[4]] %>%
    left_join(WP_sub, by = c(saida = "saida", WP_I = "WP")) %>%
    select(c(1:5, "datahora_I" = 18, "lng_I" = 19, "lat_I" = 20, 6:16)) %>%
    left_join(WP_sub, by = c(saida = "saida", WP_F = "WP")) %>%
    select(c(1:9, "datahora_F" = 21, "lng_F" = 22, "lat_F" = 23, 10:19))
  
  lista_L1$WP_extras <- NULL
  
  invisible(lista_L1)
  
  return(lista_L1)  
}
