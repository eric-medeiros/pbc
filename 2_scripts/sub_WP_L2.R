sub_WP_L2 <- function(pasta_L2) {
  
  source("2_scripts/le_planilha_L2.R")
  source("2_scripts/le_WPs_L.R")
  
  lista_L2 <- le_planilha_L2(pasta_L2)
  dados_WP <- le_WPs_L(pasta_L2)
  
  WP_sub <- lista_L2$WP_extras %>%
    transmute("registro_PONTOS" = "extra",
              "saida" = saida,
              "datahora" = datahora_extra,
              "WP" = WP_extra,
              "lng" = lng_extra,
              "lat" = lat_extra) %>%
    bind_rows(dados_WP)
  
  lista_L2[[1]] <- lista_L2[[1]] %>%
    left_join(WP_sub, by = c(saida = "saida", WP_I = "WP")) %>%
    dplyr::select(c(1:4, "datahora_I" = 12, "lng_I" = 13, "lat_I" = 14, 5:10)) %>%
    left_join(WP_sub, by = c(saida = "saida", WP_F = "WP")) %>%
    dplyr::select(c(1:8, "datahora_F" = 15, "lng_F" = 16, "lat_F" = 17, 9:13))
  
  lista_L2[[2]] <- lista_L2[[2]] %>%
    left_join(WP_sub, by = c(saida = "saida", WP_I = "WP")) %>%
    dplyr::select(c(1:4, "datahora_I" = 8, "lng_I" = 9, "lat_I" = 10, 5:6)) %>%
    left_join(WP_sub, by = c(saida = "saida", WP_F = "WP")) %>%
    dplyr::select(c(1:8, "datahora_F" = 11, "lng_F" = 12, "lat_F" = 13, 9))
  
  lista_L2[[3]] <- lista_L2[[3]] %>%
    left_join(WP_sub, by = c(saida = "saida", WP_I = "WP")) %>%
    dplyr::select(c(1:3, "datahora_I" = 13, "lng_I" = 14, "lat_I" = 15, 4:11)) %>%
    left_join(WP_sub, by = c(saida = "saida", WP_F = "WP")) %>%
    dplyr::select(c(1:7, "datahora_F" = 16, "lng_F" = 17, "lat_F" = 18, 8:14))
  
  lista_L2[[4]] <- lista_L2[[4]] %>%
    left_join(WP_sub, by = c(saida = "saida", WP_I = "WP")) %>%
    dplyr::select(c(1:6, "datahora_I" = 20, "lng_I" = 21, "lat_I" = 22, 7:18)) %>%
    left_join(WP_sub, by = c(saida = "saida", WP_F = "WP")) %>%
    dplyr::select(c(1:10, "datahora_F" = 23, "lng_F" = 24, "lat_F" = 25, 11:21))
  
  lista_L2$WP_extras <- NULL
  
  invisible(lista_L2)
  
  return(lista_L2)  
}
