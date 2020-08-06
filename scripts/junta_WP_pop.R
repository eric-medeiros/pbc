junta_WP_pop <- function (list_pop, dados_WP) {
  library(stringr)
  library(dplyr)
  
  # União das lng/lat dos WPs em saidas ----
  list_pop$saidas$data_WP_I_saida <- str_c(as.character(list_pop$saidas$data),
                                           "-",
                                           list_pop$saidas$WP_I_saida)
  
  list_pop$saidas$data_WP_F_saida <- str_c(as.character(list_pop$saidas$data),
                                           "-",
                                           list_pop$saidas$WP_F_saida)
  
  dados_WP$data_WP_WP <- str_c(as.character(as.Date(dados_WP$datahora_WP)),
                               "-",
                               dados_WP$WP_WP)
  
  list_pop$saidas <- list_pop$saidas %>%
    left_join(dados_WP,
              by = c("data_WP_I_saida" = "data_WP_WP"))
  
  list_pop$saidas <- list_pop$saidas %>% 
    rename(datahora_I = datahora_WP,
           lng_I = lng_WP,
           lat_I = lat_WP,
           WP_I = WP_WP)
  
  list_pop$saidas <- list_pop$saidas %>%
    dplyr::select(saida,
                  data,
                  barco,
                  datahora_I,
                  lng_I,
                  lat_I,
                  WP_I,
                  rota,
                  equipe,
                  litros_abastecidos,
                  OBS,
                  data_WP_F_saida)
  
  # Acima foi trocado os WPs iniciais e abaixo os WPs finais.
  
  list_pop$saidas <- list_pop$saidas %>%
    left_join(dados_WP,
              by= c("data_WP_F_saida" = "data_WP_WP"))
  
  list_pop$saidas <- list_pop$saidas %>% 
    rename(datahora_F = datahora_WP,
           lng_F = lng_WP,
           lat_F = lat_WP,
           WP_F = WP_WP)
  
  list_pop$saidas$tempo_saida <- ms(difftime(list_pop$saidas$datahora_F, list_pop$saidas$datahora_I))
  
  list_pop$saidas <- list_pop$saidas %>%
    dplyr::select(saida,
                  data,
                  barco,
                  datahora_I,
                  lng_I,
                  lat_I,
                  WP_I,
                  datahora_F,
                  lng_F,
                  lat_F,
                  WP_F,
                  tempo_saida,
                  rota,
                  equipe,
                  litros_abastecidos,
                  OBS)
  
  # Ajuste de dos tipos de variáveis de saidas
  list_pop$saidas$datahora_I <- ymd_hms(list_pop$saidas$datahora_I)
  list_pop$saidas$lng_I <- as.numeric(list_pop$saidas$lng_I)
  list_pop$saidas$lat_I <- as.numeric(list_pop$saidas$lat_I)
  list_pop$saidas$datahora_F <- ymd_hms(list_pop$saidas$datahora_F)
  list_pop$saidas$lng_F <- as.numeric(list_pop$saidas$lng_F)
  list_pop$saidas$lat_F <- as.numeric(list_pop$saidas$lat_F)
  
  # União das lng/lat dos WPs em clima ----
  
  list_pop$clima$data_WP_I_clima <- str_c(as.character(list_pop$clima$data),
                                          "-",
                                          list_pop$clima$WP_I_clima)
  
  list_pop$clima$data_WP_F_clima <- str_c(as.character(list_pop$clima$data),
                                          "-",
                                          list_pop$clima$WP_F_clima)
  
  list_pop$clima <- list_pop$clima %>%
    left_join(dados_WP,
              by = c("data_WP_I_clima" = "data_WP_WP"))
  
  list_pop$clima <- list_pop$clima %>%
    rename(datahora_I = datahora_WP,
           lng_I = lng_WP,
           lat_I = lat_WP,
           WP_I = WP_WP)
  
  list_pop$clima <- list_pop$clima %>%
    dplyr::select(saida,
                  datahora_I,
                  lng_I,
                  lat_I,
                  WP_I,
                  dir_vento,
                  veloc_vento,
                  beaufort,
                  cobert_nuvens,
                  visibilidade,
                  reflexo,
                  OBS,
                  data_WP_F_clima)
  
  # Acima foi trocado os WPs iniciais e abaixo os WPs finais.
  
  list_pop$clima <- list_pop$clima %>%
    left_join(dados_WP,
              by = c("data_WP_F_clima" = "data_WP_WP"))
  
  list_pop$clima <- list_pop$clima %>% 
    rename(datahora_F = datahora_WP,
           lng_F = lng_WP,
           lat_F = lat_WP,
           WP_F = WP_WP)

  list_pop$clima$tempo_clima <- ms(difftime(list_pop$clima$datahora_F, list_pop$clima$datahora_I)) 
  
  list_pop$clima <- list_pop$clima %>%
    dplyr::select(saida,
                  datahora_I,
                  lng_I,
                  lat_I,
                  WP_I,
                  datahora_F,
                  lng_F,
                  lat_F,
                  WP_F,
                  tempo_clima,
                  dir_vento,
                  veloc_vento,
                  beaufort,
                  cobert_nuvens,
                  visibilidade,
                  reflexo,
                  OBS)
  
  # Ajuste de dos tipos de variáveis de clima
  list_pop$clima$datahora_I <- ymd_hms(list_pop$clima$datahora_I)
  list_pop$clima$lng_I <- as.numeric(list_pop$clima$lng_I)
  list_pop$clima$lat_I <- as.numeric(list_pop$clima$lat_I)
  list_pop$clima$datahora_F <- ymd_hms(list_pop$clima$datahora_F)
  list_pop$clima$lng_F <- as.numeric(list_pop$clima$lng_F)
  list_pop$clima$lat_F <- as.numeric(list_pop$clima$lat_F)
  
  # União das lng/lat dos WPs em avistagens ----
  list_pop$avistagens$data_WP_I <- str_c(as.character(list_pop$avistagens$data),
                                         "-",
                                         list_pop$avistagens$WP_I_grupo)
  
  list_pop$avistagens$data_WP_F <- str_c(as.character(list_pop$avistagens$data),
                                         "-",
                                         list_pop$avistagens$WP_F_grupo)
  
  list_pop$avistagens <- list_pop$avistagens %>%
    left_join(dados_WP,
              by = c("data_WP_I" = "data_WP_WP"))
  
  list_pop$avistagens <- list_pop$avistagens %>%
    rename(datahora_I = datahora_WP,
           lng_I = lng_WP,
           lat_I = lat_WP,
           WP_I = WP_WP)
  
  list_pop$avistagens <- list_pop$avistagens %>%
    dplyr::select(saida,
                  data,
                  grupo,
                  datahora_I,
                  lng_I,
                  lat_I,
                  WP_I,
                  num_fotos,
                  estado,
                  coesao,
                  tam_grupo,
                  tam_min,
                  tam_max,
                  n_neonatos,
                  n_infantes,
                  n_juvenis,
                  n_adultos,
                  OBS,
                  data_WP_F)
  
  # Está funcionando, mas ficou confuso. Acima foi trocado os WPs iniciais e abaixo os WPs finais.
  
  list_pop$avistagens <- list_pop$avistagens %>%
    left_join(dados_WP,
              by = c("data_WP_F" = "data_WP_WP"))
  
  list_pop$avistagens <- list_pop$avistagens %>% 
    rename(datahora_F = datahora_WP,
           lng_F = lng_WP,
           lat_F = lat_WP,
           WP_F = WP_WP)
  
  list_pop$avistagens$tempo_grupo <- ms(difftime(list_pop$avistagens$datahora_F, list_pop$avistagens$datahora_I))
  
  list_pop$avistagens <- list_pop$avistagens %>%
    dplyr::select(saida,
                  data,
                  grupo,
                  datahora_I,
                  lng_I,
                  lat_I, 
                  WP_I,
                  datahora_F,
                  lng_F,
                  lat_F,
                  WP_F,
                  tempo_grupo,
                  num_fotos,
                  estado,
                  coesao,
                  tam_grupo,
                  tam_min,
                  tam_max,
                  n_neonatos, 
                  n_infantes, 
                  n_juvenis, 
                  n_adultos,
                  OBS)
  
  # Ajuste de dos tipos de variáveis de avistagens
  list_pop$avistagens$datahora_I <- ymd_hms(list_pop$avistagens$datahora_I)
  list_pop$avistagens$lng_I <- as.numeric(list_pop$avistagens$lng_I)
  list_pop$avistagens$lat_I <- as.numeric(list_pop$avistagens$lat_I)
  list_pop$avistagens$datahora_F <- ymd_hms(list_pop$avistagens$datahora_F)
  list_pop$avistagens$lng_F <- as.numeric(list_pop$avistagens$lng_F)
  list_pop$avistagens$lat_F <- as.numeric(list_pop$avistagens$lat_F)

  invisible(list_pop)
  
  return(list_pop)
}
